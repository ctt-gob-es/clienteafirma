package es.gob.afirma.core.misc.http;

import java.io.IOException;
import java.lang.reflect.Method;
import java.net.URL;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLHandshakeException;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.ui.CoreMessages;

/**
 * Procesa los errores de confianza en los certificados servidor para permitir a los usuarios importar
 * esos certificados en un almacen de confianza.
 */
public class SSLErrorProcessor implements HttpErrorProcessor {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String PROPERTY_HEADLESS = "headless"; //$NON-NLS-1$

	private static final String PROPERTY_SSL_HEADLESS = "sslOmitImportationDialog"; //$NON-NLS-1$

	private static boolean showingConfirmDialog = false;

	static final String TRUSTED_KS_PWD = "changeit"; //$NON-NLS-1$

	private boolean initialized = false;
	private boolean cancelled = false;
	private boolean headless;

	public SSLErrorProcessor() {
		this.headless = false;
	}

	public SSLErrorProcessor(final Properties params) {
		// Comprobamos si directamente nos indican que no se use entorno grafico
		this.headless = params != null
				&& (Boolean.parseBoolean(params.getProperty(PROPERTY_HEADLESS)) ||
						Boolean.parseBoolean(params.getProperty(PROPERTY_SSL_HEADLESS)));

		// Si podemos usar entornos grafico por lo indicado en la operacion,
		// comprobamos ahora si realmente lo tenemos disponible para poder usarlo
		if (!this.headless) {
			boolean headlessEnviroment = false;
			try {
				headlessEnviroment = !hasGraphicsEnvironment();
			}
			catch (final Throwable e) {
				headlessEnviroment = true;
			}
			this.headless = headlessEnviroment;
		}
	}

	/**
	 * Inicializa el objeto.
	 */
	private void initilize() {

		// Si no esta inicializado ya, lo hacemos
		if (!this.initialized) {
			// Si podemos usar entornos grafico por lo indicado en la operacion,
			// comprobamos ahora si realmente lo tenemos disponible para poder usarlo
			if (!this.headless) {
				boolean headlessEnviroment = false;
				try {
					headlessEnviroment = !hasGraphicsEnvironment();
				}
				catch (final Throwable e) {
					headlessEnviroment = true;
				}
				this.headless = headlessEnviroment;
			}
			this.initialized = true;
		}
	}

	/**
	 * Comprueba si hay un entorno gr&aacute;fico disponible.
	 * @return {@code true} si hay entorno gr&aacute;fico, {@code false} si no.
	 */
	private static boolean hasGraphicsEnvironment() {
		boolean headless;
		try {
			final Class<?> graphicsEnvironmentClass = Class.forName("java.awt.GraphicsEnvironment"); //$NON-NLS-1$
			final Method isHeadlessMethod = graphicsEnvironmentClass.getMethod("isHeadless"); //$NON-NLS-1$
			final Object headlessBoolean = isHeadlessMethod.invoke(null);
			headless = headlessBoolean instanceof Boolean ? ((Boolean) headlessBoolean).booleanValue() : true;
		}
		catch (final Throwable e) {
			LOGGER.warning("No se pudo comprobar si hay entorno gr&aacute;fico: " + e); //$NON-NLS-1$
			headless = true;
		}
		return !headless;
	}

	public SSLErrorProcessor(final boolean headless) {
		this.headless = headless;
	}



	@Override
	public byte[] processHttpError(final IOException cause, final UrlHttpManager urlManager,
			final String url, final int timeout, final UrlHttpMethod method,
			final Properties requestProperties) throws IOException {

		// Si la excepcion no es la que debemos tratar (error de establecimiento de SSL no provocado por el nombre del certificado),
		// relanzamos la excepcion sin hacer nada mas
		if (!(cause instanceof SSLHandshakeException)) {// || cause.getCause() instanceof java.security.cert.CertificateException) {
			LOGGER.info("El error no es de tipo SSLHandShake y no la tratamos"); //$NON-NLS-1$
			throw cause;
		}

		// Inicializamos el objeto
		initilize();

		// Si se nos ha pedido no mostrar dialogos, relanzamos la excepcion
		if (this.headless) {
			LOGGER.info("No se confia en el certificado SSL, pero no se pueden mostrar dialogos para importarlo en el almacen de confianza"); //$NON-NLS-1$
			throw cause;
		}

		// Nos aseguramos de que solo se muestre el dialogo de confirmacion
		// si no se esta mostrando ya

		if (showingConfirmDialog) {
			LOGGER.info("Ya se esta mostrando un dialogo de consulta para la importacion del certificado de confianza. Se omitira este."); //$NON-NLS-1$
			throw cause;
		}

		int userResponse;
		synchronized (LOGGER) {
			try {
				showingConfirmDialog = true;
				userResponse = AOUIFactory.showConfirmDialog(null,
						CoreMessages.getString("SSLRequestPermissionDialog.2", new URL(url).getHost()), //$NON-NLS-1$
						CoreMessages.getString("SSLRequestPermissionDialog.1"), //$NON-NLS-1$
						AOUIFactory.YES_NO_OPTION,
						AOUIFactory.WARNING_MESSAGE);
			}
			catch (final AOCancelledOperationException ex) {
				this.cancelled = true;
				throw cause;
			}
			catch (final Exception ex) {
				throw cause;
			}
			finally {
				showingConfirmDialog = false;
			}
		}

		if (userResponse != AOUIFactory.YES_OPTION) {
			LOGGER.info("El usuario no importo el certificado en el almacen de confianza"); //$NON-NLS-1$
			this.cancelled = true;
			throw cause;
		}

		// Descargamos los certificados SSL del servidor
		X509Certificate[] serverCerts;
		try {
			serverCerts = downloadFromRemoteServer(url);
		} catch (final Exception e) {
			LOGGER.severe("Error al descargar certificados SSL del servidor: " + e); //$NON-NLS-1$
			throw new IOException(e);
		}

		// Mostramos en el log los certificado que se van a importar
		for (final X509Certificate cert : serverCerts) {
			LOGGER.info("Se importa en caliente en el almacen de confianza el certificado con el numero de serie: " //$NON-NLS-1$
					+ AOUtil.hexify(cert.getSerialNumber().toByteArray(), false));
		}

		// Configuramos los certificados SSL en el almacen de confianza
		try {
			TrustStoreManager.getInstance().importCerts(serverCerts);
		} catch (final Exception e) {
			LOGGER.severe("Error al importar los certificados SSL en el almacen de confianza: " + e); //$NON-NLS-1$
			throw new IOException(e);
		}

		// Reconfiguramos el contexto SSL para que tenga en cuenta los nuevos certificados
		try {
			SslSecurityManager.configureAfirmaTrustManagers();
		} catch (final Exception e) {
			LOGGER.severe("Error reconfigurando el contexto SSL con los nuevos certificados: " + e); //$NON-NLS-1$
			throw new IOException(e);
		}

		// Reintentamos la conexion
		return urlManager.readUrl(url, timeout, method, requestProperties);
	}

	private static X509Certificate[] downloadFromRemoteServer(final String domainName) throws Exception {

		final URL url = new URL(domainName);
		final HttpsURLConnection conn = (HttpsURLConnection) url.openConnection();
		SslSecurityManager.disableSslChecks(conn);
		conn.connect();
		final Certificate[] trustedServerCerts = conn.getServerCertificates();
		conn.disconnect();

		X509Certificate[] certsToImport;
		if (trustedServerCerts.length > 1) {
			certsToImport = new X509Certificate[trustedServerCerts.length - 1];

			// Solo se obtienen los certificados emisores
			for (int i = 1 ; i < trustedServerCerts.length ; i++) {
				certsToImport[i - 1] = (X509Certificate) trustedServerCerts[i];
			}
		}
		else {
			certsToImport = new X509Certificate[] { (X509Certificate) trustedServerCerts[0] };
		}

		return certsToImport;
	}

	public boolean isCancelled() {
		return this.cancelled;
	}
}

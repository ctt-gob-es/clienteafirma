package es.gob.afirma.core.misc.http;

import java.io.IOException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.KeyStoreException;
import java.security.cert.Certificate;
import java.security.cert.CertificateParsingException;
import java.security.cert.X509Certificate;
import java.util.Collection;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLHandshakeException;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.InvalidDomainSSLCertificateException;
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

	public static  final String SYSTEM_PREFERENCE_BLOCK_AUTO_IMPORT_TRUSTED_CERTS = "blockAutoImportTrustedCerts"; //$NON-NLS-1$

	private static boolean showingConfirmDialog = false;

	private boolean blockAutoImportTrustedCerts = false;

	static final String TRUSTED_KS_PWD = "changeit"; //$NON-NLS-1$

	private boolean initialized = false;
	private boolean cancelled = false;
	private static boolean previouslyCancelled = false;
	private boolean headless;

	public SSLErrorProcessor() {
		this.headless = false;
	}

	public SSLErrorProcessor(final boolean headless) {
		this.headless = headless;
	}

	public SSLErrorProcessor(final Properties params) {
		// Comprobamos si directamente nos indican que no se use entorno grafico
		this.headless = params != null
				&& (Boolean.parseBoolean(params.getProperty(PROPERTY_HEADLESS)) ||
						Boolean.parseBoolean(params.getProperty(PROPERTY_SSL_HEADLESS)));
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
		initialize();

		// Comprobamos en las preferencias del sistema que se permite la importacion de certificados desde sitios no seguros
		if(this.blockAutoImportTrustedCerts) {
			LOGGER.severe("La importacion de certificados desde sitio no seguros no esta permitida."); //$NON-NLS-1$
			throw cause;
		}

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

		//Comprobamos que el certificado este expedido realmente para el dominio correcto
		final Certificate[] trustedServerCerts;
		try {
			trustedServerCerts = downloadFromRemoteServer(url);
			final boolean correctIssuer = checkCorrectIssuer(url, (X509Certificate) trustedServerCerts[0]);
			if (!correctIssuer) {
				throw new InvalidDomainSSLCertificateException(cause, url);
			}
		}
		catch (final InvalidDomainSSLCertificateException e) {
			LOGGER.severe("Se ha interrumpido la operacion al detectar una conexion con un entorno no seguro: " + e); //$NON-NLS-1$
			throw e;
		}
		catch (final Exception e) {
			LOGGER.severe("Error al descargar certificados SSL del servidor: " + e); //$NON-NLS-1$
			throw new IOException(e);
		}

		// Descargamos los certificados SSL del servidor
		X509Certificate[] serverCerts;
		try {
			serverCerts = getCertsToImport(trustedServerCerts);
		} catch (final Exception e) {
			LOGGER.severe("Error al descargar certificados SSL del servidor: " + e); //$NON-NLS-1$
			throw new IOException(e);
		}

		boolean allCertsAlreadyTrusted = true;

		for (final X509Certificate cert : serverCerts) {
		    try {
				if (!TrustStoreManager.getInstance().containsCert(cert)) {
				    allCertsAlreadyTrusted = false;
				    break;
				}
			} catch (final KeyStoreException e) {
				LOGGER.severe("Error al comprobar certificados SSL del servidor: " + e); //$NON-NLS-1$
				throw new IOException(e);
			}
		}

		if (!allCertsAlreadyTrusted) {

			int userResponse;
			synchronized (LOGGER) {
				try {
					// Comprobamos si anteriormente ya se cancelo la importacion del certificado, para no solicitar de nuevo que se importe
					if (previouslyCancelled) {
						throw new AOCancelledOperationException();
					}
					showingConfirmDialog = true;
					userResponse = AOUIFactory.showConfirmDialog(null,
							CoreMessages.getString("SSLRequestPermissionDialog.2", new URL(url).getHost()), //$NON-NLS-1$
							CoreMessages.getString("SSLRequestPermissionDialog.1"), //$NON-NLS-1$
							AOUIFactory.YES_NO_OPTION,
							AOUIFactory.WARNING_MESSAGE);
				}
				catch (final AOCancelledOperationException ex) {
					this.cancelled = true;
					previouslyCancelled = true;
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
				previouslyCancelled = true;
				throw cause;
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

		}

		// Reintentamos la conexion
		return urlManager.readUrl(url, timeout, method, requestProperties);
	}

	/**
	 * Inicializa el objeto.
	 */
	private void initialize() {

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

			// Comprobamos si se permite o no la importacion automatica de certificados no seguros
			final String blockAutoImportTrustedCertsProp = System.getProperty(SYSTEM_PREFERENCE_BLOCK_AUTO_IMPORT_TRUSTED_CERTS, "false"); //$NON-NLS-1$
			this.blockAutoImportTrustedCerts = Boolean.parseBoolean(blockAutoImportTrustedCertsProp);

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

	/**
	 * Transforma los certificados obtenidos y selecciona los certificados para importar.
	 * @param trustedServerCerts Certificados sin transformasr.
	 * @return Certificados ya transformados como X509Certificate.
	 * @throws Exception Error durante la transformaci&oacute;n.
	 */
	private static X509Certificate[] getCertsToImport(final Certificate[] trustedServerCerts) throws Exception {

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

	/**
	 * Comprueba que el certificado se ha emitido para este dominio.
	 * @param urlStr URL donde e encuentran los certificados.
	 * @param serverCert Certificado a comprobar
	 * @return true en caso de que sea correcto, false en caso contrario.
	 * @throws MalformedURLException Error en la estructura de la URL.
	 * @throws CertificateParsingException Error transformando certificado.
	 */
	private static boolean checkCorrectIssuer(final String urlStr, final X509Certificate serverCert)
	        throws MalformedURLException, CertificateParsingException {

	    final URL url = new URL(urlStr);
	    final String host = url.getHost().toLowerCase();

	    // Comprobamos el nombre de dominio contra los alternative names del certificado
	    final Collection<List<?>> alternativeNamesList = serverCert.getSubjectAlternativeNames();
	    if (alternativeNamesList != null && !alternativeNamesList.isEmpty()) {
	    	for (final List<?> name : alternativeNamesList) {
	    		final int type = ((Integer) name.get(0)).intValue();

	    		// Si es de tipo nombre de dominio (tipo 2) o ip (tipo 7) los comparamos,
	    		// pero en el caso de IP no admitimos wildcards
	    		if (type == 2 || type == 7) {
	    			final String dnsName = ((String) name.get(1)).toLowerCase();
	    			if (matchesHost(host, dnsName, type == 7)) {
	    				return true;
	    			}
	    		}
	    	}
	    }
	    // Comprobamos contra el common name si y solo si no habia alternative names
	    else {
	    	final String commonName = AOUtil.getRDNvalueFromLdapName("cn", serverCert.getSubjectX500Principal().toString()); //$NON-NLS-1$
	    	if (commonName != null && matchesHost(host, commonName.toLowerCase(), isIP(host))) {
				return true;
			}
	    }

	    return false;
	}

	/**
	 * Comprueba que el dominio para el que esta expedido el certificado obtenido es correcto.
	 * @param host Host de la URL.
	 * @param name Dominio/ip del certificado.
	 * @return {@code true} si es correcto, {@code false} en caso contrario.
	 */
	private static boolean matchesHost(final String host, final String name, final boolean hostIsIp) {

		// Si el nombre de host es el del certificado, es valido
	    if (name.equals(host)) {
	        return true;
	    }

	    // Si era una IP o el nombre no tenia wildcard o no era un nombre valido, no admitimos mas opciones
	    if (hostIsIp || !name.startsWith("*.") || name.endsWith(".")) { //$NON-NLS-1$ //$NON-NLS-2$
	    	return false;
	    }

	    // Si el nombre de host no termina igual que el nombre del certificado,
	    // no es valido
	    final String sanDomain = name.substring(1);
	    if (!host.endsWith(sanDomain)) {
	    	return false;
	    }

	    // Comprobamos que el wildcard cubra un nivel de dominio del host y solo uno
	    final String hostPrefix = host.substring(0, host.length() - sanDomain.length());
	    return !hostPrefix.isEmpty() && !hostPrefix.contains("."); //$NON-NLS-1$
	}

	private static boolean isIP(final String hostname) {
		return isIPv4Literal(hostname) || isIPv6Literal(hostname);
	}

    private static boolean isIPv4Literal(final String s) {
        final String[] parts = s.split("\\."); //$NON-NLS-1$
        if (parts.length != 4) {
			return false;
		}
        try {
        	for (final String p : parts) {
        		final int v = Integer.parseInt(p);
        		 // Comprobamos que sea un numero valido
        		if (v < 0 || v > 255 || p.length() > 1 && p.startsWith("0")) { //$NON-NLS-1$
        			return false;
        		}
        	}
        }
        catch (final Exception e) {
        	// Habia algun elemento que no era un numero
        	return false;
        }
        return true;
    }

    private static boolean isIPv6Literal(final String s) {
        return s.contains(":"); //$NON-NLS-1$
    }

	/**
	 * Descarga los certificados del servidor.
	 * @param domainName Direcci&oacute;n del servidor.
	 * @return Certificados descargados.
	 * @throws Exception Error durante la desarga.
	 */
	private static Certificate[] downloadFromRemoteServer(final String domainName) throws Exception {

		final URL url = new URL(domainName);
		final HttpsURLConnection conn = (HttpsURLConnection) url.openConnection();
		SslSecurityManager.disableSslChecks(conn);
		conn.connect();
		final Certificate[] trustedServerCerts = conn.getServerCertificates();
		conn.disconnect();

		return trustedServerCerts;
	}

	/**
	 * Comprueba si el di&aacute;logo para importar certificados se ha cancelado.
	 * @return true si el certificado se ha cancelado, false en caso contrario.
	 */
	public boolean isCancelled() {
		return this.cancelled;
	}

}

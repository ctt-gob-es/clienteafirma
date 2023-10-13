package es.gob.afirma.standalone;

import java.io.IOException;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.http.SSLErrorProcessor;
import es.gob.afirma.core.misc.http.UrlHttpManager;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.http.UrlHttpManagerImpl;
import es.gob.afirma.core.misc.http.UrlHttpMethod;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;

/**
 * Clase para el acceso a recursos y servicios remotos. Esta clase debe gestionar todas las conexiones
 * externas realizadas directamente por la aplicaci&oacute;n.
 */
public class HttpManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final UrlHttpManager urlManager;

	/**
	 * Construye el objeto para el acceso a los recursos y servicios.
	 */
	public HttpManager() {
		this.urlManager = UrlHttpManagerFactory.getInstalledManager();
		setSecureConnections(PreferencesManager.getBoolean(
				PreferencesManager.PREFERENCE_GENERAL_SECURE_CONNECTIONS));
		setSecureDomains(PreferencesManager.get(PreferencesManager.PREFERENCE_GENERAL_SECURE_DOMAINS_LIST));
	}

	/**
	 * Accede a un recurso o servicio remoto.
	 * @param url URL del recurso/servicio.
	 * @param method M&eacute;todo HTTP de acceso.
	 * @return Contenido del recurso o resultado del servicio.
	 * @throws IOException Cuando ocurre un error durante la recuperaci&oacute;n del resultado.
	 */
	public byte[] readUrl(final String url, final UrlHttpMethod method) throws IOException {
		final SSLErrorProcessor errorProcessor = new SSLErrorProcessor();
		try {
			return this.urlManager.readUrl(url, method, errorProcessor);
		}
		catch (final IOException e) {
			if (errorProcessor.isCancelled()) {
				LOGGER.info("El usuario no permite la importacion del certificado SSL de confianza para el acceso a " //$NON-NLS-1$
						+ LoggerUtil.getTrimStr(url.toString()));
			}
			throw e;
		}
	}

//	/**
//	 * Accede a un recurso o servicio remoto.
//	 * @param url URL del recurso/servicio.
//	 * @param method M&eacute;todo HTTP de acceso.
//	 * @param processor Procesador de errores.
//	 * @return Contenido del recurso o resultado del servicio.
//	 * @throws IOException Cuando ocurre un error durante la recuperaci&oacute;n del resultado.
//	 */
//	public byte[] readUrl(final String url, final UrlHttpMethod method, final HttpProcessor processor) throws IOException {
//		return this.urlManager.readUrl(url, -1, null, null, method, processor);
//	}

	/**
	 * Accede a un recurso o servicio remoto.
	 * @param url URL del recurso/servicio.
	 * @param method M&eacute;todo HTTP de acceso.
	 * @return Contenido del recurso o resultado del servicio.
	 * @throws IOException Cuando ocurre un error durante la recuperaci&oacute;n del resultado.
	 */
	public byte[] readUrl(final StringBuilder url, final UrlHttpMethod method) throws IOException {
		return this.urlManager.readUrl(url.toString(), method);
	}

	/**
	 * Configura si deben respetarse o no las comprobaciones de seguridad en las conexiones HTTPS.
	 * @param secure {@code true} si deben respetarse las comprobaciones, {@code false} en caso
	 * contrario.
	 */
	public static void setSecureConnections(final boolean secure) {
		System.setProperty(
				UrlHttpManagerImpl.JAVA_PARAM_DISABLE_SSL_CHECKS,
				Boolean.toString(!secure));

	}

	/**
	 * Configura los dominios que se deben de tratar como seguros en las conexiones HTTPS
	 * @param secureDomains Lista de dominios seguros
	 */
	public static void setSecureDomains(final String secureDomains) {
		if (secureDomains != null && !secureDomains.trim().isEmpty()) {
			System.setProperty(UrlHttpManagerImpl.JAVA_PARAM_SECURE_DOMAINS_LIST, secureDomains);
		}
		else {
			System.clearProperty(UrlHttpManagerImpl.JAVA_PARAM_SECURE_DOMAINS_LIST);
		}
	}
}

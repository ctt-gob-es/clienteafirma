package es.gob.afirma.standalone.protocol;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.net.URL;
import java.net.UnknownHostException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.ErrorCode;
import es.gob.afirma.core.misc.protocol.ParameterLocalAccessRequestedException;
import es.gob.afirma.core.misc.protocol.UrlParameters;
import es.gob.afirma.core.misc.protocol.UrlParametersForBatch;
import es.gob.afirma.core.misc.protocol.UrlParametersToLoad;
import es.gob.afirma.core.misc.protocol.UrlParametersToSave;
import es.gob.afirma.core.misc.protocol.UrlParametersToSelectCert;
import es.gob.afirma.core.misc.protocol.UrlParametersToSign;
import es.gob.afirma.core.misc.protocol.UrlParametersToSignAndSave;
import es.gob.afirma.core.ui.AOUIFactory;

/**
 * Gestor de seguridad de las peticiones.
 */
public class SecurityManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private boolean localhostAllowed = false;
	private boolean localhostAllowedAsked = false;

	/**
	 * Comprueba si en los par&aacute;metros de firma de lote se encuentran direcciones
	 * a servicios locales.
	 * @param params Par&aacute;metros de la petici&oacute;n.
	 * @param byServices Indica si la comunicacion es por servidor intermedio.
	 * @throws ParameterLocalAccessRequestedException Cuando una URL apunte a un servicio local y no se permita.
	 */
	public void checkServices(final UrlParametersForBatch params, final boolean byServices) throws ParameterLocalAccessRequestedException {
		if (byServices) {
			checkCommonServices(params);
		}

		if (!this.localhostAllowed) {
			try {
				verifyAllowedHost(params.getBatchPresignerUrl());
			} catch (final Exception e) {
				throw new ParameterLocalAccessRequestedException("La URL del servicio de prefirma de lote", e, ErrorCode.Request.LOCAL_PRESIGN_BATCH_URL); //$NON-NLS-1$
			}
			try {
				verifyAllowedHost(params.getBatchPostSignerUrl());
			} catch (final Exception e) {
				throw new ParameterLocalAccessRequestedException("La URL del servicio de postfirma de lote", e, ErrorCode.Request.LOCAL_POSTSIGN_BATCH_URL); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Comprueba si en los par&aacute;metros de las peticiones de carga de ficheros se encuentran direcciones
	 * a servicios locales.
	 * @param params Par&aacute;metros de la petici&oacute;n.
	 * @param byServices Indica si la comunicacion es por servidor intermedio.
	 * @throws ParameterLocalAccessRequestedException Cuando una URL apunte a un servicio local y no se permita.
	 */
	public void checkServices(final UrlParametersToLoad params, final boolean byServices) throws ParameterLocalAccessRequestedException {
		if (byServices) {
			checkCommonServices(params);
		}
	}

	/**
	 * Comprueba si en los par&aacute;metros de las peticiones de guardado de ficheros se encuentran direcciones
	 * a servicios locales.
	 * @param params Par&aacute;metros de la petici&oacute;n.
	 * @param byServices Indica si la comunicacion es por servidor intermedio.
	 * @throws ParameterLocalAccessRequestedException Cuando una URL apunte a un servicio local y no se permita.
	 */
	public void checkServices(final UrlParametersToSave params, final boolean byServices) throws ParameterLocalAccessRequestedException {
		if (byServices) {
			checkCommonServices(params);
		}
	}

	/**
	 * Comprueba si en los par&aacute;metros de las peticiones de selecci&oacute;n de certificados se encuentran
	 * direcciones a servicios locales.
	 * @param params Par&aacute;metros de la petici&oacute;n.
	 * @throws ParameterLocalAccessRequestedException Cuando una URL apunte a un servicio local y no se permita.
	 */
	public void checkServices(final UrlParametersToSelectCert params, final boolean byServices) throws ParameterLocalAccessRequestedException {
		if (byServices) {
			checkCommonServices(params);
		}
	}

	/**
	 * Comprueba si en los par&aacute;metros de una petici&oacute;n de firma se encuentran
	 * direcciones a servicios locales.
	 * @param params Par&aacute;metros de la petici&oacute;n.
	 * @param byServices Indica si la comunicacion es por servidor intermedio.
	 * @throws ParameterLocalAccessRequestedException Cuando una URL apunte a un servicio local y no se permita.
	 */
	public void checkServices(final UrlParametersToSign params, final boolean byServices) throws ParameterLocalAccessRequestedException {
		if (byServices) {
			checkCommonServices(params);
		}

		if (!this.localhostAllowed) {
			final Properties config = params.getExtraParams();
			final String serverUrl = config.getProperty("serverUrl"); //$NON-NLS-1$
			try {
				verifyAllowedHost(serverUrl);
			} catch (final Exception e) {
				throw new ParameterLocalAccessRequestedException("La URL del servidor trifasico es local", e, ErrorCode.Request.TRIPHASE_SERVICE_URL_CANT_BE_LOCAL); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Comprueba si en los par&aacute;metros de una petici&oacute;n de firma y guardado se encuentran
	 * direcciones a servicios locales.
	 * @param params Par&aacute;metros de la petici&oacute;n.
	 * @param byServices Indica si la comunicacion es por servidor intermedio.
	 * @throws ParameterLocalAccessRequestedException Cuando una URL apunte a un servicio local y no se permita.
	 */
	public void checkServices(final UrlParametersToSignAndSave params, final boolean byServices) throws ParameterLocalAccessRequestedException {
		if (byServices) {
			checkCommonServices(params);
		}

		if (!this.localhostAllowed) {
			final Properties config = params.getExtraParams();
			final String serverUrl = config.getProperty("serverUrl"); //$NON-NLS-1$
			try {
				verifyAllowedHost(serverUrl);
			} catch (final Exception e) {
				throw new ParameterLocalAccessRequestedException("La URL del servidor trifasico es local", e, ErrorCode.Request.TRIPHASE_SERVICE_URL_CANT_BE_LOCAL); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Comprueba si en los par&aacute;metros comunes de las peticiones se encuentran direcciones
	 * a servicios locales.
	 * @param params Par&aacute;metros de la petici&oacute;n.
	 * @throws ParameterLocalAccessRequestedException Cuando una URL apunte a un servicio local y no se permita.
	 */
	private void checkCommonServices(final UrlParameters params) throws ParameterLocalAccessRequestedException {
		if (!this.localhostAllowed) {
			try {
				verifyAllowedHost(params.getRetrieveServletUrl());
			} catch (final Exception e) {
				throw new ParameterLocalAccessRequestedException("La URL de recuperacion del servidor intermedio es local", e, ErrorCode.Request.LOCAL_RETRIEVE_URL); //$NON-NLS-1$
			}
			try {
				verifyAllowedHost(params.getStorageServletUrl());
			} catch (final Exception e) {
				throw new ParameterLocalAccessRequestedException("La URL de guardado del servidor intermedio es local", e, ErrorCode.Request.LOCAL_STORAGE_URL); //$NON-NLS-1$
			}
		}
	}

	/**
	 * Comprueba que un listado de URL cumpla con los requisitos de seguridad. En caso de ser necesario,
	 * se preguntar&aacute; al usuario si desea permitir su uso.
	 * @param urls Listado de URL que comprobar.
	 * @throws Exception Cuando una URL apunte a un servicio local y no se permita.
	 */
	public void verifyAllowedHost(final String urlPlain) throws Exception {
		if (urlPlain == null || urlPlain.isEmpty()) {
			return;
		}
		URL url;
		try {
			url = new URL(urlPlain);
		}
		catch (final Exception e) {
			LOGGER.warning("Se recibe URL malformada por parametro: " + e); //$NON-NLS-1$
			return;
		}

		verifyAllowedHost(url);
	}

	/**
	 * Comprueba que un listado de URL cumpla con los requisitos de seguridad. En caso de ser necesario,
	 * se preguntar&aacute; al usuario si desea permitir su uso.
	 * @param urls Listado de URL que comprobar.
	 * @throws Exception Cuando una URL apunte a un servicio local y no se permita.
	 */
	public void verifyAllowedHost(final URL url) throws Exception {
		if (url == null || url.getHost() == null) {
			return;
		}

		if (!this.localhostAllowed) {
			try {
				checkLocalhost(url.getHost());
			}
			catch (final Exception e) {
				// Preguntamos al usuario si permite el acceso a servicios locales
				askToUserIfLocalhostIsAllowed();
				// Si no se permite, lazamos un error
				if (!this.localhostAllowed) {
					throw e;
				}
			}
		}
	}

	/**
	 * Pregunta al usuario (si no lo hizo anteriormente y obtuvo respuesta),
	 * si desea permitir el acceso a un servicio en una URL local.
	 */
	private void askToUserIfLocalhostIsAllowed() {
		if (!this.localhostAllowedAsked) {
			final int choice = AOUIFactory.showConfirmDialog(
					null,
					ProtocolMessages.getString("SecurityManager.2"), //$NON-NLS-1$
					ProtocolMessages.getString("SecurityManager.1"), //$NON-NLS-1$
					AOUIFactory.YES_NO_OPTION,
					AOUIFactory.WARNING_MESSAGE
			);
			this.localhostAllowed = choice == AOUIFactory.YES_OPTION;
			this.localhostAllowedAsked = true;
		}
	}

	/**
	 * Comprueba que el host indicado no sea local y, en caso de serlo, que el usuario
	 * est&aacute; de acuerdo con su uso.
	 * @param host Host de URL.
	 * @throws Exception Cuando se identifica una URL local.
	 */
	private static void checkLocalhost(final String host) throws Exception {
		if (host == null || host.isEmpty()) {
			return;
		}

		final String lowerHost = host.toLowerCase().trim();
		if ("localhost".equals(lowerHost) || "127.0.0.1".equals(lowerHost) || "0.0.0.0".equals(lowerHost) || //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				"[::1]".equals(lowerHost) || "::1".equals(lowerHost) || "[::]".equals(lowerHost) || "::".equals(lowerHost) || //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				"localhost.localdomain".equals(lowerHost)) { //$NON-NLS-1$
			throw new Exception("La URL proporcionada se corresponde utiliza un identificador del host local: " + host); //$NON-NLS-1$
		}

		try {
			final InetAddress inetAddress = InetAddress.getByName(host);
			if (inetAddress.isLoopbackAddress() || inetAddress.isAnyLocalAddress()) {
				throw new Exception("La URL proporcionada se resuelve al host local: " + host); //$NON-NLS-1$
			}
			if (NetworkInterface.getByInetAddress(inetAddress) != null) {
				throw new Exception("El host de la URL proporcionada se corresponde con una interfaz de red local: " + host); //$NON-NLS-1$
			}
		}
		catch (final UnknownHostException e) {
			LOGGER.log(Level.WARNING, "No se pudo resolver el host para verificar si es local: " + host, e); //$NON-NLS-1$
		}
		catch (final SocketException e) {
			LOGGER.log(Level.WARNING, "Error al comprobar si la direccion pertenece a una interfaz local: " + host, e); //$NON-NLS-1$
		}
	}

}

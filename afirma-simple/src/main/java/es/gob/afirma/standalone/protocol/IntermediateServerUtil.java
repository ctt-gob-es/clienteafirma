package es.gob.afirma.standalone.protocol;

import java.io.IOException;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.http.UrlHttpMethod;

/**
 * Clases para la interaccion con el servidor intermedio.
 */
public class IntermediateServerUtil {

	private static final String METHOD_OP_PUT = "put"; //$NON-NLS-1$
	private static final String SYNTAX_VERSION = "1_0"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/**
	 * Env&iacute;a datos al servidor intermedio.
	 * @param data Buffer con los datos a enviar.
	 * @param storageServiceUrl URL del servicio de guardado.
	 * @param id Identificador a asignar a los datos a subir al servidor.
	 * @throws IOException
	 */
	public static void sendData(final StringBuilder data, final String storageServiceUrl, final String id) throws IOException {

		final StringBuilder url = new StringBuilder(storageServiceUrl)
		.append("?op=").append(METHOD_OP_PUT) //$NON-NLS-1$
		.append("&v=").append(SYNTAX_VERSION) //$NON-NLS-1$
		.append("&id=").append(id) //$NON-NLS-1$
		.append("&dat=").append(data.toString()); //$NON-NLS-1$

		send(url);
	}

	/**
	 * Env&iacute;a datos al servidor intermedio.
	 * @param data Buffer con los datos a enviar.
	 * @param storageServiceUrl URL del servicio de guardado.
	 * @param id Identificador a asignar a los datos a subir al servidor.
	 * @throws IOException
	 */
	public static void sendData(final String data, final String storageServiceUrl, final String id) throws IOException {

		final StringBuilder url = new StringBuilder(storageServiceUrl)
		.append("?op=").append(METHOD_OP_PUT) //$NON-NLS-1$
		.append("&v=").append(SYNTAX_VERSION) //$NON-NLS-1$
		.append("&id=").append(id) //$NON-NLS-1$
		.append("&dat=").append(data.toString()); //$NON-NLS-1$

		send(url);
	}

	/**
	 * Env&iacute;a datos al servidor intermedio.
	 * @param data Buffer con los datos a enviar.
	 * @param storageServiceUrl URL del servicio de guardado.
	 * @param id Identificador a asignar a los datos a subir al servidor.
	 * @throws IOException
	 */
	private static void send(final StringBuilder url) throws IOException {

		// Llamamos al servicio para guardar los datos
		final byte[] result = UrlHttpManagerFactory.getInstalledManager().readUrl(url.toString(), UrlHttpMethod.POST);

		LOGGER.info("Resultado: " + new String(result)); //$NON-NLS-1$
	}
}

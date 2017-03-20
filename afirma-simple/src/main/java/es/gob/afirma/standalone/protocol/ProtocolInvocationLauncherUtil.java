package es.gob.afirma.standalone.protocol;

import java.io.IOException;
import java.util.Arrays;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.http.UrlHttpMethod;
import es.gob.afirma.core.misc.protocol.UrlParameters;
import es.gob.afirma.standalone.crypto.CypherDataManager;

final class ProtocolInvocationLauncherUtil {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private ProtocolInvocationLauncherUtil() {
		// No instanciable
	}

	static final class InvalidEncryptedDataLengthException extends Exception {

		private static final long serialVersionUID = 1L;

		InvalidEncryptedDataLengthException(final String msg) {
			super(msg);
		}
	}

	static final class DecryptionException extends Exception {

		private static final long serialVersionUID = 1L;

		DecryptionException(final String msg, final Throwable e) {
			super(msg, e);
		}
	}

	static byte[] getDataFromRetrieveServlet(final UrlParameters params) throws DecryptionException,
	                                                                                    InvalidEncryptedDataLengthException,
	                                                                                    IOException {
		// Preparamos la URL
		final StringBuilder dataUrl = new StringBuilder(
				params.getRetrieveServletUrl().toString()).
					append("?") //$NON-NLS-1$
						.append("op=get&v=1_0&id=") //$NON-NLS-1$
							.append(params.getFileId());

		LOGGER.info("Intentamos recuperar los datos del servidor con la URL:\n" + dataUrl.toString()); //$NON-NLS-1$

		// Leemos los datos
		final byte[] recoveredData = UrlHttpManagerFactory.getInstalledManager().readUrl(dataUrl.toString(), UrlHttpMethod.POST);

		// Si los datos recibidos representan un error, detenemos la ejecucion
		if (recoveredData.length > 8 && new String(Arrays.copyOf(recoveredData, 8)).toLowerCase().startsWith("err-")) { //$NON-NLS-1$
			LOGGER.severe("Se recupera un error desde el servidor intermedio: " + new String(recoveredData)); //$NON-NLS-1$
			throw new InvalidEncryptedDataLengthException("Se recupera un error desde el servidor intermedio: " + new String(recoveredData)); //$NON-NLS-1$
		}

		// Si no ha ocurrido un error, debemos haber recibido los datos cifrados
		final byte[] data;
		try {
			data = CypherDataManager.decipherData(recoveredData, params.getDesKey());
		}
		catch (final Exception e) {
			LOGGER.severe("Error en el descifrado de los datos: " + e); //$NON-NLS-1$
			throw new DecryptionException("Error en el descifrado de los datos: " + e, e); //$NON-NLS-1$
		}

		return data;
	}
}

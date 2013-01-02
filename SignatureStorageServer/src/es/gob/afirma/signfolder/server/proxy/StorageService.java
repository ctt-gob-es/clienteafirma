package es.gob.afirma.signfolder.server.proxy;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.URLDecoder;
import java.security.GeneralSecurityException;
import java.security.InvalidKeyException;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/** Servicio de almacenamiento temporal de firmas. &Uacute;til para servir de intermediario en comunicaci&oacute;n
 * entre JavaScript y <i>Apps</i> m&oacute;viles nativas.
 * @author Tom&aacute;s Garc&iacute;a-;er&aacute;s */
@WebServlet(description = "Servicio de almacenamiento temporal de firmas", urlPatterns = { "/StorageService" })
public final class StorageService extends HttpServlet {

	private static final long serialVersionUID = -3272368448371213403L;

	/** Fichero de configuraci&oacute;n. */
	private static final String CONFIG_FILE = "/configuration.properties"; //$NON-NLS-1$

	/** Juego de carateres UTF-8. */
	private static final String UTF8 = "utf-8"; //$NON-NLS-1$

	private static final String TMP_DIR = System.getProperty("java.io.tmpdir") + File.separator + "afirma"; //$NON-NLS-1$ //$NON-NLS-2$

	private static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_DATA = "dat"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_ID = "id"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_CIPHER_KEY = "key"; //$NON-NLS-1$

	private static final String OPERATION_STORE = "put"; //$NON-NLS-1$
	private static final String SUCCESS = "OK"; //$NON-NLS-1$

	@Override
	protected void service(final HttpServletRequest request, final HttpServletResponse response) throws ServletException, IOException {
		final String operation = request.getParameter(PARAMETER_NAME_OPERATION);
		response.setContentType("text/plain"); //$NON-NLS-1$
		response.setCharacterEncoding("iso-8859-15"); //$NON-NLS-1$

		try (final PrintWriter out = response.getWriter()) {
			if (operation == null) {
				out.println(ErrorManager.genError(ErrorManager.ERROR_MISSING_OPERATION_NAME, null));
				return;
			}

			final StorageConfig config = new StorageConfig(this.getServletContext());
			config.load(CONFIG_FILE);

			switch(operation) {
			case OPERATION_STORE:
				storeSign(out, request, config);
				return;
			default:
				out.println(ErrorManager.genError(ErrorManager.ERROR_UNSUPPORTED_OPERATION_NAME, null));
			}
		}
	}

	/**
	 * Almacena una firma en servidor.
	 * @param response Respuesta a la petici&oacute;n.
	 * @param request Petici&oacute;n.
	 * @throws IOException Cuando ocurre un error al general la respuesta.
	 */
	private static void storeSign(final PrintWriter out, final HttpServletRequest request, final StorageConfig config) throws IOException {

		Logger.getLogger("es.gob.afirma").info("Se va a almacenar la respuesta.");

		//TODO: Borrar
		final StringBuilder sb = new StringBuilder("Parametros:\n");
		for (final String key : request.getParameterMap().keySet().toArray(new String[request.getParameterMap().size()])) {
			sb.append(key).append(": ").append(request.getParameter(key)).append("\n");
		}
		Logger.getLogger("es.gob.afirma").info("Propiedades de la peticion de almacenamiento: " + sb.toString());

		final String id = request.getParameter(PARAMETER_NAME_ID);
		if (id == null) {
			out.println(ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA_ID, null));
			return;
		}

		String b64Data = URLDecoder.decode(request.getParameter(PARAMETER_NAME_DATA), UTF8);
		if (b64Data == null) {
			b64Data = ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA, null);
		}

		final byte[] b64DataEncoded = b64Data.getBytes();

//		// En caso de recibir una clave de cifrado, cifraremos con ella el mensaje proporcionado
//		final String cipherKey = request.getParameter(PARAMETER_NAME_CIPHER_KEY);
//		if (cipherKey != null && cipherKey.length() > 0) {
//
//			Logger.getLogger("es.gob.afirma").info("Ciframos los datos con la clave: " + cipherKey);
//
//			try {
//				b64DataEncoded = cipherData(b64DataEncoded, cipherKey);
//			} catch (final InvalidKeyException e) {
//				Logger.getLogger("es.gob.afirma").severe("InvalidKeyException: " + e.toString());
//				out.println(ErrorManager.genError(ErrorManager.ERROR_INVALID_CIPHER_KEY, null));
//				return;
//			} catch (final GeneralSecurityException e) {
//				Logger.getLogger("es.gob.afirma").severe("GeneralSecurityException: " + e.toString());
//				out.println(ErrorManager.genError(ErrorManager.ERROR_CIPHERING, null));
//				return;
//			}
//
//			Logger.getLogger("es.gob.afirma").info("Datos cifrados que se almacenaran (mostrados en Base64): " + Base64.encode(b64DataEncoded));
//		}

		final File outFile = new File(config.getTempDir(), request.getRemoteAddr().replace(":", "_") + "-" + id); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		try (final OutputStream fos = new FileOutputStream(outFile);) {
			fos.write(b64DataEncoded);
			fos.flush();
		}

		Logger.getLogger("es.gob.afirma").info("Firma almacenada en: " + outFile.getAbsolutePath());

		out.println(SUCCESS);
	}

	/**
	 * Realiza el proceso de cifrado del mensaje proporcionado por el usuario y devuelve los datos cifrados
	 * en hexadecimal.
	 * @param data Mensaje.
	 * @param key Clave de cifrado.
	 * @return Mensaje cifrado.
	 * @throws GeneralSecurityException
	 * @throws InvalidKeyException
	 */
	private static byte[] cipherData(final byte[] data, final String key) throws InvalidKeyException, GeneralSecurityException {
		return DesCipher.cipher(data, key);
	}
}
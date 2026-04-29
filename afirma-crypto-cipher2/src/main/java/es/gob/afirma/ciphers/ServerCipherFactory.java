package es.gob.afirma.ciphers;

import java.io.IOException;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Instancia clases para cifrar datos segun el algoritmo configurado por JSON.
 */
public class ServerCipherFactory {

	private static final String ALGORITHM_DES = "DES"; //$NON-NLS-1$
	private static final String ALGORITHM_AES = "AES"; //$NON-NLS-1$

	/**
	 * Compone el objeto para el cifrado/descifrado de los datos del servidor intermedio.
	 * @param cipherConfig JSON con la configuraci&oacute;n del cifrador/descifrador.
	 * @return Objeto para el cifrado/descifrado o {@code null} si se configura un algoritmo no soportado.
	 * @throws JSONException Cuando el JSON de configuraci&oacute;n no est&aacute; bien formado.
	 * @throws IOException Cuando los valores del JSON no son v&aacute;lidos para el algoritmo seleccionado.
	 */
	public static ServerCipher newServerCipher(final byte[] cipherConfig) throws JSONException, IOException {

		final String jsonString = new String(cipherConfig, java.nio.charset.StandardCharsets.UTF_8);

		final JSONObject json = new JSONObject(jsonString);

		final String alg = json.getString("algo"); //$NON-NLS-1$

		if (ALGORITHM_AES.equals(alg)) {
			return new AesServerCipher(jsonString);
		} else if (ALGORITHM_DES.equals(alg)) {
			return new DesServerCipher(jsonString);
		}

		return null;
	}

}

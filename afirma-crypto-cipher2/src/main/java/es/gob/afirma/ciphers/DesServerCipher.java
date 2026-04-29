package es.gob.afirma.ciphers;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.spec.SecretKeySpec;

import org.json.JSONException;
import org.json.JSONObject;

import es.gob.afirma.core.misc.Base64;

public class DesServerCipher implements ServerCipher {

	/** Car&aacute;cter utilizado para separar el padding agregado a los datos para cifrarlos y los propios datos
	 * cifrados en base64. */
	private static final char PADDING_CHAR_SEPARATOR = '.';

	private static final int PADDING_LENGTH = 8;

	private final byte[] cipherKey;

	public DesServerCipher(final String cipherConfig) throws JSONException {

		final JSONObject json = new JSONObject(cipherConfig);

		this.cipherKey = json.getString("key").getBytes(); //$NON-NLS-1$
	}

	public DesServerCipher(final byte[] key) throws JSONException {
		this.cipherKey = key;
	}

	@Override
	public byte[] decipherData(final String cipheredData) throws InvalidKeyException, NoSuchAlgorithmException,
															NoSuchPaddingException, InvalidAlgorithmParameterException,
															IllegalBlockSizeException, BadPaddingException,
															GeneralSecurityException, IOException {

		int padding = 0;
		byte[] decodedData;
		final boolean customPadding = cipheredData.length() > 2 && cipheredData.charAt(1) == PADDING_CHAR_SEPARATOR;
		if (customPadding) {
			padding = Integer.parseInt(cipheredData.substring(0, 1));
			decodedData = Base64.decode(cipheredData.substring(2).replace('-', '+').replace('_', '/'));
		} else {
			decodedData = Base64.decode(cipheredData.replace('-', '+').replace('_', '/'));
		}

		final Cipher desCipher = Cipher.getInstance("DES/ECB/NoPadding"); //$NON-NLS-1$

		desCipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(this.cipherKey, "DES")); //$NON-NLS-1$
		final byte [] deciphered = desCipher.doFinal(decodedData);

		return padding == 0 ? deciphered : Arrays.copyOf(deciphered, deciphered.length - padding);
	}

	@Override
	public byte[] decipherData(final byte [] cipheredData) throws InvalidAlgorithmParameterException, GeneralSecurityException, IOException {

		final String b64Data = new String(cipheredData, StandardCharsets.UTF_8);
		return decipherData(b64Data);
	}

	@Override
	public String cipherData(final byte[] data) throws NoSuchAlgorithmException, NoSuchPaddingException,
														InvalidKeyException, IllegalBlockSizeException,
														BadPaddingException {

		final Cipher desCipher = Cipher.getInstance("DES/ECB/NoPadding"); //$NON-NLS-1$
		desCipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(this.cipherKey, "DES")); //$NON-NLS-1$
		final byte [] descipheredArray = desCipher.doFinal(padding(data, 8));

		return new StringBuilder((int)(data.length * 1.2))
				.append(Integer.toString((getPaddingLength() - data.length % getPaddingLength()) % getPaddingLength()))
				.append(PADDING_CHAR_SEPARATOR)
				.append(Base64.encode(descipheredArray, true)).toString();
	}

	/** Rellena un array de bytes, si es necesario, para que sea m&uacute;ltiplo de la cantidad indicada.
	 * @param data Datos de entrada
	 * @param padding M&acute;ltiplo
	 * @return Datos con el relleno a&ntilde;adido */
	private static byte[] padding(final byte[] data, final int padding) {
		if (data.length % padding == 0) {
			return data;
		}
		return Arrays.copyOf(data, (data.length / padding + 1) * padding);
	}

	/** Recupera la longitud del relleno requerido para el cifrado. Esto es, de que n&uacute;mero deber se
	 * m&uacute;ltiplo la longitud de los datos.
	 * @return Longitud del relleno. */
	private static int getPaddingLength() {
		return PADDING_LENGTH;
	}

}

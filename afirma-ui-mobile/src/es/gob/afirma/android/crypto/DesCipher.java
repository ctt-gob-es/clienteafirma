package es.gob.afirma.android.crypto;

import java.security.GeneralSecurityException;
import java.security.InvalidKeyException;
import java.util.Arrays;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;

import es.gob.afirma.core.misc.Base64;

/** Test. */
public class DesCipher {

	private static final String BASE64_PADDING_SEPARATOR = "."; //$NON-NLS-1$

	/** Padding requerido para el cifrado. */
	private static final int PADDING_LENGTH = 8;

	/** Cifra datos con un algoritmo DES (ECB sin relleno).
	 * @param data Datos que se desean cifrar.
	 * @param sk Contrase&ntilde;a para generar la clave secreta.
	 * @return Resultado del proceso de cifrado.
	 * @throws InvalidKeyException Cuando La clave no es valida para el algoritmo DES.
	 * @throws GeneralSecurityException Cuando los datos introducidos no son validos o se produce un error en la operaci&oacute;n.
	 */
	public static byte[] cipher(final byte[] data, final String sk) throws InvalidKeyException, GeneralSecurityException {
		final Cipher desCipher;
		try {
			desCipher = Cipher.getInstance("DES/ECB/NoPadding"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			// Este caso no deberia ocurrir nunca
			throw new GeneralSecurityException("Algoritmo o formato no soportado por la maquina virtual: " + e); //$NON-NLS-1$
		}
		desCipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(sk.getBytes(), "DES")); //$NON-NLS-1$
		return desCipher.doFinal(padding(data, 8));
	}

	/**
	 * Rellena un array de bytes para que sea m&uacute;ltiplo de
	 * @param data Datos de los
	 * @param padding
	 * @return
	 */
	private static byte[] padding(final byte[] data, final int padding) {
		if (data.length % padding == 0) {
			return data;
		}
		return Arrays.copyOf(data, (data.length / padding + 1) * padding);
	}

	/**
	 * Recupera la longitud del padding requerido para el cifrado. Esto es de que n&uacute;mero deber se
	 * m&uacute;ltiplo la longitud de los datos.
	 * @return Longitud del padding.
	 */
	public static int getPaddingLength() {
		return DesCipher.PADDING_LENGTH;
	}


	private static String generateCipherDataString(final byte[] data, final String cipherKey) throws InvalidKeyException, GeneralSecurityException {
    	return Integer.toString(DesCipher.getPaddingLength() - data.length % DesCipher.getPaddingLength()) +
    				BASE64_PADDING_SEPARATOR + Base64.encode(DesCipher.cipher(data, cipherKey));
    }

    public static void main(final String[] args) throws InvalidKeyException, GeneralSecurityException {

    	final byte[] data = "Hola Mundo!".getBytes();
    	final String cipherKey = "12345678";

    	final String cipherData = generateCipherDataString(data, cipherKey);

    	System.out.println("CipherData: " + cipherData);
	}
}

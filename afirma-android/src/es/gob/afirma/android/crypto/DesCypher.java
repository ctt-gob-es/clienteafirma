package es.gob.afirma.android.crypto;

import java.security.GeneralSecurityException;
import java.security.InvalidKeyException;
import java.util.Arrays;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;

/** Cifrador DES. */
final class DesCypher {

	private DesCypher() {
		// No permitimos la instanciacion
	}

	/** Tama&ntilde;o de bloque requerido para el cifrado. Si los datos de entrada no son m&uacute;ltiplos de este
	 * ama&ntilde;o hay que a&ntilde;adir relleno (con 0x00h) hasta que lo sea. */
	private static final int BLOCK_SIZE = 8;

	private static final String DES_MODE = "DES/ECB/NoPadding"; //$NON-NLS-1$
	private static final String DES = "DES"; //$NON-NLS-1$

	private static byte[] doDes(final byte[] data, final byte[] sk, final int opMode) throws InvalidKeyException, GeneralSecurityException {
		final Cipher desCipher;
		try {
			desCipher = Cipher.getInstance(DES_MODE);
		}
		catch (final Exception e) {
			// Este caso no deberia ocurrir nunca
			throw new GeneralSecurityException("Algoritmo o formato no soportado por la maquina virtual: " + e); //$NON-NLS-1$
		}
		desCipher.init(opMode, new SecretKeySpec(sk, DES));
		return desCipher.doFinal(data);
	}

	/** Cifra datos con un algoritmo DES (ECB sin relleno). Antes de cifrar los datos, si la longitud de estos no es m&uacute;ltiplo
	 * del tama&ntilde;o de bloque predeterminado, se les a&ntilde;ade un relleno independiente del cifrado (con 0x00h),
	 * f&aacute;cilmente eliminable cuando los datos de entrada son Base64.
	 * @param data Datos que se desean cifrar.
	 * @param sk Contrase&ntilde;a para generar la clave secreta.
	 * @return Resultado del proceso de cifrado.
	 * @throws InvalidKeyException Cuando La clave no es valida para el algoritmo DES.
	 * @throws GeneralSecurityException Cuando los datos introducidos no son validos o se produce un error en la operaci&oacute;n. */
	static byte[] cypher(final byte[] data, final byte[] sk) throws InvalidKeyException, GeneralSecurityException {
		return doDes(padData(data), sk, Cipher.ENCRYPT_MODE);
	}

	/** Descifra datos con un algoritmo DES (ECB sin relleno).
	 * @param data Datos que se desean cifrar.
	 * @param sk Contrase&ntilde;a para generar la clave secreta.
	 * @return Resultado del proceso de descifrado.
	 * @throws InvalidKeyException Cuando La clave no es valida para el algoritmo DES.
	 * @throws GeneralSecurityException Cuando los datos introducidos no son validos o se produce un error en la operaci&oacute;n. */
	static byte[] decypher(final byte[] data, final byte[] sk) throws InvalidKeyException, GeneralSecurityException {
		return doDes(data, sk, Cipher.DECRYPT_MODE);
	}

	/** Rellena un array de bytes para que sea m&uacute;ltiplo de la cantidad indicada.
	 * @param data Datos de entrada
	 * @return Datos con el relleno a&ntilde;adido */
	private static byte[] padData(final byte[] data) {
		if (data.length % BLOCK_SIZE == 0) {
			return data;
		}
		return Arrays.copyOf(data, (data.length / BLOCK_SIZE + 1) * BLOCK_SIZE);
	}

	/** Recupera el tama&ntilde;o de bloque requerido para el cifrado. Esto es, de que n&uacute;mero deber se
	 * m&uacute;ltiplo la longitud de los datos.
	 * @return Tama&ntilde;o de bloque requerido para el cifrado. */
	static int getBlockSize() {
		return DesCypher.BLOCK_SIZE;
	}

}

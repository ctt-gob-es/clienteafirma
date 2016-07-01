package es.gob.afirma.standalone.crypto;

import java.security.GeneralSecurityException;
import java.security.InvalidKeyException;
import java.util.Arrays;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;

/** Cifrador DES. */
public final class DesCipher {

	private DesCipher() {
		// No permitimos la instanciacion
	}

	/** Padding requerido para el cifrado. */
	private static final int PADDING_LENGTH = 8;

	/** Cifra datos con un algoritmo DES (ECB sin relleno). Antes de cifrar los datos se les a&ntilde;ade
	 * un relleno independiente del cifrado, f&aacute;cilmente eliminable cuando los datos de entrada
	 * con Base64.
	 * @param data Datos que se desean cifrar.
	 * @param sk Contrase&ntilde;a para generar la clave secreta.
	 * @return Resultado del proceso de cifrado.
	 * @throws InvalidKeyException Cuando La clave no es valida para el algoritmo DES.
	 * @throws GeneralSecurityException Cuando los datos introducidos no son validos o se produce un error en la operaci&oacute;n. */
	public static byte[] cipher(final byte[] data, final byte[] sk) throws InvalidKeyException, GeneralSecurityException {
		final Cipher desCipher;
		try {
			desCipher = Cipher.getInstance("DES/ECB/NoPadding"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			// Este caso no deberia ocurrir nunca
			throw new GeneralSecurityException("Algoritmo o formato no soportado por la maquina virtual: " + e); //$NON-NLS-1$
		}
		desCipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(sk, "DES")); //$NON-NLS-1$
		return desCipher.doFinal(padding(data, 8));
	}

	/** Descifra datos con un algoritmo DES (ECB sin relleno).
	 * @param data Datos que se desean cifrar.
	 * @param sk Contrase&ntilde;a para generar la clave secreta.
	 * @return Resultado del proceso de descifrado.
	 * @throws InvalidKeyException Cuando La clave no es valida para el algoritmo DES.
	 * @throws GeneralSecurityException Cuando los datos introducidos no son validos o se produce un error en la operaci&oacute;n. */
	public static byte[] decipher(final byte[] data, final byte[] sk) throws InvalidKeyException, GeneralSecurityException {
		final Cipher desCipher;
		try {
			desCipher = Cipher.getInstance("DES/ECB/NoPadding"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			// Este caso no deberia ocurrir nunca
			throw new GeneralSecurityException("Algoritmo o formato no soportado por la maquina virtual: " + e); //$NON-NLS-1$
		}
		desCipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(sk, "DES")); //$NON-NLS-1$
		return desCipher.doFinal(data);
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
	public static int getPaddingLength() {
		return DesCipher.PADDING_LENGTH;
	}

}

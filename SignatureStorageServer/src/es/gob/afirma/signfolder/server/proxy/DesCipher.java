package es.gob.afirma.signfolder.server.proxy;

import java.security.GeneralSecurityException;
import java.security.InvalidKeyException;
import java.util.Arrays;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;

/** Test. */
public class DesCipher {

	/**
	 * Cifra datos con un algoritmo DES.
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
		} catch (final Exception e) {
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
		System.out.println("Se agregan " + (((data.length / padding + 1) * padding) - data.length) + " bytes");

		return Arrays.copyOf(data, (data.length / padding + 1) * padding);
	}
}

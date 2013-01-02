package es.gob.afirma.signfolder.server.proxy;

import java.util.Arrays;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;

import es.gob.afirma.core.misc.Base64;

/** Test. */
public class TestDes {

	private static final byte[] DATA = "This is the message to encrypt!!".getBytes(); //$NON-NLS-1$
	/** Main
	 * @param args */
	public static void main(final String[] args) throws Throwable {
		final Cipher desCipher = Cipher.getInstance("DES/ECB/NoPadding"); //$NON-NLS-1$
		//final Cipher desCipher = Cipher.getInstance("DES/ECB/PKCS5PADDING"); //$NON-NLS-1$
		//final Cipher desCipher = Cipher.getInstance("DES/ECB/ISO10126PADDING"); //$NON-NLS-1$
		desCipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec("12345678".getBytes(), "DES")); //$NON-NLS-1$ //$NON-NLS-2$

		//System.out.println("Hex cifrado: " + AOUtil.hexify(desCipher.doFinal(TestDes.padding(DATA, 8)), false));
		System.out.println("Base64 cifrado: " + Base64.encode(desCipher.doFinal(TestDes.padding(DATA, 8))));

	}

	private static byte[] padding(final byte[] data, final int padding) {
		if (data.length % padding == 0) {
			return data;
		}
		return Arrays.copyOf(data, (data.length / padding + 1) * padding);
	}
}

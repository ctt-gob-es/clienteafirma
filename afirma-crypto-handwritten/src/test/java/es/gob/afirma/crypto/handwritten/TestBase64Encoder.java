package es.gob.afirma.crypto.handwritten;

import java.io.IOException;
import java.security.KeyStore.PrivateKeyEntry;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;

/** Pruebas del codificador Base64. */
public final class TestBase64Encoder {

	/** Prueba del codificador Base64.
	 * @param args No se usa.
	 * @throws IOException En caso de error en la codificaci&oacute;n. */
	public static void main(final String[] args) throws IOException {
		final String p12 = Base64.encode(
			AOUtil.getDataFromInputStream(
					TestBase64Encoder.class.getResourceAsStream(
						"/ANF.pfx" //$NON-NLS-1$
					)
				)
			);
		System.out.println(p12);
		final PrivateKeyEntry pke = BioSignerRunnerKeyHelper.getKeyFromPkcs12(p12, "12341234", "anf usuario activo", null); //$NON-NLS-1$ //$NON-NLS-2$
		System.out.println(pke.getPrivateKey());
	}
}

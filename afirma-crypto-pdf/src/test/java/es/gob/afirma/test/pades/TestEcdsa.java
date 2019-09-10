package es.gob.afirma.test.pades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.junit.Test;

import com.aowagie.text.pdf.AcroFields;
import com.aowagie.text.pdf.PdfPKCS7;
import com.aowagie.text.pdf.PdfReader;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;

/** Pruebas de firma PAdES con ECDSA.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestEcdsa {

	private static final String CRT = "/juaneliptico.p12"; //$NON-NLS-1$
	private static final char[] PWD = "12341234".toCharArray(); //$NON-NLS-1$

	private static final String PDF = "/empty_signature_field.pdf"; //$NON-NLS-1$

	private static List<byte[]> extractP7(final byte[] pdf) throws Exception {
		final PdfReader pdfReader = new PdfReader(pdf);
		final AcroFields af = pdfReader.getAcroFields();
		final List<String> names = af.getSignatureNames();
		final List<byte[]> ret = new ArrayList<>(names.size());
		for (final String signatureName : names) {
			final PdfPKCS7 p7 = af.verifySignature(signatureName);
			ret.add(p7.getEncodedPKCS7());
		}
		return ret;
	}

	/** Prueba de firma SHA256withECDSA con PAdES.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void signEcdsa() throws Exception {

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		ks.load(TestEcdsa.class.getResourceAsStream(CRT), PWD);
		final String alias = ks.aliases().nextElement();
		System.out.println(alias);
		final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(alias, new KeyStore.PasswordProtection(PWD));

		final byte[] pdf = AOUtil.getDataFromInputStream(
			TestEcdsa.class.getResourceAsStream(PDF)
		);

		final Properties extraParams = new Properties();
		final AOSigner signer = new AOPDFSigner();
		final byte[] signedPdf = signer.sign(
			pdf,
			"SHA256withECDSA", //$NON-NLS-1$
			pke.getPrivateKey(),
			pke.getCertificateChain(),
			extraParams
		);
		try (
			final OutputStream fos = new FileOutputStream(
				File.createTempFile("PKCS7_AFIRMA_ECDSA_", ".pdf") //$NON-NLS-1$ //$NON-NLS-2$
			)
		) {
			fos.write(signedPdf);
			fos.flush();
		}

		final byte[] p7 = extractP7(signedPdf).get(0);
		try (
			final OutputStream fos = new FileOutputStream(
				File.createTempFile("PKCS7_AFIRMA_ECDSA_", ".p7s") //$NON-NLS-1$ //$NON-NLS-2$
			)
		) {
			fos.write(p7);
			fos.flush();
		}

	}
}

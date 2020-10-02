package es.gob.afirma.signers.multi.cades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;

import org.junit.Before;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.cades.AOCAdESSigner;

public class TestCadesBaseline {

    private static final String CERT_PATH = "PruebaEmpleado4Activo.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "Giss2016"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "givenname=prueba4empn+serialnumber=idces-00000000t+sn=p4empape1 p4empape2 - 00000000t+cn=prueba4empn p4empape1 p4empape2 - 00000000t,ou=personales,ou=certificado electronico de empleado publico,o=secretaria de estado de la seguridad social,c=es"; //$NON-NLS-1$

	private PrivateKeyEntry pke;
	private byte[] data;

	@Before
	public void init() throws Exception {

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(CERT_PATH)
		) {
			ks.load(is, CERT_PASS.toCharArray());
		}
		this.pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

		try (
			final InputStream is = TestCadesBaseline.class.getResourceAsStream("/Original.pdf") //$NON-NLS-1$
		) {
			this.data = AOUtil.getDataFromInputStream(is);
		}
	}

	@Test
	public void testCadesBLevelCountersign() throws Exception {

		final Properties extraParams = new Properties();
		extraParams.setProperty("profile", "baseline"); //$NON-NLS-1$ //$NON-NLS-2$

		final AOCAdESSigner signer = new AOCAdESSigner();
		final byte[] signature = signer.sign(this.data, "SHA512withRSA", this.pke.getPrivateKey(), this.pke.getCertificateChain(), extraParams); //$NON-NLS-1$

		final byte[] counterSignature = signer.countersign(signature, "SHA512withRSA", CounterSignTarget.LEAFS, null, this.pke.getPrivateKey(), this.pke.getCertificateChain(), extraParams); //$NON-NLS-1$

		final File outputFile = File.createTempFile("CAdES_BLevel_contrafirma_", ".csig"); //$NON-NLS-1$ //$NON-NLS-2$
		try (final OutputStream fos = new FileOutputStream(outputFile)) {
			fos.write(counterSignature);
		}

		System.out.println("Fichero CAdES B-Level con una contrafirma: " + outputFile); //$NON-NLS-1$
	}

}

package es.gob.afirma.signers.xades;

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

public class TestXadesBaseline {

    private static final String CERT_PATH = "EIDAS_CERTIFICADO_PRUEBAS___99999999R__1234.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "1234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "eidas_certificado_pruebas___99999999r"; //$NON-NLS-1$

	private PrivateKeyEntry pke;
	private byte[] data;
	private byte[] xmlData;

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
			final InputStream is = TestXadesBaseline.class.getResourceAsStream("/rubric.jpg") //$NON-NLS-1$
		) {
			this.data = AOUtil.getDataFromInputStream(is);
		}

		try (
				final InputStream is = TestXadesBaseline.class.getResourceAsStream("/xml_with_ids.xml") //$NON-NLS-1$
			) {
				this.xmlData = AOUtil.getDataFromInputStream(is);
			}
	}

	@Test
	public void testXadesBLevelDefault() throws Exception {

		final Properties extraParams = new Properties();
		extraParams.setProperty("profile", "baseline"); //$NON-NLS-1$ //$NON-NLS-2$

		final AOXAdESSigner signer = new AOXAdESSigner();
		final byte[] signature = signer.sign(this.data, "SHA512withRSA", this.pke.getPrivateKey(), this.pke.getCertificateChain(), extraParams); //$NON-NLS-1$

		final File outputFile = File.createTempFile("XAdES_BLevel_default_", ".xsig"); //$NON-NLS-1$ //$NON-NLS-2$
		try (final OutputStream fos = new FileOutputStream(outputFile)) {
			fos.write(signature);
		}

		System.out.println("Fichero XAdES B-Level con configuracion por defecto: " + outputFile); //$NON-NLS-1$
	}

	@Test
	public void testXadesBLevelDetached() throws Exception {

		final Properties extraParams = new Properties();
		extraParams.setProperty("profile", "baseline"); //$NON-NLS-1$ //$NON-NLS-2$
		extraParams.setProperty("format", "XAdES Detached"); //$NON-NLS-1$ //$NON-NLS-2$

		final AOXAdESSigner signer = new AOXAdESSigner();
		final byte[] signature = signer.sign(this.data, "SHA512withRSA", this.pke.getPrivateKey(), this.pke.getCertificateChain(), extraParams); //$NON-NLS-1$

		final File outputFile = File.createTempFile("XAdES_BLevel_detached_", ".xsig"); //$NON-NLS-1$ //$NON-NLS-2$
		try (final OutputStream fos = new FileOutputStream(outputFile)) {
			fos.write(signature);
		}

		System.out.println("Fichero XAdES B-Level Internally Detached: " + outputFile); //$NON-NLS-1$
	}

	@Test
	public void testXadesBLevelEnveloped() throws Exception {

		final Properties extraParams = new Properties();
		extraParams.setProperty("profile", "baseline"); //$NON-NLS-1$ //$NON-NLS-2$
		extraParams.setProperty("format", "XAdES Enveloped"); //$NON-NLS-1$ //$NON-NLS-2$

		final AOXAdESSigner signer = new AOXAdESSigner();
		final byte[] signature = signer.sign(this.xmlData, "SHA512withRSA", this.pke.getPrivateKey(), this.pke.getCertificateChain(), extraParams); //$NON-NLS-1$

		final File outputFile = File.createTempFile("XAdES_BLevel_enveloped_", ".xsig"); //$NON-NLS-1$ //$NON-NLS-2$
		try (final OutputStream fos = new FileOutputStream(outputFile)) {
			fos.write(signature);
		}

		System.out.println("Fichero XAdES B-Level Enveloped: " + outputFile); //$NON-NLS-1$
	}

	@Test
	public void testContrafirmaXadesBLevel() throws Exception {

		final Properties extraParams = new Properties();
		extraParams.setProperty("profile", "baseline"); //$NON-NLS-1$ //$NON-NLS-2$

		final AOXAdESSigner signer = new AOXAdESSigner();
		final byte[] signature = signer.sign(this.data, "SHA512withRSA", this.pke.getPrivateKey(), this.pke.getCertificateChain(), extraParams); //$NON-NLS-1$
		final byte[] counterSignature = signer.countersign(signature, "SHA512withRSA", CounterSignTarget.LEAFS, null, this.pke.getPrivateKey(), this.pke.getCertificateChain(), extraParams); //$NON-NLS-1$

		final File outputFile = File.createTempFile("XAdES_BLevel_contrafirma_", ".xsig"); //$NON-NLS-1$ //$NON-NLS-2$
		try (final OutputStream fos = new FileOutputStream(outputFile)) {
			fos.write(counterSignature);
		}

		System.out.println("Fichero XAdES B-Level con una contrafirma: " + outputFile); //$NON-NLS-1$
	}

	@Test
	public void testXadesBLevelWithProductionPlace() throws Exception {

		final Properties extraParams = new Properties();
		extraParams.setProperty("profile", "baseline"); //$NON-NLS-1$ //$NON-NLS-2$
		extraParams.setProperty("signatureProductionPostalCode", "28010"); //$NON-NLS-1$ //$NON-NLS-2$
		extraParams.setProperty("signatureProductionCity", "Madrid"); //$NON-NLS-1$ //$NON-NLS-2$
		extraParams.setProperty("signatureProductionProvince", "Madrid"); //$NON-NLS-1$ //$NON-NLS-2$
		extraParams.setProperty("signatureProductionCountry", "ES"); //$NON-NLS-1$ //$NON-NLS-2$

		final AOXAdESSigner signer = new AOXAdESSigner();
		final byte[] signature = signer.sign(this.data, "SHA512withRSA", this.pke.getPrivateKey(), this.pke.getCertificateChain(), extraParams); //$NON-NLS-1$

		final File outputFile = File.createTempFile("XAdES_BLevel_lugar_", ".xsig"); //$NON-NLS-1$ //$NON-NLS-2$
		try (final OutputStream fos = new FileOutputStream(outputFile)) {
			fos.write(signature);
		}

		System.out.println("Fichero XAdES B-Level con ProductionPlace: " + outputFile); //$NON-NLS-1$
	}

	@Test
	public void testXadesBLevelWithProductionPlaceAddress() throws Exception {

		final Properties extraParams = new Properties();
		extraParams.setProperty("profile", "baseline"); //$NON-NLS-1$ //$NON-NLS-2$
		extraParams.setProperty("signatureProductionStreetAddress", "Calle Manuel Cortina, 2"); //$NON-NLS-1$ //$NON-NLS-2$
		extraParams.setProperty("signatureProductionPostalCode", "28010"); //$NON-NLS-1$ //$NON-NLS-2$
		extraParams.setProperty("signatureProductionCity", "Madrid"); //$NON-NLS-1$ //$NON-NLS-2$
		extraParams.setProperty("signatureProductionProvince", "Madrid"); //$NON-NLS-1$ //$NON-NLS-2$
		extraParams.setProperty("signatureProductionCountry", "ES"); //$NON-NLS-1$ //$NON-NLS-2$

		final AOXAdESSigner signer = new AOXAdESSigner();
		final byte[] signature = signer.sign(this.data, "SHA512withRSA", this.pke.getPrivateKey(), this.pke.getCertificateChain(), extraParams); //$NON-NLS-1$

		final File outputFile = File.createTempFile("XAdES_BLevel_lugar_con_direccion_", ".xsig"); //$NON-NLS-1$ //$NON-NLS-2$
		try (final OutputStream fos = new FileOutputStream(outputFile)) {
			fos.write(signature);
		}

		System.out.println("Fichero XAdES B-Level con ProductionPlace y direccion: " + outputFile); //$NON-NLS-1$
	}

	@Test
	public void testXadesBLevelWithClaimedRoles() throws Exception {

		final Properties extraParams = new Properties();
		extraParams.setProperty("profile", "baseline"); //$NON-NLS-1$ //$NON-NLS-2$
		extraParams.setProperty("signerClaimedRoles", "Firmante|Revisor"); //$NON-NLS-1$ //$NON-NLS-2$

		final AOXAdESSigner signer = new AOXAdESSigner();
		final byte[] signature = signer.sign(this.data, "SHA512withRSA", this.pke.getPrivateKey(), this.pke.getCertificateChain(), extraParams); //$NON-NLS-1$

		final File outputFile = File.createTempFile("XAdES_BLevel_con_roles_", ".xsig"); //$NON-NLS-1$ //$NON-NLS-2$
		try (final OutputStream fos = new FileOutputStream(outputFile)) {
			fos.write(signature);
		}

		System.out.println("Fichero XAdES B-Level con roles: " + outputFile); //$NON-NLS-1$
	}
}

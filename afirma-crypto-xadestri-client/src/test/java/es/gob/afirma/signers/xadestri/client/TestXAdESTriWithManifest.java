package es.gob.afirma.signers.xadestri.client;

import java.io.File;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.util.Properties;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;

/** Pruebas de firmas XAdES con MANIFEST. */
public final class TestXAdESTriWithManifest {

	private static final String CERT_PATH = "EIDAS_CERTIFICADO_PRUEBAS___99999999R.p12"; //$NON-NLS-1$
	private static final String CERT_PASS = "1234"; //$NON-NLS-1$
	private static final String CERT_ALIAS = "eidas_certificado_pruebas___99999999r"; //$NON-NLS-1$

    private static final String ALGORITHM = "SHA256withRSA"; //$NON-NLS-1$

	private static final String SERVER_URL = "http://localhost:8080/afirma-server-triphase-signer/SignatureService"; //$NON-NLS-1$

	/** Pruebas de firmas XAdES Enveloping de binario con MANIFEST.
	 * @throws Exception en cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testXadesEnvelopingUseManifestBinary() throws Exception {

		System.out.println("Firma XAdES Trifasica Enveloping con Manifest de datos binarios"); //$NON-NLS-1$

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());

        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("ANF_con cadena_certificacion.jks")); //$NON-NLS-1$

        final AOSigner signer = new AOXAdESTriPhaseSigner();

        final Properties p = new Properties();
        p.setProperty("useManifest", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        p.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

        System.out.println("Huella SHA512 de los datos: " + Base64.encode(MessageDigest.getInstance("SHA-512").digest(data))); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] signature = signer.sign(data, ALGORITHM, pke.getPrivateKey(), pke.getCertificateChain(), p);

        final File f = File.createTempFile("xadesEnveloping-useManifestBinary-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final OutputStream fos = new java.io.FileOutputStream(f);
		) {
        	fos.write(signature);
        	fos.flush();
		}
		System.out.println("Firma para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
	}

	/** Pruebas de firmas XAdES Enveloping de XML con MANIFEST.
	 * @throws Exception en cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testXadesEnvelopingUseManifestXML() throws Exception {

		System.out.println("Firma XAdES Enveloping con Manifest de XML"); //$NON-NLS-1$

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());

        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("xml_with_ids.xml")); //$NON-NLS-1$

        final AOSigner signer = new AOXAdESTriPhaseSigner();

        final Properties p = new Properties();
        p.setProperty("useManifest", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        p.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

        System.out.println("Huella SHA512 de los datos: " + Base64.encode(MessageDigest.getInstance("SHA-512").digest(data))); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] signature = signer.sign(data, ALGORITHM, pke.getPrivateKey(), pke.getCertificateChain(), p);

        final File f = File.createTempFile("xadesEnveloping-useManifestXML-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final OutputStream fos = new java.io.FileOutputStream(f);
		) {
        	fos.write(signature);
        	fos.flush();
        }
		System.out.println("Firma para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
	}

	/** Pruebas de firmas XAdES Enveloped con MANIFEST.
	 * @throws Exception en cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testXadesEnvelopedUseManifest() throws Exception {

		System.out.println("Firma XAdES Enveloped con Manifest"); //$NON-NLS-1$

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());

        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("xml_with_ids.xml")); //$NON-NLS-1$

        final AOSigner signer = new AOXAdESTriPhaseSigner();

        final Properties p = new Properties();
        p.setProperty("useManifest", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        p.setProperty("format", "XAdES Enveloped"); //$NON-NLS-1$ //$NON-NLS-2$
        p.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

        System.out.println("Huella SHA512 de los datos: " + Base64.encode(MessageDigest.getInstance("SHA-512").digest(data))); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] signature = signer.sign(data, ALGORITHM, pke.getPrivateKey(), pke.getCertificateChain(), p);

        final File f = File.createTempFile("xadesEnveloped-useManifestXML-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final OutputStream fos = new java.io.FileOutputStream(f);
		) {
        	fos.write(signature);
        	fos.flush();
        }
		System.out.println("Firma para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
	}

	/** Pruebas de cofirma XAdES con MANIFEST.
	 * @throws Exception en cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testCoSignXadesWithManifest() throws Exception {

		System.out.println("Cofirma de firma XAdES con Manifest"); //$NON-NLS-1$

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());

        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final byte[] signature = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("firma_xades_manifest.xsig")); //$NON-NLS-1$

        final AOSigner signer = new AOXAdESTriPhaseSigner();

        final Properties p = new Properties();
        p.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

        final byte[] cosignature = signer.cosign(signature, ALGORITHM, pke.getPrivateKey(), pke.getCertificateChain(), p);

        final File f = File.createTempFile("xades-manifest-cosign", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final OutputStream fos = new java.io.FileOutputStream(f);
		) {
        	fos.write(cosignature);
        	fos.flush();
        }
		System.out.println("Firma para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
	}

	/** Pruebas de contrafirma XAdES con MANIFEST.
	 * @throws Exception en cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testCounterSignXadesWithManifest() throws Exception {

		System.out.println("Contrafirma de firma XAdES con Manifest"); //$NON-NLS-1$

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());

        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final byte[] signature = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("firma_xades_manifest.xsig")); //$NON-NLS-1$

        final AOSigner signer = new AOXAdESTriPhaseSigner();

        final Properties p = new Properties();
        p.setProperty("serverUrl", SERVER_URL); //$NON-NLS-1$

        final byte[] cosignature = signer.countersign(signature, ALGORITHM, CounterSignTarget.LEAFS, null, pke.getPrivateKey(), pke.getCertificateChain(), p);

        final File f = File.createTempFile("xades-manifest-countersign", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final OutputStream fos = new java.io.FileOutputStream(f);
		) {
        	fos.write(cosignature);
        	fos.flush();
        }
		System.out.println("Firma para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
	}
}

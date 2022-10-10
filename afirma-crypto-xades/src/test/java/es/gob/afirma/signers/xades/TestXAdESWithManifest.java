package es.gob.afirma.signers.xades;

import java.io.File;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.util.Properties;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;

/** Pruebas de firmas XAdES con MANIFEST.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestXAdESWithManifest {

    private static final String CERT_PATH = "CATCERT CIUTADANIA PF CPIXSA-2.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "1111"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "persona f\u00EDsica de la pe\u00E7a de proves"; //$NON-NLS-1$

    private static final String ALGORITHM = "SHA256withRSA"; //$NON-NLS-1$

    /** Prueba de firma XAdES Externally Detached con Manifest y URI no dereferenciable (URN).
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testXadesExternallyDetachedUseManifest() throws Exception {
		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(CERT_PATH)
		) {
	        ks.load(is, CERT_PASS.toCharArray());
		}
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(
    		CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray())
		);

        // Los datos son la huella de los datos
        final byte[] data;
        try (
    		final InputStream is = ClassLoader.getSystemResourceAsStream("ANF_con cadena_certificacion.jks") //$NON-NLS-1$
		) {
	        data = MessageDigest.getInstance("SHA-512").digest( //$NON-NLS-1$
	    		AOUtil.getDataFromInputStream(is)
			);
        }
        System.out.println("Huella de los datos: " + Base64.encode(data)); //$NON-NLS-1$

        final AOXAdESSigner signer = new AOXAdESSigner();

        final Properties p = new Properties();
        p.setProperty("useManifest", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        p.setProperty("precalculatedHashAlgorithm", "SHA-512"); //$NON-NLS-1$ //$NON-NLS-2$
        p.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED); //$NON-NLS-1$
        p.setProperty("uri", "urn:id:001"); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] signature = signer.sign(data, ALGORITHM, pke.getPrivateKey(), pke.getCertificateChain(), p);

        final File f = File.createTempFile("xadesExternallyDetachedManifest-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final java.io.FileOutputStream fos = new java.io.FileOutputStream(f)
		) {
        	fos.write(signature);
        	fos.flush();
        }
		System.out.println("Firma para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

    }

	/** Pruebas de firmas XAdES Enveloping de binario con MANIFEST.
	 * @throws Exception en cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	@Deprecated
	public void testXadesEnvelopingUseManifestBinary() throws Exception {

		System.out.println("Firma XAdES enveloping con Manifest de datos binarios"); //$NON-NLS-1$

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(CERT_PATH)
		) {
	        ks.load(is, CERT_PASS.toCharArray());
		}

        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(
    		CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray())
		);
        final byte[] data;
        try (
    		final InputStream is = ClassLoader.getSystemResourceAsStream("ANF_con cadena_certificacion.jks") //$NON-NLS-1$
		) {
        	data = AOUtil.getDataFromInputStream(is);
        }

        final AOXAdESSigner signer = new AOXAdESSigner();

        final Properties p = new Properties();
        p.setProperty("useManifest", "true"); //$NON-NLS-1$ //$NON-NLS-2$

        System.out.println("Huella SHA512 de los datos: " + Base64.encode(MessageDigest.getInstance("SHA-512").digest(data))); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] signature = signer.sign(data, ALGORITHM, pke.getPrivateKey(), pke.getCertificateChain(), p);

        final File f = File.createTempFile("xadesEnveloping-useManifestBinary-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
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
	@Deprecated
	public void testXadesEnvelopingUseManifestXML() throws Exception {

		System.out.println("Firma XAdES Enveloping con Manifest de XML"); //$NON-NLS-1$

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(CERT_PATH)
		) {
	        ks.load(is, CERT_PASS.toCharArray());
		}

        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(
    		CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray())
		);

        final byte[] data;
        try (
    		final InputStream is = ClassLoader.getSystemResourceAsStream("xml_with_ids.xml") //$NON-NLS-1$
		) {
        	data = AOUtil.getDataFromInputStream(is);
        }

        final AOXAdESSigner signer = new AOXAdESSigner();

        final Properties p = new Properties();
        p.setProperty("useManifest", "true"); //$NON-NLS-1$ //$NON-NLS-2$

        System.out.println("Huella SHA512 de los datos: " + Base64.encode(MessageDigest.getInstance("SHA-512").digest(data))); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] signature = signer.sign(data, ALGORITHM, pke.getPrivateKey(), pke.getCertificateChain(), p);

        final File f = File.createTempFile("xadesEnveloping-useManifestXML-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final java.io.FileOutputStream fos = new java.io.FileOutputStream(f)
		) {
        	fos.write(signature);
        	fos.flush();
        }
		System.out.println("Firma para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
	}

	/** Pruebas de firmas XAdES Enveloped con MANIFEST.
	 * @throws Exception en cualquier error. */
	@SuppressWarnings("static-method")
	@Test(expected=es.gob.afirma.core.AOUnsupportedSignFormatException.class)
	@Ignore
	@Deprecated
	public void testXadesEnvelopedUseManifest() throws Exception {

		System.out.println("Firma XAdES Enveloped con Manifest"); //$NON-NLS-1$

		final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
		try (
			final InputStream is = ClassLoader.getSystemResourceAsStream(CERT_PATH)
		) {
	        ks.load(is, CERT_PASS.toCharArray());
		}

        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final byte[] data;
        try (
    		final InputStream is = ClassLoader.getSystemResourceAsStream("xml_with_ids.xml") //$NON-NLS-1$
		) {
        	data = AOUtil.getDataFromInputStream(is);
        }

        final AOXAdESSigner signer = new AOXAdESSigner();

        final Properties p = new Properties();
        p.setProperty("useManifest", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        p.setProperty("format", "XAdES Enveloped"); //$NON-NLS-1$ //$NON-NLS-2$

        System.out.println("Huella SHA512 de los datos: " + Base64.encode(MessageDigest.getInstance("SHA-512").digest(data))); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] signature = signer.sign(data, ALGORITHM, pke.getPrivateKey(), pke.getCertificateChain(), p);

        final File f = File.createTempFile("xadesEnveloped-useManifestXML-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final java.io.FileOutputStream fos = new java.io.FileOutputStream(f)
		) {
        	fos.write(signature);
        	fos.flush();
        }
		System.out.println("Firma para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
	}
}

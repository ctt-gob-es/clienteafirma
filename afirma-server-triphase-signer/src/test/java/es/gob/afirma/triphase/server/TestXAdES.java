package es.gob.afirma.triphase.server;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.signers.xades.XAdESExtraParams;
import es.gob.afirma.signers.xadestri.client.AOXAdESTriPhaseSigner;
import es.gob.afirma.signers.xml.XmlDSigProviderHelper;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.ValidateXMLSignature;

public class TestXAdES {


    private static final String CERT_PATH = "PruebaEmpleado4Activo.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "Giss2016"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "givenname=prueba4empn+serialnumber=idces-00000000t+sn=p4empape1 p4empape2 - 00000000t+cn=prueba4empn p4empape1 p4empape2 - 00000000t,ou=personales,ou=certificado electronico de empleado publico,o=secretaria de estado de la seguridad social,c=es"; //$NON-NLS-1$


    private static final String TEST_FILE_DATA_BIN =  "ANF_PF_Activo.pfx"; //$NON-NLS-1$

    private static final String TEST_FILE_XADES_1_2_2_IGAE =  "xades_enveloped_1.xml"; //$NON-NLS-1$

    private static final String TEST_FILE_XADES_COSIGNATURE =  "xades_cofirma.xsig"; //$NON-NLS-1$

    private PrivateKeyEntry pke;
    private byte[] dataBin;

    static {
    	XmlDSigProviderHelper.configureXmlDSigProvider();
    }

    @Before
    public void init() throws Exception {

    	Logger.getLogger("es.gob.afirma").setLevel(Level.FINE); //$NON-NLS-1$

    	final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
    	ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
    	this.pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

    	this.dataBin = loadTestFile(TEST_FILE_DATA_BIN);
    }

	/**
     * Prueba de firma XAdES v1.2.2.
     * @throws Exception en cualquier error
     */
    @Test
    public void testSignatureXAdES122() throws Exception {

    	final byte[] signature = generateXAdES122();

    	validate(signature);

    	final File f = saveFile(signature, "XAdES_detached_122-"); //$NON-NLS-1$
    	System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
    }

	/**
     * Prueba de firma trif&aacute;sica XAdES v1.2.2.
     * @throws Exception en cualquier error
     */
    @Test
    @Ignore
    public void testSignatureXAdES122Tri() throws Exception {

    	final byte[] signature = generateXAdES122Tri();

    	validate(signature);

    	final File f = saveFile(signature, "XAdES_detached_122_tri-"); //$NON-NLS-1$
    	System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
    }

	/**
     * Prueba de firma trif&aacute;sica XAdES.
     * @throws Exception en cualquier error
     */
    @Test
    public void testSignatureXAdES() throws Exception {

    	final byte[] signature = generateXAdES();

    	validate(signature);

    	final File f = saveFile(signature, "XAdES_detached-"); //$NON-NLS-1$
    	System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
    }

	/**
     * Prueba de firma trif&aacute;sica XAdES.
     * @throws Exception en cualquier error
     */
    @Test
    @Ignore
    public void testSignatureXAdESTri() throws Exception {

    	final byte[] signature = generateXAdESTri();

    	validate(signature);

    	final File f = saveFile(signature, "XAdES_detached_tri-"); //$NON-NLS-1$
    	System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
    }

	/**
     * Prueba de contrafirma de una firma XAdES v1.2.2.
     * @throws Exception en cualquier error
     */
    @Test
    public void testCounterSignXAdES122() throws Exception {

    	final byte[] signature = generateXAdES122();

    	final byte[] counterSignature = countersign(signature);

    	validate(counterSignature);

    	final File f = saveFile(counterSignature, "XAdES_detached_122_CounterSign-"); //$NON-NLS-1$
    	System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
    }

	/**
     * Prueba de contrafirmatrif&aacute;sica de una firma XAdES v1.2.2.
     * @throws Exception en cualquier error
     */
    @Test
    @Ignore
    public void testCounterSignTriXAdES122() throws Exception {

    	final byte[] signature = generateXAdES122();

    	final byte[] counterSignature = countersignTri(signature);

    	validate(counterSignature);

    	final File f = saveFile(counterSignature, "XAdES_detached_122_CounterSign_Tri-"); //$NON-NLS-1$
    	System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
    }

	/**
     * Prueba de cofirma de una firma XAdES v1.2.2.
     * @throws Exception en cualquier error
     */
    @Test
    public void testCoSignXAdES122() throws Exception {

    	final byte[] signature = generateXAdES122();

    	final byte[] coSignature = cosign(signature);

    	validate(coSignature);

    	final File f = saveFile(coSignature, "XAdES_detached_122_CoSign-"); //$NON-NLS-1$
    	System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
    }

	/**
     * Prueba de cofirma de una firma XAdES v1.2.2.
     * @throws Exception en cualquier error
     */
    @Test
    @Ignore
    public void testCoSignTriXAdES122() throws Exception {

    	final byte[] signature = generateXAdES122Tri();

    	final byte[] coSignature = cosignTri(signature);


    	validate(coSignature);

    	final File f = saveFile(coSignature, "XAdES_detached_122_CoSign_tri-"); //$NON-NLS-1$
    	System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
    }

	/**
     * Prueba de contrafirma de una firma XAdES v1.2.2.
     * @throws Exception en cualquier error
     */
    @Test
    public void testCounterSignXAdES122Igae() throws Exception {

    	final byte[] signature = loadTestFile(TEST_FILE_XADES_1_2_2_IGAE);

    	validate(signature);

    	final byte[] counterSignature = countersign(signature);

    	validate(counterSignature);

    	final File f = saveFile(counterSignature, "XAdES_enveloped_122_Igae_CounterSign-"); //$NON-NLS-1$
    	System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
    }

	/**
     * Prueba de contrafirma trif&aacute;sica de una firma XAdES v1.2.2.
     * @throws Exception en cualquier error.
     */
    @Test
    @Ignore
    public void testCounterSignTriXAdES122Igae() throws Exception {

    	final byte[] signature = loadTestFile(TEST_FILE_XADES_1_2_2_IGAE);

    	validate(signature);

    	final byte[] counterSignature = countersignTri(signature);

    	validate(counterSignature);

    	final File f = saveFile(counterSignature, "XAdES_enveloped_122_Igae_CounterSign-"); //$NON-NLS-1$
    	System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
    }

	/**
     * Prueba de contrafirma trif&aacute;sica de una cofirma XAdES.
     * @throws Exception en cualquier error.
     */
    @Test
    @Ignore
    public void testCounterSignTriCosignatureXAdES() throws Exception {

    	final byte[] signature = loadTestFile(TEST_FILE_XADES_COSIGNATURE);

    	validate(signature);

    	final byte[] counterSignature = countersignTri(signature);

    	validate(counterSignature);

    	final File f = saveFile(counterSignature, "XAdES_CounterSign_From_Cosign-"); //$NON-NLS-1$
    	System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
    }

    /**
     * Genera una firma XAdES v1.2.2.
     * @return Firma XAdES v1.2.2.
     * @throws Exception Cuando ocurre un error.
     */
    private final byte[] generateXAdES122() throws Exception {

    	final Properties extraParams = addXAdES122Version(addXAdESDetachedFormat(newConfig()));

    	final AOSigner signer = new AOXAdESSigner();
    	final byte[] signature = signer.sign(
    			this.dataBin,
    			AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
    			this.pke.getPrivateKey(),
    			this.pke.getCertificateChain(),
    			extraParams
    			);

    	return signature;
    }

    /**
     * Genera una firma trif&aacute;sica XAdES v1.2.2.
     * @return Firma XAdES v1.2.2.
     * @throws Exception Cuando ocurre un error.
     */
    private final byte[] generateXAdES122Tri() throws Exception {

    	final Properties config = addXAdES122Version(addXAdESDetachedFormat(addTriphaseServer(newConfig())));

    	final AOSigner signer = new AOXAdESTriPhaseSigner();
    	final byte[] signature = signer.sign(
    			this.dataBin,
    			AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
    			this.pke.getPrivateKey(),
    			this.pke.getCertificateChain(),
    			config
    			);

    	return signature;
    }

    /**
     * Genera una firma XAdES.
     * @return Firma XAdES.
     * @throws Exception Cuando ocurre un error.
     */
    private final byte[] generateXAdES() throws Exception {

    	final Properties config = addXAdESDetachedFormat(newConfig());

    	final AOSigner signer = new AOXAdESSigner();
    	final byte[] signature = signer.sign(
    			this.dataBin,
    			AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
    			this.pke.getPrivateKey(),
    			this.pke.getCertificateChain(),
    			config
    			);

    	return signature;
    }

    /**
     * Genera una firma trif&aacute;sica XAdES.
     * @return Firma XAdES.
     * @throws Exception Cuando ocurre un error.
     */
    private final byte[] generateXAdESTri() throws Exception {

    	final Properties config = addXAdESDetachedFormat(addTriphaseServer(newConfig()));

    	final AOSigner signer = new AOXAdESTriPhaseSigner();
    	final byte[] signature = signer.sign(
    			this.dataBin,
    			AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
    			this.pke.getPrivateKey(),
    			this.pke.getCertificateChain(),
    			config
    			);

    	return signature;
    }

    /**
     * Contrafirma una firma XAdES.
     * @return Contrafirma XAdES.
     * @throws Exception Cuando ocurre un error.
     */
    private final byte[] countersign(final byte[] signature) throws Exception {

    	final AOSigner signer = new AOXAdESSigner();
    	final byte[] countersignature = signer.countersign(
    			signature,
    			AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
    			CounterSignTarget.LEAFS,
    			null,
    			this.pke.getPrivateKey(),
    			this.pke.getCertificateChain(),
    			null);

    	return countersignature;
    }

    /**
     * Contrafirma de forma trif&aacute;sica una firma XAdES.
     * @return Contrafirma XAdES.
     * @throws Exception Cuando ocurre un error.
     */
    private final byte[] countersignTri(final byte[] signature) throws Exception {

    	final Properties config = addTriphaseServer(newConfig());

    	final AOSigner signer = new AOXAdESTriPhaseSigner();
    	final byte[] countersignature = signer.countersign(
    			signature,
    			AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
    			CounterSignTarget.LEAFS,
    			null,
    			this.pke.getPrivateKey(),
    			this.pke.getCertificateChain(),
    			config);

    	return countersignature;
    }

    /**
     * Cofirma una firma XAdES.
     * @return Cofirma XAdES.
     * @throws Exception Cuando ocurre un error.
     */
    private final byte[] cosign(final byte[] signature) throws Exception {

    	final AOSigner signer = new AOXAdESSigner();
    	final byte[] cosignature = signer.cosign(
    			signature,
    			AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
    			this.pke.getPrivateKey(),
    			this.pke.getCertificateChain(),
    			null);

    	return cosignature;
    }

    /**
     * Cofirma de forma trif&aacute;sica una firma XAdES.
     * @return Cofirma XAdES.
     * @throws Exception Cuando ocurre un error.
     */
    private final byte[] cosignTri(final byte[] signature) throws Exception {

    	final Properties config = addTriphaseServer(newConfig());

    	final AOSigner signer = new AOXAdESTriPhaseSigner();
    	final byte[] cosignature = signer.cosign(
    			signature,
    			AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
    			this.pke.getPrivateKey(),
    			this.pke.getCertificateChain(),
    			config);

    	return cosignature;
    }

    private static Properties newConfig() {
    	return new Properties();
    }

    private static Properties addTriphaseServer(final Properties config) {
    	config.setProperty("serverUrl", "http://localhost:8080/afirma-server-triphase-signer/SignatureService"); //$NON-NLS-1$ //$NON-NLS-2$
    	return config;
    }

    private static Properties addXAdESDetachedFormat(final Properties config) {
    	config.setProperty(XAdESExtraParams.FORMAT, AOSignConstants.SIGN_FORMAT_XADES_DETACHED);
    	return config;
    }

    private static Properties addXAdES122Version(final Properties config) {
    	config.setProperty(XAdESExtraParams.XADES_NAMESPACE, "http://uri.etsi.org/01903/v1.2.2#"); //$NON-NLS-1$
    	config.setProperty(XAdESExtraParams.SIGNED_PROPERTIES_TYPE_URL, "http://uri.etsi.org/01903/v1.2.2#SignedProperties"); //$NON-NLS-1$
    	return config;
    }

    private static byte[] loadTestFile(final String filename) throws IOException {
    	byte[] data;
    	try (InputStream is = ClassLoader.getSystemResourceAsStream(filename)) {
    		data = AOUtil.getDataFromInputStream(is);
    	}
    	return data;
    }

    /**
     * Guarda datos en un fichero temporal.
     * @param data Datos a guardar.
     * @param prefix Prefijo que se le asignara al fichero.
     * @return Fichero en el que se han guardado los datos.
     * @throws IOException Cuando ocurre cualquier error.
     */
    private static File saveFile(final byte[] data, final String prefix) throws IOException {

    	final File f = File.createTempFile(prefix, ".xml"); //$NON-NLS-1$
    	try (
    			final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
    			) {
    		fos.write(data);
    		fos.flush();
    	}
    	return f;
    }

    private static void validate(final byte[] signature) {
    	final ValidateXMLSignature validator = new ValidateXMLSignature();
    	final List<SignValidity> validity = validator.validate(signature, false);
    	Assert.assertEquals("La firma no es valida: " + validity.get(0).getErrorException(), SIGN_DETAIL_TYPE.OK, validity.get(0).getValidity()); //$NON-NLS-1$
    }

}

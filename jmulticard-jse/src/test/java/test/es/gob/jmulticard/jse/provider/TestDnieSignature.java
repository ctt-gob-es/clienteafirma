package test.es.gob.jmulticard.jse.provider;

import java.io.IOException;
import java.math.BigInteger;
import java.security.InvalidKeyException;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.Provider;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.Security;
import java.security.Signature;
import java.security.SignatureException;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;

import org.junit.Assert;
import org.junit.Test;

import es.gob.jmulticard.apdu.connection.ApduConnectionException;
import es.gob.jmulticard.jse.provider.DnieProvider;

/** Pruebas de la implementaci&oacute;n de SPI Signature para DNIe.
 * @author Alberto Mart&iacute;nez */
@SuppressWarnings("static-method")
public class TestDnieSignature {

    private static final String SHA1WITH_RSA = "SHA1withRSA"; //$NON-NLS-1$

    private static final String AFIRMA_DNIE = "DNI"; //$NON-NLS-1$

    private static final java.util.logging.Logger LOGGER = java.util.logging.Logger.getLogger(TestDnieSignature.class.getName());

    private static final String[] ALGORITHMS = new String[] {
            TestDnieSignature.SHA1WITH_RSA, "SHA256withRSA", "SHA384withRSA", "SHA512withRSA" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    };

    private static final String[] TEST_FILES = new String[] {
            "inteco.der", "inteco_def.der"//$NON-NLS-1$ //$NON-NLS-2$
    };

    /** Trata de obtener del proveedor del DNIe una instancia para cada algoritmo de firma */
    @Test
    public void obtenerUnaInstanciaDelProveedorDnie() {
        try {
            for (final String algorithm : ALGORITHMS) {
                final Signature sign = Signature.getInstance(algorithm, new DnieProvider());
                LOGGER.info("Obtenido algoritmo de firma " + sign.getAlgorithm() + " del proveedor " + sign.getProvider().getName()); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
        catch (final NoSuchAlgorithmException e) {
            Assert.fail(e.toString());
        }
    }

    /** Comprueba que se lleva correctamente a cabo la inicializaci&oacute;n del proceso de firma
     * @throws NoSuchAlgorithmException Si no se pudo obtener una instancia
     * @throws KeyStoreException Si no se puede obtener una instancia del almac&eacute;n de claves pedido
     * @throws IOException Si ocurri&oacute; alg&uacute;n problema leyendo el almac&eacute;n o la contrase&ntilde;a es err&oacute;nea
     * @throws CertificateException Si alg&uacute;n certificado del almac&eacute;n no pudo ser cargado
     * @throws UnrecoverableKeyException Si la clave no se pudo recuperar */
    @Test
    public void iniciarFirma() throws NoSuchAlgorithmException, KeyStoreException, CertificateException, IOException, UnrecoverableKeyException {
        final Signature sign = Signature.getInstance(TestDnieSignature.SHA1WITH_RSA, new DnieProvider());

        final Provider p = new DnieProvider();
        Security.addProvider(p);

        KeyStore ks = null;

        ks = KeyStore.getInstance(AFIRMA_DNIE);
        try {
            ks.load(null, null);
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.toString());
            return;
        }
        final PrivateKey privKey = (PrivateKey) ks.getKey("CertFirmaDigital", null); //$NON-NLS-1$
        try {
            sign.initSign(privKey);
        }
        catch (final InvalidKeyException e) {
            Assert.fail("No se ha podido inicializar el proceso de firma: " + e.toString()); //$NON-NLS-1$
        }
    }

    /** Comprueba que se lleva a cabo correctamente la inicializaci&oacute;n de la verificaci&oacute;n
     * @throws InvalidKeyException En caso de que la clave del certificado no sea v&aacute;lida
     * @throws NoSuchAlgorithmException Si no se pudo obtener una instancia
     * @throws CertificateException Si no se puede interpretar correctamente la entrada */
    @Test
    public void iniciarVerificacion() throws InvalidKeyException, NoSuchAlgorithmException, CertificateException {
        final Signature sign = Signature.getInstance(TestDnieSignature.SHA1WITH_RSA, new DnieProvider());

        final Certificate cert = CertificateFactory.getInstance("X.509").generateCertificate(ClassLoader.getSystemResourceAsStream(TEST_FILES[0])); //$NON-NLS-1$
        sign.initVerify(cert);

    }

    /** Comprueba que se devuelve una excepci&oacute;n de tipo InvalidKeyException al aportar una clave incorrecta
     * @throws InvalidKeyException Si la clave indicada no es v&aacute;lida
     * @throws NoSuchAlgorithmException Si no se pudo obtener una instancia */
    @Test(expected = InvalidKeyException.class)
    public void iniciarVerificacionConUnaClaveInvalidaDebeDevolverExcepcion() throws InvalidKeyException, NoSuchAlgorithmException {
        final Signature sign = Signature.getInstance(TestDnieSignature.SHA1WITH_RSA, new DnieProvider());
        sign.initVerify((PublicKey) null);
    }

    /** Comprueba que se devuelve una excepci&oacute;n de tipo InvalidKeyException al aportar una certificado con una clave inv&aacute;lida
     * @throws InvalidKeyException Si la clave contenida en el certificado no es v&aacute;lida
     * @throws NoSuchAlgorithmException Si no se pudo obtener una instancia
     * @throws CertificateException Si no se puede interpretar correctamente la entrada */
    @Test(expected = InvalidKeyException.class)
    public void iniciarVerificacionConUnaCertificadoDefectuosoDebeDevolverExcepcion() throws InvalidKeyException,
                                                                                     NoSuchAlgorithmException,
                                                                                     CertificateException {
        final Signature sign = Signature.getInstance(TestDnieSignature.SHA1WITH_RSA, new DnieProvider());
        final Certificate cert = CertificateFactory.getInstance("X.509").generateCertificate(ClassLoader.getSystemResourceAsStream(TEST_FILES[1])); //$NON-NLS-1$
        sign.initVerify(cert);
    }

    /** Comprueba la correcci&oacute;n del resultado de la firma de un array de bytes con los certificados del DNIe
     * @throws NoSuchAlgorithmException Si no se puede generar un objeto Signature o no se pudo cargar el almac&eacute;n de certificados
     * @throws KeyStoreException Si no se inicializ&oacute; correctamente el almac&eacute;n
     * @throws CertificateException Si alg&uacute;n certificado del almac&eacute;n no pudo ser cargado
     * @throws IOException Si ocurri&oacute; alg&uacute;n problema leyendo el almac&eacute;n o la contrase&ntilde;a es err&oacute;nea
     * @throws UnrecoverableKeyException Si no se pudo recuperar la clave privada */
    @Test
    public void comprobarVerificacion() throws NoSuchAlgorithmException,
                                       KeyStoreException,
                                       CertificateException,
                                       IOException,
                                       UnrecoverableKeyException {
        final Signature sign = Signature.getInstance(TestDnieSignature.SHA1WITH_RSA, new DnieProvider());

        final Provider p = new DnieProvider();
        Security.addProvider(p);

        KeyStore ks = null;

        ks = KeyStore.getInstance(AFIRMA_DNIE);
        try {
            ks.load(null, null);
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.toString());
            return;
        }
        final PrivateKey privKey = (PrivateKey) ks.getKey("CertFirmaDigital", null); //$NON-NLS-1$
        byte[] signResult = null;
        try {
            sign.initSign(privKey);
            sign.update("Texto de ejemplo para firmar".getBytes()); //$NON-NLS-1$
            signResult = sign.sign();
        }
        catch (final SignatureException e) {
            Assert.fail("No se finalizo correctamente el proceso de firma: " + e.toString()); //$NON-NLS-1$
        }
        catch (final InvalidKeyException e) {
            Assert.fail("No se ha podido inicializar el proceso de firma: " + e.toString()); //$NON-NLS-1$
        }
        try {
            sign.initVerify(ks.getCertificate("CertFirmaDigital").getPublicKey()); //$NON-NLS-1$
            sign.update("Texto de ejemplo para firmar".getBytes());  //$NON-NLS-1$
            Assert.assertTrue("No se verifico correctamente la firma", sign.verify(signResult)); //$NON-NLS-1$
        }
        catch (final SignatureException e) {
            Assert.fail("No se finalizo correctamente el proceso de firma: " + e.toString()); //$NON-NLS-1$
        }
        catch (final InvalidKeyException e) {
            Assert.fail("No se ha podido inicializar el proceso de verificacion: " + e.toString()); //$NON-NLS-1$
        }
    }

    /** Si la clave de cifrado aportada no es de un DNIe se produce una excepci&oacute;n de clave inv&aacute;lida
     * @throws SignatureException Si no se pudo llevar a cabo la firma digital
     * @throws NoSuchAlgorithmException Si no se puede generar un objeto signature o un generador de claves
     * @throws InvalidKeyException Si la clave no es v&aacute;lida */
    @Test(expected = InvalidKeyException.class)
    public void comprobarVerificacionConUnaClaveNoDNIDevuelveExcepcion() throws SignatureException, NoSuchAlgorithmException, InvalidKeyException {
        final Signature sign = Signature.getInstance(TestDnieSignature.SHA1WITH_RSA, new DnieProvider());

        final KeyPairGenerator keyGen = KeyPairGenerator.getInstance("RSA"); //$NON-NLS-1$
        keyGen.initialize(1024, new SecureRandom());
        final KeyPair keyPair = keyGen.generateKeyPair();

        sign.initVerify(keyPair.getPublic());
        sign.initSign(keyPair.getPrivate());
    }

    /** Prueba una verificacion no inicializada
     * @throws NoSuchAlgorithmException
     * @throws SignatureException */
    @Test(expected=SignatureException.class)
    public void testUninitializedVerification() throws NoSuchAlgorithmException, SignatureException {
    	final Provider p = new DnieProvider();
    	Security.addProvider(p);
    	Signature s = Signature.getInstance(ALGORITHMS[0], p);
    	s.verify("datos".getBytes()); //$NON-NLS-1$
    }
    
    /** Test del proceso completo de firma usando todos los algoritmos soportados. 
     * @throws Exception En caso de error */
    @Test
    public void testFirma() throws Exception {
        
        System.setProperty("es.gob.jmulticard.fastmode", "true"); //$NON-NLS-1$ //$NON-NLS-2$

    	// Se instancia el proveedor y se anade
    	final Provider p = new DnieProvider();
    	Security.addProvider(p);

    	// Se obtiene el almacen y se carga
    	final KeyStore ks = KeyStore.getInstance("DNI"); //$NON-NLS-1$
    	try {
            ks.load(null, null);
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.toString());
            return;
        }

    	final String alias = "CertFirmaDigital"; //$NON-NLS-1$

    	LOGGER.info("Usamos el alias: " + alias); //$NON-NLS-1$

    	LOGGER.info("Certificado que se usara para la firma: " + ks.getCertificate(alias)); //$NON-NLS-1$

   		LOGGER.info("Entrada que se usara para la firma: " + ks.getEntry(alias, null)); //$NON-NLS-1$

   		for (final String algo : ALGORITHMS) {
			// Se obtiene el motor de firma y se inicializa
			final Signature signature = Signature.getInstance(algo);
			
			signature.initSign((PrivateKey) ks.getKey(alias, null));
		
			// Vamos a firmar el texto 'hola'
			signature.update("hola".getBytes()); //$NON-NLS-1$
			signature.update((byte)0x00);
			
			// Completamos el proceso y obtenemos la firma PKCS#1
			final byte[] signatureBytes = signature.sign();
			
			Assert.assertNotNull(signatureBytes);
		
			LOGGER.info("Firma:\n" + new BigInteger(signatureBytes).toString()); //$NON-NLS-1$
   		}

    }
}
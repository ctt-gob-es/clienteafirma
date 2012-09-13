package test.es.gob.jmulticard.jse.provider;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.Provider;
import java.security.Security;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.List;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import es.gob.jmulticard.apdu.connection.ApduConnectionException;
import es.gob.jmulticard.card.dnie.FakeX509Certificate;
import es.gob.jmulticard.jse.provider.DnieProvider;
import es.gob.jmulticard.ui.passwordcallback.gui.UIPasswordCallbackAccessibility;

/** Pruebas del proveedor JCE de DNIe para KeyStore.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 * @author Alberto Mart&iacute;nez */
public class TestDnieKeyStoreProvider {

    private static final PasswordCallback PSSCALLBACK = new UIPasswordCallbackAccessibility("Introduzca el pin del DNIe", null, "Introduzca el pin del DNIe", 'I', "PIN del DNIe"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    private static final String AFIRMA_DNIE = "DNI"; //$NON-NLS-1$

    private static final Logger LOGGER = Logger.getLogger(TestDnieKeyStoreProvider.class.getName());

    private KeyStore ks = null;

    private static final List<String> ALIASES = new ArrayList<String>(2);
    static {
        ALIASES.add("CertAutenticacion"); //$NON-NLS-1$
        ALIASES.add("CertFirmaDigital"); //$NON-NLS-1$
    }

    /** Datos preparados antes de la invocaci&oacute;n de cada prueba
     * @throws java.lang.Exception Cuando se produce cualquier error en la configuraci&oacute;n. */
    @Before
    public void setUp() throws Exception {
        final Provider p = new DnieProvider();
        Security.addProvider(p);
        try {
            this.ks = KeyStore.getInstance(TestDnieKeyStoreProvider.AFIRMA_DNIE);
        }
        catch (final KeyStoreException e) {
            LOGGER.severe(e.getMessage());
            Assert.fail("No se pudo obtener una instancia del proveedor: " + e.getMessage()); //$NON-NLS-1$
        }
    }

    /** Carga el almacen de certificados del DNIe. Si no hay una tarjeta presente se controla que se devuelve una excepci&oacute;n de tipo
     * ApduConnectionException. En el resto de casos se considera que el test ha fallado.
     * @throws Exception En caso de que no se pueda verificar correctamente la integridad del almac&eacute;n de certificados */
    @Test
    public void cargaAlmacenDeCertificados() throws Exception {
        try {
            this.ks.load(null, null);
        }
        catch (final IOException e) {
            if (e.getCause() instanceof ApduConnectionException || e instanceof ApduConnectionException) {
                LOGGER.warning(e.getMessage());
            }
            else {
                LOGGER.severe(e.getMessage());
                Assert.fail(e.getMessage());
            }
        }
    }

    /** Obtiene una lista con los alias definidos en el almac&eacute;n de certificados de proveedor. Se considera que ha fallado si no se obtiene
     * ningún alias.
     * @throws KeyStoreException Si el almac&eacute;n no ha sido inicializado
     * @throws Exception En otros casos */
    @Test
    public void obtenerListaDeAliasDelProveedorDNIe() throws KeyStoreException, Exception {
        try {
            this.ks.load(null, null);
            final Enumeration<String> aliases = this.ks.aliases();
            if (aliases.hasMoreElements()) {
                while (aliases.hasMoreElements()) {
                    LOGGER.info(aliases.nextElement());
                }
            }
            else {
                Assert.fail("No se ha encontrado ningun certificado en el almacen"); //$NON-NLS-1$
            }
        }
        catch (final IOException e) {
            if (e instanceof ApduConnectionException) {
                LOGGER.warning(e.getMessage());
            }
        }
    }

    /** Comprueba si el proveedor del DNIe contiene los alias para el certificado de autenticaci&oacute;n y de firma. Si no existe alguno de ellos o
     * los dos se considera que ha fallado.
     * @throws KeyStoreException Si el almac&eacute;n no ha sido inicializado
     * @throws Exception En otros casos */
    @Test
    public void comprobarSiElProveedorContieneElAliasSolicitado() throws KeyStoreException, Exception {
        try {
            this.ks.load(null, null);
            boolean notFind = false;
            for (final String alias : ALIASES) {
                if (!this.ks.containsAlias(alias)) {
                    LOGGER.severe("No se ha encontrado el certificado cuyo alias es \"" + alias + "\""); //$NON-NLS-1$ //$NON-NLS-2$
                    notFind = true;
                }
            }
            if (notFind) {
                Assert.fail("No se obtuvieron todos los certificados esperados."); //$NON-NLS-1$
            }
        }
        catch (final IOException e) {
            if (e instanceof ApduConnectionException) {
                LOGGER.warning(e.getMessage());
            }
        }
    }

    /** Comprueba que se devuelve la lista de certificados para los alias definidos. Si no se puede obtener
     * un certificado para un alias se considera que el test ha fallado.
     * @throws KeyStoreException Si el almac&eacute;n no ha sido inicializado
     * @throws Exception En otros casos */
    @Test
    public void obtenerCertificadoAPartirDelAlias() throws KeyStoreException, Exception {
        try {

            this.ks.load(null, null);
            for (final String alias : ALIASES) {
                final X509Certificate cert = (X509Certificate) this.ks.getCertificate(alias);
                if (cert != null) {
                    LOGGER.info(cert.toString());
                }
                else {
                    Assert.fail("No existe certificado para el alias \"" + alias + "\""); //$NON-NLS-1$ //$NON-NLS-2$
                }
            }
        }
        catch (final IOException e) {
            if (e instanceof ApduConnectionException) {
                LOGGER.warning(e.getMessage());
            }
        }
    }

    /** Comprueba que dado un certificado del DNIe se puede obtener el alias de este. Si el alias no coincide con el devuelto por el certificado se
     * considera que el test ha fallado.
     * @throws KeyStoreException Si el almac&eacute;n no ha sido inicializado
     * @throws Exception En otros casos */
    @Test
    public void obtenerAliasAPartirDelCertificado() throws KeyStoreException, Exception {
        try {
            this.ks.load(null, null);
            for (final String alias : ALIASES) {
                final X509Certificate cert = (X509Certificate) this.ks.getCertificate(alias);
                final String certificateAlias = this.ks.getCertificateAlias(cert);
                if (certificateAlias == alias) {
                    LOGGER.info("El certificado corresponde con el alias indicado."); //$NON-NLS-1$
                }
                else {
                    Assert.fail();
                }
            }
        }
        catch (final IOException e) {
            if (e instanceof ApduConnectionException) {
                LOGGER.warning(e.getMessage());
            }
        }
    }

    /** Comprueba que a partir del alias de un certificado se peude obtener la lista de certificados asociados. Si la lista es nula se cosidera que el
     * test ha fallado.
     * @throws KeyStoreException Si el almac&eacute;n no ha sido inicializado
     * @throws Exception En otros casos */
    @Test
    public void obtenerCadenaAPartirDelAlias() throws KeyStoreException, Exception {
        try {
            this.ks.load(null, null);
            for (final String alias : ALIASES) {
                final Certificate[] certs = this.ks.getCertificateChain(alias);
                if (certs != null) {
                    for (final Certificate certificate : certs) {
                        LOGGER.info(certificate.toString());
                    }
                }
                else {
                    Assert.fail("cert null"); //$NON-NLS-1$
                }
            }
        }
        catch (final IOException e) {
            if (e instanceof ApduConnectionException) {
                LOGGER.warning(e.getMessage());
            }
        }
    }

    /** Comprueba que se obtiene la entrada del certificado a partir del alias. En caso contrario se considera que el test ha fallado.
     * @throws KeyStoreException Si el almac&eacute;n no ha sido inicializado
     * @throws Exception En otros casos */
    @Test
    public void obtenerEntradaAPartirDelAliasConElParametroDeProteccionIndicado() throws KeyStoreException, Exception {
        try {
            this.ks.load(null, null);
            for (final String alias : ALIASES) {
                final PrivateKeyEntry entry = (PrivateKeyEntry) this.ks.getEntry(alias, null);
                if (entry != null) {
                    LOGGER.info(entry.getCertificate().toString());
                }
                else {
                    Assert.fail();
                }
            }
        }
        catch (final IOException e) {
            if (e instanceof ApduConnectionException) {
                LOGGER.warning(e.getMessage());
            }
        }
    }

    /** Comprueba que se obtiene la clave privada a partir del alias del certificado. En caso contrario se considera que el test ha fallado.
     * @throws KeyStoreException Si el almac&eacute;n no ha sido inicializado
     * @throws Exception En otros caso */
    @Test
    public void obtenerClaveAPartirDelAliasYClave() throws KeyStoreException, Exception {
        try {
            this.ks.load(null, null);
            for (final String alias : ALIASES) {
                final PrivateKey key = (PrivateKey) this.ks.getKey(alias, null);
                if (key != null) {
                    LOGGER.info(key.getAlgorithm());
                }
                else {
                    Assert.fail();
                }
            }
        }
        catch (final IOException e) {
            if (e instanceof ApduConnectionException) {
                LOGGER.warning(e.getMessage());
            }
        }
    }

    /** Trata de obtener el proveedor criptogr&aacute;fico del almacen de claves. Si este es nulo se considera que el test ha fallado.
     * @throws KeyStoreException Si el almac&eacute;n no ha sido inicializado
     * @throws Exception En otros caso */
    @Test
    public void obtenerProveedorDelAlmacen() throws KeyStoreException, Exception {
        try {
            this.ks.load(null, null);
            final Provider prov = this.ks.getProvider();
            if (prov != null) {
                LOGGER.info(prov.getInfo() + ": " + prov.getName() + " " + prov.getVersion()); //$NON-NLS-1$ //$NON-NLS-2$
            }
            else {
                Assert.fail("El proveedor criptografico es nulo."); //$NON-NLS-1$
            }
        }
        catch (final IOException e) {
            if (e instanceof ApduConnectionException) {
                LOGGER.warning(e.getMessage());
            }
        }
    }

    /** Comprueba que el almac&eacute;n de claves es de tipo DNI. En caso contrario el test se considera err&oacute;neo.
     * @throws KeyStoreException Si el almac&eacute;n no ha sido inicializado
     * @throws Exception En otros caso */
    @Test
    public void obtenerTipodeAlmacen() throws Exception {
        try {
            this.ks.load(null, null);
            Assert.assertEquals("No se obtuvo el tipo de almacen esperado. Esperado: " + AFIRMA_DNIE + "\tObtenido: " + this.ks.getType(), //$NON-NLS-1$ //$NON-NLS-2$
                                TestDnieKeyStoreProvider.AFIRMA_DNIE,
                                this.ks.getType());
        }
        catch (final IOException e) {
            if (e instanceof ApduConnectionException) {
                LOGGER.warning(e.getMessage());
            }
        }
    }

    /** Comprueba si la entrada identificada por el alias indicado almacena un certificado.
     * @throws KeyStoreException Si el almac&eacute;n no ha sido inicializado
     * @throws Exception En otros caso */
    @Test
    public void esUnCertificadoelAlias() throws KeyStoreException, Exception {
        try {
            this.ks.load(null, null);
            for (final String alias : ALIASES) {
                Assert.assertTrue(this.ks.isCertificateEntry(alias));
            }
        }
        catch (final IOException e) {
            if (e instanceof ApduConnectionException) {
                LOGGER.warning(e.getMessage());
            }
        }
    }

    /** Comprueba si la entrada identificada por el alias indicado almacena una clave.
     * @throws KeyStoreException Si el almac&eacute;n no ha sido inicializado
     * @throws Exception En otros casos */
    @Test
    public void esUnaClaveElAlias() throws Exception {
        try {
            this.ks.load(null, null);
            for (final String alias : ALIASES) {
                Assert.assertTrue(this.ks.isKeyEntry(alias));
            }
        }
        catch (final IOException e) {
            if (e instanceof ApduConnectionException) {
                LOGGER.warning(e.getMessage());
            }
        }
    }

    /** Obtiene el n&uacute;mero de certificados almacenados en el almac&eacute;n del proveedor de DNIe
     * @throws KeyStoreException Si el almac&eacute;n no ha sido inicializado
     * @throws Exception En otros casos */
    @Test
    public void obtenerNumeroDeCertificadosDelAlmacen() throws KeyStoreException, Exception {
        try {
            this.ks.load(null, null);
            Assert.assertEquals("El numero de certificados contenidos en el DNIe solo puede ser dos", 2, this.ks.size()); //$NON-NLS-1$
        }
        catch (final IOException e) {
            if (e instanceof ApduConnectionException) {
                LOGGER.warning(e.getMessage());
            }
        }
    }

    /** Comprueba que al tratar de obtener la fecha de creación de la entrada indicada por el alias se produce una excepción de tipo
     * {@link UnsupportedOperationException}
     * @throws NoSuchAlgorithmException Si el algoritmo que verifica la integridad del almac&eacute;n no se encuentra
     * @throws CertificateException Si no se pudo obtener alg&uacute;n certificado del almac&eacute;n
     * @throws KeyStoreException Si el almac&eacute;n no se inicializ&oacute; */
    @Test
    public void obtenerFechaDeCreacionDeCertificadoDebeProducirExcepcion() throws NoSuchAlgorithmException, CertificateException, KeyStoreException {
        try {
            this.ks.load(null, null);
            if (this.ks.aliases().hasMoreElements()) {
                this.ks.getCreationDate(this.ks.aliases().nextElement());
            }
        }
        catch (final IOException e) {
            if (e instanceof ApduConnectionException) {
                LOGGER.warning(e.getMessage());
            }
        }
        catch (final Exception e) {
            if (!(e instanceof UnsupportedOperationException)) {
                Assert.fail("Resultado no esperado. Se esperaba " + UnsupportedOperationException.class.getName() //$NON-NLS-1$
                            + " pero se obtuvo " //$NON-NLS-1$
                            + e.getClass().getName());
            }
        }
    }

    /** Comprueba que al tratar de a&ntilde;adir un nuevo registro en el almac&eacute;n se produce una excepción de tipo
     * {@link UnsupportedOperationException}
     * @throws NoSuchAlgorithmException Si el algoritmo que verifica la integridad del almac&eacute;n no se encuentra
     * @throws CertificateException Si no se pudo obtener alg&uacute;n certificado del almac&eacute;n
     * @throws KeyStoreException Si el almac&eacute;n no se inicializ&oacute; */
    @Test
    public void establecerUnNuevoRegistroDebeProducirExcepcion() throws NoSuchAlgorithmException, CertificateException, KeyStoreException {
        try {
            this.ks.load(null, null);
            final String alias = this.ks.aliases().nextElement();
            this.ks.setKeyEntry(alias + "-Test", this.ks.getCertificate(alias).getPublicKey().getEncoded(), this.ks.getCertificateChain(alias)); //$NON-NLS-1$
        }
        catch (final IOException e) {
            if (e instanceof ApduConnectionException) {
                LOGGER.warning(e.getMessage());
            }
        }
        catch (final Exception e) {
            if (!(e instanceof UnsupportedOperationException)) {
                Assert.fail("Resultado no esperado. Se esperaba " + UnsupportedOperationException.class.getName() //$NON-NLS-1$
                            + " pero se obtuvo " //$NON-NLS-1$
                            + e.getClass().getName());
            }
        }
    }

    /** Comprueba que al tratar de a&ntilde;adir un nuevo certificado en el almac&eacute;n se produce una excepción de tipo
     * {@link UnsupportedOperationException}
     * @throws NoSuchAlgorithmException Si el algoritmo que verifica la integridad del almac&eacute;n no se encuentra
     * @throws CertificateException Si no se pudo obtener alg&uacute;n certificado del almac&eacute;n
     * @throws KeyStoreException Si el almac&eacute;n no se inicializ&oacute; */
    @Test
    public void establecerUnNuevoCertificadoDebeProducirExcepcion() throws NoSuchAlgorithmException, CertificateException, KeyStoreException {
        try {
            this.ks.load(null, null);
            final String alias = this.ks.aliases().nextElement();
            this.ks.setCertificateEntry(alias + "-Test", this.ks.getCertificate(alias)); //$NON-NLS-1$
        }
        catch (final IOException e) {
            if (e instanceof ApduConnectionException) {
                LOGGER.warning(e.getMessage());
            }
        }
        catch (final Exception e) {
            if (!(e instanceof UnsupportedOperationException)) {
                Assert.fail("Resultado no esperado. Se esperaba " + UnsupportedOperationException.class.getName() //$NON-NLS-1$
                            + " pero se obtuvo " //$NON-NLS-1$
                            + e.getClass().getName());
            }
        }
    }

    /** Comprueba que al tratar de guardar el almac&eacute;n en un fichero externo se produce una excepción de tipo
     * {@link UnsupportedOperationException}
     * @throws NoSuchAlgorithmException Si el algoritmo que verifica la integridad del almac&eacute;n no se encuentra
     * @throws CertificateException Si no se pudo obtener alg&uacute;n certificado del almac&eacute;n
     * @throws KeyStoreException Si el almac&eacute;n no se inicializ&oacute; */
    @Test
    public void extraerElAlmacenAUnFicheroDebeProducirExcepcion() throws NoSuchAlgorithmException, CertificateException, KeyStoreException {
        try {
            this.ks.load(null, null);
            this.ks.store(new FileOutputStream(File.createTempFile(AFIRMA_DNIE, "tmp.jks")), PSSCALLBACK.getPassword()); //$NON-NLS-1$
        }
        catch (final IOException e) {
            if (e instanceof ApduConnectionException) {
                LOGGER.warning(e.getMessage());
            }
        }
        catch (final Exception e) {
            if (!(e instanceof UnsupportedOperationException)) {
                Assert.fail("Resultado no esperado. Se esperaba " + UnsupportedOperationException.class.getName() //$NON-NLS-1$
                            + " pero se obtuvo " //$NON-NLS-1$
                            + e.getClass().getName());
            }
        }
    }
    
	/** Prueba los m&eacute;todos comunes de los certificados impostados. 
	 * @throws IOException 
	 * @throws CertificateException 
	 * @throws NoSuchAlgorithmException 
	 * @throws KeyStoreException */
	@SuppressWarnings("static-method")
	@Test
	public void testFakeCertificateCommonMethods() throws NoSuchAlgorithmException, CertificateException, IOException, KeyStoreException {
        System.setProperty("es.gob.jmulticard.fastmode", "true"); //$NON-NLS-1$ //$NON-NLS-2$

    	// Se instancia el proveedor y se anade
    	final Provider p = new DnieProvider();
    	Security.addProvider(p);

    	// Se obtiene el almacen y se carga
    	final KeyStore ks1 = KeyStore.getInstance("DNI"); //$NON-NLS-1$
    	try {
            ks1.load(null, null);
        }
        catch (final ApduConnectionException e) {
            LOGGER.warning(e.toString());
            return;
        }

    	final String alias = "CertFirmaDigital"; //$NON-NLS-1$
    	
		FakeX509Certificate c = (FakeX509Certificate) ks1.getCertificate(alias);
		
		Assert.assertTrue(c.getCriticalExtensionOIDs().contains("2.5.29.15")); //$NON-NLS-1$
		Assert.assertTrue(c.getNonCriticalExtensionOIDs().contains("2.5.29.14")); //$NON-NLS-1$
		Assert.assertFalse(c.hasUnsupportedCriticalExtension());
		c.checkValidity();
		c.checkValidity(new Date());
		Assert.assertEquals(-1, c.getBasicConstraints());
		Assert.assertNotNull(c.getIssuerDN());
		Assert.assertNotNull(c.getSubjectDN());
		Assert.assertNotNull(c.getNotAfter());
		Assert.assertNotNull(c.getNotBefore());
		Assert.assertNotNull(c.getSerialNumber());
		Assert.assertEquals("SHA1withRSA", c.getSigAlgName()); //$NON-NLS-1$
		Assert.assertEquals("1.2.840.113549.1.1.5", c.getSigAlgOID()); //$NON-NLS-1$
		Assert.assertNull(c.getSigAlgParams());
		Assert.assertEquals(3, c.getVersion());
		LOGGER.info(c.toString());
	}
	
}
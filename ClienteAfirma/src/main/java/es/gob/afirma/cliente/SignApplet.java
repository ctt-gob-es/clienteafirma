/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.cliente;

import java.awt.Component;
import java.awt.Container;
import java.awt.Frame;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.net.URLDecoder;
import java.security.AccessController;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Vector;
import java.util.logging.Logger;

import javax.swing.JApplet;
import javax.swing.JOptionPane;
import javax.swing.UIManager;

import org.ietf.jgss.Oid;

import es.gob.afirma.EntryPointsCrypto;
import es.gob.afirma.EntryPointsUtil;
import es.gob.afirma.cliente.actions.BasicPrivilegedAction;
import es.gob.afirma.cliente.actions.CipherAction;
import es.gob.afirma.cliente.actions.CoEnvelopAction;
import es.gob.afirma.cliente.actions.DecipherAction;
import es.gob.afirma.cliente.actions.InitializePlatformAction;
import es.gob.afirma.cliente.actions.LoadFileAction;
import es.gob.afirma.cliente.actions.UnwrapAction;
import es.gob.afirma.cliente.actions.WrapAction;
import es.gob.afirma.cliente.utilidades.FileUtils;
import es.gob.afirma.cliente.utilidades.LdapUtils;
import es.gob.afirma.cliente.utilidades.NormalizedNames;
import es.gob.afirma.cliente.utilidades.browser.Browser;
import es.gob.afirma.cliente.utilidades.browser.FirmadorWeb.FirmaWeb;
import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.exceptions.AOCantSaveDataException;
import es.gob.afirma.exceptions.AOCertificateException;
import es.gob.afirma.exceptions.AOCertificateKeyException;
import es.gob.afirma.exceptions.AOCertificatesNotFoundException;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOFormatFileException;
import es.gob.afirma.exceptions.AOInvalidFormatException;
import es.gob.afirma.exceptions.AOInvalidKeyException;
import es.gob.afirma.exceptions.AOInvalidRecipientException;
import es.gob.afirma.exceptions.AOKeyStoreManagerException;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeystoreAlternativeException;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;
import es.gob.afirma.misc.AOConstants.AOKeyStore;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOSignConstants;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.AsynchronousSaveData;
import es.gob.afirma.misc.DirectorySignatureHelper;
import es.gob.afirma.misc.DirectorySignatureHelper.MassiveType;
import es.gob.afirma.misc.MassiveSignatureHelper;
import es.gob.afirma.misc.MassiveSignatureHelper.MassiveSignConfiguration;
import es.gob.afirma.misc.MimeHelper;
import es.gob.afirma.misc.Platform;
import es.gob.afirma.misc.SignText;
import es.gob.afirma.signers.AOCAdESSigner;
import es.gob.afirma.signers.AOCMSSigner;
import es.gob.afirma.signers.AOSigner;
import es.gob.afirma.signers.AOSignerFactory;
import es.gob.afirma.ui.AOUIManager;

/** Reimplementaci&oacute;n del Applet original de firma del cliente AFirma. */
public final class SignApplet extends JApplet implements EntryPointsCrypto, EntryPointsUtil {

    /** Separador utilizado para separar varios valores consecutivos en una
     * cadena devuelta por el Applet. */
    public static final String STRING_SEPARATOR = "\u0024%\u0024"; //$NON-NLS-1$

    /** N&uacute;meto de puerto por defecto para la conexi&oacute;n LDAP. */
    private static final int DEFAULT_LDAP_PORT = 389;

    /** Logger para la visualizaci&oacute;n de los mensajes por consola. */
    private static final Logger logger = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Nombre del fichero donde deben guardarse los datos (firmas, datos, etc.). */
    private String outputFile = null;

    /** URI de la fuente de datos. */
    private URI fileUri = null;

    /** Indica si la propiedad fileUri apunta a un fichero en base 64. */
    private boolean fileBase64 = false;

    /** Hash que se desea firmar. */
    private byte[] hash = null;

    /** Algoritmo de firma actual. */
    private String sigAlgo = AOConstants.DEFAULT_SIGN_ALGO;

    /** Formato de firma actual. */
    private String sigFormat = AOConstants.DEFAULT_SIGN_FORMAT;

    /** Modo de firma actual. */
    private String sigMode = AOConstants.DEFAULT_SIGN_MODE;

    /** Datos a operar criptogr&aacute;ficamente. */
    private byte[] data = null;

    /** Firma electr&oacute;nica, tanto la generada por las distintas operaciones
     * de firma como la utilizada en las operaciones de firma y cofirma. */
    private byte[] signData = null;

    // /** Indica que parte del buffer de datos ya ha sido le&iacute;do y
    // devuelto al usuario. */
    // private int trunkOffset = 0;
    //
    // /** Firma electr&oacute;nica en base 64. */
    // private char[] signDataB64 = null;

    /** URL de identificaci&oacute;n de la pol&iacute;tica de firma. */
    private URL policyId = null;

    /** Descripcion de la pol&iacute;tica de firma. */
    private String policyDesc = null;

    /** Calificador de la pol&iacute;tica de firma. */
    private URI policyQualifier = null;

    /** Manejador de las funcionalidades de cifrado del Cliente. */
    private CipherManager cipherManager = null;

    /** Manejador de las funcionalidades de ensobrado del Cliente. */
    private EnveloperManager enveloperManager = null;

    /** URL del servidor LDAP. */
    private String ldapServerUrl = null;

    /** Puerto del servidor LDAP. Por defecto, 389. */
    private int ldapServerPort = DEFAULT_LDAP_PORT;

    /** Principal del certificado que se desea recuperar del servidor LDAP. */
    private String ldapCertificatePrincipal = null;

    /** Firmantes o nodos que se desean contrafirmar. */
    private String[] signersToCounterSign = new String[0];

    /** Listado de hashes a firmar en una operaci&oacute;n de firma masiva. */
    private List<String> hashesToSign = null;

    /** Tipo de operaci&oacute;n masiva a realizar. Por defecto, multifirma
     * masiva. */
    private MassiveType massiveOperation = MassiveType.SIGN;

    /** Indica si se deben firmar los ficheros de las subcarpetas del directorio
     * seleccionado durante la operacion de firma masiva. */
    private boolean recursiveSignDir = false;

    /** Directorio de donde se toman los ficheros a firmar de forma masiva. */
    private String massiveInputDirectory = null;

    /** Directorio en donde se almacenar&aacute;n las firmas masivas. */
    private String massiveOutputDirectory = null;

    /** Indica si se debe respectar el formato de firma original para la
     * multifirma masiva. */
    private boolean originalFormat = true;

    /** Extensiones por las que debe filtrarse durante la firma masiva. */
    private String[] massiveExtFiltered = null;

    /** Almacena el resultado de la firma masiva, en donde todas las firmas
     * est&aacute;n en base 64 y concatenadas con '!' (cierre de
     * exclamaci&oacute;n). Este atributo almacena la firma expl&iacute;cita de
     * los ficheros que se han seleccionado (no de sus Hashes como en versiones
     * anteriores del cliente). */
    private String massiveSignData = null;

    /** Indica si se ha producido alg&uacute;n error durante la &uacute;ltima
     * operaci&oacute;n. */
    private boolean error = false;

    /** Indica si se deben mostrar o no los hashes de los ficheros que se vayan a
     * firmar. */
    private boolean showHashes = false;

    /** Indica si el comportamiento por defecto a la hora de mostrar los
     * certificados para firmar, es mostrar tambi&eacute;n los caducados. */
    private boolean defaultShowExpiratedCertificates = true;

    /** Indica si se deben mostrar o no los mensajes de error mediante un
     * di&aacute;logo de error. */
    private boolean showErrors = false;

    /** Mensaje asociado al &uacute;ltimo error producido. */
    private String errorMsg = ""; //$NON-NLS-1$

    /** Indica si el applet ha sido inicializado. */
    private boolean initializedApplet = false;

    /** Camino de un fichero de firma electronica. Se utiliza para la cofirma y
     * contrafirma. */
    private URI electronicSignatureFile = null;

    /** MimeType obtenido de los datos proporcionados. */
    private String dataMimeType = AOConstants.DEFAULT_MIMETYPE;

    /** MimeType establecido externamente para incorporar a la firma. */
    private String extMimeType = null;

    /** Descripci&oacute;n del tipo de datos firmados. */
    private String dataDescription = null;

    /** Listado de atributos firmados que se deben agregar a una firma. */
    private Map<org.ietf.jgss.Oid, String> signedAttributes = null;

    /** Listado de atributos sin firmar que se deben agregar a una firma. */
    private Map<org.ietf.jgss.Oid, Vector<String>> unsignedAttributes = null;

    /** Listado de propiedades gen&eacute;ricas establecidas para las firmas. */
    private Properties genericConfig = new Properties();

    /** Transformaciones XML que aplicar a los formatos de firma que las
     * soporten. */
    private List<AOXMLTransform> xmlTransforms = null;

    /** Gestor del almac&eacute;n de certificados. */
    private KeyStoreConfigurationManager ksConfigManager = null;

    /** Indica si debe mostrarse una advertencia para que se inserten los
     * dispositivos criptogr&aacute;ficos externos (principalmente, tarjetas
     * inteligentes) cuando el almac&eacute;n de certificados sea Mozilla o un
     * PKCS#11. */
    private boolean showMozillaSmartCardWarning = false;

    private static final long serialVersionUID = 5692094082535848369L;

    /** Construye la clase asegurandose de que se inicializan todas las
     * propiedades necesarias. */
    public SignApplet() {
        Platform.init();
        ksConfigManager = new KeyStoreConfigurationManager(AOKeyStore.PKCS12, this);
        cipherManager = new CipherManager(this);
        enveloperManager = new EnveloperManager(this);
    }

    @Override
    public void init() {

        final InitializePlatformAction initializePlatformAction = new InitializePlatformAction();
        initializePlatformAction.setUserAgent(getParameter("userAgent")); //$NON-NLS-1$
        AccessController.doPrivileged(initializePlatformAction);

        logger.info("Cliente @firma V3.2"); //$NON-NLS-1$
        logger.info("Versi\u00F3n: " + getVersion()); //$NON-NLS-1$

        logger.info("Sistema operativo: " + Platform.getOS().toString()); //$NON-NLS-1$
        logger.info("Version del sistema operativo: " + Platform.getOsVersion()); //$NON-NLS-1$
        logger.info("Arquitectura del sistema operativo: " + Platform.getOsArch()); //$NON-NLS-1$
        logger.info("Arquitectura del JRE: " + Platform.getJavaArch()); //$NON-NLS-1$

        this.setLookAndFeel();

        // Configuramos el almacen de claves que corresponda
        final AOKeyStore keyStore = this.configureDefaultStore(Platform.getOS(), Platform.getBrowser());
        ksConfigManager = new KeyStoreConfigurationManager(keyStore, this);

        logger.info("Almacen de certificados preestablecido: " + keyStore.getDescription()); //$NON-NLS-1$

        // Configuramos si se debe mostrar en los navegadores Mozilla un dialogo
        // de advertencia
        // acerca de los token externos. Por defecto, se mostraran.
        String paramValue = getParameter("showMozillaSmartCardWarning"); //$NON-NLS-1$
        if ((paramValue == null || !paramValue.trim().equalsIgnoreCase("false")) && //$NON-NLS-1$
            (keyStore == AOConstants.AOKeyStore.MOZ_UNI || keyStore == AOConstants.AOKeyStore.PKCS11)) {
            showMozillaSmartCardWarning = true;
            ksConfigManager.setLoadingWarning(true);
        }

        // Configuramos si se deben mostrar los certificados caducados (por
        // defecto, true)
        paramValue = getParameter("showExpiratedCertificates"); //$NON-NLS-1$
        defaultShowExpiratedCertificates = (paramValue == null || !paramValue.trim().equalsIgnoreCase("false")); //$NON-NLS-1$
        ksConfigManager.setShowExpiratedCertificates(defaultShowExpiratedCertificates);

        // Creamos el manejador para el cifrado de datos
        this.cipherManager = new CipherManager(this);

        // Creamos el manejador para el ensobrado de datos
        this.enveloperManager = new EnveloperManager(this);

        // Indicamos que el cliente ya se ha inicializado
        this.initializedApplet = true;

        // Imprimimos por consola el acuerdo de licencia por si el applet se ha
        // cargado sin la
        // ayuda del bootloader, que ya se encarga de mostrarsela al usuario.
        try {
            Logger.getLogger("es.gob.afirma").info(LicenseManager.getLicenceText()); //$NON-NLS-1$
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning("No se ha podido mostrar el acuerdo de licencia"); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }

    public void initialize() {
        logger.info("Invocando initialize"); //$NON-NLS-1$
        outputFile = null;
        fileUri = null;
        fileBase64 = false;
        hash = null;
        sigAlgo = AOConstants.DEFAULT_SIGN_ALGO;
        sigFormat = AOConstants.DEFAULT_SIGN_FORMAT;
        sigMode = AOConstants.DEFAULT_SIGN_MODE;
        data = null;
        signData = null;
        cipherManager.initialize();
        enveloperManager.initialize();
        ldapServerUrl = null;
        ldapServerPort = DEFAULT_LDAP_PORT;
        ldapCertificatePrincipal = null;
        dataMimeType = AOConstants.DEFAULT_MIMETYPE;
        extMimeType = null;
        dataDescription = null;
        signersToCounterSign = new String[0];
        hashesToSign = null;
        massiveOperation = MassiveType.SIGN;
        recursiveSignDir = false;
        originalFormat = true;
        massiveInputDirectory = null;
        massiveOutputDirectory = null;
        massiveExtFiltered = null;
        massiveSignData = null;
        error = false;
        errorMsg = ""; //$NON-NLS-1$
        electronicSignatureFile = null;
        signedAttributes = null;
        unsignedAttributes = null;
        genericConfig = new Properties();
        xmlTransforms = null;
        ksConfigManager.initialize();
        ksConfigManager.setShowExpiratedCertificates(defaultShowExpiratedCertificates);
    }

    public String getCertificatesAlias() {
        logger.info("Invocando getCertificatesAlias"); //$NON-NLS-1$
        return this.concatStrings(getArrayCertificatesAlias(), STRING_SEPARATOR);
    }

    public String[] getArrayCertificatesAlias() {

        logger.info("Invocando getArrayCertificatesAlias"); //$NON-NLS-1$
        return AccessController.doPrivileged(new java.security.PrivilegedAction<String[]>() {
            public String[] run() {

                try {
                    SignApplet.this.setError(null);
                    return ksConfigManager.getArrayCertificateAlias();
                }
                catch (final AOCancelledOperationException e) {
                    logger.severe("Operacion cancelada por el usuario: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                    return new String[0];
                }
                catch (final AOKeyStoreManagerException e) {
                    logger.severe("Error inicializando el almacen de claves, se devolvera una lista vacia de certificados: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
                    return new String[0];
                }
                catch (final AOKeystoreAlternativeException e) {
                    AOKeyStore kst = e.getAlternativeKsm();
                    if (kst == null) {
                        logger.severe("Error inicializando el almacen de claves: " + e); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
                        return new String[0]; //$NON-NLS-1$
                    }
                    if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(SignApplet.this,
                                                                                AppletMessages.getString("SignApplet.4") + " " + kst.getDescription() + "?", //$NON-NLS-1$ //$NON-NLS-2$
                                                                                AppletMessages.getString("SignApplet.658"), //$NON-NLS-1$ 
                                                                                JOptionPane.WARNING_MESSAGE)) {
                        setKeyStore(null, null, kst.toString());
                        return getArrayCertificatesAlias();
                    }
                    logger.severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                    return new String[0]; //$NON-NLS-1$
                }
            }
        });
    }

    public String getCertificates() {
        logger.info("Invocando getCertificates"); //$NON-NLS-1$
        return this.concatStrings(getArrayCertificates(), STRING_SEPARATOR);
    }

    public final String[] getArrayCertificates() {
        logger.info("Invocando getArrayCertificates"); //$NON-NLS-1$

        String[] certs = new String[0];
        try {
            String aliases[] = getArrayCertificatesAlias();
            certs = new String[aliases.length];
            for (int i = 0; i < aliases.length; i++)
                certs[i] = getCertificate(aliases[i]);
        }
        catch (final Exception e) {
            logger.severe("Error obteniendo los certificados, se devolvera null : " + e); //$NON-NLS-1$
            setError(AppletMessages.getString("SignApplet.9")); //$NON-NLS-1$
            return null;
        }
        return certs;
    }

    public final String getCertificate(final String alias) {
        logger.info("Invocando getCertificate: " + alias); //$NON-NLS-1$
        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {
                final X509Certificate cert = SignApplet.this.getCertificateBinary(alias);
                if (cert == null) return null;

                final String b64CertEncode;
                try {
                    b64CertEncode = AOCryptoUtil.encodeBase64(cert.getEncoded(), false);
                }
                catch (final Exception e) {
                    logger.severe("Error al codificar el certificado, se devolvera null: " + e); //$NON-NLS-1$
                    return null;
                }

                return "Bag Attributes\r\n" + //$NON-NLS-1$
                       "friendlyName: "
                       + AOUtil.getCN(cert)
                       + "\r\n" + //$NON-NLS-1$ //$NON-NLS-2$
                       "-----BEGIN CERTIFICATE-----\r\n"
                       + //$NON-NLS-1$
                       b64CertEncode
                       + "\r\n-----END CERTIFICATE-----"; //$NON-NLS-1$
            }
        });
    }

    public String getCertificatePublicKey(final String alias) {
        logger.info("Invocando getCertificatePublicKey: " + alias); //$NON-NLS-1$
        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {
                X509Certificate cert = SignApplet.this.getCertificateBinary(alias);
                if (cert == null) return null;

                String b64PKEncoded;
                try {
                    b64PKEncoded = AOCryptoUtil.encodeBase64(cert.getPublicKey().getEncoded(), false);
                }
                catch (final Exception e) {
                    logger.severe("Error al codificar la clave publica del certificado, se devolvera null: " + e); //$NON-NLS-1$
                    return null;
                }

                return "-----BEGIN RSA PUBLIC KEY-----\r\n" + //$NON-NLS-1$
                       b64PKEncoded
                       + "\r\n-----END RSA PUBLIC KEY-----"; //$NON-NLS-1$
            }
        });
    }

    /** Recupera un certificado del repositorio activo.
     * @param alias
     *        Alias del certificado que deseamos recuperar.
     * @return Certificado. */
    private final X509Certificate getCertificateBinary(final String alias) {
        return AccessController.doPrivileged(new java.security.PrivilegedAction<X509Certificate>() {
            public X509Certificate run() {
                try {
                    SignApplet.this.setError(null);
                    return (X509Certificate) SignApplet.this.ksConfigManager.getCertificate(alias);
                }
                catch (final AOCancelledOperationException e) {
                    logger.severe("Operacion cancelada por el usuario: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                    return null;
                }
                catch (final AOKeyStoreManagerException e) {
                    logger.severe("Error al inicializar el repositorio de certificados, se devolvera null: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
                    return null;
                }
                catch (final AOKeystoreAlternativeException e) {
                    AOKeyStore kst = e.getAlternativeKsm();
                    if (kst == null) {
                        logger.severe("Error al inicializar el repositorio de certificados: " + e); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
                        return null;
                    }
                    if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(SignApplet.this,
                                                                                AppletMessages.getString("SignApplet.4") + " " + kst.getDescription() + "?", //$NON-NLS-1$ //$NON-NLS-2$
                                                                                AppletMessages.getString("SignApplet.658"), //$NON-NLS-1$ 
                                                                                JOptionPane.WARNING_MESSAGE)) {
                        setKeyStore(null, null, kst.toString());
                        return getCertificateBinary(alias);
                    }
                    logger.severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                    return null;
                }
            }
        });
    }

    private void saveDataToStorage(final byte[] binaryData, final String filename) throws AOException {
        if (binaryData == null) throw new NullPointerException("Los datos que desea almacenar no pueden ser nulos"); //$NON-NLS-1$
        if (filename == null) throw new NullPointerException("El nombre de fichero de salida no puede ser nulo"); //$NON-NLS-1$

        FileOutputStream fos = null;
        try {
            fos = new FileOutputStream(filename);
            fos.write(binaryData);
        }
        catch (final Exception e) {
            throw new AOCantSaveDataException("No se pudieron almacenar los datos en disco: " + e); //$NON-NLS-1$
        }
        finally {
            if (fos != null) {
                try {
                    fos.close();
                }
                catch (final Exception e) {
                    logger.warning("No se ha podido cerrar el fichero de salida" //$NON-NLS-1$
                    );
                }
            }
        }
    }

    public final boolean saveDataToFile(final String strUri) {
        logger.info("Invocando setDataToFile: " + strUri); //$NON-NLS-1$

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {
                if (strUri == null || strUri.length() < 1) {
                    logger.severe("El nombre de fichero para guardar datos es incorrecto, no se salvaran los datos"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.14")); //$NON-NLS-1$
                    return false;
                }
                final URI uri;
                try {
                    uri = AOUtil.createURI(strUri);
                }
                catch (final Exception e) {
                    logger.severe("La URI proporcionada no es valida (" + strUri + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.15") + strUri); //$NON-NLS-1$
                    return false;
                }
                if (!uri.getScheme().equals("file")) { //$NON-NLS-1$
                    logger.severe("Solo se permite grabar en almacenamiento local, no mediante el protocolo " + uri.getScheme()); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.17") + uri.getScheme()); //$NON-NLS-1$
                    return false;
                }
                // OK, en este punto "fileUri" es un nombre de fichero,
                // con lo que ignoramos la uri
                // y lo tratamos como tal
                try {
                    saveDataToStorage(data, strUri);
                }
                catch (final Exception e) {
                    logger.severe("Error al almacenar los datos en disco: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.18")); //$NON-NLS-1$
                    return false;
                }

                SignApplet.this.setError(null);

                return true;
            }
        });
    }

    public boolean saveDataToFile() {
        logger.info("Invocando saveDataToFile"); //$NON-NLS-1$

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {
                saveFileAsinchronously(data, outputFile, null, null);
                SignApplet.this.setError(null);
                return true;
            }
        });
    }

    public boolean saveSignToFile() {
        logger.info("Invocando saveSignToFile"); //$NON-NLS-1$

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {
                if (signData == null || signData.length < 1) {
                    logger.severe("No se dispone de datos de firma, no se creara el fichero de firma"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.23")); //$NON-NLS-1$
                    return false;
                }

                String[] extensions = null;
                String description = null;

                if (outputFile == null || outputFile.length() < 1) {

                    extensions = new String[] {
                        "sig"}; //$NON-NLS-1$
                    description = AppletMessages.getString("SignApplet.25"); //$NON-NLS-1$

                    if (AOConstants.SIGN_FORMAT_CMS.equals(sigFormat)) {
                        description = AppletMessages.getString("SignApplet.29"); //$NON-NLS-1$
                        extensions = new String[] {
                                "csig", "p7s", "sig"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                    }

                    else if (AOConstants.SIGN_FORMAT_CADES.equals(sigFormat)) {
                        description = AppletMessages.getString("SignApplet.26"); //$NON-NLS-1$
                        extensions = new String[] {
                            "csig"}; //$NON-NLS-1$
                    }

                    else if (AOConstants.SIGN_FORMAT_XADES.equals(sigFormat) || AOConstants.SIGN_FORMAT_XADES_DETACHED.equals(sigFormat)
                             || AOConstants.SIGN_FORMAT_XADES_ENVELOPED.equals(sigFormat)
                             || AOConstants.SIGN_FORMAT_XADES_ENVELOPING.equals(sigFormat)
                             || AOConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED.equals(sigFormat)
                             || AOConstants.SIGN_FORMAT_XMLDSIG.equals(sigFormat)
                             || AOConstants.SIGN_FORMAT_XMLDSIG_DETACHED.equals(sigFormat)
                             || AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED.equals(sigFormat)
                             || AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING.equals(sigFormat)
                             || AOConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED.equals(sigFormat)
                             || AOConstants.SIGN_FORMAT_SOAP.equals(sigFormat)) {
                        description = AppletMessages.getString("SignApplet.27"); //$NON-NLS-1$
                        extensions = new String[] {
                                "xsig", "xml"}; //$NON-NLS-1$ //$NON-NLS-2$
                    }

                    else if (AOConstants.SIGN_FORMAT_PDF.equals(sigFormat)) {
                        description = AppletMessages.getString("SignApplet.28"); //$NON-NLS-1$
                        extensions = new String[] {
                            "pdf"}; //$NON-NLS-1$
                    }

                    else if (AOConstants.SIGN_FORMAT_OOXML.equals(sigFormat)) {
                        description = AppletMessages.getString("SignApplet.30"); //$NON-NLS-1$
                        extensions = new String[] {
                                "docx", "xlsx", "pptx"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                    }

                    else if (AOConstants.SIGN_FORMAT_ODF.equals(sigFormat)) {
                        description = AppletMessages.getString("SignApplet.32"); //$NON-NLS-1$
                        extensions = new String[] {
                                "odt", "ods", "odp"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                    }
                }

                // Almacenamos en disco
                saveFileAsinchronously(signData, outputFile, extensions, description);

                SignApplet.this.setError(null);

                return true;
            }
        });
    }

    /** Guarda datos en disco as&iacute;ncronamente. Si no se indica la la ruta
     * del fichero de salida se mostrar&aacute; un di&aacute;logo para su
     * selecci&oacute, mostrando la descripci&oacute;n y las extensiones
     * proporcionadas.<br/>
     * Esta llamada es asincrona &uacute;nicamente en Mozilla Firefox, para
     * evitar que muestre al usuario una advertencia indic&aacute;ndole que hay
     * un script boqueado y se le d&eacute; la posibilidad de detenerlo.
     * @param dat
     *        Datos que deseamos almacenar.
     * @param outputPath
     *        Ruta del fichero de salida.
     * @param extensions
     *        Extensiones permitidas para el di&aacute;logo de guardado.
     * @param description
     *        Descrìpci&oacute;n de los datos para el di&aacute;logo de
     *        guardado. */
    private void saveFileAsinchronously(final byte[] dat, final String outputPath, final String[] extensions, final String description) {
        final AsynchronousSaveData saveDataDialog =
                new AsynchronousSaveData(dat, outputPath, description, extensions, getParentFrame(SignApplet.this), true);
        // En firefox iniciamos un proceso asincrono para que no molesten los
        // dialogos
        // de script ocupado. En el resto de navegadores dejamos que aparezcan
        // estos
        // dialogos, porque los procesos asincronos pueden causar errores
        if (Platform.getBrowser().equals(Platform.BROWSER.FIREFOX)) new Thread(saveDataDialog).start();
        else saveDataDialog.run();
    }

    public final void setData(final String data) {
        logger.info("Invocando setData"); //$NON-NLS-1$
        if (data == null) {
            this.data = null;
            return;
        }

        AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
            public Void run() {
                try {
                    SignApplet.this.data = AOCryptoUtil.decodeBase64(data);
                }
                catch (final Exception e) {
                    logger.severe("Error al establecer los datos para la firma: " + e); //$NON-NLS-1$
                    SignApplet.this.data = null;
                }
                SignApplet.this.fileUri = null;
                SignApplet.this.fileBase64 = false;
                SignApplet.this.hash = null;

                return null;
            }
        });
    }

    public final void setFileuri(final String fu) {
        logger.info("Invocando setFileUri: " + fu); //$NON-NLS-1$
        if (fu == null || fu.trim().equals("")) { //$NON-NLS-1$
            this.fileUri = null;
            return;
        }

        AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
            public Void run() {
                try {
                    SignApplet.this.fileUri = AOUtil.createURI(fu);
                }
                catch (final Exception e) {
                    logger.severe("La URI proporcionada no es valida (" + fu + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
                    SignApplet.this.fileUri = null;
                }
                return null;
            }
        });

        SignApplet.this.data = null;
        SignApplet.this.hash = null;
        SignApplet.this.fileBase64 = false;
    }

    public final void setFileuriBase64(final String fu) {
        logger.info("Invocando setFileuriBase64: " + fu); //$NON-NLS-1$
        if (fu == null || fu.trim().equals("")) { //$NON-NLS-1$
            this.fileUri = null;
            return;
        }

        AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
            public Void run() {
                try {
                    SignApplet.this.fileUri = AOUtil.createURI(fu);
                    SignApplet.this.fileBase64 = true;
                }
                catch (final Exception e) {
                    logger.severe("La URI proporcionada no es valida (" + fu + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
                    SignApplet.this.fileUri = null;
                    SignApplet.this.fileBase64 = false;
                }
                SignApplet.this.data = null;
                SignApplet.this.hash = null;

                return null;
            }
        });
    }

    public final void setHash(final String hash) {
        logger.info("Invocando setHash: " + hash); //$NON-NLS-1$
        if (hash == null) {
            this.hash = null;
            return;
        }

        SignApplet.this.hash = AOCryptoUtil.decodeBase64(hash);
        SignApplet.this.dataMimeType = AOConstants.DEFAULT_MIMETYPE;
        SignApplet.this.fileUri = null;
        SignApplet.this.fileBase64 = false;
        SignApplet.this.data = null;
    }

    public void setElectronicSignature(final String inElectronicSignature) {
        logger.info("Invocando setElectronicSignature"); //$NON-NLS-1$
        if (inElectronicSignature == null || inElectronicSignature.length() == 0) {
            signData = null;
        }
        else {
            signData = AOCryptoUtil.decodeBase64(inElectronicSignature);
        }
    }

    public void setElectronicSignatureFile(final String inElectronicSignatureFile) {
        logger.info("Invocando inElectronicSignatureFile: " + inElectronicSignatureFile); //$NON-NLS-1$
        if (inElectronicSignatureFile == null || inElectronicSignatureFile.length() == 0) {
            electronicSignatureFile = null;
        }
        else {
            try {
                electronicSignatureFile = AOUtil.createURI(inElectronicSignatureFile);
            }
            catch (final Exception e) {
                logger.severe("La URI proporcionada no es valida (" + inElectronicSignatureFile + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
                electronicSignatureFile = null;
            }
        }
    }

    public void setSignersToCounterSign(final String signers) {
        logger.info("Invocando setSignersToCounterSign" + (signers != null ? ": " + signers.trim().replace('\n', ' ').replace("\r\n", " ") : "")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
        this.signersToCounterSign = (signers == null ? new String[0] : signers.split("(\\r\\n|\\n)")); //$NON-NLS-1$
    }

    public final String getSignersStructure() {
        logger.info("Invocando getSignersStructure"); //$NON-NLS-1$

        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {

                // Tomamos la firma que deseamos analizar
                byte[] originalSign = null;
                try {
                    originalSign = SignApplet.this.getSelectedSignature(true);
                }
                catch (final AOCancelledOperationException e) {
                    logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                    return null;
                }
                catch (final AOException e) {
                    logger.info("Error al seleccionar el fichero de firma: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.69")); //$NON-NLS-1$
                    return null;
                }

                // Probamos si la firma se corresponde con el formato
                // establecido y si no es asi
                // la analizamos y tomamos el manejador correspondiente
                AOSigner signer = AOCryptoUtil.getSigner(SignApplet.this.sigFormat);
                if (signer == null || !signer.isSign(originalSign)) {
                    signer = AOSignerFactory.getSigner(originalSign);
                }

                // Si la firma no esta en un formato soportado,
                // establecemos el error
                if (signer == null) {
                    logger.severe("La firma introducida no se ajusta a ningun formato soportado"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.63")); //$NON-NLS-1$
                    return null;
                }

                // Mostramos el el arbol de firmas
                try {
                    return AOUtil.showTreeAsString(signer.getSignersStructure(originalSign, false), null, null);
                }
                catch (final Exception e) {
                    logger.severe("Arbol de firmas no valido: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.72")); //$NON-NLS-1$
                    return null;
                }
            }
        });
    }

    public boolean counterSignTree() {
        logger.info("Invocando counterSignTree"); //$NON-NLS-1$
        return this.counterSign(AOSignConstants.CounterSignTarget.Tree);
    }

    public boolean counterSignLeafs() {
        logger.info("Invocando counterSignLeafs"); //$NON-NLS-1$
        return this.counterSign(AOSignConstants.CounterSignTarget.Leafs);
    }

    public boolean counterSignSigners() {
        logger.info("Invocando counterSignSigners"); //$NON-NLS-1$
        return this.counterSign(AOSignConstants.CounterSignTarget.Signers);
    }

    public boolean counterSignIndexes() {
        logger.info("Invocando counterSignIndexes"); //$NON-NLS-1$
        return this.counterSign(AOSignConstants.CounterSignTarget.Nodes);
    }

    private boolean counterSign(final AOSignConstants.CounterSignTarget target) {

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {
                // Si no hay establecido un algoritmo de firma, tomamos
                // el por defecto pero
                // solo para esta ocasion
                final String algorithm = (sigAlgo == null ? AOConstants.DEFAULT_SIGN_ALGO : sigAlgo);

                // Si no hay establecido un formato de firma, tomamos el
                // por defecto pero
                // solo para esta ocasion
                final String format = (sigFormat == null ? AOConstants.DEFAULT_SIGN_FORMAT : sigFormat);

                // Tomamos la firma sobre la que se realiza la
                // contrafirma
                final byte[] originalSign;
                try {
                    originalSign = SignApplet.this.getSelectedSignature(true);
                }
                catch (final AOCancelledOperationException e) {
                    logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                    return false;
                }
                catch (final AOException e) {
                    logger.info("Error al recuperar los datos de firma :" + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.64")); //$NON-NLS-1$
                    return false;
                }

                // Configuramos el certificado
                PrivateKeyEntry ke = SignApplet.this.configureCertificate();
                if (ke == null) {
                    return false;
                }

                // Tomamos el manejador del formato de firma
                AOSigner signer = AOCryptoUtil.getSigner(format);
                if (signer == null) {
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.95") + format); //$NON-NLS-1$
                    logger.severe("El formato de firma '" + format + //$NON-NLS-1$
                                  "' no esta soportado. Lo formatos soportados son:\n"
                                  + //$NON-NLS-1$
                                  AOSignerFactory.getInstance().toString());
                    return false;
                }

                // Establecemos el mimetype de los datos
                try {
                    final String mimeType = SignApplet.this.extMimeType != null ? SignApplet.this.extMimeType : SignApplet.this.dataMimeType;
                    if (mimeType == null) signer.setDataObjectFormat(SignApplet.this.dataDescription, null, null, null);
                    else {
                        final String oid = MimeHelper.transformMimeTypeToOid(mimeType);
                        signer.setDataObjectFormat(SignApplet.this.dataDescription,
                                                   oid == null ? null : new Oid(oid),
                                                   new javax.activation.MimeType(mimeType),
                                                   null);
                    }
                }
                catch (final Exception e) {
                    logger.warning("MimeType mal formado, se tratara de detectar el mimetype de los datos: " + e); //$NON-NLS-1$
                }

                // Obtenemos los parametros necesarios segun tipo de
                // contrafirma. Esto son los firmantes
                // para la contrafirma de firmantes, los nodos para la
                // contrafirma de nodos y ninguno
                // para la contrafirma de todos los nodos o solo los
                // nodos hoja.
                Object[] params = null;

                // Obtenemos los parametros para la contrafirma de
                // firmantes
                if (target == AOSignConstants.CounterSignTarget.Signers) {
                    if (SignApplet.this.signersToCounterSign == null || SignApplet.this.signersToCounterSign.length < 1) {
                        try {
                            params = AOUIManager.showSignersSelectionPane(signer.getSignersStructure(originalSign, false), SignApplet.this);
                        }
                        catch (final AOCancelledOperationException e) {
                            logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                            SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                            return false;
                        }
                        catch (final AOException e) {
                            logger.severe("Error al recuperar los firmantes a contrafirmar: " + e); //$NON-NLS-1$
                            SignApplet.this.setError(AppletMessages.getString("SignApplet.97")); //$NON-NLS-1$
                            return false;
                        }
                    }
                    else params = SignApplet.this.signersToCounterSign;
                }

                // Obtenemos los parametros para la contrafirma de nodos
                else if (target == AOSignConstants.CounterSignTarget.Nodes) {
                    // Si no se establecen los nodos de firma mediante
                    // setSignersToCounterSign(String)
                    // mostramos el panel de seleccion de nodos. Este
                    // panel devolvera los nodos de firma
                    // a partir del 0.
                    if (SignApplet.this.signersToCounterSign == null || SignApplet.this.signersToCounterSign.length < 1) {
                        int[] indexes;
                        try {
                            indexes = AOUIManager.showNodeSignSelectionPane(signer.getSignersStructure(originalSign, false), SignApplet.this);
                        }
                        catch (final AOCancelledOperationException ex) {
                            logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                            SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                            return false;
                        }
                        catch (final Exception ex) {
                            logger.severe("Error al seleccionar los nodos de firma: " + ex); //$NON-NLS-1$
                            SignApplet.this.setError(AppletMessages.getString("SignApplet.99")); //$NON-NLS-1$
                            return false;
                        }

                        if (indexes.length == 0) {
                            logger.severe("Se debe seleccionar al menos un nodo de firma para contrafirmar"); //$NON-NLS-1$
                            SignApplet.this.setError(AppletMessages.getString("SignApplet.100")); //$NON-NLS-1$
                            return false;
                        }

                        // Ordenamos el array de indices (requisito del
                        // metodo de contrafirma de nodos)
                        java.util.Arrays.sort(indexes);

                        // Los indices de firma del panel de seleccion
                        // se reciben a partir del 0.
                        params = new Object[indexes.length];
                        for (int i = 0; i < indexes.length; i++)
                            params[i] = Integer.valueOf(indexes[i]);
                    }
                    else {
                        params = new Object[SignApplet.this.signersToCounterSign.length];
                        for (int i = 0; i < SignApplet.this.signersToCounterSign.length; i++)
                            params[i] = Integer.valueOf(SignApplet.this.signersToCounterSign[i]);
                    }
                }

                // Si se han especificado atributos de firma los
                // agregamos. Esto solo sera efectivo
                // para los signers a los que aplique
                SignApplet.this.addAttributes(signer);

                // Si se nos pide que mostremos el hash de los datos a
                // firmar, lo hacemos
                if (SignApplet.this.showHashes) {
                    if (!SignApplet.this.showHashMessage()) {
                        logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                        return false;
                    }
                }

                // Contrafirmamos finalmente
                final byte[] outputBuffer;
                try {
                    outputBuffer = signer.countersign(originalSign, algorithm, target, params, ke, null);
                }
                catch (final UnsupportedOperationException e) {
                    logger.severe(e.getMessage());
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.2")); //$NON-NLS-1$
                    JOptionPane.showMessageDialog(SignApplet.this, AppletMessages.getString(AppletMessages.getString("SignApplet.682")), //$NON-NLS-1$
                                                  AppletMessages.getString("SignApplet.156"), //$NON-NLS-1$ 
                                                  JOptionPane.ERROR_MESSAGE);
                    return false;
                }
                catch (final Exception e) {
                    logger.severe("Error durante el proceso de firma: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.101")); //$NON-NLS-1$
                    return false;
                }

                // Ahora vamos a guardar el resultado en el fichero de
                // salida
                if (outputBuffer == null || outputBuffer.length < 1) {
                    // No vaya a ser que saliese un resultado vacio...
                    logger.severe("El proceso de contrafirma no genero ningun resultado"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.102")); //$NON-NLS-1$
                    return false;
                }
                signData = outputBuffer;

                SignApplet.this.setError(null);

                return true;
            }
        });
    }

    public void setOutFilePath(final String outFilePath) {
        logger.info("Invocando setOutFilePath: " + outFilePath); //$NON-NLS-1$
        if (outFilePath == null) {
            logger.info("Se ha establecido el nombre de fichero de salida a null" //$NON-NLS-1$
            );
            outputFile = null;
            return;
        }
        if (outFilePath.length() < 1) {
            logger.warning("El nombre de fichero de salida indicado esta vacio, se establecera a null" //$NON-NLS-1$
            );
            outputFile = null;
            return;
        }
        outputFile = outFilePath;
    }

    public void setSignatureAlgorithm(String signatureAlgorithm) {
        logger.info("Invocando setSignatureAlgorithm: " + signatureAlgorithm); //$NON-NLS-1$

        // Para mantener la interfaz con el exterior intacta, traduciremos
        // cualquier nombre de algoritmo antiguo a su nueva forma
        signatureAlgorithm = NormalizedNames.normalizeAlgorithmName(signatureAlgorithm);
        if (signatureAlgorithm == null) {
            logger.warning("El algoritmo de firma no puede ser nulo, se establecera el algoritmo por defecto" //$NON-NLS-1$
            );
            signatureAlgorithm = AOConstants.DEFAULT_SIGN_ALGO;
        }

        // Localizamos el algoritmo indicado entre los soportados
        for (String algo : AOConstants.SUPPORTED_SIGN_ALGOS) {
            if (algo.equals(signatureAlgorithm)) {
                sigAlgo = signatureAlgorithm;
                return;
            }
        }

        // Si el algoritmo no esta soportado, indicamos los soportado y
        // establecemos el por defecto
        final StringBuilder exstr = new StringBuilder("El algoritmo de firma '") //$NON-NLS-1$
        .append(signatureAlgorithm)
                                                                                .append(AppletMessages.getString("SignApplet.7")) //$NON-NLS-1$
                                                                                .append(AOConstants.DEFAULT_SIGN_ALGO)
                                                                                .append("\nLos algoritmos de firma soportados son:\n"); //$NON-NLS-1$
        for (String algo : AOConstants.SUPPORTED_SIGN_ALGOS) {
            exstr.append(algo).append("\n"); //$NON-NLS-1$
        }
        logger.warning(exstr.toString());
        sigAlgo = AOConstants.DEFAULT_SIGN_ALGO;
    }

    public void setSignatureFormat(String signatureFormat) {
        logger.info("Invocando setSignatureFormat: " + signatureFormat); //$NON-NLS-1$

        // Si no se establece formato alguno, se mantiene el por defecto
        if (signatureFormat == null) {
            logger.warning("El formato de firma no puede ser nulo, se establecera el formato por defecto: " + AOConstants.DEFAULT_SIGN_FORMAT //$NON-NLS-1$
            );
            signatureFormat = AOConstants.DEFAULT_SIGN_FORMAT;
        }

        // Para mantener la interfaz con el exterior intacta, traduciremos
        // cualquier nombre de formato antiguo a su nueva forma
        this.sigFormat = NormalizedNames.normalizeFormatName(signatureFormat);
    }

    public void setSignatureMode(String mode) {
        logger.info("Invocando setSignatureMode: " + mode); //$NON-NLS-1$
        // Para mantener la interfaz con el exterior intacta, traduciremos
        // cualquier nombre de modo antiguo a su nueva forma
        mode = NormalizedNames.normalizeModeName(mode);

        if (mode == null) {
            logger.warning("El modo de firma no puede ser nulo, se establecera el modo por defecto" //$NON-NLS-1$
            );
            mode = AOConstants.DEFAULT_SIGN_MODE;
        }
        this.sigMode = mode;
    }

    public String getSignatureMode() {
        logger.info("Invocando getSignatureMode"); //$NON-NLS-1$
        return (this.sigMode == null ? AOConstants.DEFAULT_SIGN_MODE : this.sigMode);
    }

    public final void setKeyStore(final String path, final String password, final String type) {

        logger.info("Invocando setKeyStore de tipo '" + type + "' con el path '" + path + "'"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
            public Void run() {
                final AOKeyStore newStore = AOKeyStoreManager.getKeyStore(NormalizedNames.normalizeKeyStoreName(type));

                if (newStore == null) {
                    setError(AppletMessages.getString("SignApplet.3")); //$NON-NLS-1$
                    return null;
                }

                // Guardamos la contrasena porque nos es necesaria tanto
                // para abrir el almacen como
                // para utilizar el certificado
                ksConfigManager.setKsPath(path);
                ksConfigManager.setKsPassword(password);
                ksConfigManager.changeKeyStore(newStore);

                if (showMozillaSmartCardWarning && (newStore == AOConstants.AOKeyStore.MOZ_UNI || newStore == AOConstants.AOKeyStore.MOZILLA || newStore == AOConstants.AOKeyStore.PKCS11)) {
                    ksConfigManager.setLoadingWarning(true);
                }

                setError(null);
                return null;
            }
        });
    }

    public void setPolicy(final String identifier, final String description, final String qualifier) {
        // Configuramos la URL identificadora
        if (identifier != null) {
            try {
                this.policyId = AOUtil.createURI(identifier).toURL();
            }
            catch (final Exception e) {
                logger.severe("No se ha indicado un URL valida para la politica: " + e); //$NON-NLS-1$
            }
        }
        // Configuramos Oid calificador
        if (qualifier != null) {
            // Miramos a ver si es un OID directamente, en cuyo caso lo pasamos
            // a URN
            try {
                this.policyQualifier = new URI("urn:oid:" + new Oid(qualifier));
            }
            catch (final Exception e1) {
                // No es un OID directamente, miramos si es URI
                try {
                    this.policyQualifier = new URI(qualifier);
                }
                catch (final Exception e2) {
                    logger.severe("El calificador indicado no es ni un OID ni una URI valida: " + e1 + ", " + e2 //$NON-NLS-1$ //$NON-NLS-2$
                    );
                }
            }
        }
        // Configuramos la descripcion
        this.policyDesc = description;
    }

    public boolean sign() {
        logger.info("Invocando sign"); //$NON-NLS-1$

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {

                // Si no esta establecido el algoritmo usamos el por
                // defecto, pero solo para esta ocasion,
                // no lo establecemos para posteriores
                final String algorithm = (sigAlgo == null ? AOConstants.DEFAULT_SIGN_ALGO : sigAlgo);

                // Si no esta establecido el formato usamos el por
                // defecto, pero solo para esta ocasion,
                // no lo establecemos para posteriores
                final String format = (sigFormat == null ? AOConstants.DEFAULT_SIGN_FORMAT : sigFormat);

                // Si no esta establecido el modo usamos el por defecto,
                // pero solo para esta ocasion,
                // no lo establecemos para posteriores
                final String mode = (sigMode == null ? AOConstants.DEFAULT_SIGN_MODE : sigMode);

                // Para mantener las formas de la version 2.4 del
                // cliente, se mostrara una
                // ventana modal, en caso de solicitarse una firma
                // Enveloped en modo explicito,
                // informando de que esta configuracion es imposible
                if ((format.equals(AOConstants.SIGN_FORMAT_XADES_ENVELOPED) || format.equals(AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)) && mode.equals(AOConstants.SIGN_MODE_EXPLICIT)) {

                    logger.severe("El formato Enveloped es incompatible con el modo de firma explicito"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.139")); //$NON-NLS-1$
                    return false;
                }

                // Tomamos el Signer adecuado
                final AOSigner signer = AOCryptoUtil.getSigner(format);
                if (signer == null) {
                    logger.severe("El formato de firma '" + format + //$NON-NLS-1$
                                  "' no esta soportado. Lo formatos soportados son:\n"
                                  + //$NON-NLS-1$
                                  AOSignerFactory.getInstance().toString());
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.95") + format); //$NON-NLS-1$
                    return false;
                }

                // Como condicion excepcional, si se nos ha introducido
                // un hash para firmar
                // estableceremos el algoritmo hash que se realizo el
                // hash segun el algoritmo que
                // se nos solicitase
                if (hash != null && mode.equals(AOConstants.SIGN_MODE_EXPLICIT)) {
                    int withPos = algorithm.indexOf("with"); //$NON-NLS-1$
                    if (withPos == -1) { //$NON-NLS-1$
                        logger.severe("El formato del algoritmo de firma no es valido: " + algorithm); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.197") + algorithm); //$NON-NLS-1$
                    }
                    genericConfig.setProperty("precalculatedHashAlgorithm", algorithm.substring(0, withPos)); //$NON-NLS-1$
                }

                // -----------------------
                // Evitamos las configuraciones de firma de hashes no
                // soportadas
                // -----------------------

                // La firma de hashes solo esta soportada por los
                // formatos de firma: CMS, CAdES, XMLdSig y XAdES
                if (hash != null && (sigFormat.equals(AOConstants.SIGN_FORMAT_PDF) || sigFormat.equals(AOConstants.SIGN_FORMAT_ODF)
                                     || sigFormat.equals(AOConstants.SIGN_FORMAT_OOXML) || sigFormat.equals(AOConstants.SIGN_FORMAT_PKCS1))) {

                    logger.severe("La firma de hash no esta soportada para el formato " + sigFormat); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.198") + sigFormat); //$NON-NLS-1$
                    return false;
                }

                // La firma implicita de hash exige que se introduzcan
                // los datos a los que corresponde el hash.
                // Deben haberse introducido los datos, no se permite el
                // fichero por incompatibilidad con
                // las funciones de soporte del estilo
                // "getFileBase64Encoded",
                // "getFileHashBase64Encoded",...
                if (hash != null && data == null && mode.equals(AOConstants.SIGN_MODE_IMPLICIT)) {
                    logger.severe("La firma implicita de hash exige que se introduzcan los datos a los que corresponde el hash"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.216")); //$NON-NLS-1$
                    return false;
                }

                logger.info("Firma con algoritmo " + algorithm + ", formato " + format + " y modo " + mode); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

                // Si no se nos ha introducido un listado de hashes
                // entendemos que debe realizarse
                // una firma corriente. En este caso, se tomara
                // cualquier parametro de entrada establecido
                // como origen de los datos (dato, fichero y hash).
                byte[] dataToSign = null;
                if (SignApplet.this.hashesToSign == null) {

                    try {
                        dataToSign = SignApplet.this.getInData();
                    }
                    catch (final AOException e) {
                        // El metodo getInDataStream ya se habra
                        // encargado de establecer el mensaje en caso de
                        // error
                        return false;
                    }

                    // Establecemos el formato de los datos
                    final String mimeType = SignApplet.this.extMimeType != null ? SignApplet.this.extMimeType : SignApplet.this.dataMimeType;
                    if (mimeType == null) signer.setDataObjectFormat(SignApplet.this.dataDescription, null, null, null);
                    else {
                        try {
                            final String oid = MimeHelper.transformMimeTypeToOid(mimeType);
                            signer.setDataObjectFormat(SignApplet.this.dataDescription,
                                                       oid == null ? null : new Oid(oid),
                                                       new javax.activation.MimeType(mimeType),
                                                       null);
                        }
                        catch (final Exception e) {
                            logger.warning("No se ha podido establecer el formato de los datos firmados: " + e); //$NON-NLS-1$
                        }
                    }
                }

                // Configuramos el certificado
                PrivateKeyEntry ke = SignApplet.this.configureCertificate();
                if (ke == null) {
                    return false;
                }

                // Si se nos ha introducido un listado de hashes
                // entendemos que deben firmarse estos.
                // En este caso, se ignorara cualquier otro parametro de
                // entrada de datos de firma (dato,
                // fichero y hash).
                if (SignApplet.this.hashesToSign != null && SignApplet.this.hashesToSign.size() > 0) {

                    String[] signs = null;
                    DirectorySignatureHelper massiveSigner;
                    try {
                        massiveSigner = new DirectorySignatureHelper(algorithm, format, mode);
                    }
                    catch (final Exception e) {
                        logger.severe("No se pudo inicializar el modulo de firma masiva: " + e); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.200")); //$NON-NLS-1$
                        return false;
                    }

                    // Configuramos la politica
                    SignApplet.this.configurePolicy();

                    // Configuramos las transformaciones XML
                    SignApplet.this.configureXMLTransforms();

                    // Configuramos y ejecutamos la operacion
                    genericConfig.setProperty("format", format); //$NON-NLS-1$
                    genericConfig.setProperty("mode", mode); //$NON-NLS-1$
                    genericConfig.setProperty("ignoreStyleSheets", "true"); //$NON-NLS-1$ //$NON-NLS-2$

                    try {
                        signs =
                                massiveSigner.hashesMassiveSign(SignApplet.this.hashesToSign.toArray(new String[0]),
                                                                ke,
                                                                (X509Certificate) ke.getCertificate(),
                                                                // Recuperamos el signer que se
                                                                // utilizara para la operacion de firma
                                                                // masiva para poder agregarle
                                                                // antes los atributos de firma que
                                                                // correspondan
                                                                SignApplet.this.addAttributes(massiveSigner.getDefaultSigner()),
                                                                SignApplet.this.showHashes,
                                                                genericConfig);
                    }
                    catch (final AOException e) {
                        logger.severe("Error durante la operacion de firma masiva de hashes: " + e); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.236")); //$NON-NLS-1$
                        return false;
                    }
                    StringBuilder allSigns = new StringBuilder();
                    for (int i = 0; i < signs.length; i++) {
                        allSigns.append(signs[i]);
                        if (i < signs.length - 1) {
                            allSigns.append("!"); //$NON-NLS-1$
                        }
                    }
                    SignApplet.this.massiveSignData = allSigns.toString();
                }

                // Realizamos la operacion corriente de firma.
                else {

                    // Si se han especificado atributos de firma los
                    // agregamos. Esto solo sera efectivo
                    // para los signers a los que aplique
                    SignApplet.this.addAttributes(signer);

                    // Configuramos la politica
                    SignApplet.this.configurePolicy();

                    // Configuramos las trasnformaciones XML
                    SignApplet.this.configureXMLTransforms();

                    // Si se nos pide que mostremos el hash de los datos
                    // a firmar, lo hacemos
                    if (SignApplet.this.showHashes) {
                        if (!SignApplet.this.showHashMessage()) {
                            logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                            SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                            return false;
                        }
                    }

                    // Agregamos las ultimas configuraciones y firmamos
                    genericConfig.setProperty("mode", mode); //$NON-NLS-1$
                    genericConfig.setProperty("format", format); //$NON-NLS-1$
                    if (SignApplet.this.fileUri != null) genericConfig.setProperty("uri", SignApplet.this.fileUri.toASCIIString()); //$NON-NLS-1$

                    final byte[] outputBuffer;
                    try {
                        outputBuffer = signer.sign(dataToSign, algorithm, ke, genericConfig);
                    }
                    catch (final UnsupportedOperationException e) {
                        logger.severe(e.getMessage()); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString(AppletMessages.getString("SignApplet.682"))); //$NON-NLS-1$
                        return false;
                    }
                    catch (final AOFormatFileException e) {
                        logger.severe(e.getMessage()); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.11")); //$NON-NLS-1$
                        return false;
                    }
                    catch (final AOException e) {
                        logger.severe(e.toString()); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.101")); //$NON-NLS-1$
                        return false;
                    }
                    catch (final Exception e) {
                        logger.severe("Error durante el proceso de firma: " + e); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.101")); //$NON-NLS-1$
                        return false;
                    }

                    // Ahora vamos a guardar el resultado en el fichero
                    // de salida
                    if (outputBuffer == null || outputBuffer.length < 1) {
                        // No vaya a ser que saliese un resultado
                        // vacio...
                        logger.severe("El proceso de firma no genero ningun resultado"); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.154")); //$NON-NLS-1$
                        return false;
                    }

                    signData = outputBuffer;
                }

                SignApplet.this.setError(null);

                return true;
            }
        });
    }

    /** Agrega las transformaciones XML configuradas en el cliente a la
     * configuraci&oacute;n de firma. */
    private void configureXMLTransforms() {
        if (xmlTransforms != null) {
            genericConfig.setProperty("xmlTransforms", Integer.toString(xmlTransforms.size())); //$NON-NLS-1$
            for (int i = 0; i < xmlTransforms.size(); i++) {
                genericConfig.setProperty("xmlTransform" + (i) + "Type", xmlTransforms.get(i).getType()); //$NON-NLS-1$ //$NON-NLS-2$
                // El subtipo y el cuerpo son opcionales
                if (xmlTransforms.get(i).getSubtype() != null) genericConfig.setProperty("xmlTransform" + (i) + "Subtype", xmlTransforms.get(i).getSubtype()); //$NON-NLS-1$ //$NON-NLS-2$
                if (xmlTransforms.get(i).getBody() != null) genericConfig.setProperty("xmlTransform" + (i) + "Body", xmlTransforms.get(i).getBody()); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
    }

    /** Agrega los atributos firmados y sin firmar definidos al manejador de
     * firma cuando este corresponda a un formato de firma que los soporte.
     * @param signer
     *        Manejador de firma.
     * @return Manejador de firma configurado. */
    private AOSigner addAttributes(final AOSigner signer) {
        // Si el Signer soporta la agregacion de atributos
        if (signer instanceof AOCMSSigner || signer instanceof AOCAdESSigner) {

            // Agregamos los atributos firmados
            if (SignApplet.this.signedAttributes != null) {
                Iterator<org.ietf.jgss.Oid> itOid = SignApplet.this.signedAttributes.keySet().iterator();
                while (itOid.hasNext()) {
                    org.ietf.jgss.Oid oid = itOid.next();
                    ((AOCMSSigner) signer).addSignedAttribute(oid, SignApplet.this.signedAttributes.get(oid).getBytes());
                }
            }

            // Agregamos los atributos sin firmar
            if (SignApplet.this.unsignedAttributes != null) {
                Iterator<org.ietf.jgss.Oid> itOid = SignApplet.this.unsignedAttributes.keySet().iterator();
                while (itOid.hasNext()) {
                    org.ietf.jgss.Oid oid = itOid.next();
                    for (String value : SignApplet.this.unsignedAttributes.get(oid)) {
                        ((AOCMSSigner) signer).addUnsignedAttribute(oid, value.getBytes());
                    }
                }
            }
        }
        return signer;
    }

    /** Agrega la pol&iacute;tica de firma a la configuraci&oacute;n de la
     * operaci&oacute;n de firma.
     * @see #setPolicy(String, String, String) */
    private void configurePolicy() {
        if (this.policyId != null) genericConfig.setProperty("policyIdentifier", this.policyId.toString()); //$NON-NLS-1$
        if (this.policyDesc != null) genericConfig.setProperty("policyDescription", this.policyDesc); //$NON-NLS-1$
        if (this.policyQualifier != null) genericConfig.setProperty("policyQualifier", this.policyQualifier.toString()); //$NON-NLS-1$
    }

    public boolean coSign() {
        logger.info("Invocando cosign"); //$NON-NLS-1$

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {

                // No establecemos algoritmo por defecto, si no esta
                // establecido usamos el por
                // defecto, pero solo para esta ocasion
                String algorithm = (sigAlgo == null ? AOConstants.DEFAULT_SIGN_ALGO : sigAlgo);

                // No establecemos formato por defecto, si no esta
                // establecido usamos el por
                // defecto, pero solo para esta ocasion
                String format = (sigFormat == null ? AOConstants.DEFAULT_SIGN_FORMAT : sigFormat);

                // No establecemos formato por defecto, si no esta
                // establecido usamos el por
                // defecto, pero solo para esta ocasion
                String mode = (sigMode == null ? AOConstants.DEFAULT_SIGN_MODE : sigMode);

                // Para mantener las formas de la version 2.4 del
                // cliente, se mostrara una
                // ventana modal, en caso de solicitarse una firma
                // Enveloped en modo explicito,
                // informando de que esta configuracion es imposible
                if ((format.equals(AOConstants.SIGN_FORMAT_XADES_ENVELOPED) || format.equals(AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)) && mode.equals(AOConstants.SIGN_MODE_EXPLICIT)) {
                    logger.severe("El formato Enveloped es incompatible con el modo de firma explicito"); //$NON-NLS-1$
                    setError(AppletMessages.getString("SignApplet.139")); //$NON-NLS-1$
                    return false;
                }

                // Tomamos los datos que debemos firmar
                byte[] dataToSign = null;
                try {
                    dataToSign = SignApplet.this.getInData();
                }
                catch (final AOException e) {
                    // El metodo getInDataStream ya se habra encargado
                    // de establecer el mensaje en caso de error
                    return false;
                }

                // Tomamos la firma sobre la que se realiza la
                // contrafirma
                final byte[] originalSign;
                try {
                    originalSign = SignApplet.this.getSelectedSignature(true);
                }
                catch (final AOCancelledOperationException e) {
                    logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                    return false;
                }
                catch (final AOException e) {
                    logger.info("Error al recuperar los datos de firma: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.64")); //$NON-NLS-1$
                    return false;
                }

                // Tomamos el manejador de firma asociado al formato
                AOSigner signer = AOCryptoUtil.getSigner(format);
                if (signer == null) {
                    logger.severe("El formato de firma '" + format + //$NON-NLS-1$
                                  "' no esta soportado. Lo formatos soportados son:\n"
                                  + //$NON-NLS-1$
                                  AOSignerFactory.getInstance().toString());
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.95") + format); //$NON-NLS-1$
                    return false;
                }

                if (hash != null && mode.equals(AOConstants.SIGN_MODE_EXPLICIT)) {
                    int withPos = algorithm.indexOf("with"); //$NON-NLS-1$
                    if (withPos == -1) { //$NON-NLS-1$
                        logger.severe("El formato del algoritmo de firma no es valido: " + algorithm); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.197") + algorithm); //$NON-NLS-1$
                        return false;
                    }
                    // Establecemos el algoritmo con el que se calculo
                    // el hash externamente
                    genericConfig.setProperty("precalculatedHashAlgorithm", algorithm.substring(0, withPos)); //$NON-NLS-1$
                }

                /*
                 * La firma de hashes solo esta soportada por los
                 * formatos de firma binaria CMS y CAdES. Las firmas
                 * PDF, ODF y OOXML requieren siempre los datos, ya que
                 * van empotradas. Las firmas XML los necesitan para
                 * hacer la referencia a los mismos.
                 */
                if (hash != null && !sigFormat.equals(AOConstants.SIGN_FORMAT_CMS) && !sigFormat.equals(AOConstants.SIGN_FORMAT_CADES)) {
                    logger.severe("La firma de hashes solo esta soportada por los formatos de firma binaria CMS y CAdES"); //$NON-NLS-1$
                    setError(AppletMessages.getString("SignApplet.277")); //$NON-NLS-1$			//TODO: Permitir cofirma de hashes en XML
                    return false;
                }

                /*
                 * Evitamos las configuraciones de firma de hashes no
                 * soportadas
                 */

                // La firma implicita de hash exige que se introduzcan
                // los datos a los que corresponde el hash
                if (hash != null && data == null && mode.equals(AOConstants.SIGN_MODE_IMPLICIT)) {
                    logger.severe("La firma implicita de huella digital exige que se introduzcan los datos a los que corresponde la huella digital"); //$NON-NLS-1$
                    setError(AppletMessages.getString("SignApplet.216")); //$NON-NLS-1$
                    return false;
                }

                // Establecemos el formato de los datos
                String mimeType = SignApplet.this.extMimeType != null ? SignApplet.this.extMimeType : SignApplet.this.dataMimeType;
                if (mimeType == null) signer.setDataObjectFormat(SignApplet.this.dataDescription, null, null, null);
                else {
                    try {
                        String oid = MimeHelper.transformMimeTypeToOid(mimeType);
                        signer.setDataObjectFormat(SignApplet.this.dataDescription,
                                                   oid == null ? null : new Oid(oid),
                                                   new javax.activation.MimeType(mimeType),
                                                   null);
                    }
                    catch (final Exception e) {
                        logger.warning("No se ha podido establecer el formato de los datos firmados: " + e); //$NON-NLS-1$
                    }
                }

                // Configuramos el certificado
                final PrivateKeyEntry ke = SignApplet.this.configureCertificate();
                if (ke == null) {
                    return false;
                }

                // Si se han especificado atributos de firma los
                // agregamos. Esto solo sera efectivo
                // para los signers a los que aplique
                SignApplet.this.addAttributes(signer);

                // Si se nos pide que mostremos el hash de los datos a
                // firmar, lo hacemos
                if (SignApplet.this.showHashes) {
                    if (!SignApplet.this.showHashMessage()) {
                        logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                        return false;
                    }
                }

                // Finalmente, configuramos y operamos
                genericConfig.setProperty("mode", mode); //$NON-NLS-1$
                if (SignApplet.this.fileUri != null) genericConfig.setProperty("uri", SignApplet.this.fileUri.toASCIIString()); //$NON-NLS-1$

                byte[] outputBuffer;
                try {
                    outputBuffer = signer.cosign(dataToSign, originalSign, algorithm, ke, genericConfig);
                }
                catch (final UnsupportedOperationException e) {
                    logger.severe(AppletMessages.getString("SignApplet.682") + ": " + e.getMessage()); //$NON-NLS-1$  //$NON-NLS-2$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.682")); //$NON-NLS-1$
                    return false;
                }
                catch (final Exception e) {
                    logger.severe("Error durante el proceso de firma: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.101")); //$NON-NLS-1$
                    return false;
                }

                // Ahora vamos a guardar el resultado en el fichero de
                // salida
                if (outputBuffer == null || outputBuffer.length < 1) {
                    // No vaya a ser que saliese un resultado vacio...
                    logger.severe("El proceso de firma no genero ningun resultado"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.154")); //$NON-NLS-1$
                    return false;
                }
                signData = outputBuffer;

                SignApplet.this.setError(null);

                return true;
            }
        });
    }

    /** Muestra un di&aacute;logo para la selecci&oacute;n de un fichero de
     * firma. En caso de que se indique un formato, se usaran filtros para las
     * extensiones predeterminadas para el formato de firma concreto. Si no se
     * selecciona ningun fichero, se devolver&aacute; {@code null}.
     * @param signFormat
     *        Formato de la firma que se desea seleccionar.
     * @return Fichero de firma. */
    private String selectSignFile(String signFormat) {

        String[] exts = null;
        String desc = null;

        if (signFormat != null) {
            if (signFormat.equals(AOConstants.SIGN_FORMAT_CMS)) {
                exts = new String[] {
                        "csig", "p7s", "sig"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                desc = AppletMessages.getString("SignApplet.29"); //$NON-NLS-1$
            }
            else if (signFormat.equals(AOConstants.SIGN_FORMAT_CADES)) {
                exts = new String[] {
                        "csig", "sig"}; //$NON-NLS-1$ //$NON-NLS-2$
                desc = AppletMessages.getString("SignApplet.26"); //$NON-NLS-1$
            }
            else if (signFormat.equals(AOConstants.SIGN_FORMAT_XMLDSIG_DETACHED) || signFormat.equals(AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)
                     || signFormat.equals(AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING)
                     || signFormat.equals(AOConstants.SIGN_FORMAT_XADES_DETACHED)
                     || signFormat.equals(AOConstants.SIGN_FORMAT_XADES_ENVELOPED)
                     || signFormat.equals(AOConstants.SIGN_FORMAT_XADES_ENVELOPING)) {
                exts = new String[] {
                        "xsig", "sig", "xml"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                desc = AppletMessages.getString("SignApplet.27"); //$NON-NLS-1$
            }
            else if (signFormat.equals(AOConstants.SIGN_FORMAT_PKCS1)) {
                exts = new String[] {
                    "sig"}; //$NON-NLS-1$
                desc = AppletMessages.getString("SignApplet.318"); //$NON-NLS-1$
            }
            else if (signFormat.equals(AOConstants.SIGN_FORMAT_PDF)) {
                exts = new String[] {
                    "pdf"}; //$NON-NLS-1$
                desc = AppletMessages.getString("SignApplet.28"); //$NON-NLS-1$
            }
            else if (signFormat.equals(AOConstants.SIGN_FORMAT_ODF)) {
                exts = new String[] {
                        "odt", "ods", "odp"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                desc = AppletMessages.getString("SignApplet.32"); //$NON-NLS-1$
            }
            else if (signFormat.equals(AOConstants.SIGN_FORMAT_OOXML)) {
                exts = new String[] {
                        "docx", "xlsx", "pptx"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                desc = AppletMessages.getString("SignApplet.30"); //$NON-NLS-1$
            }
        }

        return AOUIManager.getLoadFileName(AppletMessages.getString("SignApplet.163"), //$NON-NLS-1$
                                           exts,
                                           desc,
                                           SignApplet.this);
    }

    public void setInIncludeExtensions(String extensions) {
        logger.info("Invocando setInIncludeExtensions: " + extensions); //$NON-NLS-1$
        this.massiveExtFiltered = (extensions == null ? null : extensions.split(",")); //$NON-NLS-1$
    }

    public boolean signDirectory() {
        logger.info("Invocando signDirectory"); //$NON-NLS-1$

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {
                if ((sigFormat.equals(AOConstants.SIGN_FORMAT_XADES_ENVELOPED) && sigMode.equals(AOConstants.SIGN_MODE_EXPLICIT)) || (sigFormat.equals(AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED) && sigMode.equals(AOConstants.SIGN_MODE_EXPLICIT))) {
                    error = true;
                    logger.severe("El formato Enveloped es incompatible con el modo de firma explicito"); //$NON-NLS-1$
                    setError(AppletMessages.getString("SignApplet.139")); //$NON-NLS-1$
                    return false;
                }

                // Si no se ha establecido el directorio de entrada de
                // ficheros, lo solicitamos
                final String inputDir;
                if (SignApplet.this.massiveInputDirectory != null) {
                    inputDir = SignApplet.this.massiveInputDirectory;
                }
                else {
                    inputDir = AOUIManager.selectDirectory(SignApplet.this, AppletMessages.getString("SignApplet.187")); //$NON-NLS-1$
                    if (inputDir.equals("")) { //$NON-NLS-1$
                        logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                        return false;
                    }
                }

                // Establecemos el directorio de salida, si no existe se
                // creara un directorio temporal
                // al que poder acceder para la posterior lectura de los
                // datos de firma
                String outputDir;
                if (SignApplet.this.massiveOutputDirectory != null) {
                    outputDir = SignApplet.this.massiveOutputDirectory;
                }
                else {
                    logger.warning("No se ha indicado un directorio para el guardado de los firmas generadas, se almacenaran en el mismo directorio de entrada: " + inputDir); //$NON-NLS-1$
                    outputDir = inputDir;
                }

                // Configuramos el certificado
                final PrivateKeyEntry ke = SignApplet.this.configureCertificate();
                if (ke == null) {
                    return false;
                }

                // Creamos el manejador de firma masiva
                final DirectorySignatureHelper massiveSigner;
                try {
                    massiveSigner = new DirectorySignatureHelper(SignApplet.this.sigAlgo, SignApplet.this.sigFormat, SignApplet.this.sigMode);
                }
                catch (final Exception e) {
                    logger.severe("No se pudo inicializar el modulo de firma masiva: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.200")); //$NON-NLS-1$
                    return false;
                }

                // Le introducimos el filtro al manejador de firma
                // masiva
                if (SignApplet.this.massiveExtFiltered != null && SignApplet.this.massiveExtFiltered.length > 0) {
                    StringBuilder description = new StringBuilder(AppletMessages.getString("SignApplet.201")); //$NON-NLS-1$
                    for (int i = 0; i < massiveExtFiltered.length; i++) {
                        description.append("*.").append(massiveExtFiltered[i]); //$NON-NLS-1$
                        if (i + 1 != massiveExtFiltered.length) description.append(","); //$NON-NLS-1$
                    }
                    massiveSigner.setFileFilter(new AOUIManager.ExtFilter(massiveExtFiltered, description.toString()));
                }

                // Configuramos la operacion
                SignApplet.this.genericConfig.setProperty("format", SignApplet.this.sigFormat); //$NON-NLS-1$
                SignApplet.this.genericConfig.setProperty("mode", SignApplet.this.sigMode); //$NON-NLS-1$
                SignApplet.this.configurePolicy();
                SignApplet.this.configureXMLTransforms();

                boolean allOk = true;
                try {
                    allOk =
                            massiveSigner.massiveSign(SignApplet.this.massiveOperation,
                                                      inputDir,
                                                      SignApplet.this.recursiveSignDir,
                                                      outputDir,
                                                      true,
                                                      SignApplet.this.originalFormat,
                                                      ke,
                                                      (X509Certificate) ke.getCertificate(),
                                                      SignApplet.this.genericConfig);
                }
                catch (final Exception e) {
                    logger.severe("Error grave durante la operacion de firma masiva: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.205")); //$NON-NLS-1$
                    return false;
                }

                if (allOk) SignApplet.this.setError(null);

                return allOk;
            }
        });
    }

    public void setMassiveOperation(String massiveOperationName) {
        logger.info("Invocando setMassiveOperation: " + massiveOperationName); //$NON-NLS-1$

        if (massiveOperationName == null || massiveOperationName.equals("")) { //$NON-NLS-1$
            massiveOperationName = AOConstants.DEFAULT_MASSIVE_OPERATION;
        }

        if (massiveOperationName.equals(AOConstants.MASSIVE_OPERATION_SIGN)) {
            this.massiveOperation = MassiveType.SIGN;
        }
        else if (massiveOperationName.equals(AOConstants.MASSIVE_OPERATION_COSIGN)) {
            this.massiveOperation = MassiveType.COSIGN;
        }
        else if (massiveOperationName.equals(AOConstants.MASSIVE_OPERATION_COUNTERSIGN_TREE)) {
            this.massiveOperation = MassiveType.COUNTERSIGN_ALL;
        }
        else if (massiveOperationName.equals(AOConstants.MASSIVE_OPERATION_COUNTERSIGN_LEAFS)) {
            this.massiveOperation = MassiveType.COUNTERSIGN_LEAFS;
        }
        else {
            logger.warning("Operacion masiva no reconocida, se realizara la operacion: " + //$NON-NLS-1$
                           AOConstants.DEFAULT_MASSIVE_OPERATION);
            setMassiveOperation(AOConstants.DEFAULT_MASSIVE_OPERATION);
        }

        // Si ya hay una configuracion de firma masiva establecida, la
        // actualizamos con la nueva operacion
        if (this.massiveSignatureHelper != null && this.massiveSignatureHelper.isInitialized()) {
            this.massiveSignatureHelper.setMassiveOperation(this.massiveOperation);
        }
    }

    public void addMassiveHash(String hashData) {
        if (this.hashesToSign == null) {
            this.hashesToSign = new Vector<String>();
        }
        this.hashesToSign.add(hashData);
    }

    /** Obtiene los datos con los que se deben reaizar las operaciones de firma y
     * ensobrado de datos.
     * @return Datos de entrada.
     * @throws AOException
     *         Si ocurren errores obtener los datos. */
    private byte[] getInData() throws AOException {
        byte[] tempData = null;

        // Comprobamos si se nos han introducido los datos directamente. Aun en
        // caso de que se nos haya
        // introducido directamente, en caso de que tambien se haya introducido
        // el hash y el modo de firma
        // sea explicito, hacemos una firma del hash.
        if (data == null || (hash != null && SignApplet.this.sigMode.equals(AOConstants.SIGN_MODE_EXPLICIT))) {

            // Si no, comprobamos si se nos ha introducido un hash para firmar
            if (hash == null) {

                // Si no, comprobamos si se nos ha indicado un fichero de
                // entrada
                if (fileUri == null) {

                    // Si no, le pedimos al usuario que seleccione un fichero y
                    // lo configuramos
                    final String fileName = AOUIManager.getLoadFileName(AppletMessages.getString("SignApplet.356"), null, null, this); //$NON-NLS-1$
                    if (fileName == null) {
                        logger.severe("Se ha cancelado la seleccion del fichero de entrada"); //$NON-NLS-1$
                        this.setError(AppletMessages.getString("SignApplet.212")); //$NON-NLS-1$
                        throw new AOException("Se ha cancelado la seleccion del fichero de entrada, se cancelara toda la operacion" //$NON-NLS-1$
                        );
                    }

                    try {
                        fileUri = AOUtil.createURI(fileName);
                    }
                    catch (final Exception e) {
                        logger.severe("Se ha proporcionado un nombre de fichero no valido '" + fileName + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
                        this.setError(AppletMessages.getString("SignApplet.214") + fileName); //$NON-NLS-1$ //$NON-NLS-2$
                        throw new AOException("Se ha proporcionado un nombre de fichero no valido: " + fileName, e); //$NON-NLS-1$
                    }
                    fileBase64 = false;
                }

                // Cargamos los datos de la URI configurada
                final LoadFileAction loadFileAction = new LoadFileAction(fileUri, SignApplet.this);
                loadFileAction.setBase64Encoded(fileBase64);
                AccessController.doPrivileged(loadFileAction);
                if (loadFileAction.isError()) {
                    logger.severe(loadFileAction.getErrorMessage());
                    this.setError(AppletMessages.getString("SignApplet.407")); //$NON-NLS-1$
                    throw new AOException(loadFileAction.getErrorMessage(), loadFileAction.getException());
                }
                tempData = loadFileAction.getResult();
            } // Se nos ha introducido un hash
            else tempData = hash;
        } // Se nos ha introducido un dato
        else tempData = data;

        // Si la entrada son datos o un fichero lo analizamos
        if (data != null || fileUri != null) {
            analizeMimeType(tempData);
        }

        return tempData;
    }

    /** Analiza datos para establecer su MimeType y la descripci&oacute;n textual
     * de su tipo.
     * @param dataContent
     *        Contenidos que se desean analizar. */
    private void analizeMimeType(final byte[] dataContent) {
        // Intentamos extraer el mimetype y su descripcion
        if (dataContent != null) {
            final MimeHelper mtHelper = new MimeHelper(dataContent);
            dataMimeType = mtHelper.getMimeType();
            dataDescription = mtHelper.getDescription();
        }
    }

    public String getSignaturesBase64Encoded() {
        logger.info("Invocando getSignaturesBase64Encoded"); //$NON-NLS-1$
        return this.massiveSignData;
    }

    // ==============================================
    // Funcionalidades de multifirma masiva programatica
    // ==============================================

    /** Manejador de firma masiva. */
    private MassiveSignatureHelper massiveSignatureHelper = null;

    public boolean initMassiveSignature() {
        logger.info("Invocando initMassiveSignature"); //$NON-NLS-1$

        // Desactivamos la configuracion de error actual
        this.setError(null);

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {

                // Configuramos el certificado
                final PrivateKeyEntry ke = SignApplet.this.configureCertificate();
                if (ke == null) {
                    return false;
                }

                // Configuramos el entorno
                SignApplet.this.configurePolicy();
                SignApplet.this.configureXMLTransforms();

                // Establecemos la configuracion que se usara para la
                // firma masiva
                MassiveSignConfiguration massiveConfiguration = new MassiveSignConfiguration(ke, (X509Certificate) ke.getCertificate());
                massiveConfiguration.setExtraParams(SignApplet.this.genericConfig);
                massiveConfiguration.setAlgorithm(SignApplet.this.sigAlgo);
                massiveConfiguration.setDefaultFormat(SignApplet.this.sigFormat);
                massiveConfiguration.setMode(SignApplet.this.sigMode);
                massiveConfiguration.setOriginalFormat(SignApplet.this.originalFormat);
                massiveConfiguration.setMassiveOperation(SignApplet.this.massiveOperation != null
                                                                                                 ? SignApplet.this.massiveOperation
                                                                                                 : MassiveType.valueOf(AOConstants.DEFAULT_MASSIVE_OPERATION));

                try {
                    SignApplet.this.massiveSignatureHelper = new MassiveSignatureHelper(massiveConfiguration);
                }
                catch (final AOException e) {
                    logger.severe("Error al inicializar el modulo de multifirma masiva: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.8")); //$NON-NLS-1$
                    return false;
                }
                return true;
            }
        });
    }

    public void endMassiveSignature() {
        logger.info("Invocando endMassiveSignature"); //$NON-NLS-1$
        if (this.massiveSignatureHelper == null) {
            logger.warning("No se ha inicializado la operacion de firma masiva"); //$NON-NLS-1$
            return;
        }
        this.massiveSignatureHelper.release();
    }

    public String massiveSignatureData(final String b64Data) {
        logger.info("Invocando massiveSignatureData"); //$NON-NLS-1$
        this.setError(null);
        if (this.massiveSignatureHelper == null || !this.massiveSignatureHelper.isInitialized()) {
            this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
            return null;
        }

        // Ejecutamos la operacion
        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {
                String result = SignApplet.this.massiveSignatureHelper.signData(b64Data);
                if (result == null) SignApplet.this.setError(SignApplet.this.massiveSignatureHelper.getCurrentLogEntry());
                return result;
            }
        });
    }

    public String massiveSignatureHash(final String b64Hash) {
        logger.info("Invocando massiveSignatureHash"); //$NON-NLS-1$
        this.setError(null);
        if (this.massiveSignatureHelper == null || !this.massiveSignatureHelper.isInitialized()) {
            this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
            return null;
        }

        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {
                final String result = SignApplet.this.massiveSignatureHelper.signHash(b64Hash);
                if (result == null) SignApplet.this.setError(SignApplet.this.massiveSignatureHelper.getCurrentLogEntry());
                return result;
            }
        });
    }

    public String massiveSignatureFile(final String fileuri) {
        logger.info("Invocando massiveSignatureFile: " + fileuri); //$NON-NLS-1$
        this.setError(null);
        if (this.massiveSignatureHelper == null || !this.massiveSignatureHelper.isInitialized()) {
            this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
            return null;
        }

        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {
                String result = SignApplet.this.massiveSignatureHelper.signFile(fileuri);
                if (result == null) SignApplet.this.setError(SignApplet.this.massiveSignatureHelper.getCurrentLogEntry());

                return result;
            }
        });
    }

    public String getMassiveSignatureCurrentLog() {
        logger.info("Invocando getMassiveSignatureCurrentLog"); //$NON-NLS-1$
        this.setError(null);
        if (this.massiveSignatureHelper == null) {
            this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
            return null;
        }
        return this.massiveSignatureHelper.getCurrentLogEntry();
    }

    public String getMassiveSignatureLog() {
        logger.info("Invocando getMassiveSignatureLog"); //$NON-NLS-1$
        this.setError(null);
        if (this.massiveSignatureHelper == null) {
            this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
            return null;
        }
        return this.massiveSignatureHelper.getAllLogEntries();
    }

    public void saveMassiveSignatureLog() {
        logger.info("Invocando saveMassiveSignatureLog"); //$NON-NLS-1$
        this.setError(null);
        if (this.massiveSignatureHelper == null) {
            this.setError(AppletMessages.getString("SignApplet.381")); //$NON-NLS-1$
            return;
        }

        // Guardamos la firma en fichero
        AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
            public Void run() {
                SignApplet.this.saveFileAsinchronously(SignApplet.this.getMassiveSignatureLog().getBytes(), SignApplet.this.outputFile, new String[] {
                    "txt"}, //$NON-NLS-1$
                                                       AppletMessages.getString("SignApplet.383")); //$NON-NLS-1$

                return null;
            }
        });
    }

    // ==========================================
    // Operaciones de firma Web
    // ==========================================

    public String webSign(final String html) {
        logger.info("Invocando webSign: " + html); //$NON-NLS-1$

        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {
                FirmaWeb firmaWeb;
                try {
                    String signAlgorithm = SignApplet.this.sigAlgo == null ? AOConstants.DEFAULT_SIGN_ALGO : SignApplet.this.sigAlgo;

                    // Recuperamos el algoritmo de Hash de la firma
                    int p = signAlgorithm.toLowerCase().indexOf("with"); //$NON-NLS-1$
                    String hashAlg = p != -1 ? signAlgorithm.substring(0, p) : signAlgorithm;

                    // Realizamos la firma Web
                    firmaWeb = new Browser().browse(html, hashAlg);
                    if (firmaWeb != null) {
                        SignApplet.this.setFileuri(firmaWeb.tmpWebDataFile.getAbsolutePath());
                        SignApplet.this.sign();
                    }
                    else {
                        throw new AOCancelledOperationException("Operacion cancelada por el usuario"); //$NON-NLS-1$
                    }
                }
                catch (final Exception e) {
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.389")); //$NON-NLS-1$
                    logger.severe("Error durante el proceso de firma Web: " + e); //$NON-NLS-1$
                    firmaWeb = null;
                }

                if (firmaWeb != null) return firmaWeb.tmpWebDataFile.getAbsolutePath();
                return "ERROR"; //$NON-NLS-1$
            }
        });
    }

    public final void setSelectedCertificateAlias(final String cAlias) {
        logger.info("Invocando setSelectedCertificateAlias: " + cAlias); //$NON-NLS-1$
        this.ksConfigManager.setSelectedAlias(cAlias);
    }

    /** Preconfigura (pero no inicializa) un almac&eacute;n de certificados a
     * partir del sistema operativo y el navegador en el que se est&eacute;
     * ejecutando el applet.
     * @param currentOS
     *        Sistema operativo.
     * @param currentBrowser
     *        Navegador indicado por el userAgent del mismo.
     * @return Almac&eacute;n por defecto para el entorno seleccionado. */
    private AOKeyStore configureDefaultStore(final Platform.OS currentOS, final Platform.BROWSER currentBrowser) {

        logger.info("Navegador: " + currentBrowser); //$NON-NLS-1$

        if (Platform.OS.LINUX.equals(currentOS) || Platform.OS.SOLARIS.equals(currentOS) || Platform.BROWSER.FIREFOX.equals(currentBrowser)) {
            // Usamos Mozilla siempre en UNIX y en otros sistemas operativos
            // solo si estamos sobre Firefox
            return AOConstants.AOKeyStore.MOZ_UNI;
        }
        if (Platform.OS.MACOSX.equals(currentOS)) { // Mac OS X
            // En Mac OS X siempre el de Apple cuando no es Firefox
            return AOConstants.AOKeyStore.APPLE;
        }
        if (Platform.OS.WINDOWS.equals(currentOS)) { // Windows
            return AOConstants.AOKeyStore.WINDOWS;
        }

        return AOConstants.AOKeyStore.PKCS12;
    }

    private final void setLookAndFeel() {
        final String lookandfeel = UIManager.getSystemLookAndFeelClassName();
        try {
            UIManager.setLookAndFeel(lookandfeel);
        }
        catch (final Exception e) {
            logger.warning("No se ha podido establecer el Look&Feel '" + lookandfeel + "', las " + //$NON-NLS-1$ //$NON-NLS-2$
                           "ventanas careceran de decoracion: "
                           + e //$NON-NLS-1$
            );
        }

        // Nos aseguramos de que los dialogos salgan decorados
        javax.swing.JDialog.setDefaultLookAndFeelDecorated(true);
        javax.swing.JFrame.setDefaultLookAndFeelDecorated(true);
    }

    public final boolean isInitialized() {
        logger.info("Invocando isInitialized"); //$NON-NLS-1$
        return this.initializedApplet;
    }

    public final boolean signData(final String b64data) {
        logger.info("Invocando signData"); //$NON-NLS-1$
        if (b64data == null) {
            logger.severe("No se han introducido los datos que se desean firmar"); //$NON-NLS-1$
            SignApplet.this.setError(AppletMessages.getString("SignApplet.278")); //$NON-NLS-1$
            return false;
        }
        data = AOCryptoUtil.decodeBase64(b64data);
        return sign();
    }

    public String getSignCertificateBase64Encoded() {
        logger.info("Invocando getSignCertificateBase64Encoded"); //$NON-NLS-1$
        final byte[] certEnconded;
        try {
            certEnconded = ksConfigManager.getSelectedCertificate().getEncoded();
        }
        catch (final CertificateEncodingException e) {
            logger.warning("La codificacion del certificado no es valida, se devolvera una cadena vacia: " + e); //$NON-NLS-1$
            return ""; //$NON-NLS-1$
        }

        return AOCryptoUtil.encodeBase64(certEnconded, false);
    }

    public String getSignatureBase64Encoded() {
        logger.info("Invocando getSignatureBase64Encoded"); //$NON-NLS-1$
        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {

                byte[] sign = null;
                try {
                    sign = SignApplet.this.getSelectedSignature(false);
                }
                catch (final Exception e) {
                    logger.severe("No se ha podido recuperar la firma electr&oacute;nica: " + e //$NON-NLS-1$
                    );
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.64")); //$NON-NLS-1$
                }
                return (sign == null) ? null : AOCryptoUtil.encodeBase64(sign, false);
            }
        });
    }

    public String getSignatureText() {
        logger.info("Invocando getSignatureText"); //$NON-NLS-1$
        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {

                byte[] sign = null;
                try {
                    sign = SignApplet.this.getSelectedSignature(false);
                }
                catch (final Exception e) {
                    logger.severe("No se ha podido recuperar la firma electr&oacute;nica: " + e //$NON-NLS-1$
                    );
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.64")); //$NON-NLS-1$
                }
                return (sign == null) ? null : new String(sign);
            }
        });
    }

    public String getFilePath() {
        logger.info("Invocando getFilePath"); //$NON-NLS-1$
        if (this.outputFile == null) {
            logger.warning("No se dispone de la direccion del fichero de firma, se devolvera una cadena vacia"); //$NON-NLS-1$
            return ""; //$NON-NLS-1$
        }
        return this.outputFile;
    }

    public String getFileUsedPath() {
        logger.info("Invocando getFileUsedPath"); //$NON-NLS-1$
        if (this.fileUri == null) {
            logger.warning("No se dispone de la direccion del fichero de datos de entrada, se devolvera una cadena vacia"); //$NON-NLS-1$
            return ""; //$NON-NLS-1$
        }

        String path = ""; //$NON-NLS-1$
        try {
            path = URLDecoder.decode(this.fileUri.toASCIIString(), "UTF-8"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            logger.warning("Codificacion de caracteres no valida: " + e); //$NON-NLS-1$
        }

        // Si es un fichero local eliminamos el esquema de la ruta
        if (path.startsWith("file://")) path = path.substring(7); //$NON-NLS-1$

        return path;
    }

    public String getErrorMessage() {
        logger.info("Invocando getErrorMessage"); //$NON-NLS-1$
        return (this.error ? errorMsg : ""); //$NON-NLS-1$
    }

    public boolean isError() {
        logger.info("Invocando isError"); //$NON-NLS-1$
        return this.error;
    }

    /** Establece un mensaje de error y almacena que este se ha producido o, en
     * caso de introducirse <code>null</code> o cadena vac&iacute;a, se indica
     * que no hay error. Tambi&eacute;n muestra este error por consola siempre y
     * cuando no sea nulo. En caso de estar configurado el cliente para mostrar
     * los errores, se mostrar&aacute; una ventana modal al usuario con el error
     * establecido.
     * @param errorMsg
     *        Mensaje de error. */
    private void setError(final String errMsg) {
        if (errMsg == null || errMsg.length() < 1) {
            this.error = false;
            this.errorMsg = ""; //$NON-NLS-1$
        }
        else {
            this.error = true;
            this.errorMsg = errMsg;
        }

        // Mostramos, si procede, el mensaje de error que corresponda
        if (this.showErrors && this.error) {
            JOptionPane.showMessageDialog(this, this.errorMsg, AppletMessages.getString("SignApplet.156"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
        }
    }

    public String getTextFileContent(final String url) {
        logger.info("Invocando getTextFileContent: " + url); //$NON-NLS-1$

        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {
                final InputStream is;
                try {
                    is = AOUtil.loadFile(AOUtil.createURI(url), SignApplet.this, true);
                }
                catch (final Exception e) {
                    logger.severe("El fichero indicado no existe o no es posible acceder a el: " + e); //$NON-NLS-1$
                    return ""; //$NON-NLS-1$
                }
                final String result;
                try {
                    result = new String(AOUtil.getDataFromInputStream(is));
                }
                catch (final Exception e) {
                    logger.severe("No se pudo leer el contenido del fichero indicado: " + e); //$NON-NLS-1$
                    return ""; //$NON-NLS-1$
                }
                try {
                    is.close();
                }
                catch (final Exception e) {
                    logger.warning("Error al cerrar un flujo de datos: " + e);} //$NON-NLS-1$

                return result;
            }
        });
    }

    public String getTextFromBase64(final String b64) {
        logger.info("Invocando getTextFromBase64"); //$NON-NLS-1$
        return new String(AOCryptoUtil.decodeBase64(b64));
    }

    public String getBase64FromText(final String plainText) {
        logger.info("Invocando getBase64FromText"); //$NON-NLS-1$
        String encoding = null;
        if (plainText.startsWith("<?xml")) {
            // Intentamos detectar la codificacion
            final String xmlHeader = plainText.substring(0, plainText.indexOf("?>"));
            int encodingPos = xmlHeader.indexOf("encoding=\"");
            if (encodingPos != -1) {
                encoding = xmlHeader.substring(encodingPos + 10, xmlHeader.indexOf("\"", encodingPos + 10));
            }
        }
        if (encoding != null) {
            try {
                return AOCryptoUtil.encodeBase64(plainText.getBytes(encoding), false);
            }
            catch (final Exception e) {
                logger.warning("El XML introducido parece tener una codificacion " + encoding
                               + ", pero no ha sido posible usarla para generar el Base64: "
                               + e);
            }
        }
        return AOCryptoUtil.encodeBase64(plainText.getBytes(), false);
    }

    public String getFileBase64Encoded(final String strUri, final boolean showProgress) {
        logger.info("Invocando getFileBase64Encoded: " + strUri); //$NON-NLS-1$
        try {
            return getFileBase64Encoded(AOUtil.createURI(strUri), showProgress);
        }
        catch (final Exception e) {
            logger.severe("Error al leer el fichero '" + strUri + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
            setError(AppletMessages.getString("SignApplet.81") + strUri); //$NON-NLS-1$
            return null;
        }
    }

    public String getFileBase64Encoded(final boolean showProgress) {
        logger.info("Invocando getFileBase64Encoded"); //$NON-NLS-1$
        return getFileBase64Encoded(this.fileUri, showProgress);
    }

    /** Recupera el contenido de un fichero codificado en base 64.
     * @param uri
     *        Ruta del fichero.
     * @param showProgress
     *        Si desea mostrarse una barra de progreso para la carga.
     * @return Contentido en base 64. */
    private String getFileBase64Encoded(final URI uri, final boolean showProgress) {
        if (uri == null) {
            logger.severe("No se ha establecido un fichero que desea obtener en base 64"); //$NON-NLS-1$
            return null; //$NON-NLS-1$
        }

        final byte[] fileContent = FileUtils.loadFile(uri, showProgress, this);

        if (fileContent == null) {
            this.setError(AppletMessages.getString("SignApplet.407")); //$NON-NLS-1$
            return null;
        }

        final String base64 = fileBase64 ? new String(fileContent) : AOCryptoUtil.encodeBase64(fileContent, false);

        logger.info("Ya en Base64"); //$NON-NLS-1$

        JOptionPane.showMessageDialog(this, "Ya en Base64"); //$NON-NLS-1$

        this.save(base64);

        JOptionPane.showMessageDialog(this, "Salvado"); //$NON-NLS-1$

        return base64;
    }

    private void save(final String text) {
        AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
            public Void run() {
                try {
                    FileOutputStream fos = new FileOutputStream("C:\\Users\\A122466\\Desktop\\Borrar\\pruebatamano"); //$NON-NLS-1$
                    fos.write(text.getBytes());
                    try {
                        fos.close();
                    }
                    catch (final Exception e) {}
                    JOptionPane.showMessageDialog(SignApplet.this, "OK guardado"); //$NON-NLS-1$
                }
                catch (final Exception e) {
                    Logger.getLogger("es.gob.afirma").severe(e.toString()); //$NON-NLS-1$
                    JOptionPane.showMessageDialog(SignApplet.this, "Error"); //$NON-NLS-1$
                }
                return null;
            }
        });
    }

    public String getFileHashBase64Encoded(final boolean showProgress) {
        logger.info("Invocando getFileHashBase64Encoded"); //$NON-NLS-1$

        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {
                if (SignApplet.this.fileUri == null) {
                    logger.severe("No se ha establecido el fichero del que calcular el Hash"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.348")); //$NON-NLS-1$
                    return null;
                }

                InputStream is;
                try {
                    is = AOUtil.loadFile(fileUri, SignApplet.this, showProgress, fileBase64);
                }
                catch (final Exception e) {
                    setError(AppletMessages.getString("SignApplet.85")); //$NON-NLS-1$
                    logger.severe(e.toString());
                    return null;
                }

                byte[] binaryData;
                try {
                    binaryData = AOUtil.getDataFromInputStream(is);
                }
                catch (final Exception e) {
                    logger.severe("Error durante la lectura del fichero " + SignApplet.this.fileUri + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.407") + SignApplet.this.fileUri); //$NON-NLS-1$
                    return null;
                }

                try {
                    is.close();
                }
                catch (final IOException e) {
                    logger.warning("Error al cerrar el stream de entrada de datos: " + e); //$NON-NLS-1$
                }

                String digestAlg = AOUtil.getDigestAlgorithm(SignApplet.this.sigAlgo);
                try {
                    return AOCryptoUtil.encodeBase64(AOCryptoUtil.getMessageDigest(binaryData, digestAlg), false);
                }
                catch (final NoSuchAlgorithmException e) {
                    logger.severe("El algoritmo de hash '" + digestAlg + "' no esta soportado: " + e); //$NON-NLS-1$ //$NON-NLS-2$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.464") + digestAlg); //$NON-NLS-1$
                    return null;
                }
            }
        });
    }

    public void setCipherData(final String data) {
        logger.info("Invocando setCipherData"); //$NON-NLS-1$
        cipherManager.setCipheredData(data);
    }

    public void setPlainData(String data) {
        logger.info("Invocando setPlainData"); //$NON-NLS-1$
        cipherManager.setPlainData(data == null ? null : data.getBytes());
    }

    public String getCipherData() {
        logger.info("Invocando getCipherData"); //$NON-NLS-1$
        return cipherManager.getCipheredDataB64Encoded();
    }

    public String getPlainData() {
        logger.info("Invocando getPlainData"); //$NON-NLS-1$
        return cipherManager.getPlainData() == null ? null : new String(cipherManager.getPlainData());
    }

    public String getKey() {
        logger.info("Invocando getKey"); //$NON-NLS-1$
        return cipherManager.getCipherB64Key();
    }

    public void setKey(String newKey) {
        logger.info("Invocando setKey: " + newKey); //$NON-NLS-1$
        cipherManager.setCipherB64Key(newKey);
    }

    public String getPassword() {
        logger.info("Invocando getPassword"); //$NON-NLS-1$
        return cipherManager.getCipherPassword() == null ? null : String.valueOf(cipherManager.getCipherPassword());
    }

    public boolean setPassword(String password) {
        logger.info("Invocando setPassword"); //$NON-NLS-1$
        if (!CipherManager.isValidPassword(password)) {
            logger.warning("La contrasena introducida no es una cadena ASCII"); //$NON-NLS-1$
            return false;
        }
        cipherManager.setCipherPassword(password.toCharArray());
        return true;
    }

    public void setCipherAlgorithm(String algorithm) {
        logger.info("Invocando setCipherAlgorithm: " + algorithm); //$NON-NLS-1$

        final AOCipherAlgorithm algo = AOCipherAlgorithm.getValueOf(algorithm);
        if (algo == null) {
            logger.warning("Algoritmo de cifrado no reconocido, se establecera el por defecto: " + //$NON-NLS-1$
                           AOCipherAlgorithm.getDefault().getName());
        }
        cipherManager.setCipherAlgorithm(algo);
    }

    public String getCipherAlgorithm() {
        logger.info("Invocando getCipherAlgorithm"); //$NON-NLS-1$
        return cipherManager.getCipherAlgorithmConfiguration();
    }

    public void setKeyMode(String keyMode) {
        logger.info("Invocando setKeyMode: " + keyMode); //$NON-NLS-1$
        cipherManager.setKeyMode(keyMode);
    }

    public String getKeyMode() {
        logger.info("Invocando getKeyMode"); //$NON-NLS-1$
        return cipherManager.getKeyMode();
    }

    public boolean savePlainDataToFile(final String strUri) {
        logger.info("Invocando savePlainDataToFile: " + strUri); //$NON-NLS-1$
        if (cipherManager.getPlainData() == null) {
            logger.severe("No hay datos en claro que guardar"); //$NON-NLS-1$
            SignApplet.this.setError(AppletMessages.getString("SignApplet.394")); //$NON-NLS-1$
            return false;
        }
        if (strUri == null) {
            logger.severe("El fichero de salida para los datos no puede ser nulo"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.396")); //$NON-NLS-1$
            return false;
        }

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {
                try {
                    SignApplet.this.saveDataToStorage(SignApplet.this.cipherManager.getPlainData(), strUri);
                }
                catch (final Exception e) {
                    logger.severe("No se pudo almacenar el texto plano (establecido o cifrado) en " + strUri + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.392") + strUri); //$NON-NLS-1$
                    return false;
                }
                return true;
            }
        });
    }

    public boolean saveCipherDataToFile(final String strUri) {
        logger.info("Invocando saveCipherDataToFile: " + strUri); //$NON-NLS-1$
        if (cipherManager.getCipheredData() == null) {
            logger.severe("No hay datos cifrados que guardar"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.395")); //$NON-NLS-1$
            return false;
        }
        if (strUri == null) {
            logger.severe("El fichero de salida para los datos no puede ser nulo"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.396")); //$NON-NLS-1$
            return false;
        }

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {
                try {
                    SignApplet.this.saveDataToStorage(cipherManager.getCipheredData(), strUri);
                }
                catch (final Exception e) {
                    logger.severe("No se pudo almacenar el texto cifrado en" + strUri + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.397") + strUri); //$NON-NLS-1$
                    return false;
                }
                return true;
            }
        });
    }

    public void setUseCipherKeyStore(boolean useKeyStore) {
        logger.info("Invocando setUseCipherKeyStore con el valor: " + useKeyStore); //$NON-NLS-1$
        cipherManager.setUseCipherKeyStore(useKeyStore);
    }

    public boolean cipherFile(final String strUri) {
        logger.info("Invocando cipherFile: " + strUri); //$NON-NLS-1$
        this.setPlainData(null);

        final byte[] dat = FileUtils.loadFile(strUri, true, this);
        if (dat == null) {
            setError(AppletMessages.getString("SignApplet.401")); //$NON-NLS-1$
            return false;
        }

        return cipherData(dat);
    }

    public boolean cipherData() {
        logger.info("Invocando cipherData"); //$NON-NLS-1$
        return cipherData(null);
    }

    /** Cifra los datos introducido o los configurados en la instancia actual de
     * cipherManager.
     * @param dat
     *        Datos que deseamos cifrar.
     * @return Devuelve {@code true} si la operaci&oacute; finaliz&oacute;
     *         correctamente. */
    public boolean cipherData(byte[] dat) {

        // El resultado queda almacenado en el objeto CipherManager
        final BasicPrivilegedAction<Boolean, Void> cipherAction = new CipherAction(cipherManager, dat);
        AccessController.doPrivileged(cipherAction);
        if (cipherAction.isError()) {
            setError(AppletMessages.getString("SignApplet.93")); //$NON-NLS-1$
            if (cipherAction.getException() instanceof AOCancelledOperationException) {
                setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
            }
            else {
                setError(AppletMessages.getString("SignApplet.92")); //$NON-NLS-1$
            }

            logger.severe(cipherAction.getErrorMessage() + ": " + cipherAction.getException()); //$NON-NLS-1$
            return false;
        }
        return true;
    }

    public boolean decipherFile(final String strUri) {
        logger.info("Invocando decipherFile: " + strUri); //$NON-NLS-1$
        this.setCipherData(null);

        final byte[] dat = FileUtils.loadFile(strUri, true, this);
        if (data == null) {
            setError(AppletMessages.getString("SignApplet.402")); //$NON-NLS-1$
            return false;
        }

        return decipherData(dat);
    }

    public boolean decipherData() {
        logger.info("Invocando decipherData"); //$NON-NLS-1$
        return decipherData(null);
    }

    /** Descifra los datos introducido o los configurados en la instancia actual
     * de cipherManager.
     * @param dat
     *        Datos que deseamos descifrar.
     * @return Devuelve {@code true} si la operaci&oacute; finaliz&oacute;
     *         correctamente. */
    public boolean decipherData(final byte[] dat) {

        // El resultado quedara almacenado en el objeto CipherManager
        final BasicPrivilegedAction<Boolean, Void> decipherAction = new DecipherAction(cipherManager, dat);
        AccessController.doPrivileged(decipherAction);
        if (decipherAction.isError()) {
            logger.severe(decipherAction.getErrorMessage() + ": " + decipherAction.getException()); //$NON-NLS-1$
            if (decipherAction.getException() instanceof AOCancelledOperationException) {
                setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
            }
            else if (decipherAction.getException() instanceof AOInvalidKeyException) {
                setError(AppletMessages.getString("SignApplet.111")); //$NON-NLS-1$
            }
            else {
                setError(AppletMessages.getString("SignApplet.92")); //$NON-NLS-1$
            }
            return false;
        }

        return true;
    }

    public String showCertSelectionDialog() {
        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {

                // Configuramos el certificado
                PrivateKeyEntry ke = SignApplet.this.configureCertificate();
                if (ke == null) {
                    return null;
                }

                return ksConfigManager.getSelectedAlias();
            }
        });
    }

    /** Recupera la informaci&oacute;n de firma establecida o generada por el
     * cliente. Si no hay firma establecida y se ha pedido que no se le deje
     * seleccionar al usuario, se devolver&aacute; {@code null}.
     * @param select
     *        Indica si mostrar un di&aacute;logo al usuario para la
     *        selecci&oacute;n de la firma cuando no est&eacute;
     *        establecida.
     * @return Firma electr&oacute;nica.
     * @throws AOCancelledOperationException
     *         Cuando el usuario cancela la operaci&oacute;n.
     * @throws AOException
     *         Cuando ocurre un error en la lectura de la informaci&oacute;n
     *         de firma. */
    private byte[] getSelectedSignature(final boolean select) throws AOCancelledOperationException, AOException {
        final byte[] originalSign;
        if (signData != null) {
            originalSign = signData;
        }
        else if (electronicSignatureFile == null && !select) {
            originalSign = null;
        }
        else {
            if (electronicSignatureFile == null) {
                final String fileName = SignApplet.this.selectSignFile(SignApplet.this.sigFormat);
                if (fileName == null) throw new AOCancelledOperationException("Operacion cancelada por el usuario"); //$NON-NLS-1$
                try {
                    electronicSignatureFile = AOUtil.createURI(fileName);
                }
                catch (final Exception e) {
                    logger.severe("La URI proporcionada no es valida (" + fileName + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
                    throw new AOException("El nombre de fichero '" + fileName + "' no es valido ", e); //$NON-NLS-1$ //$NON-NLS-2$
                }
            } // Fin 'else': Si no habia fichero seleccionado

            // Cargamos el fichero que estaba seleccionado o recien elegido por
            // el usuario
            try {
                final InputStream is = AOUtil.loadFile(electronicSignatureFile, this, true);
                originalSign = AOUtil.getDataFromInputStream(is);
                try {
                    is.close();
                }
                catch (final Exception e) {}
            }
            catch (final FileNotFoundException e) {
                logger.severe("No se encuentra el fichero de firma '" + electronicSignatureFile.getPath() + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                throw new AOException("No se encuentra el fichero de firma '" + electronicSignatureFile.getPath() + "'"); //$NON-NLS-1$ //$NON-NLS-2$
            }
            catch (final Exception e) {
                logger.severe("Error tratando de leer el fichero de firma original (" + electronicSignatureFile.getPath() + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                throw new AOException("Error tratando de leer el fichero de firma '" + electronicSignatureFile.getPath() + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }

        return originalSign;
    }

    public void setRecipientsToCMS(final String s) {
        logger.info("Invocando setRecipientsToCMS: " + s); //$NON-NLS-1$ //$NON-NLS-2$
        this.setError(null);

        if (s == null || s.equals("")) { //$NON-NLS-1$
            this.enveloperManager.removeAllRecipients();
            logger.info("Se han eliminado los destinatarios configurados para el sobre electronico"); //$NON-NLS-1$
            return;
        }

        for (final String recipientsCertFile : s.split("\n")) { //$NON-NLS-1$

            byte[] recipientsCert = FileUtils.loadFile(recipientsCertFile, true, this);
            if (recipientsCert == null) {
                this.setError(AppletMessages.getString("SignApplet.468", recipientsCertFile)); //$NON-NLS-1$
                continue;
            }
            this.addRecipientToCMS(recipientsCert);
        }
    }

    public void addRecipientToCMS(final String certB64) {
        logger.info("Invocando addRecipientToCMS"); //$NON-NLS-1$
        if (certB64 == null || certB64.length() == 0) {
            logger.warning("No se han introducido destinatarios"); //$NON-NLS-1$
        }
        addRecipientToCMS(AOCryptoUtil.decodeBase64(certB64));
    }

    /** Configura un nuevo destinatario para el sobre electr&oacute;nico por
     * medio de su certificado.
     * @param cert
     *        Certificado del destinatario. */
    private void addRecipientToCMS(final byte[] cert) {
        try {
            enveloperManager.addRecipient(cert);
        }
        catch (final AOCertificateException e) {
            logger.severe("Error al decodificar el certificado de destinatario, asegurese de que es un certificado valido: " + e); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.10")); //$NON-NLS-1$
        }
        catch (final Exception e) {
            logger.severe("Error al cargar el certificado de destinatario: " + e); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.572")); //$NON-NLS-1$
        }
    }

    public void removeRecipientToCMS(final String certB64) {

        if (certB64 == null || certB64.length() == 0) throw new NullPointerException("No se ha introducido el certificado que se desea eliminar"); //$NON-NLS-1$

        try {
            enveloperManager.removeRecipient(AOCryptoUtil.decodeBase64(certB64));
        }
        catch (final AOCertificateException e) {
            logger.severe("Error al decodificar el certificado de destinatario, asegurese de que es un certificado valido: " + e); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.61")); //$NON-NLS-1$
        }
        catch (final Exception e) {
            logger.severe("Error al eliminar el certificado de la lista de destinatarios: " + e); //$NON-NLS-1$
            SignApplet.this.setError(AppletMessages.getString("SignApplet.572")); //$NON-NLS-1$
        }
    }

    public void setLdapConfiguration(final String address, String port, final String root) {
        logger.info("Invocando setLdapConfiguration"); //$NON-NLS-1$
        if (address == null) {
            throw new NullPointerException("No se ha indicado la URL del directorio LDAP"); //$NON-NLS-1$
        }

        // Si no se indica el puerto se toma el por defecto
        if (port == null) {
            logger.warning("No se ha indicado el puerto para la configuracion del LDAP, se utilizara el puerto " + DEFAULT_LDAP_PORT); //$NON-NLS-1$
            port = Integer.toString(DEFAULT_LDAP_PORT);
        }

        try {
            ldapServerPort = Integer.parseInt(port);
            if (ldapServerPort < 1 || ldapServerPort > 65535) throw new IllegalArgumentException("Numero de puerto no valido, el numero de puerto debe estar entre 1 y 65535"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            logger.warning("No se ha insertado un numero de puerto valido para el LDAP, se usara el puerto " + DEFAULT_LDAP_PORT + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
            ldapServerPort = DEFAULT_LDAP_PORT;
        }

        ldapServerUrl = ""; //$NON-NLS-1$
        ldapServerUrl += address;
        // ldapRootPath = (root == null || root.trim().length() == 0) ? null :
        // root;
    }

    public void setLdapCertificatePrincipal(final String ldapCertificatePrincipal) {
        logger.info("Invocando setLdapCertificatePrincipal con el parametro: " + ldapCertificatePrincipal); //$NON-NLS-1$
        this.ldapCertificatePrincipal = (ldapCertificatePrincipal != null && ldapCertificatePrincipal.length() > 0 ? ldapCertificatePrincipal : null);
    }

    public String getLdapCertificate() {
        logger.info("Invocando getLdapCertificate()"); //$NON-NLS-1$
        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {

                final X509Certificate cert;

                // Si se ha establecido la direccion LDAP del
                // certificado, se descarga; si no
                // se muestra un navegador LDAP para seleccionarlo y
                // descargarlo
                if (ldapCertificatePrincipal != null) {
                    try {
                        cert =
                                LdapUtils.getCertificate(SignApplet.this.ldapServerUrl,
                                                         SignApplet.this.ldapServerPort,
                                                         SignApplet.this.ldapCertificatePrincipal);
                    }
                    catch (final Exception e) {
                        logger.severe("Error al recuperar el certificado '" + ldapCertificatePrincipal + "' del directorio LDAP: " + e); //$NON-NLS-1$ //$NON-NLS-2$
                        setError(AppletMessages.getString("SignApplet.74") + ldapCertificatePrincipal); //$NON-NLS-1$
                        return null;
                    }
                }

                else {
                    logger.severe("No se especifico el Principal del certificado que se desea seleccionar"); //$NON-NLS-1$
                    return null;
                }

                // Devolvemos el certificado codificado en Base64
                try {
                    return AOCryptoUtil.encodeBase64(cert.getEncoded(), false);
                }
                catch (final Exception e) {
                    logger.severe("Error al codificar el certificado recuperado del directorio LDAP : " + e); //$NON-NLS-1$
                    setError(AppletMessages.getString("SignApplet.83")); //$NON-NLS-1$
                    return null;
                }
            }
        });
    }

    public void setCMSContentType(String contentType) {
        logger.info("Invocando setCMSContentType: " + contentType); //$NON-NLS-1$
        enveloperManager.setCmsContentType(contentType);
    }

    public boolean buildCMSEncrypted() {
        logger.info("Invocando buildCMSEncrypted"); //$NON-NLS-1$
        return this.doEnvelopOperation(null, AOConstants.CMS_CONTENTTYPE_ENCRYPTEDDATA);
    }

    public boolean buildCMSEnveloped() {
        logger.info("Invocando buildCMSEnveloped"); //$NON-NLS-1$
        return this.doEnvelopOperation(null, AOConstants.CMS_CONTENTTYPE_ENVELOPEDDATA);
    }

    public boolean buildCMSAuthenticated() {
        logger.info("Invocando buildCMSAuthenticated"); //$NON-NLS-1$
        return this.doEnvelopOperation(null, AOConstants.CMS_CONTENTTYPE_AUTHENVELOPEDDATA);
    }

    public boolean buildCMSStructure() {
        logger.info("Invocando buildCMSStructure"); //$NON-NLS-1$
        return this.doEnvelopOperation(null, null);
    }

    public boolean signAndPackData() {
        logger.info("Invocando signAndPackData"); //$NON-NLS-1$
        return this.doEnvelopOperation(null, AOConstants.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA);
    }

    public boolean signAndPackFile(final String uri) {
        logger.info("Invocando signAndPackFile: " + uri); //$NON-NLS-1$

        final byte[] dat = FileUtils.loadFile(uri, true, this);
        if (data == null) {
            setError(AppletMessages.getString("SignApplet.403")); //$NON-NLS-1$
            return false;
        }

        return doEnvelopOperation(dat, AOConstants.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA);
    }

    /** Genera un sobre electr&oacute;nico.
     * @param dat
     *        Datos que se desean ensobrar. Si no se indica, se
     *        tomar&acute;n los configurados actualmente en el manejador de
     *        ensobrado.
     * @param contentType
     *        Tipo de sobre que se desea generar. Si no se indica, se
     *        tomar&acute; el tipo configurado actualmente en el manejador
     *        de ensobrado.
     * @return Devuelve {@code true} si el sobre se genera correctamente. */
    private boolean doEnvelopOperation(final byte[] dat, final String contentType) {

        final byte[] contentData;
        try {
            contentData = (dat != null ? dat : getInData());
        }
        catch (final AOException e) {
            return false; // getInData() establece el mensaje en caso de error
        }

        if (contentType != null) {
            enveloperManager.setCmsContentType(contentType);
        }

        // Le pasamos la configuracion de almacenes y cifrado establecidas
        enveloperManager.setKsConfigManager(ksConfigManager);
        enveloperManager.setCipherManager(cipherManager);

        final BasicPrivilegedAction<Boolean, byte[]> envelopAction = new WrapAction(enveloperManager, contentData);
        boolean result = AccessController.doPrivileged(envelopAction);
        if (envelopAction.isError()) {
            if (envelopAction.getException() instanceof AOCancelledOperationException) {
                setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
            }
            else {
                setError(AppletMessages.getString("SignApplet.65")); //$NON-NLS-1$
            }
            logger.severe(envelopAction.getErrorMessage());
            return false;
        }

        this.data = envelopAction.getResult();

        return result;
    }

    public boolean coEnvelop() {
        logger.info("Invocando coEnvelop"); //$NON-NLS-1$

        // Reiniciamos el mensaje de error
        this.setError(null);

        // Leemos los datos
        final byte[] envelop;
        try {
            envelop = SignApplet.this.getInData();
        }
        catch (final AOException e) {
            // El metodo getInDataStream ya se habra encargado de establecer el
            // mensaje en caso de error
            return false;
        }

        enveloperManager.setKsConfigManager(ksConfigManager);
        enveloperManager.setCipherManager(cipherManager);

        final BasicPrivilegedAction<Boolean, byte[]> coEnvelopAction = new CoEnvelopAction(enveloperManager, envelop);

        boolean result = AccessController.doPrivileged(coEnvelopAction);
        if (coEnvelopAction.isError()) {
            if (coEnvelopAction.getException() instanceof AOCancelledOperationException) {
                setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
            }
            else {
                setError("Ocurrio un error al agregar un nuevo remitente al sobre electr\u00F3nico"); //$NON-NLS-1$
            }
            logger.severe(coEnvelopAction.getErrorMessage());
            return false;
        }

        this.data = coEnvelopAction.getResult();

        return result;
    }

    public boolean recoverCMS() {
        logger.info("Invocando recoverCMS"); //$NON-NLS-1$

        // Reiniciamos el mensaje de error
        this.setError(null);

        byte[] contentData;
        try {
            contentData = (data != null ? data : getInData());
        }
        catch (final AOException e) {
            return false; // getInData() establece el mensaje en caso de error
        }

        enveloperManager.setKsConfigManager(ksConfigManager);
        enveloperManager.setCipherManager(cipherManager);

        final BasicPrivilegedAction<Boolean, byte[]> unwrapAction = new UnwrapAction(this.enveloperManager, contentData);
        boolean result = AccessController.doPrivileged(unwrapAction);

        if (unwrapAction.isError()) {
            if (unwrapAction.getException() instanceof AOCancelledOperationException) {
                setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
            }
            else if (unwrapAction.getException() instanceof AOInvalidRecipientException) {
                setError(AppletMessages.getString("SignApplet.112")); //$NON-NLS-1$
            }
            else if (unwrapAction.getException() instanceof AOInvalidFormatException) {
                setError(AppletMessages.getString("SignApplet.75")); //$NON-NLS-1$
            }
            else {
                setError(AppletMessages.getString("SignApplet.77")); //$NON-NLS-1$
            }
            logger.severe(unwrapAction.getErrorMessage());
            return false;
        }

        this.data = unwrapAction.getResult();

        return result;
    }

    public String formatEncryptedCMS(String b64) {
        logger.info("Invocando formatEncryptedCMS"); //$NON-NLS-1$
        return this.getCMSInfo(b64);
    }

    public String formatEnvelopedCMS(String b64) {
        logger.info("Invocando formatEnvelopedCMS"); //$NON-NLS-1$
        return this.getCMSInfo(b64);
    }

    /** Recupera la informaci&oacute;n de un objeto CMS reconocido. Si ocurre un
     * error durante el proceso, se devuelve cadena vac&iacute;a.
     * @param b64
     *        Objeto CMS en base 64.
     * @return Informaci&oacute;n del objeto CMS introducido. */
    private String getCMSInfo(final String b64) {
        return enveloperManager.getCMSInfo(AOCryptoUtil.decodeBase64(b64));
    }

    public void setDataMimeType(String mimetype) {
        logger.info("Invocando setDataMimeType: " + mimetype); //$NON-NLS-1$
        if (mimetype == null || mimetype.length() == 0) {
            this.extMimeType = AOConstants.DEFAULT_MIMETYPE;
        }
        this.extMimeType = mimetype;
    }

    public String getB64Data() {
        logger.info("Invocando getB64Data"); //$NON-NLS-1$
        if (this.data == null) logger.warning("No se dispone de datos de salida, se devolvera cadena vacia"); //$NON-NLS-1$
        return (this.data == null ? "" : AOCryptoUtil.encodeBase64(this.data, false)); //$NON-NLS-1$
    }

    @Deprecated
    public String getCMSData() {
        logger.info("Invocando getCMSData"); //$NON-NLS-1$
        return getB64Data();
    }

    public String getData() {
        logger.info("Invocando getData"); //$NON-NLS-1$
        if (this.data == null) logger.warning("No se dispone de datos de salida, se devolvera cadena vacia"); //$NON-NLS-1$
        return (this.data == null ? "" : new String(this.data)); //$NON-NLS-1$
    }

    public void setInRecursiveDirectorySign(boolean recursiveSignDir) {
        logger.info("Invocando setInRecursiveDirectorySign: " + recursiveSignDir); //$NON-NLS-1$
        this.recursiveSignDir = recursiveSignDir;
    }

    public void setInputDirectoryToSign(String directory) {
        logger.info("Invocando setInputDirectoryToSign: " + directory); //$NON-NLS-1$
        this.massiveInputDirectory = directory;
    }

    public String getInputDirectoryToSign() {
        logger.info("Invocando getInputDirectoryToSign"); //$NON-NLS-1$
        return this.massiveInputDirectory;
    }

    public void setOutputDirectoryToSign(String directory) {
        logger.info("Invocando setOutputDirectoryToSign: " + directory); //$NON-NLS-1$
        this.massiveOutputDirectory = directory;
    }

    public String getOutputDirectoryToSign() {
        logger.info("Invocando getOutputDirectoryToSign"); //$NON-NLS-1$
        return this.massiveOutputDirectory;
    }

    public void setOriginalFormat(boolean originalFormat) {
        logger.info("Invocando setOriginalFormat: " + originalFormat); //$NON-NLS-1$
        this.originalFormat = originalFormat;
    }

    // *************************************************************************
    // ******************** METODOS PUBLICOS DEPRECADOS ************************
    // *************************************************************************

    public void setShowErrors(boolean showErrors) {
        logger.info("Invocando setShowErrors: " + showErrors); //$NON-NLS-1$
        this.showErrors = showErrors;
    }

    @Deprecated
    public void setShowHashMessage(boolean showHashMessage) {
        logger.info("Invocando setShowHashMessage: " + showHashMessage);//$NON-NLS-1$
        this.showHashes = showHashMessage;
    }

    public final void setShowExpiratedCertificates(boolean showExpiratedCerts) {
        logger.info("Invocando setShowExpiratedCertificates: " + showExpiratedCerts);//$NON-NLS-1$
        this.ksConfigManager.setShowExpiratedCertificates(showExpiratedCerts);
    }

    /** Muestra un di&aacute;logo pidiendo confirmaci&oacute;n para la firma de
     * los datos representados por un hash en base 64. La entrada de datos para
     * la operaci&oacute;n de firma, cofirma o contrafirma debe ser
     * v&aacute;lida.
     * @return <code>true</code> cuando se confirma la operaci&oacute;n <code>false</code> en caso contrario. */
    private boolean showHashMessage() {
        String digestAlgo = AOUtil.getDigestAlgorithm(this.sigAlgo);
        String hashData = getHexDigestData(digestAlgo);

        // Mostramos el mensaje informando del hash de los datos o, en caso de
        // no haber podido calcularlo,
        // lo informamos.
        int result =
                JOptionPane.showConfirmDialog(this,
                                              (hashData != null ? AppletMessages.getString("SignApplet.655") + "\n" + digestAlgo + ": " + hashData : //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                                                               AppletMessages.getString("SignApplet.657")), //$NON-NLS-1$
                                              AppletMessages.getString("SignApplet.658"), //$NON-NLS-1$
                                              JOptionPane.OK_CANCEL_OPTION,
                                              JOptionPane.WARNING_MESSAGE);
        return result == JOptionPane.OK_OPTION;
    }

    /** Recupera el hash en hexadecimal de los datos de entrada del applet para
     * firma.
     * @return Hash en hexadecimal formateado o <code>null</code> si no se
     *         introdujeron o no pudieron leerse los datos. */
    private String getHexDigestData(String algorithm) {

        byte[] hashData = null;
        // Si estamos firmando datos o si es una firma de hash implicita
        // (contamos con los datos)
        if (this.data != null && (this.hash == null || this.sigMode == AOConstants.SIGN_MODE_IMPLICIT)) {
            try {
                hashData = AOCryptoUtil.getMessageDigest(this.data, algorithm);
            }
            catch (final Exception e) {
                hashData = null;
            }
        }
        // Si estamos firmando un fichero
        else if (this.fileUri != null) {
            try {
                hashData = AOCryptoUtil.getMessageDigest(AOUtil.getDataFromInputStream(AOUtil.loadFile(this.fileUri, null, false)), algorithm);
            }
            catch (final Exception e) {
                hashData = null;
            }
        }
        // Si estamos firmando un hash en modo explicito (el caso de modo
        // implicito ya se trato)
        else if (this.hash != null) {
            hashData = this.hash;
        }
        // Si no se esta firmando nada de lo anterior (que deberia abarcar todos
        // los casos de firmar y cofirma)
        // comprobamos si se ha introducido la informacion de una firma
        // electronica (caso unicamente de
        // contrafirma ya que la cofirma debe mostrar la informacion de los
        // datos, no de la firma ya existente).
        // Comprobamos si se ha introducido la firma directamente
        else if (this.signData != null) {
            try {
                hashData = AOCryptoUtil.getMessageDigest(this.signData, algorithm);
            }
            catch (final Exception e) {
                hashData = null;
            }
        }
        // Comprobamos si se ha introducido la localizacion de la firma
        else if (this.electronicSignatureFile != null) {
            try {
                hashData =
                        AOCryptoUtil.getMessageDigest(AOUtil.getDataFromInputStream(AOUtil.loadFile(this.electronicSignatureFile, null, false)),
                                                      algorithm);
            }
            catch (final Exception e) {
                hashData = null;
            }
        }

        if (hashData == null) {
            logger.severe("No se han indicado o no se han podido leer los datos para el calculo de la huella digital"); //$NON-NLS-1$
            return null;
        }

        return AOUtil.hexify(hashData, ":"); //$NON-NLS-1$
    }

    public String Firma(final String datos) {
        logger.info("Invocando Firma"); //$NON-NLS-1$

        // Inicializamos los valores por codigo
        initialize();

        // Descodificamos los datos introducidos, que son los parametros
        // separados por '#' que
        // definen la operacion a realizar
        String[] params = (new String(AOCryptoUtil.decodeBase64(datos))).split("#"); //$NON-NLS-1$

        // Comprobamos que hemos podido extraer todos los parametros
        if (params == null || params.length < 3) {
            logger.severe("La cadena codificada con los parametros no es valida"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.669")); //$NON-NLS-1$
            return null;
        }

        // Formato de firma: CMS
        this.setSignatureFormat(AOConstants.SIGN_FORMAT_CMS);

        // Algoritmo de firma: SHA1 con RSA
        this.setSignatureAlgorithm(AOConstants.SIGN_ALGORITHM_SHA1WITHRSA);

        // Recogemos de los parametros la operacion que debemos realizar
        int operation = Integer.parseInt(params[0]);

        // Incluimos como atributo de firma el segundo
        // parametro extraido (identificador de transaccion)
        if (!this.addSignedAttribute("2.5.4.45", params[1])) { //$NON-NLS-1$
            logger.severe("No se pudo agregar el identificador de transaccion a la firma"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.673")); //$NON-NLS-1$
            return null;
        }

        boolean allOK = false;
        switch (operation) {

            // Firma
            case 0:
                // Establecemos el hash en base 64 para la firma
                this.setHash(params[2]);
                allOK = this.sign();
                logger.info("Se ha realizado una operacion de firma"); //$NON-NLS-1$
                break;

            // Cofirma (Firma en paralelo)
            case 1:
                // Establecemos el hash en base 64 para la firma
                this.setHash(params[2]);
                // Establecemos la firma
                setElectronicSignature(params[3]);
                // Cofirmamos
                allOK = this.coSign();
                logger.info("Se ha realizado una operacion de cofirma"); //$NON-NLS-1$
                break;

            // Contrafirma (Firma en cascada)
            case 2:
                // Establecemos la firma
                setElectronicSignature(params[2]);
                // Establecemos los firmantes que se desean contrafirmar
                setSignersToCounterSign(params[3]);
                // Contrafirmamos
                allOK = this.counterSignIndexes();
                logger.info("Se ha realizado una operacion de contrafirma"); //$NON-NLS-1$
                break;
            default:
                logger.severe("Operacion de firma no soportada"); //$NON-NLS-1$ 
                this.setError(AppletMessages.getString("SignApplet.682")); //$NON-NLS-1$
                return null;
        }

        if (!allOK) {
            logger.severe("No se pudo completar la firma de datos"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.101")); //$NON-NLS-1$
            return null;
        }

        // Indicamos que no se ha producido error.
        SignApplet.this.setError(null);

        // Devolvemos "cert=<CERTIFICADO>;enc=<FIRMA>"
        return "cert=" + getSignCertificateBase64Encoded() + ";enc=" + getSignatureBase64Encoded(); //$NON-NLS-1$ //$NON-NLS-2$
    }

    public boolean addSignedAttribute(String oid, String value) {
        logger.warning("Invocando addSignedAttribute: " + oid + " = " + value); //$NON-NLS-1$ //$NON-NLS-2$

        // Comprobaciones de seguridad
        if (oid == null || value == null) {
            logger.severe("Ni el OID ni el valor del atributo firmado a agregar pueden ser nulos"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.690")); //$NON-NLS-1$
            return false;
        }

        // Creamos primeramente el listado de atributos si no lo esta ya
        if (this.signedAttributes == null) this.signedAttributes = new HashMap<org.ietf.jgss.Oid, String>();

        // Comprobamos que OID se valido
        org.ietf.jgss.Oid newOid = null;
        try {
            newOid = new org.ietf.jgss.Oid(oid);
        }
        catch (final Exception e) {
            logger.severe("El OID especificado no tiene un formato de OID valido: " + e); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.693")); //$NON-NLS-1$
            return false;
        }

        // Comprobamos que el OID no estuviese ya agregado
        if (this.signedAttributes.containsKey(newOid)) {
            logger.warning("Ya existia un atributo con el OID especificado, se sustituira su valor por el nuevo"); //$NON-NLS-1$
        }

        // Agregamos el nuevo atributo
        this.signedAttributes.put(newOid, value);

        return true;
    }

    public boolean removeSignedAttribute(String oid) {
        logger.warning("Invocando removeSignedAttribute: " + oid); //$NON-NLS-1$

        // Comprobamos que el oid no sea nulo
        if (oid == null) {
            logger.severe("El OID del atributo firmado que se desea eliminar no puede ser nulo"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.698")); //$NON-NLS-1$
            return false;
        }

        // Comprobamos la validez del OID a eliminar
        org.ietf.jgss.Oid oidToRemove = null;
        try {
            oidToRemove = new org.ietf.jgss.Oid(oid);
        }
        catch (final Exception e) {
            logger.severe("El OID especificado no tiene un formato valido: " + e); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.693")); //$NON-NLS-1$
            return false;
        }

        // Comprobamos que el atributo exista
        if (this.signedAttributes == null || !this.signedAttributes.containsKey(oidToRemove)) {
            logger.severe("El OID especificado no se agrego previamente a la firma"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.704")); //$NON-NLS-1$
            return false;
        }

        // Eliminamos el atributo
        this.signedAttributes.remove(oidToRemove);

        // Si esta vacio el listado de atributos, liberamos memoria
        if (this.signedAttributes.isEmpty()) {
            this.signedAttributes = null;
        }
        return true;
    }

    public boolean addUnsignedAttribute(String oid, String value) {
        logger.warning("Invocando addUnsignedAttribute: " + oid + " = " + value); //$NON-NLS-1$ //$NON-NLS-2$

        // Comprobaciones de seguridad
        if (oid == null || value == null) {
            logger.severe("Ni el OID ni el valor del atributo no firmado a agregar pueden ser nulos"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.690")); //$NON-NLS-1$
            return false;
        }

        // Creamos primeramente el listado de atributos si no lo esta ya
        if (this.unsignedAttributes == null) this.unsignedAttributes = new HashMap<org.ietf.jgss.Oid, Vector<String>>();

        // Comprobamos que OID se valido
        org.ietf.jgss.Oid newOid = null;
        try {
            newOid = new org.ietf.jgss.Oid(oid);
        }
        catch (final Exception e) {
            logger.severe("El OID especificado no tiene un formato valido: " + e); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.693")); //$NON-NLS-1$
            return false;
        }

        // Agregamos el valor del atributo, teniendo en cuenta que el OID
        // especificado ya podria tener otros atributos asignados
        Vector<String> values = null;
        if (this.unsignedAttributes.containsKey(newOid)) {
            values = this.unsignedAttributes.get(newOid);
        }
        else {
            values = new Vector<String>();
        }
        values.add(value);

        // Agregamos el atributo con todos sus valores
        this.unsignedAttributes.put(newOid, values);

        return true;
    }

    public boolean removeUnsignedAttribute(String oid, String value) {
        logger.warning("Invocando removeUnsignedAttribute: " + oid); //$NON-NLS-1$

        // Comprobamos que el oid no sea nulo
        if (oid == null) {
            logger.severe("El OID del atributo no firmado que se desea eliminar no puede ser nulo"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.698")); //$NON-NLS-1$
            return false;
        }

        // Comprobamos la validez del OID a eliminar
        org.ietf.jgss.Oid oidToRemove = null;
        try {
            oidToRemove = new org.ietf.jgss.Oid(oid);
        }
        catch (final Exception e) {
            logger.severe("El OID especificado no tiene un formato valido: " + e); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.693")); //$NON-NLS-1$
            return false;
        }

        // Comprobamos que el atributo exista y si tiene mas valores asignados
        // para eliminar lo que corresponda
        if (this.unsignedAttributes != null && this.unsignedAttributes.containsKey(oidToRemove)) {
            Vector<String> values = this.unsignedAttributes.get(oidToRemove);
            if (values.contains(value)) {
                // Si el atributo existe y solo tiene ese valor, eliminamos el
                // atributo completo
                if (values.size() == 1) {
                    this.unsignedAttributes.remove(oidToRemove);
                }
                else {
                    // Si el atributo tiene mas valores, eliminamos solo el que
                    // corresponda
                    values.remove(value);
                    this.unsignedAttributes.put(oidToRemove, values);
                }
            }
        }

        // Si esta vacio el listado de atributos, liberamos memoria
        if (this.unsignedAttributes != null && this.unsignedAttributes.isEmpty()) this.unsignedAttributes = null;

        return true;
    }

    public void addExtraParam(String key, String value) {
        logger.warning("Invocando addExtraParam: " + key); //$NON-NLS-1$

        // Si no se ha indicado una clave o valor, abortamos la operacion
        if (key == null || value == null) return;

        // Establecemos la propiedad
        genericConfig.setProperty(key, value);
    }

    public void removeExtraParam(String key) {
        logger.warning("Invocando removeExtraParam: " + key); //$NON-NLS-1$

        // Si no se ha indicado una clave, abortamos la operacion
        if (key == null) return;

        // Eliminamos la propiedad
        genericConfig.remove(key);
    }

    public void addXMLTransform(String type, String subtype, String body) {
        if (this.xmlTransforms == null) this.xmlTransforms = new Vector<AOXMLTransform>();

        this.xmlTransforms.add(new AOXMLTransform(type, subtype, body));
    }

    public void resetXMLTransforms() {
        this.xmlTransforms = null;
    }

    public String getVersion() {

        Properties p = new Properties();
        try {
            p.load(SignApplet.class.getResourceAsStream("/version.properties")); //$NON-NLS-1$
        }
        catch (final Exception e) {
            logger.warning("No se han podido obtener los datos de version del cliente de firma"); //$NON-NLS-1$
        }
        final StringBuilder version = new StringBuilder();
        version.append(p.getProperty("version.mayor", "0")).append(".") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
               .append(p.getProperty("version.minor", "0")).append(".") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
               .append(p.getProperty("version.build", "0")); //$NON-NLS-1$ //$NON-NLS-2$

        String desc = p.getProperty("build"); //$NON-NLS-1$
        if (desc != null && !desc.trim().equals("")) { //$NON-NLS-1$
            version.append(" ").append(desc); //$NON-NLS-1$
        }
        return version.toString();
    }

    // *************************************************************************
    // **************** Metodos de ayuda a los integradores ********************
    // *************************************************************************

    public String loadFilePath(final String title, final String exts, final String description) {
        logger.info("Invocando loadFilePath"); //$NON-NLS-1$
        try {
            return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
                public String run() {
                    return AOUIManager.getLoadFileName(title,
                                                       ((exts == null || exts.trim().length() == 0)
                                                                                                   ? null
                                                                                                   : AOUtil.split(exts, SignApplet.STRING_SEPARATOR)),
                                                       description,
                                                       SignApplet.this);
                }
            });
        }
        catch (final AOCancelledOperationException e) {
            logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
            return null;
        }
        catch (final Exception e) {
            logger.warning("Error al seleccionar el fichero: " + e); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.233")); //$NON-NLS-1$
            return null;
        }
    }

    public String selectDirectory() {
        logger.info("Invocando selectDirectory"); //$NON-NLS-1$
        try {
            return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
                public String run() {
                    return AOUIManager.selectDirectory(SignApplet.this, AppletMessages.getString("SignApplet.104")); //$NON-NLS-1$
                }
            });
        }
        catch (final AOCancelledOperationException e) {
            logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
            return null;
        }
        catch (final Exception e) {
            logger.warning("Error al seleccionar el directorio: " + e); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.333")); //$NON-NLS-1$
            return null;
        }
    }

    public X509Certificate getSignCertificate() {
        logger.info("Invocando getSignCertificate"); //$NON-NLS-1$
        final X509Certificate cert = this.ksConfigManager.getSelectedCertificate();
        if (cert == null) {
            logger.warning("No se dispone del certificado de firma, se nulo"); //$NON-NLS-1$
        }
        return cert;
    }

    // *************************************************************************
    // *************************************************************************

    /** Recupera el frame del que cuelga un componente.
     * @return Frame padre del componente. */
    private Frame getParentFrame(final Component c) {
        if (c == null) return null;
        final Container cont = c.getParent();
        if (cont instanceof Frame) return (Frame) cont;
        return getParentFrame(cont);
    }

    /** Toma un array de cadenas y las concatena separ&aacute;ndolas con un
     * delimitador.
     * @return Cadena concatenada. */
    private String concatStrings(final String[] strings, final String delimitator) {
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < strings.length; i++) {
            if (i > 0) sb.append(delimitator);
            sb.append(strings[i]);
        }
        return sb.toString();
    }

    public void setCertFilterRFC2254(final String subjectFilter, final String issuerFilter, final boolean onlySignatureCertificates) {
        logger.info("Invocando setCertFilterRFC2254"); //$NON-NLS-1$
        this.setRFC2254Filter(subjectFilter, issuerFilter, onlySignatureCertificates);
        this.ksConfigManager.setMandatoryCert(false);
    }

    public void setMandatoryCertificateConditionRFC2254(final String subjectFilter, final String issuerFilter, final boolean onlySignatureCertificates) {
        logger.info("Invocando setMandatoryCertificateConditionRFC2254"); //$NON-NLS-1$
        this.setRFC2254Filter(subjectFilter, issuerFilter, onlySignatureCertificates);

        // Si se establecio alguno de los tres filtros, activamos la seleccion
        // de un unico certificado
        this.ksConfigManager.setMandatoryCert(subjectFilter != null || issuerFilter != null || onlySignatureCertificates);
    }

    /** Establece filtros de certificados acorde a la RFC2254 para el Subject, el
     * Issuer y seg&uacute;n los KeyUsage de los certificados.
     * @param subjectFilter
     *        Filtro para el titular del certificado, seg&uacute;n formato
     *        definido en la normativa RFC 2554 <br>
     *        Filter for Certificate's holder, as defined by RFC 2554.
     * @param issuerFilter
     *        Filtro para el emisor del certificado, seg&uacute;n formato
     *        definido en la normativa RFC 2554 <br>
     *        Filter for the certificate issuer, as defined by RFC 2554.
     * @param onlySignatureCertificates
     *        Si se establece a <code>true</code> se muestran para
     *        selecci&oacute;n &uacute;nicamente los certificados con el bit
     *        <i>nonRepudiation</i> del campo <i>KeyUsage</i> activado, si
     *        se establece a <code>false</code> se muestran todos los
     *        certificados <br>
     *        If set to <code>true</code> only certificates with an active
     *        <i>nonRepudiation</i> bit in field <i>KeyUsage</i> are
     *        displayed for selection, if set to <code>false</code> all
     *        certificates are displayed. */
    private void setRFC2254Filter(final String subjectFilter, final String issuerFilter, final boolean onlySignatureCertificates) {
        ksConfigManager.addCertFilter(new RFC2254CertificateFilter(
               subjectFilter, 
               issuerFilter, 
               (onlySignatureCertificates) ? AOConstants.SIGN_CERT_USAGE : null
        ));
    }

    /** Selecciona un certificado del usuario y devuelve la referencia a su clave
     * privada. En caso de error configura el mensaje de error correspondiente.
     * @return Referencia a la clave privada del certificado o {@code null} si
     *         ocurri&oacute; alg&uacute;n error. */
    private PrivateKeyEntry configureCertificate() {

        if (!ksConfigManager.isSelectedCertificate()) {
            try {
                ksConfigManager.selectCertificate();
            }
            catch (final AOCancelledOperationException e) {
                logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
            }
            catch (final AOCertificateException e) {
                logger.severe("Error al seleccionar el certificado del repositorio: " + e); //$NON-NLS-1$
                this.setError(AppletMessages.getString("SignApplet.363")); //$NON-NLS-1$
            }
            catch (final AOCertificateKeyException e) {
                logger.info("Error al extraer la clave privada del certificado: " + e); //$NON-NLS-1$
                this.setError(AppletMessages.getString("SignApplet.114")); //$NON-NLS-1$
            }
            catch (final AOKeyStoreManagerException e) {
                logger.severe("Error inicializando el almacen de claves: " + e); //$NON-NLS-1$
                this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
            }
            catch (final AOCertificatesNotFoundException e) {
                logger.info("No se han encontrado en el almacen certificados compatibles con la aplicacion: " + e); //$NON-NLS-1$
                this.setError(AppletMessages.getString("SignApplet.115")); //$NON-NLS-1$
            }
            catch (final AOKeystoreAlternativeException e) {
                AOKeyStore kst = e.getAlternativeKsm();
                if (kst == null) {
                    logger.severe("Error inicializando el almacen de claves: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
                }
                else if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(SignApplet.this,
                                                                                 AppletMessages.getString("SignApplet.80") + kst.getDescription() + "?", //$NON-NLS-1$ //$NON-NLS-2$
                                                                                 AppletMessages.getString("SignApplet.658"), //$NON-NLS-1$ 
                                                                                 JOptionPane.WARNING_MESSAGE)) {
                    setKeyStore(null, null, kst.toString());
                    configureCertificate();
                }
                else {
                    logger.severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                }
            }

        }
        return ksConfigManager.getCertificateKeyEntry();
    }

    /** Firma una cadena de texto simulando el funcionamiento del m&eacute;todo {@code [window.]crypto
     * .signText(stringToSign, caOption, [caNameString1, [caNameString2, . . . ]])} de JavaScript en navegadores Mozilla / Firefox.
     * @param stringToSign
     *        El texto a firmar. Si se especifica "ask" en el
     *        par&aacute;metro caOption se presenta el texto al usuario en
     *        un di&aacute;logo de una forma legible
     * @param caOption
     *        Puede ser una de las siguientes opciones:
     *        <ul>
     *        <li>"auto" indica que el programa seleccionar&aacute; autom&aacute;ticamente un certificado. Si se ha indicado uno o m&aacute;s nombre
     *        de CA mediante los par&aacute;metros caNameN la selecci&oacute;n autom&aacute;tica se limita a los certificado emitidos por las CA
     *        indicadas.</li>
     *        <li>"ask" indica que se debe solicitar al usuario que seleccione un certificado. Si se ha indicado uno o m&aacute;s nombre de CA
     *        mediante los par&aacute;metros caNameN la selecci&oacute;n autom&aacute;tica se limita a los certificado emitidos por las CA indicadas.</li>
     *        </ul>
     * @param caNameN
     *        DN de las CA cuyos certificados deben tenerse en cuenta para
     *        la selecci&oacute;n de firmante, debe proporcionarse un
     *        par&aacute;metro por cada CA. Para mayor informaci&oacute;n
     *        sobre el formato DN consulte <a
     *        href="http://www.faqs.org/rfcs/rfc1485.html">String
     *        Representation of Distinguished Names</a>.
     * @return Si el usuario aprob&oacute; la operaci&oacute;n y esta
     *         termin&oacute; correctamente se devuelve el objeto firmado en
     *         formato CMS codificado en Base64. En caso contrario devuelve uno
     *         de los siguientes c&oacute;digos de error:
     *         <ul>
     *         <li>error:noMatchingCert si el usuario no dispone de ning&uacute;n certificado emitido por las CA indicadas.</li>
     *         <li>error:userCancel si el usuario cancela la operaci&oacute;n.</li>
     *         <li>error:internalError si ocurre cualquier error durante el proceso.</li>
     *         </ul> */
    public String signText(final String stringToSign, final String caOption, final String[] caNameN) {
        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {

            public String run() {

                final AOKeyStoreManager ksManager;
                try {
                    ksManager = SignApplet.this.ksConfigManager.getKeyStoreManager();
                }
                catch (final AOCancelledOperationException e) {
                    logger.severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                    return "error:userCancel"; //$NON-NLS-1$
                }
                catch (final AOKeyStoreManagerException e) {
                    logger.severe("Error inicializando el almacen de claves: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
                    return "error:internalError"; //$NON-NLS-1$
                }
                catch (final AOKeystoreAlternativeException e) {
                    AOKeyStore kst = e.getAlternativeKsm();
                    if (kst == null) {
                        logger.severe("Error inicializando el almacen de claves: " + e); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
                        return "error:internalError"; //$NON-NLS-1$
                    }
                    if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(SignApplet.this,
                                                                                AppletMessages.getString("SignApplet.80") + kst.getDescription() + "?", //$NON-NLS-1$ //$NON-NLS-2$
                                                                                AppletMessages.getString("SignApplet.658"), //$NON-NLS-1$ 
                                                                                JOptionPane.WARNING_MESSAGE)) {
                        setKeyStore(null, null, kst.toString());
                        return signText(stringToSign, caOption, caNameN);
                    }
                    logger.severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                    return "error:userCancel"; //$NON-NLS-1$
                }

                final SignText signTextComponent =
                        new SignText(getArrayCertificatesAlias(),
                                     ksManager,
                                     SignApplet.this,
                                     AOCryptoUtil.getCertificatePC(SignApplet.this.ksConfigManager.getKeyStore(), SignApplet.this));

                // El metodo signText muestra el dialogo propio de la
                // funcion document.signText() de Mozilla
                // ejecuta la operacion de firma y, si ocurre algun
                // error, establece como resultado el codigo
                // de error correspondiente
                signTextComponent.signText(stringToSign, caOption, caNameN);

                return signTextComponent.getResult();
            }
        });
    }

    /** Firma una cadena de texto simulando el funcionamiento del m&eacute;todo {@code [window.]crypto
     * .signText(stringToSign, caOption, [caNameString1, [caNameString2, . . . ]])} de JavaScript en navegadores Mozilla / Firefox.
     * @param stringToSign
     *        El texto a firmar. Si se especifica "ask" en el
     *        par&aacute;metro caOption se presenta el texto al usuario en
     *        un di&aacute;logo de una forma legible
     * @param caOption
     *        Puede ser una de las siguientes opciones:
     *        <ul>
     *        <li>"auto" indica que el programa seleccionar&aacute; autom&aacute;ticamente un certificado. Si se ha indicado uno o m&aacute;s nombre
     *        de CA mediante los par&aacute;metros caNameN la selecci&oacute;n autom&aacute;tica se limita a los certificado emitidos por las CA
     *        indicadas.</li>
     *        <li>"ask" indica que se debe solicitar al usuario que seleccione un certificado. Si se ha indicado uno o m&aacute;s nombre de CA
     *        mediante los par&aacute;metros caNameN la selecci&oacute;n autom&aacute;tica se limita a los certificado emitidos por las CA indicadas.</li>
     *        </ul>
     * @return Si el usuario aprob&oacute; la operaci&oacute;n y esta
     *         termin&oacute; correctamente se devuelve el objeto firmado en
     *         formato CMS codificado en Base64. En caso contrario devuelve uno
     *         de los siguientes c&oacute;digos de error:
     *         <ul>
     *         <li>error:noMatchingCert si el usuario no dispone de ning&uacute;n certificado emitido por las CA indicadas.</li>
     *         <li>error:userCancel si el usuario cancela la operaci&oacute;n.</li>
     *         <li>error:internalError si ocurre cualquier error durante el proceso.</li>
     *         </ul> */
    public String signText(final String stringToSign, final String caOption) {
        return signText(stringToSign, caOption, null);
    }

    public void changeLanguage(final String locale) {
        if (locale != null) {
            Locale.setDefault(new Locale(locale));
        }
    }
}

/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.awt.Component;
import java.awt.Container;
import java.awt.Frame;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URL;
import java.net.URLDecoder;
import java.security.AccessController;
import java.security.KeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivilegedActionException;
import java.security.UnrecoverableEntryException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
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

import es.gob.afirma.applet.old.websign.Browser;
import es.gob.afirma.applet.old.websign.FirmadorWeb.FirmaWeb;
import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.envelopers.cms.AOInvalidRecipientException;
import es.gob.afirma.keystores.filters.old.OldFilter;
import es.gob.afirma.keystores.filters.rfc.KeyUsageFilter;
import es.gob.afirma.keystores.filters.rfc.RFC2254CertificateFilter;
import es.gob.afirma.keystores.main.common.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerException;
import es.gob.afirma.keystores.main.common.AOKeystoreAlternativeException;
import es.gob.afirma.keystores.main.common.KeyStoreUtilities;
import es.gob.afirma.keystores.main.filters.CertificateFilter;
import es.gob.afirma.keystores.main.filters.MultipleCertificateFilter;
import es.gob.afirma.massive.DirectorySignatureHelper;
import es.gob.afirma.massive.MassiveSignatureHelper;
import es.gob.afirma.massive.MassiveSignatureHelper.MassiveSignConfiguration;
import es.gob.afirma.massive.MassiveType;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.cms.AOCMSSigner;


/** Reimplementaci&oacute;n del Applet original de firma del cliente AFirma.
 * Por seguridad los siguientes m&eacute;todos piden confirmaci&oacute;n directamente al usuario:
 * <ul>
 * <li><code>cipherFile(final String filename)</code></li>
 * <li><code>decipherFile(final String filename)</code></li>
 * <li><code>getFileBase64Encoded(final String filename, final boolean showProgress)</code></li>
 * <li><code>getTextFileContent(final String filename)</code></li>
 * <li><code>massiveSignatureFile(final String filename)</code></li>
 * <li><code>saveCipherDataToFile(final String filename)</code></li>
 * <li><code>saveDataToFile(final String filename)</code></li>
 * <li><code>savePlainDataToFile(final String filename)</code></li>
 * <li><code>setElectronicSignatureFile(final String filename)</code></li>
 * <li><code>setFileuri(final String uri)</code></li>
 * <li><code>setFileuriBase64(final String uri)</code></li>
 * <li><code>setInputDirectoryToSign(final String directory)</code></li>
 * <li><code>setKeyStore(final String filename, final String password, final String type)</code></li>
 * <li><code>setOutFilePath(final String filename)</code></li>
 * <li><code>setOutputDirectoryToSign(final String directory)</code></li>
 * <li><code>signAndPackFile(final String filename)</code></li>
 * </ul> */
public final class SignApplet extends JApplet implements EntryPointsCrypto, EntryPointsUtil {

	/** Estado del  modo DEBUG. Mantener a {@code false} en la compilaci&oacute;n final. */
	private static final boolean DEBUG = true;

    private static final String CR = "\n"; //$NON-NLS-1$

    /** Separador utilizado para separar varios valores consecutivos en una
     * cadena devuelta por el Applet. */
    public static final String STRING_SEPARATOR = "\u0024%\u0024"; //$NON-NLS-1$

    /** N&uacute;meto de puerto por defecto para la conexi&oacute;n LDAP. */
    private static final int DEFAULT_LDAP_PORT = 389;

    /** Logger para la visualizaci&oacute;n de los mensajes por consola. */
    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private String userAgent = null;

    /** Nombre del fichero donde deben guardarse los datos (firmas, datos, etc.). */
    String outputFile = null;

    /** URI de la fuente de datos. */
    String fileUri = null;

    /** Indica si la propiedad fileUri apunta a un fichero en base 64. */
    boolean fileBase64 = false;

    /** Hash que se desea firmar. */
    byte[] hash = null;

    /** Algoritmo de firma actual. */
    String sigAlgo = AOSignConstants.DEFAULT_SIGN_ALGO;

    /** Formato de firma actual. */
    String sigFormat = AOSignConstants.DEFAULT_SIGN_FORMAT;

    /** Modo de firma actual. */
    String sigMode = AOSignConstants.DEFAULT_SIGN_MODE;

    /** Datos a operar criptogr&aacute;ficamente. */
    byte[] data = null;

    /** Firma electr&oacute;nica, tanto la generada por las distintas operaciones
     * de firma como la utilizada en las operaciones de firma y cofirma. */
    byte[] signData = null;

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

    /** Valor de la huella digital de la pol&iacute;tica de firma. */
    private final String policyHashB64 = null;

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
    List<String> hashesToSign = null;

    /** Tipo de operaci&oacute;n masiva a realizar. Por defecto, multifirma
     * masiva. */
    MassiveType massiveOperation = MassiveType.SIGN;

    /** Indica si se deben firmar los ficheros de las subcarpetas del directorio
     * seleccionado durante la operacion de firma masiva. */
    boolean recursiveSignDir = false;

    /** Directorio de donde se toman los ficheros a firmar de forma masiva. */
    String massiveInputDirectory = null;

    /** Directorio en donde se almacenar&aacute;n las firmas masivas. */
    String massiveOutputDirectory = null;

    /** Indica si se debe respectar el formato de firma original para la
     * multifirma masiva. */
    boolean originalFormat = true;

    /** Extensiones por las que debe filtrarse durante la firma masiva. */
    String[] massiveExtFiltered = null;

    /** Almacena el resultado de la firma masiva, en donde todas las firmas
     * est&aacute;n en base 64 y concatenadas con '!' (cierre de
     * exclamaci&oacute;n). Este atributo almacena la firma expl&iacute;cita de
     * los ficheros que se han seleccionado (no de sus Hashes como en versiones
     * anteriores del cliente). */
    String massiveSignData = null;

    /** Indica si se ha producido alg&uacute;n error durante la &uacute;ltima
     * operaci&oacute;n. */
    boolean error = false;

    /** Indica si se deben mostrar o no los hashes de los ficheros que se vayan a
     * firmar. */
    boolean showHashes = false;

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
    private String dataMimeType = MimeHelper.DEFAULT_MIMETYPE;

    /** MimeType establecido externamente para incorporar a la firma. */
    private String extMimeType = null;

    /** Listado de atributos firmados que se deben agregar a una firma. */
    private Map<org.ietf.jgss.Oid, String> signedAttributes = null;

    /** Listado de atributos sin firmar que se deben agregar a una firma. */
    private Map<org.ietf.jgss.Oid, Vector<String>> unsignedAttributes = null;

    /** Listado de propiedades gen&eacute;ricas establecidas para las firmas. */
    Properties genericConfig = new Properties();

    /** Transformaciones XML que aplicar a los formatos de firma que las
     * soporten. */
    private List<AOXMLTransform> xmlTransforms = null;

    /** Gestor del almac&eacute;n de certificados. */
    KeyStoreConfigurationManager ksConfigManager = null;

    /** Indica si debe mostrarse una advertencia para que se inserten los
     * dispositivos criptogr&aacute;ficos externos (principalmente, tarjetas
     * inteligentes) cuando el almac&eacute;n de certificados sea Mozilla o un
     * PKCS#11. */
    boolean showMozillaSmartCardWarning = false;

    private static final long serialVersionUID = 5692094082535848369L;

    /** Construye la clase asegurandose de que se inicializan todas las
     * propiedades necesarias. */
    public SignApplet() {
        this.ksConfigManager = new KeyStoreConfigurationManager(AOKeyStore.PKCS12, this);
        this.cipherManager = new CipherManager(this);
        this.enveloperManager = new EnveloperManager(this);
    }

    @Override
    public void init() {

    	changeLocale(getParameter("locale")); //$NON-NLS-1$

    	LOGGER.info("Cliente @firma"); //$NON-NLS-1$
        LOGGER.info("Versi\u00F3n: " + getVersion()); //$NON-NLS-1$

        LOGGER.info("Sistema operativo: " + Platform.getOS().toString()); //$NON-NLS-1$
        LOGGER.info("Arquitectura del JRE: " + Platform.getJavaArch()); //$NON-NLS-1$

        SignApplet.setLookAndFeel();

        this.userAgent = getParameter("userAgent"); //$NON-NLS-1$

        // Configuramos el almacen de claves que corresponda
        final AOKeyStore keyStore = SignApplet.configureDefaultStore(Platform.getOS(), Platform.getBrowser(this.userAgent));
        this.ksConfigManager = new KeyStoreConfigurationManager(keyStore, this);

        LOGGER.info("Almacen de certificados preestablecido: " + keyStore.getDescription()); //$NON-NLS-1$

        // Configuramos si se debe mostrar en los navegadores Mozilla un dialogo
        // de advertencia
        // acerca de los token externos. Por defecto, se mostraran.
        String paramValue = getParameter("showMozillaSmartCardWarning"); //$NON-NLS-1$
        if ((paramValue == null || !paramValue.trim().equalsIgnoreCase("false")) && //$NON-NLS-1$
            (keyStore == AOKeyStore.MOZ_UNI || keyStore == AOKeyStore.PKCS11)) {
            this.showMozillaSmartCardWarning = true;
            this.ksConfigManager.setLoadingWarning(true);
        }

        // Configuramos si se deben mostrar los certificados caducados (por
        // defecto, true)
        paramValue = getParameter("showExpiratedCertificates"); //$NON-NLS-1$
        this.defaultShowExpiratedCertificates = (paramValue == null || !paramValue.trim().equalsIgnoreCase("false")); //$NON-NLS-1$
        this.ksConfigManager.setShowExpiratedCertificates(this.defaultShowExpiratedCertificates);

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

    /** {@inheritDoc} */
    public void initialize() {
        LOGGER.info("Invocando initialize"); //$NON-NLS-1$
        this.outputFile = null;
        this.fileUri = null;
        this.fileBase64 = false;
        this.hash = null;
        this.sigAlgo = AOSignConstants.DEFAULT_SIGN_ALGO;
        this.sigFormat = AOSignConstants.DEFAULT_SIGN_FORMAT;
        this.sigMode = AOSignConstants.DEFAULT_SIGN_MODE;
        this.data = null;
        this.signData = null;
        this.cipherManager.initialize();
        this.enveloperManager.initialize();
        this.ldapServerUrl = null;
        this.ldapServerPort = DEFAULT_LDAP_PORT;
        this.ldapCertificatePrincipal = null;
        this.dataMimeType = MimeHelper.DEFAULT_MIMETYPE;
        this.extMimeType = null;
        this.signersToCounterSign = new String[0];
        this.hashesToSign = null;
        this.massiveOperation = MassiveType.SIGN;
        this.recursiveSignDir = false;
        this.originalFormat = true;
        this.massiveInputDirectory = null;
        this.massiveOutputDirectory = null;
        this.massiveExtFiltered = null;
        this.massiveSignData = null;
        this.error = false;
        this.errorMsg = ""; //$NON-NLS-1$
        this.electronicSignatureFile = null;
        this.signedAttributes = null;
        this.unsignedAttributes = null;
        this.genericConfig = new Properties();
        this.xmlTransforms = null;
        this.ksConfigManager.initialize();
        this.ksConfigManager.setShowExpiratedCertificates(this.defaultShowExpiratedCertificates);
    }

    /** {@inheritDoc} */
    public String getCertificatesAlias() {
        LOGGER.info("Invocando getCertificatesAlias"); //$NON-NLS-1$
        return SignApplet.concatStrings(getArrayCertificatesAlias(), STRING_SEPARATOR);
    }

    /** {@inheritDoc} */
    public String[] getArrayCertificatesAlias() {

        LOGGER.info("Invocando getArrayCertificatesAlias"); //$NON-NLS-1$
        return AccessController.doPrivileged(new java.security.PrivilegedAction<String[]>() {
            public String[] run() {

                try {
                    SignApplet.this.setError(null);
                    return SignApplet.this.ksConfigManager.getArrayCertificateAlias();
                }
                catch (final AOCancelledOperationException e) {
                    LOGGER.severe("Operacion cancelada por el usuario: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                    return new String[0];
                }
                catch (final AOKeyStoreManagerException e) {
                    LOGGER.severe("Error inicializando el almacen de claves, se devolvera una lista vacia de certificados: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
                    return new String[0];
                }
                catch (final AOKeystoreAlternativeException e) {
                    final AOKeyStore kst = e.getAlternativeKsm();
                    if (kst == null) {
                        LOGGER.severe("Error inicializando el almacen de claves: " + e); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
                        return new String[0];
                    }
                    if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(SignApplet.this,
                                                                                AppletMessages.getString("SignApplet.4") + " " + kst.getDescription() + "?", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                                                                                AppletMessages.getString("SignApplet.658"), //$NON-NLS-1$
                                                                                JOptionPane.WARNING_MESSAGE)) {
                        setKeyStore(null, null, kst.toString());
                        return getArrayCertificatesAlias();
                    }
                    LOGGER.severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                    return new String[0];
                }
            }
        });
    }

    /** {@inheritDoc} */
    public String getCertificates() {
        LOGGER.info("Invocando getCertificates"); //$NON-NLS-1$
        return SignApplet.concatStrings(getArrayCertificates(), STRING_SEPARATOR);
    }

    /** {@inheritDoc} */
    public String[] getArrayCertificates() {
        LOGGER.info("Invocando getArrayCertificates"); //$NON-NLS-1$

        String[] certs = new String[0];
        try {
            final String aliases[] = getArrayCertificatesAlias();
            certs = new String[aliases.length];
            for (int i = 0; i < aliases.length; i++) {
                certs[i] = getCertificate(aliases[i]);
            }
        }
        catch (final Exception e) {
            LOGGER.severe("Error obteniendo los certificados, se devolvera null : " + e); //$NON-NLS-1$
            setError(AppletMessages.getString("SignApplet.9")); //$NON-NLS-1$
            return null;
        }
        return certs;
    }

    /** {@inheritDoc} */
    public String getCertificate(final String alias) {
        LOGGER.info("Invocando getCertificate: " + alias); //$NON-NLS-1$
        final X509Certificate cert = getCertificateBinary(alias);
        if (cert == null) {
        	return null;
        }

        final String b64CertEncode;
        try {
        	b64CertEncode = Base64.encode(cert.getEncoded());
        }
        catch (final Exception e) {
        	LOGGER.severe("Error al codificar el certificado, se devolvera null: " + e); //$NON-NLS-1$
        	return null;
        }
        return "Bag Attributes\r\n" + //$NON-NLS-1$
        "friendlyName: " //$NON-NLS-1$
        + AOUtil.getCN(cert)
        + "\r\n" + //$NON-NLS-1$
        "-----BEGIN CERTIFICATE-----\r\n" //$NON-NLS-1$
        +
        b64CertEncode
        + "\r\n-----END CERTIFICATE-----"; //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    public String getCertificatePublicKey(final String alias) {
    	LOGGER.info("Invocando getCertificatePublicKey: " + alias); //$NON-NLS-1$
    	final X509Certificate cert = getCertificateBinary(alias);
    	if (cert == null) {
    		return null;
    	}

    	try {
    		return "-----BEGIN RSA PUBLIC KEY-----\r\n" + //$NON-NLS-1$
    		Base64.encode(cert.getPublicKey().getEncoded())
    		+ "\r\n-----END RSA PUBLIC KEY-----"; //$NON-NLS-1$
    	}
    	catch (final Exception e) {
    		LOGGER.severe("Error al codificar la clave publica del certificado, se devolvera null: " + e); //$NON-NLS-1$
    		return null;
    	}
    }

    private final X509Certificate getCertificateBinary(final String alias) {
    	try {
    		return AccessController.doPrivileged(new GetCertificateAction(alias, this.ksConfigManager));
    	} catch (final PrivilegedActionException e) {
    		if (e.getCause() instanceof AOKeystoreAlternativeException) {
    			final AOKeyStore kst = ((AOKeystoreAlternativeException) e.getCause()).getAlternativeKsm();
    			if (kst == null) {
    				LOGGER.severe("Error al inicializar el repositorio de certificados: " + e.getCause()); //$NON-NLS-1$
    				this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
    				return null;
    			}
    			if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(this,
    					AppletMessages.getString("SignApplet.4", kst.getDescription()), //$NON-NLS-1$
    					AppletMessages.getString("SignApplet.658"), //$NON-NLS-1$
    					JOptionPane.WARNING_MESSAGE)) {
    				setKeyStore(null, null, kst.toString());
    				return getCertificateBinary(alias);
    			}
    			LOGGER.severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
    			SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
    			return null;
    		}
			LOGGER.severe(e.toString());
			this.setError(e.getMessage());
			return null;
    	}
    }

    static void saveDataToStorage(final byte[] binaryData, final String filename) throws AOException {
        if (binaryData == null) {
            throw new IllegalArgumentException("Los datos que desea almacenar no pueden ser nulos"); //$NON-NLS-1$
        }
        if (filename == null) {
            throw new IllegalArgumentException("El nombre de fichero de salida no puede ser nulo"); //$NON-NLS-1$
        }

        OutputStream fos = null;
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
                    LOGGER.warning("No se ha podido cerrar el fichero de salida" //$NON-NLS-1$
                    );
                }
            }
        }
    }

    /** {@inheritDoc} */
    public boolean saveDataToFile(final String filename) {
        LOGGER.info("Invocando setDataToFile: " + filename); //$NON-NLS-1$

        if (filename == null || filename.length() < 1) {
            LOGGER.severe("El nombre de fichero para guardar datos es incorrecto, no se salvaran los datos"); //$NON-NLS-1$
            SignApplet.this.setError(AppletMessages.getString("SignApplet.14")); //$NON-NLS-1$
            return false;
        }

        if (!checkUserPermision(AppletMessages.getString("SignApplet.5") + CR + filename //$NON-NLS-1$
    			+ CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
        	return false;
        }


        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {
                final URI uri;
                try {
                    uri = AOUtil.createURI(filename);
                }
                catch (final Exception e) {
                    LOGGER.severe("La URI proporcionada no es valida (" + filename + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.15") + filename); //$NON-NLS-1$
                    return Boolean.FALSE;
                }
                if (!uri.getScheme().equals("file")) { //$NON-NLS-1$
                    LOGGER.severe("Solo se permite grabar en almacenamiento local, no mediante el protocolo " + uri.getScheme()); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.17") + uri.getScheme()); //$NON-NLS-1$
                    return Boolean.FALSE;
                }
                // OK, en este punto "fileUri" es un nombre de fichero,
                // con lo que ignoramos la uri
                // y lo tratamos como tal
                try {
                    saveDataToStorage(SignApplet.this.data, filename);
                }
                catch (final Exception e) {
                    LOGGER.severe("Error al almacenar los datos en disco: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.18")); //$NON-NLS-1$
                    return Boolean.FALSE;
                }

                SignApplet.this.setError(null);

                return Boolean.TRUE;
            }
        }).booleanValue();
    }

    /** {@inheritDoc} */
    public boolean saveDataToFile() {
        LOGGER.info("Invocando saveDataToFile"); //$NON-NLS-1$

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {
                saveFileAsinchronously(SignApplet.this.data, SignApplet.this.outputFile, null, null);
                SignApplet.this.setError(null);
                return Boolean.TRUE;
            }
        }).booleanValue();
    }

    /** {@inheritDoc} */
    public boolean saveSignToFile() {
        LOGGER.info("Invocando saveSignToFile"); //$NON-NLS-1$

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {
                if (SignApplet.this.signData == null || SignApplet.this.signData.length < 1) {
                    LOGGER.severe("No se dispone de datos de firma, no se creara el fichero de firma"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.23")); //$NON-NLS-1$
                    return Boolean.FALSE;
                }

                String[] extensions = null;
                String description = null;

                if (SignApplet.this.outputFile == null || SignApplet.this.outputFile.length() < 1) {

                    extensions = new String[] {
                        "sig"}; //$NON-NLS-1$
                    description = AppletMessages.getString("SignApplet.25"); //$NON-NLS-1$

                    if (AOSignConstants.SIGN_FORMAT_CMS.equals(SignApplet.this.sigFormat)) {
                        description = AppletMessages.getString("SignApplet.29"); //$NON-NLS-1$
                        extensions = new String[] {
                                "csig", "p7s", "sig"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                    }

                    else if (AOSignConstants.SIGN_FORMAT_CADES.equals(SignApplet.this.sigFormat)) {
                        description = AppletMessages.getString("SignApplet.26"); //$NON-NLS-1$
                        extensions = new String[] {
                            "csig"}; //$NON-NLS-1$
                    }

                    else if (AOSignConstants.SIGN_FORMAT_XADES.equals(SignApplet.this.sigFormat) || AOSignConstants.SIGN_FORMAT_XADES_DETACHED.equals(SignApplet.this.sigFormat)
                             || AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED.equals(SignApplet.this.sigFormat)
                             || AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING.equals(SignApplet.this.sigFormat)
                             || AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED.equals(SignApplet.this.sigFormat)
                             || AOSignConstants.SIGN_FORMAT_XMLDSIG.equals(SignApplet.this.sigFormat)
                             || AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED.equals(SignApplet.this.sigFormat)
                             || AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED.equals(SignApplet.this.sigFormat)
                             || AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING.equals(SignApplet.this.sigFormat)
                             || AOSignConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED.equals(SignApplet.this.sigFormat)
                             || AOSignConstants.SIGN_FORMAT_SOAP.equals(SignApplet.this.sigFormat)) {
                        description = AppletMessages.getString("SignApplet.27"); //$NON-NLS-1$
                        extensions = new String[] {
                                "xsig", "xml"}; //$NON-NLS-1$ //$NON-NLS-2$
                    }

                    else if (AOSignConstants.SIGN_FORMAT_PDF.equals(SignApplet.this.sigFormat)) {
                        description = AppletMessages.getString("SignApplet.28"); //$NON-NLS-1$
                        extensions = new String[] {
                            "pdf"}; //$NON-NLS-1$
                    }

                    else if (AOSignConstants.SIGN_FORMAT_OOXML.equals(SignApplet.this.sigFormat)) {
                        description = AppletMessages.getString("SignApplet.30"); //$NON-NLS-1$
                        extensions = new String[] {
                                "docx", "xlsx", "pptx"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                    }

                    else if (AOSignConstants.SIGN_FORMAT_ODF.equals(SignApplet.this.sigFormat)) {
                        description = AppletMessages.getString("SignApplet.32"); //$NON-NLS-1$
                        extensions = new String[] {
                                "odt", "ods", "odp"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                    }
                }

                // Almacenamos en disco
                saveFileAsinchronously(SignApplet.this.signData, SignApplet.this.outputFile, extensions, description);

                SignApplet.this.setError(null);

                return Boolean.TRUE;
            }
        }).booleanValue();
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
     *        Descripci&oacute;n de los datos para el di&aacute;logo de
     *        guardado. */
    void saveFileAsinchronously(final byte[] dat, final String outputPath, final String[] extensions, final String description) {
        final AsynchronousSaveData saveDataDialog =
                new AsynchronousSaveData(dat, outputPath, description, extensions, getParentFrame(SignApplet.this), true);
        // En firefox iniciamos un proceso asincrono para que no molesten los
        // dialogos
        // de script ocupado. En el resto de navegadores dejamos que aparezcan
        // estos
        // dialogos, porque los procesos asincronos pueden causar errores
        if (Platform.getBrowser(this.userAgent).equals(Platform.BROWSER.FIREFOX)) {
            new Thread(saveDataDialog).start();
        }
        else {
            saveDataDialog.run();
        }
    }

    /** {@inheritDoc} */
	@Deprecated
	public void setCertFilter(final String certFilter) {
		LOGGER.info("Invocando setCertFilter: " + certFilter); //$NON-NLS-1$

		if (certFilter != null) {
			this.ksConfigManager.resetFilters();
			this.ksConfigManager.addCertFilter(new OldFilter(certFilter));
		}
        this.ksConfigManager.setMandatoryCert(false);
	}

	/** {@inheritDoc} */
	@Deprecated
	public void setMandatoryCertificateCondition(final String certFilter) {
		LOGGER.info("Invocando setMandatoryCertificateCondition: " + certFilter); //$NON-NLS-1$

		if (certFilter != null) {
			this.ksConfigManager.resetFilters();
			this.ksConfigManager.addCertFilter(new OldFilter(certFilter));
		}
		this.ksConfigManager.setMandatoryCert(true);
	}

	/** {@inheritDoc} */
    public void setData(final String data) {
        LOGGER.info("Invocando setData"); //$NON-NLS-1$
        if (data == null) {
            this.data = null;
            return;
        }

        this.data = Base64.decode(data);
        this.fileUri = null;
        this.fileBase64 = false;
        this.hash = null;

    }

    /** {@inheritDoc} */
    public void setFileuri(final String uri) {
        LOGGER.info("Invocando setFileUri: " + uri); //$NON-NLS-1$

        if (uri == null || uri.trim().equals("")) { //$NON-NLS-1$
            this.fileUri = null;
            return;
        }

        if (!checkUserPermision(AppletMessages.getString("SignApplet.19") + CR + uri + //$NON-NLS-1$
        		CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
        	return;
        }

        this.fileUri = uri;
        this.data = null;
        this.hash = null;
        this.fileBase64 = false;
    }

    /** {@inheritDoc} */
    public void setFileuriBase64(final String uri) {

        LOGGER.info("Invocando setFileuriBase64: " + uri); //$NON-NLS-1$

        if (uri == null || uri.trim().equals("")) { //$NON-NLS-1$
            this.fileUri = null;
            return;
        }


        if (!checkUserPermision(AppletMessages.getString("SignApplet.19") + CR + uri + //$NON-NLS-1$
        		CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
            return;
        }

        this.fileUri = uri;
        this.fileBase64 = true;
        this.data = null;
        this.hash = null;
    }

    /** {@inheritDoc} */
    public void setHash(final String hash) {
        LOGGER.info("Invocando setHash: " + hash); //$NON-NLS-1$
        if (hash == null) {
            this.hash = null;
            return;
        }

        SignApplet.this.hash = Base64.decode(hash);
        SignApplet.this.dataMimeType = MimeHelper.DEFAULT_MIMETYPE;
        SignApplet.this.fileUri = null;
        SignApplet.this.fileBase64 = false;
        SignApplet.this.data = null;
    }

    /** {@inheritDoc} */
    public void setElectronicSignature(final String signatureB64) {
        LOGGER.info("Invocando setElectronicSignature"); //$NON-NLS-1$
        if (signatureB64 == null || signatureB64.length() == 0) {
            this.signData = null;
        }
        else {
            this.signData = Base64.decode(signatureB64);
        }
        this.electronicSignatureFile = null;
    }

    /** {@inheritDoc} */
    public void setElectronicSignatureFile(final String filename) {

        LOGGER.info("Invocando inElectronicSignatureFile: " + filename); //$NON-NLS-1$

        if (filename == null || filename.length() == 0) {
            this.electronicSignatureFile = null;
            return;
        }

        if (!checkUserPermision(AppletMessages.getString("SignApplet.33") + CR + filename + //$NON-NLS-1$
        		CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
            return;
        }
        try {
            this.electronicSignatureFile = AOUtil.createURI(filename);
        }
        catch (final Exception e) {
            LOGGER.severe("La URI proporcionada no es valida (" + filename + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
            this.electronicSignatureFile = null;
        }
        this.signData = null;
    }

    /** {@inheritDoc} */
    public void setSignersToCounterSign(final String signers) {
        LOGGER.info("Invocando setSignersToCounterSign" + (signers != null ? ": " + signers.trim().replace('\n', ' ').replace("\r\n", " ") : "")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        this.signersToCounterSign = (signers == null ? new String[0] : signers.split("(\\r\\n|\\n)")); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    public String getSignersStructure() {
        LOGGER.info("Invocando getSignersStructure"); //$NON-NLS-1$

        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {

                // Tomamos la firma que deseamos analizar
                final byte[] originalSign;
                try {
                	final GetSignatureAction getSignatureAction = new GetSignatureAction(
                			SignApplet.this.signData, SignApplet.this.electronicSignatureFile);
                	getSignatureAction.setSelectFile(true, SignApplet.this.sigFormat, SignApplet.this);
                	originalSign = AccessController.doPrivileged(getSignatureAction);
                	SignApplet.this.electronicSignatureFile = getSignatureAction.getSelectedSignatureFile();
                }
                catch (final PrivilegedActionException e) {
                	if (e.getCause() instanceof AOCancelledOperationException) {
                		LOGGER.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                		SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                	} else if (e.getCause() instanceof AOException) {
                		LOGGER.info("Error al seleccionar el fichero de firma: " + e.getCause()); //$NON-NLS-1$
                		SignApplet.this.setError(AppletMessages.getString("SignApplet.69")); //$NON-NLS-1$
                	}
                	return null;
                }

                // Probamos si la firma se corresponde con el formato
                // establecido y si no es asi
                // la analizamos y tomamos el manejador correspondiente
                AOSigner signer = AOSignerFactory.getSigner(SignApplet.this.sigFormat);
                if (signer == null || !signer.isSign(originalSign)) {
                    signer = AOSignerFactory.getSigner(originalSign);
                }

                // Si la firma no esta en un formato soportado,
                // establecemos el error
                if (signer == null) {
                    LOGGER.severe("La firma introducida no se ajusta a ningun formato soportado"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.63")); //$NON-NLS-1$
                    return null;
                }

                // Mostramos el el arbol de firmas
                try {
                    return AOUtil.showTreeAsString(signer.getSignersStructure(originalSign, false), null, null);
                }
                catch (final Exception e) {
                    LOGGER.severe("Arbol de firmas no valido: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.72")); //$NON-NLS-1$
                    return null;
                }
            }
        });
    }

    /** {@inheritDoc} */
    public boolean counterSignTree() {
        LOGGER.info("Invocando counterSignTree"); //$NON-NLS-1$
        return this.counterSign(CounterSignTarget.TREE);
    }

    /** {@inheritDoc} */
    public boolean counterSignLeafs() {
        LOGGER.info("Invocando counterSignLeafs"); //$NON-NLS-1$
        return this.counterSign(CounterSignTarget.LEAFS);
    }

    /** {@inheritDoc} */
    public boolean counterSignSigners() {
        LOGGER.info("Invocando counterSignSigners"); //$NON-NLS-1$
        return this.counterSign(CounterSignTarget.SIGNERS);
    }

    /** {@inheritDoc} */
    public boolean counterSignIndexes() {
        LOGGER.info("Invocando counterSignIndexes"); //$NON-NLS-1$
        return this.counterSign(CounterSignTarget.NODES);
    }

    private boolean counterSign(final CounterSignTarget target) {

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {
                final String algorithm = (SignApplet.this.sigAlgo == null ? AOSignConstants.DEFAULT_SIGN_ALGO : SignApplet.this.sigAlgo);
                final String format = (SignApplet.this.sigFormat == null ? AOSignConstants.DEFAULT_SIGN_FORMAT : SignApplet.this.sigFormat);

                // Tomamos la firma sobre la que se realiza la
                // contrafirma
                final byte[] originalSign;
                try {
                	final GetSignatureAction getSignatureAction = new GetSignatureAction(
                			SignApplet.this.signData, SignApplet.this.electronicSignatureFile);
                	getSignatureAction.setSelectFile(true, SignApplet.this.sigFormat, SignApplet.this);
                    originalSign = AccessController.doPrivileged(getSignatureAction);
                    SignApplet.this.electronicSignatureFile = getSignatureAction.getSelectedSignatureFile();
                }
                catch (final PrivilegedActionException e) {
                	if (e.getCause() instanceof AOCancelledOperationException) {
                		LOGGER.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                		SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                	} else if (e.getCause() instanceof AOException) {
                		LOGGER.info("Error al recuperar los datos de firma: " + e.getCause()); //$NON-NLS-1$
                		SignApplet.this.setError(AppletMessages.getString("SignApplet.64")); //$NON-NLS-1$
                	}
                	return Boolean.FALSE;
                }

                // Configuramos el certificado
                final PrivateKeyEntry ke = SignApplet.this.configureCertificate();
                if (ke == null) {
                    return Boolean.FALSE;
                }

                // Tomamos el manejador del formato de firma
                final AOSigner signer = AOSignerFactory.getSigner(format);
                if (signer == null) {
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.95") + format); //$NON-NLS-1$
                    LOGGER.severe("El formato de firma '" + format + //$NON-NLS-1$
                                  "' no esta soportado. Lo formatos soportados son:\n"  //$NON-NLS-1$
                                  +
                                  AOSignerFactory.getInstance().toString());
                    return Boolean.FALSE;
                }

                final Properties extraParams = new Properties();

                // Establecemos el mimetype de los datos
                configureDataTypeExtraParams(extraParams);

                // Obtenemos los parametros necesarios segun tipo de
                // contrafirma. Esto son los firmantes
                // para la contrafirma de firmantes, los nodos para la
                // contrafirma de nodos y ninguno
                // para la contrafirma de todos los nodos o solo los
                // nodos hoja.
                Object[] params = null;

                // Obtenemos los parametros para la contrafirma de
                // firmantes
                if (target == CounterSignTarget.SIGNERS) {
                    if (SignApplet.this.signersToCounterSign == null || SignApplet.this.signersToCounterSign.length < 1) {
                        try {
                            params = UIDialogs.showSignersSelectionPane(signer.getSignersStructure(originalSign, false), SignApplet.this);
                        }
                        catch (final AOCancelledOperationException e) {
                            LOGGER.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                            SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                            return Boolean.FALSE;
                        }
                        catch (final AOException e) {
                            LOGGER.severe("Error al recuperar los firmantes a contrafirmar: " + e); //$NON-NLS-1$
                            SignApplet.this.setError(AppletMessages.getString("SignApplet.97")); //$NON-NLS-1$
                            return Boolean.FALSE;
                        }
                    }
                    else {
                        params = SignApplet.this.signersToCounterSign;
                    }
                }

                // Obtenemos los parametros para la contrafirma de nodos
                else if (target == CounterSignTarget.NODES) {
                    // Si no se establecen los nodos de firma mediante
                    // setSignersToCounterSign(String)
                    // mostramos el panel de seleccion de nodos. Este
                    // panel devolvera los nodos de firma
                    // a partir del 0.
                    if (SignApplet.this.signersToCounterSign == null || SignApplet.this.signersToCounterSign.length < 1) {
                        int[] indexes;
                        try {
                            indexes = UIDialogs.showNodeSignSelectionPane(signer.getSignersStructure(originalSign, false), SignApplet.this);
                        }
                        catch (final AOCancelledOperationException ex) {
                            LOGGER.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                            SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                            return Boolean.FALSE;
                        }
                        catch (final Exception ex) {
                            LOGGER.severe("Error al seleccionar los nodos de firma: " + ex); //$NON-NLS-1$
                            SignApplet.this.setError(AppletMessages.getString("SignApplet.99")); //$NON-NLS-1$
                            return Boolean.FALSE;
                        }

                        if (indexes.length == 0) {
                            LOGGER.severe("Se debe seleccionar al menos un nodo de firma para contrafirmar"); //$NON-NLS-1$
                            SignApplet.this.setError(AppletMessages.getString("SignApplet.100")); //$NON-NLS-1$
                            return Boolean.FALSE;
                        }

                        // Ordenamos el array de indices (requisito del
                        // metodo de contrafirma de nodos)
                        java.util.Arrays.sort(indexes);

                        // Los indices de firma del panel de seleccion
                        // se reciben a partir del 0.
                        params = new Object[indexes.length];
                        for (int i = 0; i < indexes.length; i++) {
                            params[i] = Integer.valueOf(indexes[i]);
                        }
                    }
                    else {
                        params = new Object[SignApplet.this.signersToCounterSign.length];
                        for (int i = 0; i < SignApplet.this.signersToCounterSign.length; i++) {
                            params[i] = Integer.valueOf(SignApplet.this.signersToCounterSign[i]);
                        }
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
                        LOGGER.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                        return Boolean.FALSE;
                    }
                }

                // Contrafirmamos finalmente
                final byte[] outputBuffer;
                try {
                    outputBuffer = signer.countersign(originalSign, algorithm, target, params, ke, extraParams);
                }
                catch (final UnsupportedOperationException e) {
                    LOGGER.severe(e.getMessage());
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.2")); //$NON-NLS-1$
                    JOptionPane.showMessageDialog(SignApplet.this, AppletMessages.getString(AppletMessages.getString("SignApplet.682")), //$NON-NLS-1$
                                                  AppletMessages.getString("SignApplet.156"), //$NON-NLS-1$
                                                  JOptionPane.ERROR_MESSAGE);
                    return Boolean.FALSE;
                }
                catch (final Exception e) {
                    LOGGER.severe("Error durante el proceso de firma: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.101")); //$NON-NLS-1$
                    return Boolean.FALSE;
                }

                // Ahora vamos a guardar el resultado en el fichero de
                // salida
                if (outputBuffer == null || outputBuffer.length < 1) {
                    // No vaya a ser que saliese un resultado vacio...
                    LOGGER.severe("El proceso de contrafirma no genero ningun resultado"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.102")); //$NON-NLS-1$
                    return Boolean.FALSE;
                }
                SignApplet.this.signData = outputBuffer;

                SignApplet.this.setError(null);

                return Boolean.TRUE;
            }
        }).booleanValue();
    }

    /** {@inheritDoc} */
    public void setOutFilePath(final String filename) {
        LOGGER.info("Invocando setOutFilePath: " + filename); //$NON-NLS-1$
        if (filename == null || "".equals(filename)) { //$NON-NLS-1$
            LOGGER.info("Se ha establecido el nombre de fichero de salida a null" //$NON-NLS-1$
            );
            this.outputFile = null;
            return;
        }
        if (!checkUserPermision(AppletMessages.getString("SignApplet.5") + CR + filename + //$NON-NLS-1$
        		CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
        	return;
        }
        this.outputFile = filename;
    }

    /** {@inheritDoc} */
    public void setSignatureAlgorithm(final String algorithm) {
        LOGGER.info("Invocando setSignatureAlgorithm: " + algorithm); //$NON-NLS-1$

        String signatureAlgorithm = algorithm;

        // Para mantener la interfaz con el exterior intacta, traduciremos
        // cualquier nombre de algoritmo antiguo a su nueva forma
        signatureAlgorithm = NormalizedNames.normalizeAlgorithmName(signatureAlgorithm);
        if (signatureAlgorithm == null) {
            LOGGER.warning("El algoritmo de firma no puede ser nulo, se establecera el algoritmo por defecto" //$NON-NLS-1$
            );
            signatureAlgorithm = AOSignConstants.DEFAULT_SIGN_ALGO;
        }

        // Localizamos el algoritmo indicado entre los soportados
        for (final String algo : AOSignConstants.SUPPORTED_SIGN_ALGOS) {
            if (algo.equals(signatureAlgorithm)) {
                this.sigAlgo = signatureAlgorithm;
                return;
            }
        }

        // Si el algoritmo no esta soportado, indicamos los soportado y
        // establecemos el por defecto
        final StringBuilder exstr = new StringBuilder("El algoritmo de firma '") //$NON-NLS-1$
            .append(signatureAlgorithm)
            .append("' no esta soportado, se establecera el algoritmo por ") //$NON-NLS-1$
            .append(AOSignConstants.DEFAULT_SIGN_ALGO)
            .append("\nLos algoritmos de firma soportados son:\n"); //$NON-NLS-1$
        for (final String algo : AOSignConstants.SUPPORTED_SIGN_ALGOS) {
            exstr.append(algo).append("\n"); //$NON-NLS-1$
        }
        LOGGER.warning(exstr.toString());
        this.sigAlgo = AOSignConstants.DEFAULT_SIGN_ALGO;
    }

    /** {@inheritDoc} */
    public void setSignatureFormat(final String format) {
        LOGGER.info("Invocando setSignatureFormat: " + format); //$NON-NLS-1$

        String signatureFormat = format;

        // Si no se establece formato alguno, se mantiene el por defecto
        if (signatureFormat == null) {
            LOGGER.warning("El formato de firma no puede ser nulo, se establecera el formato por defecto: " + AOSignConstants.DEFAULT_SIGN_FORMAT //$NON-NLS-1$
            );
            signatureFormat = AOSignConstants.DEFAULT_SIGN_FORMAT;
        }

        // Para mantener la interfaz con el exterior intacta, traduciremos
        // cualquier nombre de formato antiguo a su nueva forma
        this.sigFormat = NormalizedNames.normalizeFormatName(signatureFormat);
    }

    /** {@inheritDoc} */
    public void setSignatureMode(final String mode) {
        LOGGER.info("Invocando setSignatureMode: " + mode); //$NON-NLS-1$
        // Para mantener la interfaz con el exterior intacta, traduciremos
        // cualquier nombre de modo antiguo a su nueva forma
        if (mode == null) {
            LOGGER.warning("El modo de firma no puede ser nulo, se establecera el modo por defecto" //$NON-NLS-1$
            );
            this.sigMode = AOSignConstants.DEFAULT_SIGN_MODE;
        }
        this.sigMode = NormalizedNames.normalizeModeName(mode);
    }

    /** {@inheritDoc} */
    public String getSignatureMode() {
        LOGGER.info("Invocando getSignatureMode"); //$NON-NLS-1$
        return (this.sigMode == null ? AOSignConstants.DEFAULT_SIGN_MODE : this.sigMode);
    }

    /** {@inheritDoc} */
    public void setKeyStore(final String filename, final String password, final String type) {

    	if ((filename != null) && !checkUserPermision(
    			AppletMessages.getString("SignApplet.19") + CR + filename + //$NON-NLS-1$
    			CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
    		return;
    	}

        LOGGER.info("Invocando setKeyStore de tipo '" + type + "' con el path '" + filename + "'"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
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
                SignApplet.this.ksConfigManager.setKsPath(filename);
                SignApplet.this.ksConfigManager.setKsPassword(password);
                SignApplet.this.ksConfigManager.changeKeyStore(newStore);

                if (SignApplet.this.showMozillaSmartCardWarning && (newStore == AOKeyStore.MOZ_UNI || newStore == AOKeyStore.PKCS11)) {
                    SignApplet.this.ksConfigManager.setLoadingWarning(true);
                }

                setError(null);
                return null;
            }
        });
    }

    /** {@inheritDoc} */
    public void setPolicy(final String identifier, final String description, final String qualifier, final String hashB64) {
        // Configuramos la URL identificadora
        if (identifier != null) {
            try {
                this.policyId = AOUtil.createURI(identifier).toURL();
            }
            catch (final Exception e) {
                LOGGER.severe("No se ha indicado un URL valida para la politica: " + e); //$NON-NLS-1$
            }
        }
        // Configuramos Oid calificador
        if (qualifier != null) {
            // Miramos a ver si es un OID directamente, en cuyo caso lo pasamos
            // a URN
            try {
                this.policyQualifier = new URI("urn:oid:" + new Oid(qualifier)); //$NON-NLS-1$
            }
            catch (final Exception e1) {
                // No es un OID directamente, miramos si es URI
                try {
                    this.policyQualifier = new URI(qualifier);
                }
                catch (final Exception e2) {
                    LOGGER.severe("El calificador indicado no es ni un OID ni una URI valida: " + e1 + ", " + e2 //$NON-NLS-1$ //$NON-NLS-2$
                    );
                }
            }
        }
        // Configuramos la descripcion
        this.policyDesc = description;
    }

    /** {@inheritDoc} */
    public boolean sign() {
        LOGGER.info("Invocando sign"); //$NON-NLS-1$

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {

                // Si no esta establecido el algoritmo usamos el por
                // defecto, pero solo para esta ocasion,
                // no lo establecemos para posteriores
                final String algorithm = (SignApplet.this.sigAlgo == null ? AOSignConstants.DEFAULT_SIGN_ALGO : SignApplet.this.sigAlgo);

                // Si no esta establecido el formato usamos el por
                // defecto, pero solo para esta ocasion,
                // no lo establecemos para posteriores
                final String format = (SignApplet.this.sigFormat == null ? AOSignConstants.DEFAULT_SIGN_FORMAT : SignApplet.this.sigFormat);

                // Si no esta establecido el modo usamos el por defecto,
                // pero solo para esta ocasion,
                // no lo establecemos para posteriores
                final String mode = (SignApplet.this.sigMode == null ? AOSignConstants.DEFAULT_SIGN_MODE : SignApplet.this.sigMode);

                // Para mantener las formas de la version 2.4 del
                // cliente, se mostrara una
                // ventana modal, en caso de solicitarse una firma
                // Enveloped en modo explicito,
                // informando de que esta configuracion es imposible
                if ((format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED) || format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)) && mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT)) {

                    LOGGER.severe("El formato Enveloped es incompatible con el modo de firma explicito"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.139")); //$NON-NLS-1$
                    return Boolean.FALSE;
                }

                // Tomamos el Signer adecuado
                final AOSigner signer = AOSignerFactory.getSigner(format);
                if (signer == null) {
                    LOGGER.severe("El formato de firma '" + format + //$NON-NLS-1$
                                  "' no esta soportado. Lo formatos soportados son:\n" //$NON-NLS-1$
                                  +
                                  AOSignerFactory.getInstance().toString());
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.95") + format); //$NON-NLS-1$
                    return Boolean.FALSE;
                }

                // Como condicion excepcional, si se nos ha introducido
                // un hash para firmar
                // estableceremos el algoritmo hash que se realizo el
                // hash segun el algoritmo que
                // se nos solicitase
                if (SignApplet.this.hash != null && mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT)) {
                    final int withPos = algorithm.indexOf("with"); //$NON-NLS-1$
                    if (withPos == -1) {
                        LOGGER.severe("El formato del algoritmo de firma no es valido: " + algorithm); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.197") + algorithm); //$NON-NLS-1$
                    }
                    SignApplet.this.genericConfig.setProperty("precalculatedHashAlgorithm", algorithm.substring(0, withPos)); //$NON-NLS-1$
                }

                // -----------------------
                // Evitamos las configuraciones de firma de hashes no
                // soportadas
                // -----------------------

                // La firma de hashes solo esta soportada por los
                // formatos de firma: CMS, CAdES, XMLdSig y XAdES
                if (SignApplet.this.hash != null && (SignApplet.this.sigFormat.equals(AOSignConstants.SIGN_FORMAT_PDF) || SignApplet.this.sigFormat.equals(AOSignConstants.SIGN_FORMAT_ODF)
                                     || SignApplet.this.sigFormat.equals(AOSignConstants.SIGN_FORMAT_OOXML) || SignApplet.this.sigFormat.equals(AOSignConstants.SIGN_FORMAT_PKCS1))) {

                    LOGGER.severe("La firma de hash no esta soportada para el formato " + SignApplet.this.sigFormat); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.198") + SignApplet.this.sigFormat); //$NON-NLS-1$
                    return Boolean.FALSE;
                }

                // La firma implicita de hash exige que se introduzcan
                // los datos a los que corresponde el hash.
                // Deben haberse introducido los datos, no se permite el
                // fichero por incompatibilidad con
                // las funciones de soporte del estilo
                // "getFileBase64Encoded",
                // "getFileHashBase64Encoded",...
                if (SignApplet.this.hash != null && SignApplet.this.data == null && mode.equals(AOSignConstants.SIGN_MODE_IMPLICIT)) {
                    LOGGER.severe("La firma implicita de hash exige que se introduzcan los datos a los que corresponde el hash"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.216")); //$NON-NLS-1$
                    return Boolean.FALSE;
                }

                LOGGER.info("Firma con algoritmo " + algorithm + ", formato " + format + " y modo " + mode); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

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
                        return Boolean.FALSE;
                    }

                    // Configuramos el mimetype de los datos
                    configureDataTypeExtraParams(SignApplet.this.genericConfig);
                }

                // Configuramos el certificado
                final PrivateKeyEntry ke = SignApplet.this.configureCertificate();
                if (ke == null) {
                    return Boolean.FALSE;
                }

                // Si se nos ha introducido un listado de hashes
                // entendemos que deben firmarse estos.
                // En este caso, se ignorara cualquier otro parametro de
                // entrada de datos de firma (dato,
                // fichero y hash).
                if (SignApplet.this.hashesToSign != null && SignApplet.this.hashesToSign.size() > 0) {

                    final String[] signs;
                    final DirectorySignatureHelper massiveSigner;
                    try {
                        massiveSigner = new DirectorySignatureHelper(algorithm, format, mode);
                    }
                    catch (final Exception e) {
                        LOGGER.severe("No se pudo inicializar el modulo de firma masiva: " + e); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.200")); //$NON-NLS-1$
                        return Boolean.FALSE;
                    }

                    // Configuramos la politica
                    SignApplet.this.configurePolicy();

                    // Configuramos las transformaciones XML
                    SignApplet.this.configureXMLTransforms();

                    // Configuramos y ejecutamos la operacion
                    SignApplet.this.genericConfig.setProperty("format", format); //$NON-NLS-1$
                    SignApplet.this.genericConfig.setProperty("mode", mode); //$NON-NLS-1$
                    SignApplet.this.genericConfig.setProperty("ignoreStyleSheets", "true"); //$NON-NLS-1$ //$NON-NLS-2$

                    try {
                        signs =
                                massiveSigner.hashesMassiveSign(SignApplet.this.hashesToSign.toArray(new String[0]),
                                                                ke,
                                                                // Recuperamos el signer que se
                                                                // utilizara para la operacion de firma
                                                                // masiva para poder agregarle
                                                                // antes los atributos de firma que
                                                                // correspondan
                                                                SignApplet.this.addAttributes(massiveSigner.getDefaultSigner()),
                                                                SignApplet.this.genericConfig);
                    }
                    catch (final AOException e) {
                        LOGGER.severe("Error durante la operacion de firma masiva de hashes: " + e); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.236")); //$NON-NLS-1$
                        return Boolean.FALSE;
                    }
                    final StringBuilder allSigns = new StringBuilder();
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
                            LOGGER.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                            SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                            return Boolean.FALSE;
                        }
                    }

                    // Agregamos las ultimas configuraciones y firmamos
                    SignApplet.this.genericConfig.setProperty("mode", mode); //$NON-NLS-1$
                    SignApplet.this.genericConfig.setProperty("format", format); //$NON-NLS-1$
                    if (SignApplet.this.fileUri != null) {
                        SignApplet.this.genericConfig.setProperty("uri", SignApplet.this.fileUri); //$NON-NLS-1$
                    }

                    final byte[] outputBuffer;
                    try {
                        outputBuffer = signer.sign(dataToSign, algorithm, ke, SignApplet.this.genericConfig);
                    }
                    catch (final UnsupportedOperationException e) {
                        LOGGER.severe(e.getMessage());
                        SignApplet.this.setError(AppletMessages.getString(AppletMessages.getString("SignApplet.682"))); //$NON-NLS-1$
                        return Boolean.FALSE;
                    }
                    catch (final AOFormatFileException e) {
                        LOGGER.severe(e.getMessage());
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.11")); //$NON-NLS-1$
                        return Boolean.FALSE;
                    }
                    catch (final AOException e) {
                        LOGGER.severe(e.toString());
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.101")); //$NON-NLS-1$
                        e.printStackTrace();
                        return Boolean.FALSE;
                    }
                    catch (final Exception e) {
                        LOGGER.severe("Error durante el proceso de firma: " + e); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.101")); //$NON-NLS-1$
                        return Boolean.FALSE;
                    }

                    // Ahora vamos a guardar el resultado en el fichero
                    // de salida
                    if (outputBuffer == null || outputBuffer.length < 1) {
                        // No vaya a ser que saliese un resultado
                        // vacio...
                        LOGGER.severe("El proceso de firma no genero ningun resultado"); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.154")); //$NON-NLS-1$
                        return Boolean.FALSE;
                    }

                    SignApplet.this.signData = outputBuffer;
                }

                SignApplet.this.setError(null);

                return Boolean.TRUE;
            }
        }).booleanValue();
    }

    /** Agrega las transformaciones XML configuradas en el cliente a la
     * configuraci&oacute;n de firma. */
    void configureXMLTransforms() {
        if (this.xmlTransforms != null) {
            this.genericConfig.setProperty("xmlTransforms", Integer.toString(this.xmlTransforms.size())); //$NON-NLS-1$
            for (int i = 0; i < this.xmlTransforms.size(); i++) {
                this.genericConfig.setProperty("xmlTransform" + (i) + "Type", this.xmlTransforms.get(i).getType()); //$NON-NLS-1$ //$NON-NLS-2$
                // El subtipo y el cuerpo son opcionales
                if (this.xmlTransforms.get(i).getSubtype() != null) {
                    this.genericConfig.setProperty("xmlTransform" + (i) + "Subtype", this.xmlTransforms.get(i).getSubtype()); //$NON-NLS-1$ //$NON-NLS-2$
                }
                if (this.xmlTransforms.get(i).getBody() != null) {
                    this.genericConfig.setProperty("xmlTransform" + (i) + "Body", this.xmlTransforms.get(i).getBody()); //$NON-NLS-1$ //$NON-NLS-2$
                }
            }
        }
    }

    void configureDataTypeExtraParams(final Properties extraParams) {
        final String mimeType = SignApplet.this.extMimeType != null ? SignApplet.this.extMimeType : SignApplet.this.dataMimeType;
        if (mimeType != null) {
            extraParams.setProperty("mimeType", mimeType); //$NON-NLS-1$
            final String oid = MimeHelper.transformMimeTypeToOid(mimeType);
            if (oid != null) {
                extraParams.setProperty("oid", oid); //$NON-NLS-1$
            }
        }
    }

    /** Agrega los atributos firmados y sin firmar definidos al manejador de
     * firma cuando este corresponda a un formato de firma que los soporte.
     * @param signer
     *        Manejador de firma.
     * @return Manejador de firma configurado. */
    AOSigner addAttributes(final AOSigner signer) {
        // Si el Signer soporta la agregacion de atributos
        if (signer instanceof AOCMSSigner || signer instanceof AOCAdESSigner) {

            // Agregamos los atributos firmados
            if (SignApplet.this.signedAttributes != null) {
                final Iterator<org.ietf.jgss.Oid> itOid = SignApplet.this.signedAttributes.keySet().iterator();
                while (itOid.hasNext()) {
                    final org.ietf.jgss.Oid oid = itOid.next();
                    ((AOCMSSigner) signer).addSignedAttribute(oid.toString(), SignApplet.this.signedAttributes.get(oid).getBytes());
                }
            }

            // Agregamos los atributos sin firmar
            if (SignApplet.this.unsignedAttributes != null) {
                final Iterator<org.ietf.jgss.Oid> itOid = SignApplet.this.unsignedAttributes.keySet().iterator();
                while (itOid.hasNext()) {
                    final org.ietf.jgss.Oid oid = itOid.next();
                    for (final String value : SignApplet.this.unsignedAttributes.get(oid)) {
                        ((AOCMSSigner) signer).addUnsignedAttribute(oid.toString(), value.getBytes());
                    }
                }
            }
        }
        return signer;
    }

    /** Agrega la pol&iacute;tica de firma a la configuraci&oacute;n de la
     * operaci&oacute;n de firma.
     * @see #setPolicy(String, String, String, String) */
    void configurePolicy() {
        if (this.policyId != null) {
            this.genericConfig.setProperty("policyIdentifier", this.policyId.toString()); //$NON-NLS-1$
        }
        if (this.policyDesc != null) {
            this.genericConfig.setProperty("policyDescription", this.policyDesc); //$NON-NLS-1$
        }
        if (this.policyQualifier != null) {
            this.genericConfig.setProperty("policyQualifier", this.policyQualifier.toString()); //$NON-NLS-1$
        }
        if (this.policyHashB64 != null) {
        	this.genericConfig.setProperty("policyIdentifierHashAlgorithm", "http://www.w3.org/2000/09/xmldsig#sha1"); //$NON-NLS-1$ //$NON-NLS-2$
            this.genericConfig.setProperty("policyIdentifierHash", this.policyHashB64); //$NON-NLS-1$
        }
    }

    /** {@inheritDoc} */
    public boolean coSign() {
        LOGGER.info("Invocando cosign"); //$NON-NLS-1$

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {

                // No establecemos algoritmo por defecto, si no esta
                // establecido usamos el por
                // defecto, pero solo para esta ocasion
                final String algorithm = (SignApplet.this.sigAlgo == null ? AOSignConstants.DEFAULT_SIGN_ALGO : SignApplet.this.sigAlgo);

                // No establecemos formato por defecto, si no esta
                // establecido usamos el por
                // defecto, pero solo para esta ocasion
                final String format = (SignApplet.this.sigFormat == null ? AOSignConstants.DEFAULT_SIGN_FORMAT : SignApplet.this.sigFormat);

                // No establecemos formato por defecto, si no esta
                // establecido usamos el por
                // defecto, pero solo para esta ocasion
                final String mode = (SignApplet.this.sigMode == null ? AOSignConstants.DEFAULT_SIGN_MODE : SignApplet.this.sigMode);

                // Para mantener las formas de la version 2.4 del
                // cliente, se mostrara una
                // ventana modal, en caso de solicitarse una firma
                // Enveloped en modo explicito,
                // informando de que esta configuracion es imposible
                if ((format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED) || format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)) && mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT)) {
                    LOGGER.severe("El formato Enveloped es incompatible con el modo de firma explicito"); //$NON-NLS-1$
                    setError(AppletMessages.getString("SignApplet.139")); //$NON-NLS-1$
                    return Boolean.FALSE;
                }

                // Tomamos los datos que debemos firmar
                final byte[] dataToSign;
                try {
                    dataToSign = SignApplet.this.getInData();
                }
                catch (final AOException e) {
                    // El metodo getInDataStream ya se habra encargado
                    // de establecer el mensaje en caso de error
                    return Boolean.FALSE;
                }

                // Tomamos la firma sobre la que se realiza la
                // contrafirma
                final byte[] originalSign;
                try {
                	final GetSignatureAction getSignatureAction = new GetSignatureAction(
                			SignApplet.this.signData, SignApplet.this.electronicSignatureFile);
                	getSignatureAction.setSelectFile(true, SignApplet.this.sigFormat, SignApplet.this);
                    originalSign = AccessController.doPrivileged(getSignatureAction);
                    SignApplet.this.electronicSignatureFile = getSignatureAction.getSelectedSignatureFile();
                }
                catch (final PrivilegedActionException e) {
                	if (e.getCause() instanceof AOCancelledOperationException) {
                		LOGGER.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                		SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                	} else if (e.getCause() instanceof AOException) {
                		LOGGER.info("Error al recuperar los datos de firma: " + e.getCause()); //$NON-NLS-1$
                		SignApplet.this.setError(AppletMessages.getString("SignApplet.64")); //$NON-NLS-1$
                	}
                	return Boolean.FALSE;
                }

                // Tomamos el manejador de firma asociado al formato
                final AOSigner signer = AOSignerFactory.getSigner(format);
                if (signer == null) {
                    LOGGER.severe("El formato de firma '" + format + //$NON-NLS-1$
                                  "' no esta soportado. Lo formatos soportados son:\n" //$NON-NLS-1$
                                  +
                                  AOSignerFactory.getInstance().toString());
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.95") + format); //$NON-NLS-1$
                    return Boolean.FALSE;
                }

                if (SignApplet.this.hash != null && mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT)) {
                    final int withPos = algorithm.indexOf("with"); //$NON-NLS-1$
                    if (withPos == -1) {
                        LOGGER.severe("El formato del algoritmo de firma no es valido: " + algorithm); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.197") + algorithm); //$NON-NLS-1$
                        return Boolean.FALSE;
                    }
                    // Establecemos el algoritmo con el que se calculo
                    // el hash externamente
                    SignApplet.this.genericConfig.setProperty("precalculatedHashAlgorithm", algorithm.substring(0, withPos)); //$NON-NLS-1$
                }

                /*
                 * La firma de hashes solo esta soportada por los
                 * formatos de firma binaria CMS y CAdES. Las firmas
                 * PDF, ODF y OOXML requieren siempre los datos, ya que
                 * van empotradas. Las firmas XML los necesitan para
                 * hacer la referencia a los mismos.
                 */
                if (SignApplet.this.hash != null && !SignApplet.this.sigFormat.equals(AOSignConstants.SIGN_FORMAT_CMS) && !SignApplet.this.sigFormat.equals(AOSignConstants.SIGN_FORMAT_CADES)) {
                    LOGGER.severe("La firma de hashes solo esta soportada por los formatos de firma binaria CMS y CAdES"); //$NON-NLS-1$
                    setError(AppletMessages.getString("SignApplet.277")); //$NON-NLS-1$			//TODO: Permitir cofirma de hashes en XML
                    return Boolean.FALSE;
                }

                /*
                 * Evitamos las configuraciones de firma de hashes no
                 * soportadas
                 */

                // La firma implicita de hash exige que se introduzcan
                // los datos a los que corresponde el hash
                if (SignApplet.this.hash != null && SignApplet.this.data == null && mode.equals(AOSignConstants.SIGN_MODE_IMPLICIT)) {
                    LOGGER.severe("La firma implicita de huella digital exige que se introduzcan los datos a los que corresponde la huella digital"); //$NON-NLS-1$
                    setError(AppletMessages.getString("SignApplet.216")); //$NON-NLS-1$
                    return Boolean.FALSE;
                }

                // Establecemos el formato de los datos
                configureDataTypeExtraParams(SignApplet.this.genericConfig);

                // Configuramos el certificado
                final PrivateKeyEntry ke = SignApplet.this.configureCertificate();
                if (ke == null) {
                    return Boolean.FALSE;
                }

                // Si se han especificado atributos de firma los
                // agregamos. Esto solo sera efectivo
                // para los signers a los que aplique
                SignApplet.this.addAttributes(signer);

                // Si se nos pide que mostremos el hash de los datos a
                // firmar, lo hacemos
                if (SignApplet.this.showHashes) {
                    if (!SignApplet.this.showHashMessage()) {
                        LOGGER.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                        SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                        return Boolean.FALSE;
                    }
                }

                // Finalmente, configuramos y operamos
                SignApplet.this.genericConfig.setProperty("mode", mode); //$NON-NLS-1$
                if (SignApplet.this.fileUri != null) {
                    SignApplet.this.genericConfig.setProperty("uri", SignApplet.this.fileUri); //$NON-NLS-1$
                }

                byte[] outputBuffer;
                try {
                    outputBuffer = signer.cosign(dataToSign, originalSign, algorithm, ke, SignApplet.this.genericConfig);
                }
                catch (final UnsupportedOperationException e) {
                    LOGGER.severe(AppletMessages.getString("SignApplet.682") + ": " + e.getMessage()); //$NON-NLS-1$  //$NON-NLS-2$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.682")); //$NON-NLS-1$
                    return Boolean.FALSE;
                }
                catch (final Exception e) {
                    LOGGER.severe("Error durante el proceso de firma: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.101")); //$NON-NLS-1$
                    return Boolean.FALSE;
                }

                // Ahora vamos a guardar el resultado en el fichero de
                // salida
                if (outputBuffer == null || outputBuffer.length < 1) {
                    // No vaya a ser que saliese un resultado vacio...
                    LOGGER.severe("El proceso de firma no genero ningun resultado"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.154")); //$NON-NLS-1$
                    return Boolean.FALSE;
                }
                SignApplet.this.signData = outputBuffer;

                SignApplet.this.setError(null);

                return Boolean.TRUE;
            }
        }).booleanValue();
    }

    /** {@inheritDoc} */
    public void setInIncludeExtensions(final String extensions) {
        LOGGER.info("Invocando setInIncludeExtensions: " + extensions); //$NON-NLS-1$
        this.massiveExtFiltered = (extensions == null ? null : extensions.split(",")); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    public boolean signDirectory() {
        LOGGER.info("Invocando signDirectory"); //$NON-NLS-1$

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {
                if ((SignApplet.this.sigFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED) && SignApplet.this.sigMode.equals(AOSignConstants.SIGN_MODE_EXPLICIT)) || (SignApplet.this.sigFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED) && SignApplet.this.sigMode.equals(AOSignConstants.SIGN_MODE_EXPLICIT))) {
                    SignApplet.this.error = true;
                    LOGGER.severe("El formato Enveloped es incompatible con el modo de firma explicito"); //$NON-NLS-1$
                    setError(AppletMessages.getString("SignApplet.139")); //$NON-NLS-1$
                    return Boolean.FALSE;
                }

                // Si no se ha establecido el directorio de entrada de
                // ficheros, lo solicitamos
                final String inputDir;
                if (SignApplet.this.massiveInputDirectory != null) {
                    inputDir = SignApplet.this.massiveInputDirectory;
                }
                else {
                    inputDir = UIDialogs.selectDirectory(SignApplet.this, AppletMessages.getString("SignApplet.187")); //$NON-NLS-1$
                    if (inputDir.equals("")) { //$NON-NLS-1$
                        LOGGER.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                        return Boolean.FALSE;
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
                    LOGGER.warning("No se ha indicado un directorio para el guardado de los firmas generadas, se almacenaran en el mismo directorio de entrada: " + inputDir); //$NON-NLS-1$
                    outputDir = inputDir;
                }

                // Configuramos el certificado
                final PrivateKeyEntry ke = SignApplet.this.configureCertificate();
                if (ke == null) {
                    return Boolean.FALSE;
                }

                // Creamos el manejador de firma masiva
                final DirectorySignatureHelper massiveSigner;
                try {
                    massiveSigner = new DirectorySignatureHelper(SignApplet.this.sigAlgo, SignApplet.this.sigFormat, SignApplet.this.sigMode);
                }
                catch (final Exception e) {
                    LOGGER.severe("No se pudo inicializar el modulo de firma masiva: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.200")); //$NON-NLS-1$
                    return Boolean.FALSE;
                }

                // Le introducimos el filtro al manejador de firma
                // masiva
                if (SignApplet.this.massiveExtFiltered != null && SignApplet.this.massiveExtFiltered.length > 0) {
                    final StringBuilder description = new StringBuilder(AppletMessages.getString("SignApplet.201")); //$NON-NLS-1$
                    for (int i = 0; i < SignApplet.this.massiveExtFiltered.length; i++) {
                        description.append("*.").append(SignApplet.this.massiveExtFiltered[i]); //$NON-NLS-1$
                        if (i + 1 != SignApplet.this.massiveExtFiltered.length) {
                            description.append(","); //$NON-NLS-1$
                        }
                    }
                    massiveSigner.setFileFilter(new ExtFilter(SignApplet.this.massiveExtFiltered, description.toString()));
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
                                                      SignApplet.this.genericConfig);
                }
                catch (final Exception e) {
                    LOGGER.severe("Error grave durante la operacion de firma masiva: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.205")); //$NON-NLS-1$
                    return Boolean.FALSE;
                }

                if (allOk) {
                    SignApplet.this.setError(null);
                }

                return Boolean.valueOf(allOk);
            }
        }).booleanValue();
    }

    /** {@inheritDoc} */
    public void setMassiveOperation(final String operationName) {
        LOGGER.info("Invocando setMassiveOperation: " + operationName); //$NON-NLS-1$

        String massiveOperationName = operationName;

        if (massiveOperationName == null || massiveOperationName.equals("")) { //$NON-NLS-1$
            massiveOperationName = AOSignConstants.DEFAULT_MASSIVE_OPERATION;
        }

        if (massiveOperationName.equals(AOSignConstants.MASSIVE_OPERATION_SIGN)) {
            this.massiveOperation = MassiveType.SIGN;
        }
        else if (massiveOperationName.equals(AOSignConstants.MASSIVE_OPERATION_COSIGN)) {
            this.massiveOperation = MassiveType.COSIGN;
        }
        else if (massiveOperationName.equals(AOSignConstants.MASSIVE_OPERATION_COUNTERSIGN_TREE)) {
            this.massiveOperation = MassiveType.COUNTERSIGN_ALL;
        }
        else if (massiveOperationName.equals(AOSignConstants.MASSIVE_OPERATION_COUNTERSIGN_LEAFS)) {
            this.massiveOperation = MassiveType.COUNTERSIGN_LEAFS;
        }
        else {
            LOGGER.warning("Operacion masiva no reconocida, se realizara la operacion: " + //$NON-NLS-1$
                    AOSignConstants.DEFAULT_MASSIVE_OPERATION);
            setMassiveOperation(AOSignConstants.DEFAULT_MASSIVE_OPERATION);
        }

        // Si ya hay una configuracion de firma masiva establecida, la
        // actualizamos con la nueva operacion
        if (this.massiveSignatureHelper != null && this.massiveSignatureHelper.isInitialized()) {
            this.massiveSignatureHelper.setMassiveOperation(this.massiveOperation);
        }
    }

    /** {@inheritDoc} */
    public void addMassiveHash(final String hashData) {
        if (this.hashesToSign == null) {
            this.hashesToSign = new Vector<String>();
        }
        this.hashesToSign.add(hashData);
    }

    /** Obtiene los datos con los que se deben reaizar las operaciones de firma y
     * ensobrado de datos.
     * @return Datos de entrada.
     * @throws AOException Si ocurren errores obtener los datos. */
    byte[] getInData() throws AOException {
        byte[] tempData = null;

        // Comprobamos si se nos han introducido los datos directamente. Aun en
        // caso de que se nos haya
        // introducido directamente, en caso de que tambien se haya introducido
        // el hash y el modo de firma
        // sea explicito, hacemos una firma del hash.
        if (this.data == null || (this.hash != null && SignApplet.this.sigMode.equals(AOSignConstants.SIGN_MODE_EXPLICIT))) {

            // Si no, comprobamos si se nos ha introducido un hash para firmar
            if (this.hash == null) {

                // Si no, comprobamos si se nos ha indicado un fichero de
                // entrada
                if (this.fileUri == null) {

                    // Si no, le pedimos al usuario que seleccione un fichero y
                    // lo configuramos
                    final String fileName = AOUIFactory.getLoadFileName(AppletMessages.getString("SignApplet.356"), null, null, this); //$NON-NLS-1$
                    if (fileName == null) {
                        LOGGER.severe("Se ha cancelado la seleccion del fichero de entrada"); //$NON-NLS-1$
                        this.setError(AppletMessages.getString("SignApplet.212")); //$NON-NLS-1$
                        throw new AOException("Se ha cancelado la seleccion del fichero de entrada, se cancelara toda la operacion" //$NON-NLS-1$
                        );
                    }

                    try {
                        this.fileUri = fileName;
                    }
                    catch (final Exception e) {
                        LOGGER.severe("Se ha proporcionado un nombre de fichero no valido '" + fileName + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
                        this.setError(AppletMessages.getString("SignApplet.214") + fileName); //$NON-NLS-1$
                        throw new AOException("Se ha proporcionado un nombre de fichero no valido: " + fileName, e); //$NON-NLS-1$
                    }
                    this.fileBase64 = false;
                }

                // Cargamos los datos de la URI configurada
                try {
                	tempData = AccessController.doPrivileged(new LoadFileAction(this.fileUri));
                } catch (final Exception e) {
                	LOGGER.severe(e.toString());
                	this.setError(AppletMessages.getString("SignApplet.407")); //$NON-NLS-1$
                	throw new AOException(e.getMessage(), e);
                }
            } // Se nos ha introducido un hash
            else {
                tempData = this.hash;
            }
        } // Se nos ha introducido un dato
        else {
            tempData = this.data;
        }

        // Si la entrada son datos o un fichero lo analizamos
        if (this.data != null || this.fileUri != null) {
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
            this.dataMimeType = mtHelper.getMimeType();
        }
    }

    /** {@inheritDoc} */
    public String getSignaturesBase64Encoded() {
        LOGGER.info("Invocando getSignaturesBase64Encoded"); //$NON-NLS-1$
        return this.massiveSignData;
    }

    // ==============================================
    // Funcionalidades de multifirma masiva programatica
    // ==============================================

    /** Manejador de firma masiva. */
    MassiveSignatureHelper massiveSignatureHelper = null;

    /** {@inheritDoc} */
    public boolean initMassiveSignature() {
        LOGGER.info("Invocando initMassiveSignature"); //$NON-NLS-1$

        // Desactivamos la configuracion de error actual
        this.setError(null);

        if (!checkUserPermision(AppletMessages.getString("SignApplet.1") + //$NON-NLS-1$
        		CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
        }

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {

                // Configuramos el certificado
                final PrivateKeyEntry ke = SignApplet.this.configureCertificate();
                if (ke == null) {
                    return Boolean.FALSE;
                }

                // Configuramos el entorno
                SignApplet.this.configurePolicy();
                SignApplet.this.configureXMLTransforms();

                // Establecemos la configuracion que se usara para la
                // firma masiva
                final MassiveSignConfiguration massiveConfiguration = new MassiveSignConfiguration(ke, (X509Certificate) ke.getCertificate());
                massiveConfiguration.setExtraParams(SignApplet.this.genericConfig);
                massiveConfiguration.setAlgorithm(SignApplet.this.sigAlgo);
                massiveConfiguration.setDefaultFormat(SignApplet.this.sigFormat);
                massiveConfiguration.setMode(SignApplet.this.sigMode);
                massiveConfiguration.setOriginalFormat(SignApplet.this.originalFormat);
                massiveConfiguration.setMassiveOperation(SignApplet.this.massiveOperation != null
                                                                                                 ? SignApplet.this.massiveOperation
                                                                                                 : MassiveType.valueOf(AOSignConstants.DEFAULT_MASSIVE_OPERATION));

                try {
                    SignApplet.this.massiveSignatureHelper = new MassiveSignatureHelper(massiveConfiguration);
                }
                catch (final AOException e) {
                    LOGGER.severe("Error al inicializar el modulo de multifirma masiva: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.8")); //$NON-NLS-1$
                    return Boolean.FALSE;
                }
                return Boolean.TRUE;
            }
        }).booleanValue();
    }

    /** {@inheritDoc} */
    public void endMassiveSignature() {
        LOGGER.info("Invocando endMassiveSignature"); //$NON-NLS-1$
        if (this.massiveSignatureHelper == null) {
            LOGGER.warning("No se ha inicializado la operacion de firma masiva"); //$NON-NLS-1$
            return;
        }
        this.massiveSignatureHelper.release();
    }

    /** {@inheritDoc} */
    public String massiveSignatureData(final String b64Data) {
        LOGGER.info("Invocando massiveSignatureData"); //$NON-NLS-1$
        this.setError(null);
        if (this.massiveSignatureHelper == null || !this.massiveSignatureHelper.isInitialized()) {
            this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
            return null;
        }

        // Ejecutamos la operacion
        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {
                final String result = SignApplet.this.massiveSignatureHelper.signData(b64Data);
                if (result == null) {
                    SignApplet.this.setError(SignApplet.this.massiveSignatureHelper.getCurrentLogEntry());
                }
                return result;
            }
        });
    }

    /** {@inheritDoc} */
    public String massiveSignatureHash(final String b64Hash) {
        LOGGER.info("Invocando massiveSignatureHash"); //$NON-NLS-1$
        this.setError(null);
        if (this.massiveSignatureHelper == null || !this.massiveSignatureHelper.isInitialized()) {
            this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
            return null;
        }
        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {
                final String result = SignApplet.this.massiveSignatureHelper.signHash(b64Hash);
                if (result == null) {
                    SignApplet.this.setError(SignApplet.this.massiveSignatureHelper.getCurrentLogEntry());
                }
                return result;
            }
        });
    }

    /** {@inheritDoc} */
    public String massiveSignatureFile(final String filename) {

        LOGGER.info("Invocando massiveSignatureFile: " + filename); //$NON-NLS-1$

        if (this.massiveSignatureHelper == null || !this.massiveSignatureHelper.isInitialized()) {
            this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
            return null;
        }

        if (filename == null || "".equals(filename)) { //$NON-NLS-1$
            setError(AppletMessages.getString("SignApplet.48")); //$NON-NLS-1$
            return null;
        }

        this.setError(null);

        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {
                final String result = SignApplet.this.massiveSignatureHelper.signFile(filename);
                if (result == null){
                    SignApplet.this.setError(SignApplet.this.massiveSignatureHelper.getCurrentLogEntry());
                }
                return result;
            }
        });
    }

    /** {@inheritDoc} */
    public String getMassiveSignatureCurrentLog() {
        LOGGER.info("Invocando getMassiveSignatureCurrentLog"); //$NON-NLS-1$
        this.setError(null);
        if (this.massiveSignatureHelper == null) {
            this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
            return null;
        }
        return this.massiveSignatureHelper.getCurrentLogEntry();
    }

    /** {@inheritDoc} */
    public String getMassiveSignatureLog() {
        LOGGER.info("Invocando getMassiveSignatureLog"); //$NON-NLS-1$
        this.setError(null);
        if (this.massiveSignatureHelper == null) {
            this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
            return null;
        }
        return this.massiveSignatureHelper.getAllLogEntries();
    }

    /** {@inheritDoc} */
    public void saveMassiveSignatureLog() {
        LOGGER.info("Invocando saveMassiveSignatureLog"); //$NON-NLS-1$
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

    /** {@inheritDoc} */
    public String webSign(final String html) {
        LOGGER.info("Invocando webSign: " + html); //$NON-NLS-1$

        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {
                FirmaWeb firmaWeb;
                try {
                    final String signAlgorithm = SignApplet.this.sigAlgo == null ? AOSignConstants.DEFAULT_SIGN_ALGO : SignApplet.this.sigAlgo;

                    // Recuperamos el algoritmo de Hash de la firma
                    final int p = signAlgorithm.toLowerCase().indexOf("with"); //$NON-NLS-1$
                    final String hashAlg = p != -1 ? signAlgorithm.substring(0, p) : signAlgorithm;

					// Realizamos la firma Web
                    firmaWeb = Browser.browse(html, hashAlg);
                    if (firmaWeb != null) {
                        SignApplet.this.setFileuri(firmaWeb.getTmpWebDataFile().getAbsolutePath());
                        SignApplet.this.sign();
                    }
                    else {
                        throw new AOCancelledOperationException("Operacion cancelada por el usuario"); //$NON-NLS-1$
                    }
                }
                catch (final Exception e) {
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.389")); //$NON-NLS-1$
                    LOGGER.severe("Error durante el proceso de firma Web: " + e); //$NON-NLS-1$
                    firmaWeb = null;
                }

                if (firmaWeb != null) {
                    return firmaWeb.getTmpWebDataFile().getAbsolutePath();
                }
                return "ERROR"; //$NON-NLS-1$
            }
        });
    }

    /** {@inheritDoc} */
    public void setSelectedCertificateAlias(final String cAlias) {
        LOGGER.info("Invocando setSelectedCertificateAlias: " + cAlias); //$NON-NLS-1$
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
    private static AOKeyStore configureDefaultStore(final Platform.OS currentOS, final Platform.BROWSER currentBrowser) {

        LOGGER.info("Navegador: " + currentBrowser); //$NON-NLS-1$

        if (Platform.OS.LINUX.equals(currentOS) || Platform.OS.SOLARIS.equals(currentOS) || Platform.BROWSER.FIREFOX.equals(currentBrowser)) {
            // Usamos Mozilla siempre en UNIX y en otros sistemas operativos
            // solo si estamos sobre Firefox
            return AOKeyStore.MOZ_UNI;
        }
        if (Platform.OS.MACOSX.equals(currentOS)) { // Mac OS X
            // En Mac OS X siempre el de Apple cuando no es Firefox
            return AOKeyStore.APPLE;
        }
        if (Platform.OS.WINDOWS.equals(currentOS)) { // Windows
            return AOKeyStore.WINDOWS;
        }

        return AOKeyStore.PKCS12;
    }

    private final static void setLookAndFeel() {
        final String lookandfeel = UIManager.getSystemLookAndFeelClassName();
        try {
            UIManager.setLookAndFeel(lookandfeel);
        }
        catch (final Exception e) {
            LOGGER.warning("No se ha podido establecer el Look&Feel '" + lookandfeel + "', las " + //$NON-NLS-1$ //$NON-NLS-2$
                           "ventanas careceran de decoracion: " //$NON-NLS-1$
                           + e);
        }

        // Nos aseguramos de que los dialogos salgan decorados
        javax.swing.JDialog.setDefaultLookAndFeelDecorated(true);
        javax.swing.JFrame.setDefaultLookAndFeelDecorated(true);
    }

    /** {@inheritDoc} */
    public boolean isInitialized() {
        LOGGER.info("Invocando isInitialized"); //$NON-NLS-1$
        return this.initializedApplet;
    }

    /** {@inheritDoc} */
    public boolean signData(final String b64data) {
        LOGGER.info("Invocando signData"); //$NON-NLS-1$
        if (b64data == null) {
            LOGGER.severe("No se han introducido los datos que se desean firmar"); //$NON-NLS-1$
            SignApplet.this.setError(AppletMessages.getString("SignApplet.278")); //$NON-NLS-1$
            return false;
        }
        this.data = Base64.decode(b64data);
        return sign();
    }

    /** {@inheritDoc} */
    public String getSignCertificateBase64Encoded() {
        LOGGER.info("Invocando getSignCertificateBase64Encoded"); //$NON-NLS-1$
        final byte[] certEnconded;
        try {
        	certEnconded = this.ksConfigManager.getSelectedCertificate().getEncoded();
        }
        catch (final AOKeyStoreManagerException e) {
        	LOGGER.warning("No se ha inicializado el almacen de certificados: " + e); //$NON-NLS-1$
        	return ""; //$NON-NLS-1$
        } catch (final AOKeystoreAlternativeException e) {
        	LOGGER.warning("Error al acceder al almacen de certificados: " + e); //$NON-NLS-1$
        	return ""; //$NON-NLS-1$
        }
        catch (final CertificateEncodingException e) {
            LOGGER.warning("La codificacion del certificado no es valida, se devolvera una cadena vacia: " + e); //$NON-NLS-1$
            return ""; //$NON-NLS-1$
        }
        return Base64.encode(certEnconded);
    }

    /** {@inheritDoc} */
    public String getSignatureBase64Encoded() {
    	LOGGER.info("Invocando getSignatureBase64Encoded"); //$NON-NLS-1$
    	byte[] sign = null;
    	try {
    		sign = AccessController.doPrivileged(new GetSignatureAction(
    				SignApplet.this.signData, SignApplet.this.electronicSignatureFile));
    	}
    	catch (final Exception e) {
    		LOGGER.severe("No se ha podido recuperar la firma electronica: " + e); //$NON-NLS-1$
    		SignApplet.this.setError(AppletMessages.getString("SignApplet.64")); //$NON-NLS-1$
    	}
    	return (sign == null) ? null : Base64.encode(sign);
    }

    /** {@inheritDoc} */
    @Deprecated
    public String getSignatureText() {
        LOGGER.info("Invocando getSignatureText"); //$NON-NLS-1$
        return getSignatureByText(null);
    }

    /** {@inheritDoc} */
    public String getSignatureText(final String charsetName) {
        LOGGER.info("Invocando getSignatureText(charsetName)"); //$NON-NLS-1$
        return getSignatureByText(charsetName);
    }

    private String getSignatureByText(final String charsetName) {
    	byte[] sign = null;
    	try {
    		sign = AccessController.doPrivileged(new GetSignatureAction(
    				SignApplet.this.signData, SignApplet.this.electronicSignatureFile));
    	}
    	catch (final Exception e) {
    		LOGGER.severe("No se ha podido recuperar la firma electronica: " + e); //$NON-NLS-1$
    		SignApplet.this.setError(AppletMessages.getString("SignApplet.64")); //$NON-NLS-1$
    	}
    	try {
    		return (sign == null) ?
    				null : (charsetName == null ?
    						new String(sign) : new String(sign, charsetName));
    	} catch (final UnsupportedEncodingException e) {
    		LOGGER.warning("Codificacion no soportada (" + charsetName + //$NON-NLS-1$
    		"), se devolvera la firma con la codificacion por defecto"); //$NON-NLS-1$
    		return new String(sign);
    	}
    }

    /** {@inheritDoc} */
    public String getFilePath() {
        LOGGER.info("Invocando getFilePath"); //$NON-NLS-1$
        if (this.outputFile == null) {
            LOGGER.warning("No se dispone de la direccion del fichero de firma, se devolvera una cadena vacia"); //$NON-NLS-1$
            return ""; //$NON-NLS-1$
        }
        return this.outputFile;
    }

    /** {@inheritDoc} */
    public String getFileUsedPath() {
        LOGGER.info("Invocando getFileUsedPath"); //$NON-NLS-1$
        if (this.fileUri == null) {
            LOGGER.warning("No se dispone de la direccion del fichero de datos de entrada, se devolvera una cadena vacia"); //$NON-NLS-1$
            return ""; //$NON-NLS-1$
        }

        String path = ""; //$NON-NLS-1$
        try {
            path = URLDecoder.decode(this.fileUri, "UTF-8"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            LOGGER.warning("Codificacion de caracteres no valida: " + e); //$NON-NLS-1$
        }

        // Si es un fichero local eliminamos el esquema de la ruta
        if (path.startsWith("file://")) { //$NON-NLS-1$
            path = path.substring(7);
        }

        return path;
    }

    /** {@inheritDoc} */
    public String getErrorMessage() {
        LOGGER.info("Invocando getErrorMessage"); //$NON-NLS-1$
        return (this.error ? this.errorMsg : ""); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    public boolean isError() {
        LOGGER.info("Invocando isError"); //$NON-NLS-1$
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
    void setError(final String errorMsg) {
        if (errorMsg == null || errorMsg.length() < 1) {
            this.error = false;
            this.errorMsg = ""; //$NON-NLS-1$
        }
        else {
            this.error = true;
            this.errorMsg = errorMsg;
        }

        // Mostramos, si procede, el mensaje de error que corresponda
        if (this.showErrors && this.error) {
            JOptionPane.showMessageDialog(this, this.errorMsg, AppletMessages.getString("SignApplet.156"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
        }
    }

    /** {@inheritDoc} */
    @Deprecated
    public String getTextFileContent(final String filename) {
        LOGGER.info("Invocando getTextFileContent: " + filename); //$NON-NLS-1$

        if (filename == null || "".equals(filename)) { //$NON-NLS-1$
            setError(AppletMessages.getString("SignApplet.54")); //$NON-NLS-1$
            return null;
        }

        if (!checkUserPermision(AppletMessages.getString("SignApplet.19") + CR + filename + //$NON-NLS-1$
        		CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
            return null;
        }

        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {
                final InputStream is;
                try {
                    is = AOUtil.loadFile(AOUtil.createURI(filename));
                }
                catch (final Exception e) {
                    LOGGER.severe("El fichero indicado no existe o no es posible acceder a el: " + e); //$NON-NLS-1$
                    return null;
                }
                final String result;
                try {
                    result = new String(AOUtil.getDataFromInputStream(is));
                }
                catch (final Exception e) {
                    LOGGER.severe("No se pudo leer el contenido del fichero indicado: " + e); //$NON-NLS-1$
                    return null;
                }
                try {
                    is.close();
                }
                catch (final Exception e) {
                    LOGGER.warning("Error al cerrar un flujo de datos: " + e);} //$NON-NLS-1$

                return result;
            }
        });
    }

    /** {@inheritDoc} */
    public String getTextFromBase64(final String b64) {
        LOGGER.info("Invocando getTextFromBase64"); //$NON-NLS-1$
        return new String(Base64.decode(b64));
    }

    /** {@inheritDoc} */
    public String getTextFromBase64(final String b64, final String charsetName) {
        LOGGER.info("Invocando getTextFromBase64"); //$NON-NLS-1$
        try {
        	return new String(Base64.decode(b64), charsetName);
        } catch (final Exception e) {
    		LOGGER.warning("Codificacion no soportada (" + charsetName + //$NON-NLS-1$
    		"), se utilizara la codificacion por defecto"); //$NON-NLS-1$
        	return new String(Base64.decode(b64));
        }
    }

    /** {@inheritDoc} */
    public String getBase64FromText(final String plainText) {
    	LOGGER.info("Invocando getBase64FromText"); //$NON-NLS-1$
        String encoding = null;
        if (plainText.startsWith("<?xml")) { //$NON-NLS-1$
            // Intentamos detectar la codificacion
            final String xmlHeader = plainText.substring(0, plainText.indexOf("?>")); //$NON-NLS-1$
            final int encodingPos = xmlHeader.indexOf("encoding=\""); //$NON-NLS-1$
            if (encodingPos != -1) {
                encoding = xmlHeader.substring(encodingPos + 10, xmlHeader.indexOf("\"", encodingPos + 10)); //$NON-NLS-1$
            }
        }
        if (encoding != null) {
            try {
                return Base64.encode(plainText.getBytes(encoding));
            }
            catch (final Exception e) {
                LOGGER.warning("El XML introducido parece tener una codificacion " + encoding //$NON-NLS-1$
                               + ", pero no ha sido posible usarla para generar el Base64: " //$NON-NLS-1$
                               + e);
            }
        }
        return Base64.encode(plainText.getBytes());
    }

    /** {@inheritDoc} */
    public String getBase64FromText(final String plainText, final String charsetName) {
        LOGGER.info("Invocando getBase64FromText"); //$NON-NLS-1$
        if (charsetName != null) {
            try {
                return Base64.encode(plainText.getBytes(charsetName));
            }
            catch (final Exception e) {
                LOGGER.warning("Codificacion no soportada (" + charsetName + //$NON-NLS-1$
        		"), se utilizara la codificacion por defecto"); //$NON-NLS-1$
            }
        }
        return Base64.encode(plainText.getBytes());
    }

    /** {@inheritDoc} */
    public String getFileBase64Encoded(final String filename, final boolean showProgress) {
        LOGGER.info("Invocando getFileBase64Encoded: " + filename); //$NON-NLS-1$

        if (filename == null || "".equals(filename)) { //$NON-NLS-1$
            setError(AppletMessages.getString("SignApplet.58")); //$NON-NLS-1$
            return null;
        }

        if (!checkUserPermision(AppletMessages.getString("SignApplet.19") + CR + filename +  //$NON-NLS-1$
        		CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
            return null;
        }

        try {
            return getFileBase64Encoded(AOUtil.createURI(filename));
        }
        catch (final Exception e) {
            LOGGER.severe("Error al leer el fichero '" + filename + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
            setError(AppletMessages.getString("SignApplet.81") + filename); //$NON-NLS-1$
            return null;
        }
    }

    /** {@inheritDoc} */
    public String getFileBase64Encoded(final boolean showProgress) {
    	LOGGER.info("Invocando getFileBase64Encoded"); //$NON-NLS-1$
    	try {
    		return getFileBase64Encoded(AOUtil.createURI(this.fileUri));
    	}
    	catch (final Exception e) {
    		LOGGER.severe("Error al leer el fichero '" + this.fileUri + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
    		setError(AppletMessages.getString("SignApplet.81") + this.fileUri); //$NON-NLS-1$
    		return null;
    	}
    }

    /** Recupera el contenido de un fichero codificado en base 64.
     * @param uri
     *        Ruta del fichero.
     * @param showProgress
     *        Si desea mostrarse una barra de progreso para la carga.
     * @return Contentido en base 64. */
    private String getFileBase64Encoded(final URI uri) {
        if (uri == null) {
            LOGGER.severe("No se ha establecido un fichero que desea obtener en base 64"); //$NON-NLS-1$
            return null;
        }

        final byte[] fileContent = FileUtils.loadFile(uri);
        if (fileContent == null) {
            this.setError(AppletMessages.getString("SignApplet.407")); //$NON-NLS-1$
            return null;
        }

        return this.fileBase64 ? new String(fileContent) : Base64.encode(fileContent);
    }

    /** {@inheritDoc} */
    public String getFileHashBase64Encoded() {
        LOGGER.info("Invocando getFileHashBase64Encoded"); //$NON-NLS-1$
        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {
                if (SignApplet.this.fileUri == null) {
                    LOGGER.severe("No se ha establecido el fichero del que calcular el Hash"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.348")); //$NON-NLS-1$
                    return null;
                }
                final InputStream is;
                try {
                    is = AOUtil.loadFile(AOUtil.createURI(SignApplet.this.fileUri));
                }
                catch (final Exception e) {
                    setError(AppletMessages.getString("SignApplet.85")); //$NON-NLS-1$
                    LOGGER.severe(e.toString());
                    return null;
                }
                final byte[] binaryData;
                try {
                	if (SignApplet.this.fileBase64) {
                		binaryData = Base64.decode(new String(AOUtil.getDataFromInputStream(is)));
                	} else {
                		binaryData = AOUtil.getDataFromInputStream(is);
                	}

                }
                catch (final Exception e) {
                    LOGGER.severe("Error durante la lectura del fichero " + SignApplet.this.fileUri + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.407") + SignApplet.this.fileUri); //$NON-NLS-1$
                    return null;
                }
                try {
                    is.close();
                }
                catch (final IOException e) {
                    LOGGER.warning("Error al cerrar el stream de entrada de datos: " + e); //$NON-NLS-1$
                }
                final String digestAlg = AOSignConstants.getDigestAlgorithmName(SignApplet.this.sigAlgo);
                try {
                    return Base64.encode(CryptoUtils.getMessageDigest(binaryData, digestAlg));
                }
                catch (final NoSuchAlgorithmException e) {
                    LOGGER.severe("El algoritmo de hash '" + digestAlg + "' no esta soportado: " + e); //$NON-NLS-1$ //$NON-NLS-2$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.464") + digestAlg); //$NON-NLS-1$
                    return null;
                }
            }
        });
    }

    /** {@inheritDoc} */
    public void setCipherData(final String data) {
        LOGGER.info("Invocando setCipherData"); //$NON-NLS-1$
        this.cipherManager.setCipheredData(data == null ? null : Base64.decode(data));
    }

    /** {@inheritDoc} */
    public void setPlainData(final String data) {
        LOGGER.info("Invocando setPlainData"); //$NON-NLS-1$
        this.cipherManager.setPlainData(data == null ? null : Base64.decode(data));
    }

    /** {@inheritDoc} */
    public String getCipherData() {
        LOGGER.info("Invocando getCipherData"); //$NON-NLS-1$
        return Base64.encode(this.cipherManager.getCipheredData());
    }

    /** {@inheritDoc} */
    public String getPlainData() {
        LOGGER.info("Invocando getPlainData"); //$NON-NLS-1$
        return this.cipherManager.getPlainData() == null ? null : Base64.encode(this.cipherManager.getPlainData());
    }

    /** {@inheritDoc} */
    public String getKey() {
        LOGGER.info("Invocando getKey"); //$NON-NLS-1$
        return Base64.encode(this.cipherManager.getCipherKey());
    }

    /** {@inheritDoc} */
    public void setKey(final String newKey) {
        LOGGER.info("Invocando setKey"); //$NON-NLS-1$
        this.cipherManager.setCipherKey(newKey == null ? null : Base64.decode(newKey));
    }

    /** {@inheritDoc} */
    public String getPassword() {
        LOGGER.info("Invocando getPassword"); //$NON-NLS-1$
        return this.cipherManager.getCipherPassword() == null ? null : String.valueOf(this.cipherManager.getCipherPassword());
    }

    /** {@inheritDoc} */
    public boolean setPassword(final String password) {
        LOGGER.info("Invocando setPassword"); //$NON-NLS-1$
        if (!CipherManager.isValidPassword(password)) {
            LOGGER.warning("La contrasena introducida no es una cadena ASCII"); //$NON-NLS-1$
            return false;
        }
        this.cipherManager.setCipherPassword(password.toCharArray());
        return true;
    }

    /** {@inheritDoc} */
    public void setCipherAlgorithm(final String algorithm) {
        LOGGER.info("Invocando setCipherAlgorithm: " + algorithm); //$NON-NLS-1$

        final AOCipherAlgorithm algo = AOCipherAlgorithm.getValueOf(algorithm);
        if (algo == null) {
            LOGGER.warning("Algoritmo de cifrado no reconocido, se establecera el por defecto: " + //$NON-NLS-1$
                           AOCipherAlgorithm.getDefault().getName());
        }
        this.cipherManager.setCipherAlgorithm(algo);
    }

    /** {@inheritDoc} */
    public String getCipherAlgorithm() {
        LOGGER.info("Invocando getCipherAlgorithm"); //$NON-NLS-1$
        return this.cipherManager.getCipherAlgorithm().getName();
    }

    /** {@inheritDoc} */
    public void setKeyMode(final String keyMode) {
        LOGGER.info("Invocando setKeyMode: " + keyMode); //$NON-NLS-1$
        this.cipherManager.setKeyMode(keyMode);
    }

    /** {@inheritDoc} */
    public String getKeyMode() {
        LOGGER.info("Invocando getKeyMode"); //$NON-NLS-1$
        return this.cipherManager.getKeyMode();
    }

    /** {@inheritDoc} */
    public boolean savePlainDataToFile(final String filename) {
        LOGGER.info("Invocando savePlainDataToFile: " + filename); //$NON-NLS-1$

        if (this.cipherManager.getPlainData() == null) {
            LOGGER.severe("No hay datos en claro que guardar"); //$NON-NLS-1$
            SignApplet.this.setError(AppletMessages.getString("SignApplet.394")); //$NON-NLS-1$
            return false;
        }
        if (filename == null) {
            LOGGER.severe("El fichero de salida para los datos no puede ser nulo"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.396")); //$NON-NLS-1$
            return false;
        }

        if (!checkUserPermision(AppletMessages.getString("SignApplet.66") + CR + filename + //$NON-NLS-1$
        		CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
        	return false;
        }

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {
                try {
                    SignApplet.saveDataToStorage(SignApplet.this.cipherManager.getPlainData(), filename);
                }
                catch (final Exception e) {
                    LOGGER.severe("No se pudo almacenar el texto plano (establecido o cifrado) en " + filename + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.392") + filename); //$NON-NLS-1$
                    return Boolean.FALSE;
                }
                return Boolean.TRUE;
            }
        }).booleanValue();
    }

    /** {@inheritDoc} */
    public boolean saveCipherDataToFile(final String filename) {
        LOGGER.info("Invocando saveCipherDataToFile: " + filename); //$NON-NLS-1$

        if (this.cipherManager.getCipheredData() == null) {
            LOGGER.severe("No hay datos cifrados que guardar"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.395")); //$NON-NLS-1$
            return false;
        }

        if (filename == null) {
            LOGGER.severe("El fichero de salida para los datos no puede ser nulo"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.396")); //$NON-NLS-1$
            return false;
        }

        if (!checkUserPermision(AppletMessages.getString("SignApplet.71") + CR + filename + //$NON-NLS-1$
                                                  CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
            return false;
        }

        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {
                try {
                    SignApplet.saveDataToStorage(SignApplet.this.cipherManager.getCipheredData(), filename);
                }
                catch (final Exception e) {
                    LOGGER.severe("No se pudo almacenar el texto cifrado en" + filename + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.397") + filename); //$NON-NLS-1$
                    return Boolean.FALSE;
                }
                return Boolean.TRUE;
            }
        }).booleanValue();
    }

    /** {@inheritDoc} */
    public void setUseCipherKeyStore(final boolean useKeyStore) {
        LOGGER.info("Invocando setUseCipherKeyStore con el valor: " + useKeyStore); //$NON-NLS-1$
        this.cipherManager.setUseCipherKeyStore(useKeyStore);
    }

    /** {@inheritDoc} */
    public boolean cipherFile(final String filename) {
        LOGGER.info("Invocando cipherFile: " + filename); //$NON-NLS-1$

        if (filename == null || "".equals(filename)) { //$NON-NLS-1$
            setError(AppletMessages.getString("SignApplet.78")); //$NON-NLS-1$
            return false;
        }

        if (!checkUserPermision(AppletMessages.getString("SignApplet.79") + CR + filename + //$NON-NLS-1$
        		CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
            return false;
        }

        this.setPlainData(null);

        final byte[] dat = FileUtils.loadFile(filename);
        if (dat == null) {
            setError(AppletMessages.getString("SignApplet.401")); //$NON-NLS-1$
            return false;
        }

        return cipherData(dat);
    }

    /** {@inheritDoc} */
    public boolean cipherData() {
        LOGGER.info("Invocando cipherData"); //$NON-NLS-1$
        return cipherData(null);
    }

    /** Cifra los datos introducido o los configurados en la instancia actual de
     * cipherManager.
     * @param dat
     *        Datos que deseamos cifrar.
     * @return Devuelve {@code true} si la operaci&oacute; finaliz&oacute;
     *         correctamente.
     * @deprecated Utilizar los m&eacute;todos {@link SignApplet#setPlainData(String)} para
     * indicar los datos en Base64 y {@link SignApplet#cipherData()} para ejecutar la
     * operaci&oacute;n.
     */
    @Deprecated
    public boolean cipherData(final byte[] dat) {

        // El resultado queda almacenado en el objeto CipherManager
        try {
        	AccessController.doPrivileged(new CipherAction(this.cipherManager, dat));
        } catch (final PrivilegedActionException e) {
        	if (e.getCause() instanceof AOCancelledOperationException) {
        		setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
        		LOGGER.severe("Error: " + e.toString()); //$NON-NLS-1$
        		return false;
        	}
        	setError(AppletMessages.getString("SignApplet.93")); //$NON-NLS-1$
        	LOGGER.severe("Error: " + e.toString()); //$NON-NLS-1$
        	return false;
        }
        return true;
    }

    /** {@inheritDoc} */
    public boolean decipherFile(final String filename) {
        LOGGER.info("Invocando decipherFile: " + filename); //$NON-NLS-1$

        if (filename == null || "".equals(filename)) { //$NON-NLS-1$
            setError(AppletMessages.getString("SignApplet.87")); //$NON-NLS-1$
            return false;
        }

        if (!checkUserPermision(AppletMessages.getString("SignApplet.88") + CR + filename + //$NON-NLS-1$
        		CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
            return false;
        }

        this.setCipherData(null);

        final byte[] cipheredData = FileUtils.loadFile(filename);
        if (cipheredData == null) {
            setError(AppletMessages.getString("SignApplet.402")); //$NON-NLS-1$
            return false;
        }

        return decipherData(cipheredData);
    }

    /** {@inheritDoc} */
    public boolean decipherData() {
        LOGGER.info("Invocando decipherData"); //$NON-NLS-1$
        return decipherData(null);
    }

    /** Descifra los datos introducido o los configurados en la instancia actual
     * de cipherManager.
     * @param dat
     *        Datos que deseamos descifrar.
     * @return Devuelve {@code true} si la operaci&oacute; finaliz&oacute;
     *         correctamente.
     * @deprecated Utilizar los m&eacute;todos {@link #setCipherData(String)} para indicar los
     * datos en Base64 y {@link SignApplet#decipherData()} para ejecutar la operaci&oacute;n.
     */
    @Deprecated
    public boolean decipherData(final byte[] dat) {

        // El resultado quedara almacenado en el objeto CipherManager
    	try {
    		AccessController.doPrivileged(new DecipherAction(this.cipherManager, dat));
    	} catch (final PrivilegedActionException e) {
    		if (e.getCause() instanceof AOCancelledOperationException) {
    			setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
    			return false;
    		} else if (e.getCause() instanceof KeyException) {
    			setError(AppletMessages.getString("SignApplet.111")); //$NON-NLS-1$
    			return false;
    		} else {
    			setError(AppletMessages.getString("SignApplet.92")); //$NON-NLS-1$
    			return false;
    		}
    	}
        return true;
    }

    /** {@inheritDoc} */
    public String showCertSelectionDialog() {
        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {
                // Configuramos el certificado
                final PrivateKeyEntry ke = SignApplet.this.configureCertificate();
                if (ke == null) {
                    return null;
                }
                return SignApplet.this.ksConfigManager.getSelectedAlias();
            }
        });
    }

    /** {@inheritDoc} */
    public void setRecipientsToCMS(final String s) {
        LOGGER.info("Invocando setRecipientsToCMS: " + s); //$NON-NLS-1$
        this.setError(null);

        if (s == null || s.equals("")) { //$NON-NLS-1$
            this.enveloperManager.removeAllRecipients();
            LOGGER.info("Se han eliminado los destinatarios configurados para el sobre electronico"); //$NON-NLS-1$
            return;
        }

        for (final String recipientsCertFile : s.split(CR)) {

            final byte[] recipientsCert = FileUtils.loadFile(recipientsCertFile);
            if (recipientsCert == null) {
                this.setError(AppletMessages.getString("SignApplet.468", recipientsCertFile)); //$NON-NLS-1$
                continue;
            }
            this.addRecipientToCMS(recipientsCert);
        }
    }

    /** {@inheritDoc} */
    public void addRecipientToCMS(final String certB64) {
        LOGGER.info("Invocando addRecipientToCMS"); //$NON-NLS-1$
        if (certB64 == null || certB64.length() == 0) {
            LOGGER.warning("No se han introducido destinatarios"); //$NON-NLS-1$
        }
        addRecipientToCMS(Base64.decode(certB64));
    }

    /** Configura un nuevo destinatario para el sobre electr&oacute;nico por
     * medio de su certificado.
     * @param cert
     *        Certificado del destinatario. */
    private void addRecipientToCMS(final byte[] cert) {
        try {
            this.enveloperManager.addRecipient(cert);
        }
        catch (final CertificateException e) {
            LOGGER.severe("Error al decodificar el certificado de destinatario, asegurese de que es un certificado valido: " + e); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.10")); //$NON-NLS-1$
        }
        catch (final Exception e) {
            LOGGER.severe("Error al cargar el certificado de destinatario: " + e); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.572")); //$NON-NLS-1$
        }
    }

    /** {@inheritDoc} */
    public void removeRecipientToCMS(final String certB64) {

        if (certB64 == null || certB64.length() == 0) {
            throw new IllegalArgumentException("No se ha introducido el certificado que se desea eliminar"); //$NON-NLS-1$
        }

        try {
            this.enveloperManager.removeRecipient(Base64.decode(certB64));
        }
        catch (final CertificateException e) {
            LOGGER.severe("Error al decodificar el certificado de destinatario, asegurese de que es un certificado valido: " + e); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.10")); //$NON-NLS-1$
        }
        catch (final Exception e) {
            LOGGER.severe("Error al eliminar el certificado de la lista de destinatarios: " + e); //$NON-NLS-1$
            SignApplet.this.setError(AppletMessages.getString("SignApplet.572")); //$NON-NLS-1$
        }
    }

    /** {@inheritDoc} */
    public void setLdapConfiguration(final String address, final String port, final String root) {
        LOGGER.info("Invocando setLdapConfiguration"); //$NON-NLS-1$
        if (address == null) {
            throw new IllegalArgumentException("No se ha indicado la URL del directorio LDAP"); //$NON-NLS-1$
        }

        // Si no se indica el puerto se toma el por defecto
        if (port == null) {
            LOGGER.warning("No se ha indicado el puerto para la configuracion del LDAP, se utilizara el puerto " + DEFAULT_LDAP_PORT); //$NON-NLS-1$
            this.ldapServerPort = DEFAULT_LDAP_PORT;
        }
        else {
	        try {
	            this.ldapServerPort = Integer.parseInt(port);
	            if (this.ldapServerPort < 1 || this.ldapServerPort > 65535) {
	                throw new IllegalArgumentException("Numero de puerto no valido, el numero de puerto debe estar entre 1 y 65535"); //$NON-NLS-1$
	            }
	        }
	        catch (final Exception e) {
	            LOGGER.warning("No se ha insertado un numero de puerto valido para el LDAP, se usara el puerto " + DEFAULT_LDAP_PORT + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
	            this.ldapServerPort = DEFAULT_LDAP_PORT;
	        }
        }

        this.ldapServerUrl = ""; //$NON-NLS-1$
        this.ldapServerUrl += address;
        // ldapRootPath = (root == null || root.trim().length() == 0) ? null :
        // root;
    }

    /** {@inheritDoc} */
    public void setLdapCertificatePrincipal(final String ldapCertificatePrincipal) {
        LOGGER.info("Invocando setLdapCertificatePrincipal con el parametro: " + ldapCertificatePrincipal); //$NON-NLS-1$
        this.ldapCertificatePrincipal = (ldapCertificatePrincipal != null && ldapCertificatePrincipal.length() > 0 ? ldapCertificatePrincipal : null);
    }

    /** {@inheritDoc} */
    public String getLdapCertificate() {
        LOGGER.info("Invocando getLdapCertificate()"); //$NON-NLS-1$
        return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
            public String run() {

                final X509Certificate cert;

                // Si se ha establecido la direccion LDAP del
                // certificado, se descarga; si no
                // se muestra un navegador LDAP para seleccionarlo y
                // descargarlo
                if (SignApplet.this.ldapCertificatePrincipal != null) {
                    try {
                        cert =
                                LdapUtils.getCertificate(SignApplet.this.ldapServerUrl,
                                                         SignApplet.this.ldapServerPort,
                                                         SignApplet.this.ldapCertificatePrincipal);
                    }
                    catch (final Exception e) {
                        LOGGER.severe("Error al recuperar el certificado '" + SignApplet.this.ldapCertificatePrincipal + "' del directorio LDAP: " + e); //$NON-NLS-1$ //$NON-NLS-2$
                        setError(AppletMessages.getString("SignApplet.74") + SignApplet.this.ldapCertificatePrincipal); //$NON-NLS-1$
                        return null;
                    }
                }

                else {
                    LOGGER.severe("No se especifico el Principal del certificado que se desea seleccionar"); //$NON-NLS-1$
                    return null;
                }

                // Devolvemos el certificado codificado en Base64
                try {
                    return Base64.encode(cert.getEncoded());
                }
                catch (final Exception e) {
                    LOGGER.severe("Error al codificar el certificado recuperado del directorio LDAP : " + e); //$NON-NLS-1$
                    setError(AppletMessages.getString("SignApplet.83")); //$NON-NLS-1$
                    return null;
                }
            }
        });
    }

    /** {@inheritDoc} */
    public void setCMSContentType(final String contentType) {
        LOGGER.info("Invocando setCMSContentType: " + contentType); //$NON-NLS-1$
        this.enveloperManager.setCmsContentType(contentType);
    }

    /** {@inheritDoc} */
    public boolean buildCMSEncrypted() {
        LOGGER.info("Invocando buildCMSEncrypted"); //$NON-NLS-1$
        return this.doEnvelopOperation(null, AOSignConstants.CMS_CONTENTTYPE_ENCRYPTEDDATA);
    }

    /** {@inheritDoc} */
    public boolean buildCMSEnveloped() {
        LOGGER.info("Invocando buildCMSEnveloped"); //$NON-NLS-1$
        return this.doEnvelopOperation(null, AOSignConstants.CMS_CONTENTTYPE_ENVELOPEDDATA);
    }

    /** {@inheritDoc} */
    public boolean buildCMSAuthenticated() {
        LOGGER.info("Invocando buildCMSAuthenticated"); //$NON-NLS-1$
        return this.doEnvelopOperation(null, AOSignConstants.CMS_CONTENTTYPE_AUTHENVELOPEDDATA);
    }

    /** {@inheritDoc} */
    public boolean buildCMSStructure() {
        LOGGER.info("Invocando buildCMSStructure"); //$NON-NLS-1$
        return this.doEnvelopOperation(null, null);
    }

    /** {@inheritDoc} */
    public boolean signAndPackData() {
        LOGGER.info("Invocando signAndPackData"); //$NON-NLS-1$
        return this.doEnvelopOperation(null, AOSignConstants.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA);
    }

    /** {@inheritDoc} */
    public boolean signAndPackFile(final String filename) {
        LOGGER.info("Invocando signAndPackFile: " + filename); //$NON-NLS-1$

        if (filename == null || "".equals(filename)) { //$NON-NLS-1$
            setError(AppletMessages.getString("SignApplet.91")); //$NON-NLS-1$
            return false;
        }

        if (!checkUserPermision(AppletMessages.getString("SignApplet.94") + CR + filename + //$NON-NLS-1$
        		CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
            return false;
        }

        final byte[] plainData = FileUtils.loadFile(filename);
        if (plainData == null) {
            setError(AppletMessages.getString("SignApplet.403")); //$NON-NLS-1$
            return false;
        }

        return doEnvelopOperation(plainData, AOSignConstants.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA);
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
            this.enveloperManager.setCmsContentType(contentType);
        }
        if (this.enveloperManager.getCmsContentType() == null) {
        	this.enveloperManager.setCmsContentType(AOSignConstants.CMS_CONTENTTYPE_ENVELOPEDDATA);
        }

        // Le pasamos la configuracion de almacenes y cifrado establecidas
        this.enveloperManager.setKsConfigManager(this.ksConfigManager);
        this.enveloperManager.setCipherManager(this.cipherManager);

        try {
        	this.data = AccessController.doPrivileged(new WrapAction(this.enveloperManager, contentData));
        } catch (final PrivilegedActionException e) {
        	if (e.getCause() instanceof AOCancelledOperationException) {
        		setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
        		LOGGER.severe(e.toString());
        		return false;
        	}
        	setError(AppletMessages.getString("SignApplet.65")); //$NON-NLS-1$
        	LOGGER.severe(e.toString());
        	return false;
        }
        return true;
    }

    /** {@inheritDoc} */
    public boolean coEnvelop() {
        LOGGER.info("Invocando coEnvelop"); //$NON-NLS-1$

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

        this.enveloperManager.setKsConfigManager(this.ksConfigManager);
        this.enveloperManager.setCipherManager(this.cipherManager);

        try {
        	this.data = AccessController.doPrivileged(new CoEnvelopAction(this.enveloperManager, envelop));
        } catch (final PrivilegedActionException e) {
        	if (e.getCause() instanceof AOCancelledOperationException) {
        		setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
        		LOGGER.severe(e.toString());
        		return false;
        	}
        	setError(AppletMessages.getString("SignApplet.0")); //$NON-NLS-1$
        	LOGGER.severe(e.toString());
        	return false;
        }
        return true;
    }

    /** {@inheritDoc} */
    public boolean recoverCMS() {
        LOGGER.info("Invocando recoverCMS"); //$NON-NLS-1$

        // Reiniciamos el mensaje de error
        this.setError(null);

        final byte[] contentData;
        try {
            contentData = (this.data != null ? this.data : getInData());
        }
        catch (final AOException e) {
            return false; // getInData() establece el mensaje en caso de error
        }

        this.enveloperManager.setKsConfigManager(this.ksConfigManager);
        this.enveloperManager.setCipherManager(this.cipherManager);

        try {
        	this.data = AccessController.doPrivileged(new UnwrapAction(this.enveloperManager, contentData));
        } catch (final PrivilegedActionException e) {
        	if (e.getCause() instanceof AOCancelledOperationException) {
        		setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
        		LOGGER.severe(e.toString());
        		return false;
        	} else if (e.getCause() instanceof AOInvalidRecipientException) {
        		setError(AppletMessages.getString("SignApplet.112")); //$NON-NLS-1$
        		LOGGER.severe(e.toString());
        		return false;
        	} else if (e.getCause() instanceof AOInvalidFormatException) {
        		setError(AppletMessages.getString("SignApplet.75")); //$NON-NLS-1$
        		LOGGER.severe(e.toString());
        		return false;
        	} else {
        		setError(AppletMessages.getString("SignApplet.77")); //$NON-NLS-1$
        		LOGGER.severe(e.toString());
        		return false;
        	}
        }
        return true;
    }

    /** {@inheritDoc} */
    public String formatEncryptedCMS(final String b64) {
        LOGGER.info("Invocando formatEncryptedCMS"); //$NON-NLS-1$
        return SignApplet.getCMSInfo(b64);
    }

    /** {@inheritDoc} */
    public String formatEnvelopedCMS(final String b64) {
        LOGGER.info("Invocando formatEnvelopedCMS"); //$NON-NLS-1$
        return SignApplet.getCMSInfo(b64);
    }

    /** Recupera la informaci&oacute;n de un objeto CMS reconocido. Si ocurre un
     * error durante el proceso, se devuelve cadena vac&iacute;a.
     * @param b64
     *        Objeto CMS en base 64.
     * @return Informaci&oacute;n del objeto CMS introducido. */
    private static String getCMSInfo(final String b64) {
        return EnveloperManager.getCMSInfo(Base64.decode(b64));
    }

    /** {@inheritDoc} */
    public void setDataMimeType(final String mimetype) {
        LOGGER.info("Invocando setDataMimeType: " + mimetype); //$NON-NLS-1$
        if (mimetype == null || mimetype.length() == 0) {
            this.extMimeType = MimeHelper.DEFAULT_MIMETYPE;
        }
        this.extMimeType = mimetype;
    }

    /** {@inheritDoc} */
    public String getB64Data() {
        LOGGER.info("Invocando getB64Data"); //$NON-NLS-1$
        if (this.data == null) {
            LOGGER.warning("No se dispone de datos de salida, se devolvera cadena vacia"); //$NON-NLS-1$
            return "";  //$NON-NLS-1$
        }
        return Base64.encode(this.data);
    }

    /** {@inheritDoc} */
    public String getData() {
        LOGGER.info("Invocando getData"); //$NON-NLS-1$
        if (this.data == null) {
            LOGGER.warning("No se dispone de datos de salida, se devolvera cadena vacia"); //$NON-NLS-1$
        }
        return (this.data == null ? "" : new String(this.data)); //$NON-NLS-1$
    }

    /** {@inheritDoc} */
    public void setInRecursiveDirectorySign(final boolean recursiveSignDir) {
        LOGGER.info("Invocando setInRecursiveDirectorySign: " + recursiveSignDir); //$NON-NLS-1$
        this.recursiveSignDir = recursiveSignDir;
    }

    /** {@inheritDoc} */
    public void setInputDirectoryToSign(final String directory) {
        LOGGER.info("Invocando setInputDirectoryToSign: " + directory); //$NON-NLS-1$

        if ((directory != null) && !checkUserPermision(
        		AppletMessages.getString("SignApplet.103") + CR + directory  + //$NON-NLS-1$
        		CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
            return;
        }

        this.massiveInputDirectory = directory;
    }

    /** {@inheritDoc} */
    public String getInputDirectoryToSign() {
        LOGGER.info("Invocando getInputDirectoryToSign"); //$NON-NLS-1$
        return this.massiveInputDirectory;
    }

    /** {@inheritDoc} */
    public void setOutputDirectoryToSign(final String directory) {
        LOGGER.info("Invocando setOutputDirectoryToSign: " + directory); //$NON-NLS-1$
        if ((directory != null) && !checkUserPermision(
        		AppletMessages.getString("SignApplet.88") + CR + directory + //$NON-NLS-1$
        		CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
        	return;
        }
        this.massiveOutputDirectory = directory;
    }

    /** {@inheritDoc} */
    public String getOutputDirectoryToSign() {
        LOGGER.info("Invocando getOutputDirectoryToSign"); //$NON-NLS-1$
        return this.massiveOutputDirectory;
    }

    /** {@inheritDoc} */
    public void setOriginalFormat(final boolean originalFormat) {
        LOGGER.info("Invocando setOriginalFormat: " + originalFormat); //$NON-NLS-1$
        this.originalFormat = originalFormat;
    }

    // *************************************************************************
    // ******************** METODOS PUBLICOS DEPRECADOS ************************
    // *************************************************************************
    /** {@inheritDoc} */
    public void setShowErrors(final boolean showErrors) {
        LOGGER.info("Invocando setShowErrors: " + showErrors); //$NON-NLS-1$
        this.showErrors = showErrors;
    }

    /** {@inheritDoc} */
    @Deprecated
    public void setShowHashMessage(final boolean showHashMessage) {
        LOGGER.info("Invocando setShowHashMessage: " + showHashMessage);//$NON-NLS-1$
        this.showHashes = showHashMessage;
    }

    /** {@inheritDoc} */
    public void setShowExpiratedCertificates(final boolean showExpiratedCerts) {
        LOGGER.info("Invocando setShowExpiratedCertificates: " + showExpiratedCerts);//$NON-NLS-1$
        this.ksConfigManager.setShowExpiratedCertificates(showExpiratedCerts);
    }

    /** Muestra un di&aacute;logo pidiendo confirmaci&oacute;n para la firma de
     * los datos representados por un hash en base 64. La entrada de datos para
     * la operaci&oacute;n de firma, cofirma o contrafirma debe ser
     * v&aacute;lida.
     * @return <code>true</code> cuando se confirma la operaci&oacute;n <code>false</code> en caso contrario. */
    boolean showHashMessage() {
        final String digestAlgo = AOSignConstants.getDigestAlgorithmName(this.sigAlgo);
        final String hashData = this.getHexDigestData(digestAlgo);

        // Mostramos el mensaje informando del hash de los datos o, en caso de
        // no haber podido calcularlo,
        // lo informamos.
        final int result =
                JOptionPane.showConfirmDialog(this,
                                              (hashData != null ? AppletMessages.getString("SignApplet.655") + CR + digestAlgo + ": " + hashData : //$NON-NLS-1$ //$NON-NLS-2$
                                                               AppletMessages.getString("SignApplet.657")), //$NON-NLS-1$
                                              AppletMessages.getString("SignApplet.658"), //$NON-NLS-1$
                                              JOptionPane.OK_CANCEL_OPTION,
                                              JOptionPane.WARNING_MESSAGE);
        return result == JOptionPane.OK_OPTION;
    }

    /** {@inheritDoc} */
    public boolean addSignedAttribute(final String oid, final String value) {
        LOGGER.warning("Invocando addSignedAttribute: " + oid + " = " + value); //$NON-NLS-1$ //$NON-NLS-2$

        // Comprobaciones de seguridad
        if (oid == null || value == null) {
            LOGGER.severe("Ni el OID ni el valor del atributo firmado a agregar pueden ser nulos"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.690")); //$NON-NLS-1$
            return false;
        }

        // Creamos primeramente el listado de atributos si no lo esta ya
        if (this.signedAttributes == null) {
            this.signedAttributes = new HashMap<org.ietf.jgss.Oid, String>();
        }

        // Comprobamos que OID se valido
        final org.ietf.jgss.Oid newOid;
        try {
            newOid = new org.ietf.jgss.Oid(oid);
        }
        catch (final Exception e) {
            LOGGER.severe("El OID especificado no tiene un formato de OID valido: " + e); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.693")); //$NON-NLS-1$
            return false;
        }

        // Comprobamos que el OID no estuviese ya agregado
        if (this.signedAttributes.containsKey(newOid)) {
            LOGGER.warning("Ya existia un atributo con el OID especificado, se sustituira su valor por el nuevo"); //$NON-NLS-1$
        }

        // Agregamos el nuevo atributo
        this.signedAttributes.put(newOid, value);

        return true;
    }

    /** {@inheritDoc} */
    public boolean removeSignedAttribute(final String oid) {
        LOGGER.warning("Invocando removeSignedAttribute: " + oid); //$NON-NLS-1$

        // Comprobamos que el oid no sea nulo
        if (oid == null) {
            LOGGER.severe("El OID del atributo firmado que se desea eliminar no puede ser nulo"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.698")); //$NON-NLS-1$
            return false;
        }

        // Comprobamos la validez del OID a eliminar
        final org.ietf.jgss.Oid oidToRemove;
        try {
            oidToRemove = new org.ietf.jgss.Oid(oid);
        }
        catch (final Exception e) {
            LOGGER.severe("El OID especificado no tiene un formato valido: " + e); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.693")); //$NON-NLS-1$
            return false;
        }

        // Comprobamos que el atributo exista
        if (this.signedAttributes == null || !this.signedAttributes.containsKey(oidToRemove)) {
            LOGGER.severe("El OID especificado no se agrego previamente a la firma"); //$NON-NLS-1$
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

    /** {@inheritDoc} */
    public boolean addUnsignedAttribute(final String oid, final String value) {
        LOGGER.warning("Invocando addUnsignedAttribute: " + oid + " = " + value); //$NON-NLS-1$ //$NON-NLS-2$

        // Comprobaciones de seguridad
        if (oid == null || value == null) {
            LOGGER.severe("Ni el OID ni el valor del atributo no firmado a agregar pueden ser nulos"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.690")); //$NON-NLS-1$
            return false;
        }

        // Creamos primeramente el listado de atributos si no lo esta ya
        if (this.unsignedAttributes == null) {
            this.unsignedAttributes = new HashMap<org.ietf.jgss.Oid, Vector<String>>();
        }

        // Comprobamos que OID se valido
        org.ietf.jgss.Oid newOid = null;
        try {
            newOid = new org.ietf.jgss.Oid(oid);
        }
        catch (final Exception e) {
            LOGGER.severe("El OID especificado no tiene un formato valido: " + e); //$NON-NLS-1$
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

    /** {@inheritDoc} */
    public boolean removeUnsignedAttribute(final String oid, final String value) {
        LOGGER.warning("Invocando removeUnsignedAttribute: " + oid); //$NON-NLS-1$

        // Comprobamos que el oid no sea nulo
        if (oid == null) {
            LOGGER.severe("El OID del atributo no firmado que se desea eliminar no puede ser nulo"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.698")); //$NON-NLS-1$
            return false;
        }

        // Comprobamos la validez del OID a eliminar
        org.ietf.jgss.Oid oidToRemove = null;
        try {
            oidToRemove = new org.ietf.jgss.Oid(oid);
        }
        catch (final Exception e) {
            LOGGER.severe("El OID especificado no tiene un formato valido: " + e); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.693")); //$NON-NLS-1$
            return false;
        }

        // Comprobamos que el atributo exista y si tiene mas valores asignados
        // para eliminar lo que corresponda
        if (this.unsignedAttributes != null && this.unsignedAttributes.containsKey(oidToRemove)) {
            final Vector<String> values = this.unsignedAttributes.get(oidToRemove);
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
        if (this.unsignedAttributes != null && this.unsignedAttributes.isEmpty()) {
            this.unsignedAttributes = null;
        }

        return true;
    }

    /** {@inheritDoc} */
    public void addExtraParam(final String key, final String value) {
        LOGGER.warning("Invocando addExtraParam: " + key); //$NON-NLS-1$

        // Si no se ha indicado una clave o valor, abortamos la operacion
        if (key == null || value == null) {
            return;
        }

        // Establecemos la propiedad
        this.genericConfig.setProperty(key, value);
    }

    /** {@inheritDoc} */
    public void removeExtraParam(final String key) {
        LOGGER.warning("Invocando removeExtraParam: " + key); //$NON-NLS-1$

        // Si no se ha indicado una clave, abortamos la operacion
        if (key == null) {
            return;
        }

        // Eliminamos la propiedad
        this.genericConfig.remove(key);
    }

    /** {@inheritDoc} */
    public void addXMLTransform(final String type, final String subtype, final String body) {
        if (this.xmlTransforms == null) {
            this.xmlTransforms = new Vector<AOXMLTransform>();
        }

        this.xmlTransforms.add(new AOXMLTransform(type, subtype, body));
    }

    /** {@inheritDoc} */
    public void resetXMLTransforms() {
        this.xmlTransforms = null;
    }

    /** {@inheritDoc} */
    public String getVersion() {

        final Properties p = new Properties();
        try {
            p.load(SignApplet.class.getResourceAsStream("/version.properties")); //$NON-NLS-1$
        }
        catch (final Exception e) {
            LOGGER.warning("No se han podido obtener los datos de version del cliente de firma"); //$NON-NLS-1$
        }
        final StringBuilder version = new StringBuilder();
        version.append(p.getProperty("version.mayor", "0")).append(".") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
               .append(p.getProperty("version.minor", "0")).append(".") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
               .append(p.getProperty("version.build", "0")); //$NON-NLS-1$ //$NON-NLS-2$

        final String desc = p.getProperty("build"); //$NON-NLS-1$
        if (desc != null && !desc.trim().equals("")) { //$NON-NLS-1$
            version.append(" ").append(desc); //$NON-NLS-1$
        }
        return version.toString();
    }

    // *************************************************************************
    // **************** Metodos de ayuda a los integradores ********************
    // *************************************************************************
    /** {@inheritDoc} */
    public String loadFilePath(final String title, final String exts, final String description) {
        LOGGER.info("Invocando loadFilePath"); //$NON-NLS-1$
        try {
            return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
                public String run() {
                    return AOUIFactory.getLoadFileName(title,
                                                       ((exts == null || exts.trim().length() == 0)
                                                                                                   ? null
                                                                                                   : AOUtil.split(exts, SignApplet.STRING_SEPARATOR)),
                                                       description,
                                                       SignApplet.this);
                }
            });
        }
        catch (final AOCancelledOperationException e) {
            LOGGER.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
            return null;
        }
        catch (final Exception e) {
            LOGGER.warning("Error al seleccionar el fichero: " + e); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.233")); //$NON-NLS-1$
            return null;
        }
    }

    /** {@inheritDoc} */
    public String selectDirectory() {
        LOGGER.info("Invocando selectDirectory"); //$NON-NLS-1$
        try {
            return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
                public String run() {
                    return UIDialogs.selectDirectory(SignApplet.this, AppletMessages.getString("SignApplet.104")); //$NON-NLS-1$
                }
            });
        }
        catch (final AOCancelledOperationException e) {
            LOGGER.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
            return null;
        }
        catch (final Exception e) {
            LOGGER.warning("Error al seleccionar el directorio: " + e); //$NON-NLS-1$
            this.setError(AppletMessages.getString("SignApplet.333")); //$NON-NLS-1$
            return null;
        }
    }

    /** {@inheritDoc} */
    public X509Certificate getSignCertificate() {
        LOGGER.info("Invocando getSignCertificate"); //$NON-NLS-1$
        X509Certificate cert = null;
		try {
			cert = this.ksConfigManager.getSelectedCertificate();
		} catch (final AOKeyStoreManagerException e) {
            LOGGER.warning("No se ha inicializado el almacen de certificados: " + e); //$NON-NLS-1$
			return null;
		} catch (final AOKeystoreAlternativeException e) {
            LOGGER.warning("Error al acceder al almacen de certificados: " + e); //$NON-NLS-1$
			return null;
		}
        if (cert == null) {
            LOGGER.warning("No se dispone del certificado de firma, se devolvera nulo"); //$NON-NLS-1$
        }

        return cert;
    }

    // *************************************************************************
    // *************************************************************************

    /** Recupera el frame del que cuelga un componente.
     * @return Frame padre del componente. */
    private Frame getParentFrame(final Component c) {
        if (c == null) {
            return null;
        }
        final Container cont = c.getParent();
        if (cont instanceof Frame) {
            return (Frame) cont;
        }
        return getParentFrame(cont);
    }

    /** Toma un array de cadenas y las concatena separ&aacute;ndolas con un
     * delimitador.
     * @return Cadena concatenada. */
    private static String concatStrings(final String[] strings, final String delimitator) {
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < strings.length; i++) {
            if (i > 0) {
                sb.append(delimitator);
            }
            sb.append(strings[i]);
        }
        return sb.toString();
    }

    /** {@inheritDoc} */
    @Deprecated
    public void setCertFilterRFC2254(final String subjectFilter, final String issuerFilter, final boolean onlySignatureCertificates) {
        LOGGER.info("Invocando setCertFilterRFC2254"); //$NON-NLS-1$
        this.addRFC2254CertificateFilter(subjectFilter, issuerFilter, onlySignatureCertificates);
        this.ksConfigManager.setMandatoryCert(false);
    }

    /** {@inheritDoc} */
    @Deprecated
    public void setMandatoryCertificateConditionRFC2254(final String subjectFilter, final String issuerFilter, final boolean onlySignatureCertificates) {
        LOGGER.info("Invocando setMandatoryCertificateConditionRFC2254"); //$NON-NLS-1$
        this.addRFC2254CertificateFilter(subjectFilter, issuerFilter, onlySignatureCertificates);

        // Si se establecio alguno de los tres filtros, activamos la seleccion
        // de un unico certificado
        this.ksConfigManager.setMandatoryCert(subjectFilter != null || issuerFilter != null || onlySignatureCertificates);
    }

    /** Agrega un filtro de certificados acorde a la RFC2254 para el Subject, el
     * Issuer y seg&uacute;n los KeyUsage de los certificados.
     * @param subjectFilter
     *        Filtro para el titular del certificado, seg&uacute;n formato
     *        definido en la normativa RFC 2554 <br>
     *        Filter for Certificate's holder, as defined by RFC 2554.
     * @param issuerFilter
     *        Filtro para el emisor del certificado, seg&uacute;n formato
     *        definido en la normativa RFC 2554 <br>
     *        Filter for the certificate issuer, as defined by RFC 2554.
     * @param signatureCertificate
     *        Si se establece a <code>true</code> se muestran para
     *        selecci&oacute;n &uacute;nicamente los certificados con el bit
     *        <i>nonRepudiation</i> del campo <i>KeyUsage</i> activado, si
     *        se establece a <code>false</code> se muestran todos los
     *        certificados <br>
     *        If set to <code>true</code> only certificates with an active
     *        <i>nonRepudiation</i> bit in field <i>KeyUsage</i> are
     *        displayed for selection, if set to <code>false</code> all
     *        certificates are displayed. */
    public void addRFC2254CertificateFilter(final String subjectFilter, final String issuerFilter, final boolean signatureCertificate) {
    	final CertificateFilter filter;
    	if (subjectFilter != null || issuerFilter != null || signatureCertificate) {
    		if (signatureCertificate) {
    			if (subjectFilter != null || issuerFilter != null) {
    				filter = new MultipleCertificateFilter(new CertificateFilter[] {
    						new RFC2254CertificateFilter(subjectFilter, issuerFilter),
    						new KeyUsageFilter(KeyUsageFilter.SIGN_CERT_USAGE)});
    			}
    			else {
    				filter = new KeyUsageFilter(KeyUsageFilter.SIGN_CERT_USAGE);
    			}
    		} else {
    			filter = new RFC2254CertificateFilter(subjectFilter, issuerFilter);
    		}
    		this.ksConfigManager.addCertFilter(filter);
    	}
    }

    /**
     * Elimina todos los filtros definidos hasta el momento.
     */
    public void resetFilters() {
    	this.ksConfigManager.resetFilters();
    }

    /**
     * Permite indicar si deben o no mostrarse &uacute;nicamente los certificados de firma.
     * Por defecto, s&oacute;lo se mostrar&aacute;n los certificados de firma.
     * @param showOnlySignatureCertificates Indica si se deben mostrar s&oacute;lo los
     * certificados de firma.
     */
    public void setShowOnlySignatureCertificates(final boolean showOnlySignatureCertificates) {
    	this.ksConfigManager.setShowOnlySignatureCertificates(showOnlySignatureCertificates);
    }

    /**
     * Inidica si se debe seleccionar autom&aacute;ticamente un certificado si es el
     * &uacute;nico del almac&eacute;n o el &uacute;nico que pasa los filtros establecidos.
     * @param mandatoryCertificate Selecci&oacute;n autom&aacute;tica del certificado.
     */
    public void setMandatoryCertificate(final boolean mandatoryCertificate) {
    	this.ksConfigManager.setMandatoryCert(mandatoryCertificate);
    }


    /** Selecciona un certificado del usuario y devuelve la referencia a su clave
     * privada. En caso de error configura el mensaje de error correspondiente.
     * @return Referencia a la clave privada del certificado o {@code null} si
     *         ocurri&oacute; alg&uacute;n error. */
    PrivateKeyEntry configureCertificate() {

        if (!this.ksConfigManager.isSelectedCertificate()) {
            try {
                this.ksConfigManager.selectCertificate();
            }
            catch (final AOCancelledOperationException e) {
                LOGGER.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
                this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
            }
            catch (final CertificateException e) {
                LOGGER.severe("Error al seleccionar el certificado del repositorio: " + e); //$NON-NLS-1$
                this.setError(AppletMessages.getString("SignApplet.363")); //$NON-NLS-1$
            }
            catch (final NoSuchAlgorithmException e) {
                LOGGER.info("Error al extraer la clave privada del certificado: " + e); //$NON-NLS-1$
                this.setError(AppletMessages.getString("SignApplet.114")); //$NON-NLS-1$
            }
            catch (final UnrecoverableEntryException e) {
            	LOGGER.info("Error al extraer la clave privada del certificado: " + e); //$NON-NLS-1$
                this.setError(AppletMessages.getString("SignApplet.114")); //$NON-NLS-1$
            }
            catch (final AOKeyStoreManagerException e) {
                LOGGER.severe("Error inicializando el almacen de claves: " + e); //$NON-NLS-1$
                this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
            }
            catch (final KeyStoreException e) {
                LOGGER.severe("Error al gestionar el almacen de claves: " + e); //$NON-NLS-1$
                this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
            }
            catch (final AOCertificatesNotFoundException e) {
                LOGGER.info("No se han encontrado en el almacen certificados compatibles con la aplicacion: " + e); //$NON-NLS-1$
                this.setError(AppletMessages.getString("SignApplet.115")); //$NON-NLS-1$
            }
            catch (final AOKeystoreAlternativeException e) {
                final AOKeyStore kst = e.getAlternativeKsm();
                if (kst == null) {
                    LOGGER.severe("Error inicializando el almacen de claves: " + e); //$NON-NLS-1$
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
                    LOGGER.severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                }
            }
        }
        return this.ksConfigManager.getCertificateKeyEntry();
    }

    /** Recupera el hash en hexadecimal de los datos de entrada del applet para
     * firma.
     * @return Hash en hexadecimal formateado o <code>null</code> si no se
     *         introdujeron o no pudieron leerse los datos. */
    private String getHexDigestData(final String algorithm) {

        byte[] hashData = null;
        // Si estamos firmando datos o si es una firma de hash implicita
        // (contamos con los datos)
        if (this.data != null && (this.hash == null || this.sigMode == AOSignConstants.SIGN_MODE_IMPLICIT)) {
            try {
                hashData = CryptoUtils.getMessageDigest(this.data, algorithm);
            }
            catch (final Exception e) {
                hashData = null;
            }
        }
        // Si estamos firmando un fichero
        else if (this.fileUri != null) {
            try {
                hashData = CryptoUtils.getMessageDigest(AOUtil.getDataFromInputStream(AOUtil.loadFile(AOUtil.createURI(this.fileUri))), algorithm);
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
                hashData = CryptoUtils.getMessageDigest(this.signData, algorithm);
            }
            catch (final Exception e) {
                hashData = null;
            }
        }
        // Comprobamos si se ha introducido la localizacion de la firma
        else if (this.electronicSignatureFile != null) {
            try {
                hashData =
                        CryptoUtils.getMessageDigest(AOUtil.getDataFromInputStream(AOUtil.loadFile(this.electronicSignatureFile)),
                                                      algorithm);
            }
            catch (final Exception e) {
                hashData = null;
            }
        }

        if (hashData == null) {
            LOGGER.severe("No se han indicado o no se han podido leer los datos para el calculo de la huella digital"); //$NON-NLS-1$
            return null;
        }

        return AOUtil.hexify(hashData, ":"); //$NON-NLS-1$
    }

    /**
     * Muestra un di&aacute;logo modal en el que se solicita al usuario permiso para
     * realizar una determinada acci&oacute;n.
     * @param message Mensaje con la petici&oacute;n realizada al usuario
     * @return {@code true} cuando el usuario da su consentimiento, {@code false} en caso contrario.
     */
    private boolean checkUserPermision(final String message) {
    	if (DEBUG) {
    		return true;
    	}

    	return JOptionPane.showConfirmDialog(this,
    			message,
    			AppletMessages.getString("SignApplet.16"), //$NON-NLS-1$
    			JOptionPane.YES_NO_OPTION,
    			JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION;
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
                    LOGGER.severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                    return "error:userCancel"; //$NON-NLS-1$
                }
                catch (final AOKeyStoreManagerException e) {
                    LOGGER.severe("Error inicializando el almacen de claves: " + e); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
                    return "error:internalError"; //$NON-NLS-1$
                }
                catch (final AOKeystoreAlternativeException e) {
                    final AOKeyStore kst = e.getAlternativeKsm();
                    if (kst == null) {
                        LOGGER.severe("Error inicializando el almacen de claves: " + e); //$NON-NLS-1$
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
                    LOGGER.severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
                    SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
                    return "error:userCancel"; //$NON-NLS-1$
                }

                final SignText signTextComponent =
                        new SignText(getArrayCertificatesAlias(),
                                     ksManager,
                                     SignApplet.this,
                                     KeyStoreUtilities.getCertificatePC(SignApplet.this.ksConfigManager.getKeyStore(), SignApplet.this));

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

    /**
     * Cambia el idioma de la aplicaci&oacute;n.
     * @param language Idioma.
     * @param country Pa&iacute;s.
     * @param variant Variante.
     */
    private static void changeLocale(final String locale) {
    	if (locale != null) {
    		final Locale newLocale = parseLocale(locale);
    		if (newLocale != null) {
    			try {
    				Locale.setDefault(newLocale);
    			}
    			catch (final SecurityException e) {
    				LOGGER.warning("No se pudo cambiar el idioma por restricciones de seguridad en el sistema: " + e); //$NON-NLS-1$
    			}
    		}
    	}
    }

    /**
     * Obtiene el locale a partir de la cadena de localizaci&oacute;n indicada.
     * @param locale Cadena de localizaci&oacute;n.
     * @return Localizaci&oacute;n.
     */
    private static Locale parseLocale(final String locale) {
    	if (locale == null) {
    		return null;
    	}

    	try {
    		final int i1 = locale.indexOf('_');
    		if (i1 == -1) {
    			return new Locale(locale);
    		}

    		final String language = locale.substring(0, locale.indexOf('_'));
    		if (i1 == locale.length() - 1) {
    			return new Locale(language);
    		}
    		if (locale.indexOf('_', i1 + 1) == -1) {
    			return new Locale(language, locale.substring(i1 + 1));
    		}

    		final int i2 = locale.indexOf('_', i1 + 1);
    		final String country = locale.substring(i1 + 1, i2);
    		if (i2 == locale.length() - 1) {
    			return new Locale(language, country);
    		}
    		return new Locale(language, country, locale.substring(i2 + 1));

    	} catch (final Exception e) {
    		LOGGER.warning("No se pudo identificar el idioma a partir del locale indicado: " + e); //$NON-NLS-1$
    		return null;
    	}
    }
}

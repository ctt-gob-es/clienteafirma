/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
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
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.charset.Charset;
import java.security.AccessController;
import java.security.KeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.security.UnrecoverableEntryException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.JApplet;
import javax.swing.JOptionPane;
import javax.swing.UIManager;

import org.ietf.jgss.Oid;

import com.dmurph.tracking.AnalyticsConfigData;
import com.dmurph.tracking.JGoogleAnalyticsTracker;
import com.dmurph.tracking.JGoogleAnalyticsTracker.GoogleAnalyticsVersion;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.misc.AOFileUtils;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.core.util.tree.AOTreeUtil;
import es.gob.afirma.crypto.jarverifier.JarSignatureCertExtractor;
import es.gob.afirma.envelopers.cms.AOInvalidRecipientException;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerException;
import es.gob.afirma.keystores.AOKeystoreAlternativeException;
import es.gob.afirma.keystores.filters.CertificateFilter;
import es.gob.afirma.keystores.filters.MultipleCertificateFilter;
import es.gob.afirma.keystores.filters.rfc.KeyUsageFilter;
import es.gob.afirma.keystores.filters.rfc.RFC2254CertificateFilter;
import es.gob.afirma.massive.DirectorySignatureHelper;
import es.gob.afirma.massive.MassiveSignConfiguration;
import es.gob.afirma.massive.MassiveSignatureHelper;
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

	private static final long serialVersionUID = 5692094082535848369L;

	private static final String GOOGLE_ANALYTICS_TRACKING_CODE = "UA-41615516-3"; //$NON-NLS-1$

	/** Estado del  modo DEBUG. Mantener a {@code false} en la compilaci&oacute;n final. */
	private static final boolean DEBUG = false;

	private static final String CR = "\n"; //$NON-NLS-1$

	/** Separador utilizado para separar varios valores consecutivos en una
	 * cadena devuelta por el Applet. */
	public static final String STRING_SEPARATOR = "\u0024%\u0024"; //$NON-NLS-1$

	/** N&uacute;meto de puerto por defecto para la conexi&oacute;n LDAP. */
	private static final int DEFAULT_LDAP_PORT = 389;

	private static final String EXTRAPARAM_KEY_MIMETYPE = "mimeType"; //$NON-NLS-1$

	private static final String APPLET_PARAM_CUSTOM_JAVA_ARGUMENTS = "custom_java_arguments"; //$NON-NLS-1$

	private static final String APPLET_PARAM_LOCALE = "locale"; //$NON-NLS-1$

	private static final String DEFAULT_MESSAGE_DIGEST_ALGORITHM = "SHA1"; //$NON-NLS-1$

	/** Tama&ntilde;o m&aacute;ximo para mostrar al usuario en el nombre de ruta de un fichero.
	 * Si los nombres son m&aacute;s largos se acortan con puntos suspensivos en el medio. */
	private static final int MAX_PATHNAME_LENGTH = 80;

	/** Logger para la visualizaci&oacute;n de los mensajes por consola. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	static Logger getLogger() {
		return LOGGER;
	}

	private String userAgent = null;

	/** Nombre del fichero donde deben guardarse los datos (firmas, datos, etc.). */
	private String outputFile = null;

	String getOutputFile() {
		return this.outputFile;
	}

	/** URI de la fuente de datos. */
	private String fileUri = null;

	String getInternalFileUri() {
		return this.fileUri;
	}

	/** &Uacute;ltima ruta seleccionada por el usuario a trav&eacute;s de los m&eacute;todos
	 * disponibles para ello. Si esta es la ruta utilizada para la entrada de datos con el
	 * m&eacute;todo setFileuri(), no se mostrar&aacute; di&aacute;logo de advertencia por
	 * considerarse segura. */
	private String userSelectedPath = null;

	/** Indica si la propiedad fileUri apunta a un fichero en base 64. */
	private boolean fileBase64 = false;

	boolean isFileBase64() {
		return this.fileBase64;
	}

	/** Hash que se desea firmar. */
	private byte[] hash = null;

	byte[] getHash() {
		return this.hash;
	}

	/** Algoritmo de firma actual. */
	private String sigAlgo = AOSignConstants.DEFAULT_SIGN_ALGO;

	String getSigAlgo() {
		return this.sigAlgo;
	}

	/** Formato de firma actual. */
	private String sigFormat = AOSignConstants.DEFAULT_SIGN_FORMAT;

	String getSigFormat() {
		return this.sigFormat;
	}

	/** Modo de firma actual. */
	private String sigMode = AOSignConstants.DEFAULT_SIGN_MODE;

	String getSigMode() {
		return this.sigMode;
	}

	/** Datos a operar criptogr&aacute;ficamente. */
	private byte[] data = null;

	byte[] getInternalData() {
		return this.data;
	}

	/** Firma electr&oacute;nica, tanto la generada por las distintas operaciones
	 * de firma como la utilizada en las operaciones de firma y cofirma. */
	private byte[] signData = null;

	byte[] getSignData() {
		return this.signData;
	}

	void setSignData(final byte[] data) {
		// Establecemos los datos sin clonarlos ya que este metodo solo se utiliza internamenamente y
		// no se modificara su contenido
		this.signData = data;
	}

	// /** Indica que parte del buffer de datos ya ha sido le&iacute;do y
	// devuelto al usuario. */
	// private int trunkOffset = 0;
	//
	// /** Firma electr&oacute;nica en base 64. */
	// private char[] signDataB64 = null;

	/** Identificador de identificaci&oacute;n de la pol&iacute;tica de firma. */
	private String policyId = null;

	/** Descripcion de la pol&iacute;tica de firma. */
	private String policyDesc = null;

	/** Calificador de la pol&iacute;tica de firma. */
	private URI policyQualifier = null;

	/** Valor de la huella digital de la pol&iacute;tica de firma. */
	private String policyHashB64 = null;

	/** Firmantes o nodos que se desean contrafirmar. */
	private String[] signersToCounterSign = new String[0];

	String[] getSignersToCounterSign() {
		return this.signersToCounterSign;
	}

	/** Tipo de operaci&oacute;n masiva a realizar. Por defecto, multifirma
	 * masiva. */
	private MassiveType massiveOperation = MassiveType.SIGN;

	MassiveType getMassiveOperation() {
		return this.massiveOperation;
	}

	/**
	 * Marca si el proximo es el primer fichero a firmar dentro de una operaci&oacute;n masiva
	 * program&aacute;tica.
	 */
	private boolean firstMassiveFile = true;

	/** Indica si se deben firmar los ficheros de las subcarpetas del directorio
	 * seleccionado durante la operacion de firma masiva. */
	private boolean recursiveSignDir = false;

	boolean isRecursiveSignDir() {
		return this.recursiveSignDir;
	}

	/** Directorio de donde se toman los ficheros a firmar de forma masiva. */
	private String massiveInputDirectory = null;

	String getMassiveInputDirectory() {
		return this.massiveInputDirectory;
	}

	/** Directorio en donde se almacenar&aacute;n las firmas masivas. */
	private String massiveOutputDirectory = null;

	String getMassiveOutputDirectory() {
		return this.massiveOutputDirectory;
	}

	/** Indica si se debe respectar el formato de firma original para la
	 * multifirma masiva. */
	private boolean originalFormat = true;

	boolean isOriginalFormat() {
		return this.originalFormat;
	}

	/** Extensiones por las que debe filtrarse durante la firma masiva. */
	private String[] massiveExtFiltered = null;

	String[] getMassiveExtFiltered() {
		return this.massiveExtFiltered;
	}

	/** Indica si se ha producido alg&uacute;n error durante la &uacute;ltima
	 * operaci&oacute;n. */
	private boolean error = false;

	void setError(final boolean e) {
		this.error = e;
	}

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

	URI getElectronicSignatureFile() {
		return this.electronicSignatureFile;
	}

	void setElectronicSignatureFile(final URI uri) {
		this.electronicSignatureFile = uri;
	}

	/** Listado de atributos firmados que se deben agregar a una firma. */
	private Map<String, String> signedAttributes = null;

	/** Listado de atributos sin firmar que se deben agregar a una firma. */
	private Map<String, List<String>> unsignedAttributes = null;

	/** Listado de propiedades gen&eacute;ricas establecidas para las firmas. */
	private Properties genericConfig = new Properties();

	Properties getGenericConfig() {
		return this.genericConfig;
	}

	/** Transformaciones XML que aplicar a los formatos de firma que las
	 * soporten. */
	private List<AOXMLTransform> xmlTransforms = null;

	/** Gestor del almac&eacute;n de certificados. */
	private KeyStoreConfigurationManager ksConfigManager = null;

	KeyStoreConfigurationManager getKsConfigManager() {
		return this.ksConfigManager;
	}

	/** Indica si debe mostrarse una advertencia para que se inserten los
	 * dispositivos criptogr&aacute;ficos externos (principalmente, tarjetas
	 * inteligentes) cuando el almac&eacute;n de certificados sea Mozilla o un
	 * PKCS#11. */
	private boolean showMozillaSmartCardWarning = false;

	boolean getShowMozillaSmartCardWarning() {
		return this.showMozillaSmartCardWarning;
	}

	/** Manejador de las funcionalidades de cifrado del Cliente. */
	private CipherManager cipherManager = null;

	CipherManager getCipherManager() {
		return this.cipherManager;
	}

	/** Manejador de las funcionalidades de ensobrado del Cliente. */
	private EnveloperManager enveloperManager = null;

	/** URL del servidor LDAP. */
	private String ldapServerUrl = null;

	String getLdapServerUrl() {
		return this.ldapServerUrl;
	}

	/** Puerto del servidor LDAP. Por defecto, 389. */
	private int ldapServerPort = DEFAULT_LDAP_PORT;

	int getLdapServerPort() {
		return this.ldapServerPort;
	}

	/** Principal del certificado que se desea recuperar del servidor LDAP. */
	private String ldapCertificatePrincipal = null;

	String getLdapCertificatePrincipal() {
		return this.ldapCertificatePrincipal;
	}

	/** Construye la clase asegurandose de que se inicializan todas las
	 * propiedades necesarias. */
	public SignApplet() {
		this.ksConfigManager = new KeyStoreConfigurationManager(AOKeyStore.PKCS12, this);
		this.cipherManager = new CipherManager(this);
		this.enveloperManager = new EnveloperManager(this);
	}

	/** {@inheritDoc} */
	@Override
	public void init() {

		// Establecemos propiedades del sistema en base a argumentos de Java
		setSystemProperties(this.getParameter(APPLET_PARAM_CUSTOM_JAVA_ARGUMENTS));

		// Establecemos la localizacion
		setLocale(this.getParameter(APPLET_PARAM_LOCALE));

		changeLocale(getParameter("locale")); //$NON-NLS-1$

		LOGGER.info("Cliente @firma"); //$NON-NLS-1$
		LOGGER.info("Versi\u00F3n: " + getVersion()); //$NON-NLS-1$

		LOGGER.info("Sistema operativo: " + System.getProperty("os.name")); //$NON-NLS-1$ //$NON-NLS-2$
		LOGGER.info("Version del SO: " + System.getProperty("os.version")); //$NON-NLS-1$ //$NON-NLS-2$
		LOGGER.info("Version de Java: " + System.getProperty("java.version")); //$NON-NLS-1$ //$NON-NLS-2$
		LOGGER.info("Arquitectura del JRE: " + Platform.getJavaArch()); //$NON-NLS-1$
		LOGGER.info("Java Vendor: " + System.getProperty("java.vm.vendor")); //$NON-NLS-1$ //$NON-NLS-2$

		SignApplet.setLookAndFeel();

		// Google Analytics
		final AnalyticsConfigData config = new AnalyticsConfigData(GOOGLE_ANALYTICS_TRACKING_CODE);
		final JGoogleAnalyticsTracker tracker = new JGoogleAnalyticsTracker(config, GoogleAnalyticsVersion.V_4_7_2);
		tracker.trackPageView(
			getCodeBase().toString(),
			"MiniApplet Cliente @firma " + getVersion(), //$NON-NLS-1$
			getCodeBase().getHost().toString()
		);

		this.userAgent = getParameter("userAgent"); //$NON-NLS-1$

		// Configuramos el almacen de claves que corresponda
		final AOKeyStore keyStore = SignApplet.configureDefaultStore(Platform.getOS(), Platform.getBrowser(this.userAgent));
		this.ksConfigManager = new KeyStoreConfigurationManager(keyStore, this);

		LOGGER.info("Almacen de certificados preestablecido: " + keyStore.getName()); //$NON-NLS-1$

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
		this.defaultShowExpiratedCertificates = paramValue == null || !paramValue.trim().equalsIgnoreCase("false"); //$NON-NLS-1$
		this.ksConfigManager.setShowExpiratedCertificates(this.defaultShowExpiratedCertificates);

		// Creamos el manejador para el cifrado de datos
		this.cipherManager = new CipherManager(this);

		// Creamos el manejador para el ensobrado de datos
		this.enveloperManager = new EnveloperManager(this);

		// Indicamos que el cliente ya se ha inicializado
		this.initializedApplet = true;
	}

	/** {@inheritDoc} */
	@Override
	public void start() {
		try {
			AccessController.doPrivileged(new PrivilegedExceptionAction<Void>() {
				@Override
				public Void run() throws Exception {
					JarSignatureCertExtractor.insertJarSignerOnCACerts(this);
					return null;
				}
			});

		} catch (final Exception e) {
			LOGGER.warning(
					"No se ha podido insertar la cadena de confianza del certificado de firma del applet en el almacen de confianza de Java: " + e); //$NON-NLS-1$
		}
	}

	/** {@inheritDoc} */
	@Override
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
		this.signersToCounterSign = new String[0];
		this.massiveOperation = MassiveType.SIGN;
		this.firstMassiveFile = true;
		this.recursiveSignDir = false;
		this.originalFormat = true;
		this.massiveInputDirectory = null;
		this.massiveOutputDirectory = null;
		this.massiveExtFiltered = null;
		this.error = false;
		this.errorMsg = ""; //$NON-NLS-1$
		this.electronicSignatureFile = null;
		this.signedAttributes = null;
		this.unsignedAttributes = null;
		this.genericConfig = new Properties();
		this.xmlTransforms = null;
		this.ksConfigManager.initialize();
		this.ksConfigManager.setShowExpiratedCertificates(this.defaultShowExpiratedCertificates);
		this.cipherManager.initialize();
		this.enveloperManager.initialize();
	}

	/** {@inheritDoc} */
	@Override
	public String getCertificatesAlias() {
		LOGGER.info("Invocando getCertificatesAlias"); //$NON-NLS-1$
		return SignApplet.concatStrings(getArrayCertificatesAlias(), STRING_SEPARATOR);
	}

	/** {@inheritDoc} */
	@Override
	public String[] getArrayCertificatesAlias() {

		LOGGER.info("Invocando getArrayCertificatesAlias"); //$NON-NLS-1$
		return AccessController.doPrivileged(new java.security.PrivilegedAction<String[]>() {
			/** {@inheritDoc} */
			@Override
			public String[] run() {
				try {
					SignApplet.this.setError(null);
					return SignApplet.this.getKsConfigManager().getArrayCertificateAlias();
				}
				catch (final AOCancelledOperationException e) {
					getLogger().severe("Operacion cancelada por el usuario: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					return new String[0];
				}
				catch (final AOKeyStoreManagerException e) {
					getLogger().severe("Error inicializando el almacen de claves, se devolvera una lista vacia de certificados: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
					return new String[0];
				}
				catch (final AOKeystoreAlternativeException e) {
					final AOKeyStore kst = e.getAlternativeKsm();
					if (kst == null) {
						getLogger().severe("Error inicializando el almacen de claves: " + e); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
						return new String[0];
					}
					if (AOUIFactory.YES_OPTION == AOUIFactory.showConfirmDialog(
						SignApplet.this,
						AppletMessages.getString("SignApplet.4", kst.getName()), //$NON-NLS-1$
						AppletMessages.getString("SignApplet.658"), //$NON-NLS-1$
						AOUIFactory.YES_NO_OPTION,
						AOUIFactory.WARNING_MESSAGE)
					) {
						setKeyStore(null, null, kst.toString());
						return getArrayCertificatesAlias();
					}
					getLogger().severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					return new String[0];
				}
			}
		});
	}

	/** {@inheritDoc} */
	@Override
	public String getCertificates() {
		LOGGER.info("Invocando getCertificates"); //$NON-NLS-1$
		return SignApplet.concatStrings(getArrayCertificates(), STRING_SEPARATOR);
	}

	/** {@inheritDoc} */
	@Override
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
	@Override
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
	@Override
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

	private X509Certificate getCertificateBinary(final String alias) {
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
				if (AOUIFactory.YES_OPTION == AOUIFactory.showConfirmDialog(
					this,
					AppletMessages.getString("SignApplet.4", kst.getName()), //$NON-NLS-1$
					AppletMessages.getString("SignApplet.658"), //$NON-NLS-1$
					AOUIFactory.YES_NO_OPTION,
					AOUIFactory.WARNING_MESSAGE)
				) {
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

		try {
			final OutputStream fos = new FileOutputStream(filename);
			fos.write(binaryData);
			fos.flush();
			fos.close();
		}
		catch (final Exception e) {
			throw new AOCantSaveDataException("No se pudieron almacenar los datos en disco: " + e); //$NON-NLS-1$
		}
	}

	/** {@inheritDoc} */
	@Override
	public boolean saveDataToFile(final String filename) {
		LOGGER.info("Invocando saveDataToFile: " + filename); //$NON-NLS-1$

		if (filename == null || filename.trim().length() < 1) {
			LOGGER.severe("El nombre de fichero para guardar datos es incorrecto, no se salvaran los datos"); //$NON-NLS-1$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.14")); //$NON-NLS-1$
			return false;
		}

		if (!checkUserPermision(AppletMessages.getString("SignApplet.5") + CR + //$NON-NLS-1$
				AOFileUtils.pathLengthShortener(filename, MAX_PATHNAME_LENGTH)
				+ CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.496", filename)); //$NON-NLS-1$
			return false;
		}


		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			/** {@inheritDoc} */
			@Override
			public Boolean run() {
				final URI uri;
				try {
					uri = AOUtil.createURI(filename);
				}
				catch (final Exception e) {
					getLogger().severe("La URI proporcionada no es valida (" + filename + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.15") + filename); //$NON-NLS-1$
					return Boolean.FALSE;
				}
				if (!uri.getScheme().equals("file")) { //$NON-NLS-1$
					getLogger().severe("Solo se permite grabar en almacenamiento local, no mediante el protocolo " + uri.getScheme()); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.17") + uri.getScheme()); //$NON-NLS-1$
					return Boolean.FALSE;
				}
				// OK, en este punto "fileUri" es un nombre de fichero,
				// con lo que ignoramos la uri
				// y lo tratamos como tal
				try {
					saveDataToStorage(SignApplet.this.getInternalData(), filename);
				}
				catch (final Exception e) {
					getLogger().severe("Error al almacenar los datos en disco: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.18")); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				SignApplet.this.setError(null);

				return Boolean.TRUE;
			}
		}).booleanValue();
	}

	/** {@inheritDoc} */
	@Override
	public boolean saveDataToFile() {
		LOGGER.info("Invocando saveDataToFile"); //$NON-NLS-1$

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			/** {@inheritDoc} */
			@Override
			public Boolean run() {
				saveFileAsinchronously(SignApplet.this.getInternalData(), AppletMessages.getString("SignApplet.499"), SignApplet.this.getOutputFile(), null, null); //$NON-NLS-1$
				SignApplet.this.setError(null);
				return Boolean.TRUE;
			}
		}).booleanValue();
	}

	/** {@inheritDoc} */
	@Override
	public boolean saveSignToFile() {
		LOGGER.info("Invocando saveSignToFile"); //$NON-NLS-1$

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			/** {@inheritDoc} */
			@Override
			public Boolean run() {
				if (SignApplet.this.getSignData() == null || SignApplet.this.getSignData().length < 1) {
					getLogger().severe("No se dispone de datos de firma, no se creara el fichero de firma"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.23")); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				String[] extensions = null;
				String description = null;

				if (SignApplet.this.getOutputFile() == null || SignApplet.this.getOutputFile().length() < 1) {

					extensions = new String[] {
					"sig"}; //$NON-NLS-1$
					description = AppletMessages.getString("SignApplet.25"); //$NON-NLS-1$

					if (AOSignConstants.SIGN_FORMAT_CMS.equals(SignApplet.this.getSigFormat())) {
						description = AppletMessages.getString("SignApplet.29"); //$NON-NLS-1$
						extensions = new String[] {
								"csig", "p7s", "sig"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					}

					else if (AOSignConstants.SIGN_FORMAT_CADES.equals(SignApplet.this.getSigFormat())) {
						description = AppletMessages.getString("SignApplet.26"); //$NON-NLS-1$
						extensions = new String[] {
						"csig"}; //$NON-NLS-1$
					}

					else if (AOSignConstants.SIGN_FORMAT_XADES.equals(SignApplet.this.getSigFormat()) || AOSignConstants.SIGN_FORMAT_XADES_DETACHED.equals(SignApplet.this.getSigFormat())
							|| AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED.equals(SignApplet.this.getSigFormat())
							|| AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING.equals(SignApplet.this.getSigFormat())
							|| AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED.equals(SignApplet.this.getSigFormat())
							|| AOSignConstants.SIGN_FORMAT_XMLDSIG.equals(SignApplet.this.getSigFormat())
							|| AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED.equals(SignApplet.this.getSigFormat())
							|| AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED.equals(SignApplet.this.getSigFormat())
							|| AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING.equals(SignApplet.this.getSigFormat())
							|| AOSignConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED.equals(SignApplet.this.getSigFormat())
							|| AOSignConstants.SIGN_FORMAT_SOAP.equals(SignApplet.this.getSigFormat())) {
						description = AppletMessages.getString("SignApplet.27"); //$NON-NLS-1$
						extensions = new String[] {
								"xsig", "xml"}; //$NON-NLS-1$ //$NON-NLS-2$
					}

					else if (AOSignConstants.SIGN_FORMAT_FACTURAE.equals(SignApplet.this.getSigFormat())) {
						description = AppletMessages.getString("SignApplet.31"); //$NON-NLS-1$
						extensions = new String[] {
						"xml"}; //$NON-NLS-1$
					}

					else if (AOSignConstants.SIGN_FORMAT_PDF.equals(SignApplet.this.getSigFormat())) {
						description = AppletMessages.getString("SignApplet.28"); //$NON-NLS-1$
						extensions = new String[] {
						"pdf"}; //$NON-NLS-1$
					}

					else if (AOSignConstants.SIGN_FORMAT_OOXML.equals(SignApplet.this.getSigFormat())) {
						description = AppletMessages.getString("SignApplet.30"); //$NON-NLS-1$
						extensions = new String[] {
								"docx", "xlsx", "pptx"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					}

					else if (AOSignConstants.SIGN_FORMAT_ODF.equals(SignApplet.this.getSigFormat())) {
						description = AppletMessages.getString("SignApplet.32"); //$NON-NLS-1$
						extensions = new String[] {
								"odt", "ods", "odp"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					}
				}

				// Almacenamos en disco
				saveFileAsinchronously(SignApplet.this.getSignData(), AppletMessages.getString("SignApplet.500") , SignApplet.this.getOutputFile(), extensions, description); //$NON-NLS-1$

				SignApplet.this.setError(null);

				return Boolean.TRUE;
			}
		}).booleanValue();
	}

	/** Guarda datos en disco as&iacute;ncronamente. Si no se indica la la ruta
	 * del fichero de salida se mostrar&aacute; un di&aacute;logo para su
	 * selecci&oacute;n, mostrando la descripci&oacute;n y las extensiones
	 * proporcionadas.<br>
	 * Esta llamada es as&iacute;ncrona &uacute;nicamente en Mozilla Firefox, para
	 * evitar que muestre al usuario una advertencia indic&aacute;ndole que hay
	 * un script boqueado y se le d&eacute; la posibilidad de detenerlo.
	 * @param dat Datos que deseamos almacenar.
	 * @param dialogTitle T&iacute;tulo de la ventana de di&aacute;logo.
	 * @param outputPath Ruta del fichero de salida.
	 * @param extensions Extensiones permitidas para el di&aacute;logo de guardado.
	 * @param description Descripci&oacute;n de los datos para el di&aacute;logo de
	 *                    guardado. */
	void saveFileAsinchronously(final byte[] dat, final String dialogTitle, final String outputPath, final String[] extensions, final String description) {
		final AsynchronousSaveData saveDataDialog =
			new AsynchronousSaveData(dat, dialogTitle, outputPath, description, extensions, getParentFrame(SignApplet.this), true);
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
		this.outputFile = saveDataDialog.getSavingTarget();
	}

	/** {@inheritDoc} */
	@Override
	public void setData(final String data) {
		LOGGER.info("Invocando setData"); //$NON-NLS-1$
		if (data == null) {
			this.data = null;
			return;
		}

		try {
			this.data = Base64.decode(data);
		} catch (final Exception e) {
			LOGGER.warning("Los datos insertados no estan en Base64: " + e); //$NON-NLS-1$
			this.data = null;
		}
		this.fileUri = null;
		this.fileBase64 = false;
		this.hash = null;

	}

	/** {@inheritDoc} */
	@Override
	public void setFileuri(final String uri) {
		LOGGER.info("Invocando setFileUri: " + uri); //$NON-NLS-1$

		setError(null);

		if (uri == null || uri.trim().length() < 1) {
			this.fileUri = null;
			return;
		}

		// En caso de que no se acceda a una ruta directamente seleccionada por el usuario y sea
		// un host distinto del que se ha descargado el applet, incluso si es el equipo local,
		// se pide confirmacion al usuario.
		if (!normalizePath(uri).equals(this.userSelectedPath)) {
			try {
				final URI uriObject = AOUtil.createURI(uri);
				if ("file".equalsIgnoreCase(uriObject.getScheme())) { //$NON-NLS-1$
					if (!checkUserPermision(
							AppletMessages.getString("SignApplet.19") + CR +  //$NON-NLS-1$
								AOFileUtils.pathLengthShortener(uri, MAX_PATHNAME_LENGTH) + CR +
									AppletMessages.getString("SignApplet.12")) //$NON-NLS-1$
					) {
						setError(AppletMessages.getString("SignApplet.494", uri)); //$NON-NLS-1$
						return;
					}
				}
				else if (
					!getCodeBase().getHost().equals(uriObject.getHost()) &&
					!checkUserPermision(
						AppletMessages.getString("SignApplet.34") + CR + uriObject.getScheme() + "://" + uriObject.getHost() + CR + AppletMessages.getString("SignApplet.12") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					)
				) {
					setError(AppletMessages.getString("SignApplet.494", uri)); //$NON-NLS-1$
					return;
				}
			}
			catch (final NullPointerException e) {
				LOGGER.warning("No se ha podido obtener informacion de la ruta indicada, se preguntara si debe permitirse el acceso"); //$NON-NLS-1$
				// En caso de error obteniendo informacion de la ruta, se pregunta siempre
				if (!checkUserPermision(
						AppletMessages.getString("SignApplet.19") + CR + //$NON-NLS-1$
							AOFileUtils.pathLengthShortener(uri, MAX_PATHNAME_LENGTH) + CR +
								AppletMessages.getString("SignApplet.12")) //$NON-NLS-1$
				) {
					setError(AppletMessages.getString("SignApplet.494", uri)); //$NON-NLS-1$
					return;
				}
			}
			catch (final URISyntaxException e) {
				// Si la URI es erronea lo obviamos para evitar que el integrador tenga que hacer la comprobacion
				// de error despues de este set (rompiendo compatibilidad con despliegues previos). El problema
				// se detectara durante la operacion con el recurso.
				LOGGER.warning("La URI proporcionada no es valida (" + uri + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}

		this.fileUri = uri;
		this.data = null;
		this.hash = null;
		this.fileBase64 = false;
	}

	/** {@inheritDoc} */
	@Override
	public void setFileuriBase64(final String uri) {
		LOGGER.info("Invocando setFileuriBase64: " + uri); //$NON-NLS-1$

		setError(null);

		if (uri == null || uri.trim().length() < 1) {
			this.fileUri = null;
			return;
		}

		// En caso de que no se acceda a una ruta directamente seleccionada por el usuario y sea
		// un host distinto del que se ha descargado el applet, incluso si es el equipo local,
		// se pide confirmacion al usuario.
		if (!normalizePath(uri).equals(this.userSelectedPath)) {
			try {
				if (!getCodeBase().getHost().equals(AOUtil.createURI(uri).getHost()) &&
						!checkUserPermision(
							AppletMessages.getString("SignApplet.19") + CR + //$NON-NLS-1$
								AOFileUtils.pathLengthShortener(uri, MAX_PATHNAME_LENGTH) + CR +
									AppletMessages.getString("SignApplet.12") //$NON-NLS-1$
							)
						)
				{
					setError(AppletMessages.getString("SignApplet.494", uri)); //$NON-NLS-1$
					return;
				}
			} catch (final URISyntaxException e) {
				// Si la URI es erronea lo obviamos para evitar que el integrador tenga que hacer la comprobacion
				// de error despues de este set (rompiendo compatibilidad con despliegues previos). El problema
				// se detectara durante la operacion con el recurso.
				LOGGER.warning("La URI proporcionada no es valida (" + uri + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}

		this.fileUri = uri;
		this.fileBase64 = true;
		this.data = null;
		this.hash = null;
	}

	/** {@inheritDoc} */
	@Override
	public void setHash(final String hash) {
		LOGGER.info("Invocando setHash: " + hash); //$NON-NLS-1$
		if (hash == null) {
			this.hash = null;
			return;
		}

		try {
			SignApplet.this.hash = Base64.decode(hash);
		} catch (final Exception e) {
			LOGGER.warning("El hash insertado no esta en Base64: " + e); //$NON-NLS-1$
			SignApplet.this.hash = null;
		}
		SignApplet.this.genericConfig.setProperty(EXTRAPARAM_KEY_MIMETYPE, MimeHelper.DEFAULT_MIMETYPE);
		SignApplet.this.fileUri = null;
		SignApplet.this.fileBase64 = false;
		SignApplet.this.data = null;
	}

	/** {@inheritDoc} */
	@Override
	public void setElectronicSignature(final String signatureB64) {
		LOGGER.info("Invocando setElectronicSignature"); //$NON-NLS-1$
		if (signatureB64 == null || signatureB64.length() == 0) {
			this.signData = null;
		}
		else {
			try {
				this.signData = Base64.decode(signatureB64);
			} catch (final Exception e) {
				LOGGER.warning("La firma insertada no esta en Base64: " + e); //$NON-NLS-1$
				this.signData = null;
			}
		}
		this.electronicSignatureFile = null;
	}

	/** {@inheritDoc} */
	@Override
	public void setElectronicSignatureFile(final String filename) {
		LOGGER.info("Invocando setElectronicSignatureFile: " + filename); //$NON-NLS-1$

		setError(null);

		if (filename == null || filename.trim().length() < 1) {
			this.electronicSignatureFile = null;
			return;
		}

		URI uri;
		try {
			uri = AOUtil.createURI(filename);
		}
		catch (final Exception e) {
			LOGGER.severe("La URI proporcionada no es valida (" + filename + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
			this.electronicSignatureFile = null;
			return;
		}

		// En caso de que el usuario no haya seleccionado expresamente ese archivo para dar acceso,
		// y este en un host distinto del que se ha descargado el applet, incluso si es el equipo
		// local, se pide confirmacion al usuario.
		if (!normalizePath(filename).equals(this.userSelectedPath)) {
			URL codeBase;
			try {
				codeBase = getCodeBase();
			}
			catch(final Exception e) {
				codeBase = null;
				LOGGER.warning("No se ha podido obtener el CodeBase del Applet: " + e); //$NON-NLS-1$
			}
			if (codeBase == null || !codeBase.getHost().equals(uri.getHost()) &&
					!checkUserPermision(AppletMessages.getString("SignApplet.33") + CR + //$NON-NLS-1$
							AOFileUtils.pathLengthShortener(filename, MAX_PATHNAME_LENGTH) +
							CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
				setError(AppletMessages.getString("SignApplet.494", filename)); //$NON-NLS-1$
				this.electronicSignatureFile = null;
				return;
			}
		}

		this.electronicSignatureFile = uri;
		this.signData = null;
	}

	/** {@inheritDoc} */
	@Override
	public void setSignersToCounterSign(final String signers) {
		LOGGER.info("Invocando setSignersToCounterSign" + (signers != null ? ": " + signers.trim().replace('\n', ' ').replace("\r\n", " ") : "")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
		this.signersToCounterSign = signers == null ? new String[0] : signers.split("(\\r\\n|\\n)"); //$NON-NLS-1$
	}

	/** {@inheritDoc} */
	@Override
	public String getSignersStructure() {
		LOGGER.info("Invocando getSignersStructure"); //$NON-NLS-1$

		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			/** {@inheritDoc} */
			@Override
			public String run() {
				// Tomamos la firma que deseamos analizar
				final byte[] originalSign;
				try {
					final GetSignatureAction getSignatureAction = new GetSignatureAction(
							SignApplet.this.getSignData(), SignApplet.this.getElectronicSignatureFile());
					getSignatureAction.setSelectFile(true, SignApplet.this.getSigFormat(), SignApplet.this);
					originalSign = AccessController.doPrivileged(getSignatureAction);
					SignApplet.this.setElectronicSignatureFile(getSignatureAction.getSelectedSignatureFile());
				}
				catch (final PrivilegedActionException e) {
					if (e.getCause() instanceof AOCancelledOperationException) {
						getLogger().info("Operacion cancelada por el usuario"); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					}
					else if (e.getCause() instanceof AOException) {
						getLogger().info("Error al seleccionar el fichero de firma: " + e.getCause()); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.69")); //$NON-NLS-1$
					}
					return null;
				}

				// Probamos si la firma se corresponde con el formato
				// establecido y si no es asi
				// la analizamos y tomamos el manejador correspondiente
				AOSigner signer = AOSignerFactory.getSigner(SignApplet.this.getSigFormat());
				try {
					if (signer == null || !signer.isSign(originalSign)) {
						signer = AOSignerFactory.getSigner(originalSign);
					}
				} catch (final IOException e) {
					getLogger().severe("Error al leer la firma: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.63")); //$NON-NLS-1$
					return null;
				}

				// Si la firma no esta en un formato soportado,
				// establecemos el error
				if (signer == null) {
					getLogger().severe("La firma introducida no se ajusta a ningun formato soportado"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.63")); //$NON-NLS-1$
					return null;
				}

				// Mostramos el el arbol de firmas
				try {
					return AOTreeUtil.showTreeAsString(signer.getSignersStructure(originalSign, false), null, null);
				}
				catch (final Exception e) {
					getLogger().severe("Arbol de firmas no valido: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.72")); //$NON-NLS-1$
					return null;
				}
			}
		});
	}

	/** {@inheritDoc} */
	@Override
	public boolean counterSignTree() {
		LOGGER.info("Invocando counterSignTree"); //$NON-NLS-1$
		return this.counterSign(CounterSignTarget.TREE);
	}

	/** {@inheritDoc} */
	@Override
	public boolean counterSignLeafs() {
		LOGGER.info("Invocando counterSignLeafs"); //$NON-NLS-1$
		return this.counterSign(CounterSignTarget.LEAFS);
	}

	/** {@inheritDoc} */
	@Override
	public boolean counterSignSigners() {
		LOGGER.info("Invocando counterSignSigners"); //$NON-NLS-1$
		return this.counterSign(CounterSignTarget.SIGNERS);
	}

	/** {@inheritDoc} */
	@Override
	public boolean counterSignIndexes() {
		LOGGER.info("Invocando counterSignIndexes"); //$NON-NLS-1$
		return this.counterSign(CounterSignTarget.NODES);
	}

	private boolean counterSign(final CounterSignTarget target) {

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			/** {@inheritDoc} */
			@Override
			public Boolean run() {
				final String algorithm = SignApplet.this.getSigAlgo() == null ? AOSignConstants.DEFAULT_SIGN_ALGO : SignApplet.this.getSigAlgo();
				final String format = SignApplet.this.getSigFormat() == null ? AOSignConstants.DEFAULT_SIGN_FORMAT : SignApplet.this.getSigFormat();

				// Tomamos la firma sobre la que se realiza la
				// contrafirma
				final byte[] originalSign;
				try {
					final GetSignatureAction getSignatureAction = new GetSignatureAction(
							SignApplet.this.getSignData(), SignApplet.this.getElectronicSignatureFile());
					getSignatureAction.setSelectFile(true, SignApplet.this.getSigFormat(), SignApplet.this);
					originalSign = AccessController.doPrivileged(getSignatureAction);
					SignApplet.this.setElectronicSignatureFile(getSignatureAction.getSelectedSignatureFile());
				}
				catch (final PrivilegedActionException e) {
					if (e.getCause() instanceof AOCancelledOperationException) {
						getLogger().info("Operacion cancelada por el usuario"); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					}
					else if (e.getCause() instanceof AOException) {
						getLogger().info("Error al recuperar los datos de firma: " + e.getCause()); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.64")); //$NON-NLS-1$
					}
					return Boolean.FALSE;
				}

				// Configuramos el certificado
				final PrivateKeyEntry ke = SignApplet.this.configureCertificate();
				if (ke == null) {
					getLogger().severe("Error en la seleccion del certificado"); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				// Tomamos el manejador del formato de firma
				final AOSigner signer = AOSignerFactory.getSigner(format);
				if (signer == null) {
					getLogger().severe("El formato de firma '" + format + "' no esta soportado"); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.95") + format); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				// Establecemos el mimetype de los datos
				configureDataTypeExtraParams(SignApplet.this.getGenericConfig());

				// Configuramos la politica
				SignApplet.this.configurePolicy();

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
					if (SignApplet.this.getSignersToCounterSign() == null || SignApplet.this.getSignersToCounterSign().length < 1) {
						try {
							params = UIDialogs.showSignersSelectionPane(signer.getSignersStructure(originalSign, false), SignApplet.this);
						}
						catch (final AOCancelledOperationException e) {
							getLogger().info("Operacion cancelada por el usuario"); //$NON-NLS-1$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
							return Boolean.FALSE;
						}
						catch (final IOException e) {
							getLogger().severe("Error al leer la firma"); //$NON-NLS-1$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.64")); //$NON-NLS-1$
							return Boolean.FALSE;
						}
						catch (final Exception e) {
							getLogger().severe("Error al recuperar los firmantes de la firma: " + e); //$NON-NLS-1$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.97")); //$NON-NLS-1$
							return Boolean.FALSE;
						}
					}
					else {
						params = SignApplet.this.getSignersToCounterSign();
					}
				}

				// Obtenemos los parametros para la contrafirma de nodos
				else if (target == CounterSignTarget.NODES) {
					// Si no se establecen los nodos de firma mediante
					// setSignersToCounterSign(String)
					// mostramos el panel de seleccion de nodos. Este
					// panel devolvera los nodos de firma
					// a partir del 0.
					if (SignApplet.this.getSignersToCounterSign() == null || SignApplet.this.getSignersToCounterSign().length < 1) {
						int[] indexes;
						try {
							indexes = UIDialogs.showNodeSignSelectionPane(signer.getSignersStructure(originalSign, false), SignApplet.this);
						}
						catch (final AOCancelledOperationException e) {
							getLogger().info("Operacion cancelada por el usuario"); //$NON-NLS-1$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
							return Boolean.FALSE;
						}
						catch (final IOException e) {
							getLogger().severe("Error al leer la firma"); //$NON-NLS-1$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.64")); //$NON-NLS-1$
							return Boolean.FALSE;
						}
						catch (final Exception e) {
							getLogger().severe("Error al recuperar los nodos de la firma: " + e); //$NON-NLS-1$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.99")); //$NON-NLS-1$
							return Boolean.FALSE;
						}

						if (indexes.length == 0) {
							getLogger().severe("Se debe seleccionar al menos un nodo de firma para contrafirmar"); //$NON-NLS-1$
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
						params = new Object[SignApplet.this.getSignersToCounterSign().length];
						for (int i = 0; i < SignApplet.this.getSignersToCounterSign().length; i++) {
							params[i] = Integer.valueOf(SignApplet.this.getSignersToCounterSign()[i]);
						}
					}
				}

				// Si se han especificado atributos de firma los
				// agregamos. Esto solo sera efectivo
				// para los signers a los que aplique
				SignApplet.this.addAttributes(signer);

				// Contrafirmamos finalmente
				final byte[] outputBuffer;
				try {
					outputBuffer = signer.countersign(
						originalSign,
						algorithm,
						target,
						params,
						ke.getPrivateKey(),
						ke.getCertificateChain(),
						SignApplet.this.getGenericConfig()
					);
				}
				catch (final UnsupportedOperationException e) {
					getLogger().severe("Operacion no soportada: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.682"), true); //$NON-NLS-1$
					return Boolean.FALSE;
				}
				catch (final Exception e) {
					getLogger().severe("Error durante el proceso de contrafirma: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.101")); //$NON-NLS-1$
					return Boolean.FALSE;
				}
				catch (final OutOfMemoryError e) {
					getLogger().severe("Error de falta de memoria durante la contrafirma: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
					return Boolean.FALSE;
				}
				catch (final Throwable e) {
					getLogger().severe("Error desconocido durante la contrafirma: " + e); //$NON-NLS-1$
					e.printStackTrace();
					SignApplet.this.setError(AppletMessages.getString("SignApplet.140")); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				// Ahora vamos a guardar el resultado en el fichero de
				// salida
				if (outputBuffer == null || outputBuffer.length < 1) {
					// No vaya a ser que saliese un resultado vacio...
					getLogger().severe("El proceso de contrafirma no genero ningun resultado"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.102")); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				SignApplet.this.setSignData(outputBuffer);

				SignApplet.this.setError(null);

				return Boolean.TRUE;
			}
		}).booleanValue();
	}

	/** {@inheritDoc} */
	@Override
	public void setOutFilePath(final String filename) {
		LOGGER.info("Invocando setOutFilePath: " + filename); //$NON-NLS-1$

		setError(null);

		if (filename == null || filename.trim().length() < 1) {
			LOGGER.info("Se ha establecido el nombre de fichero de salida a null" //$NON-NLS-1$
			);
			this.outputFile = null;
			return;
		}
		if (!checkUserPermision(AppletMessages.getString("SignApplet.5") + CR + //$NON-NLS-1$
				AOFileUtils.pathLengthShortener(filename, MAX_PATHNAME_LENGTH) +
				CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.494", filename)); //$NON-NLS-1$
			return;
		}
		this.outputFile = filename;
	}

	/** {@inheritDoc} */
	@Override
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
	@Override
	public void setSignatureFormat(final String format) {
		LOGGER.info("Invocando setSignatureFormat: " + format); //$NON-NLS-1$

		String signatureFormat = format;

		// Si no se establece formato alguno, se mantiene el por defecto
		if (signatureFormat == null) {
			LOGGER.warning("El formato de firma no puede ser nulo, se establecera el formato por defecto: " + //$NON-NLS-1$
					AOSignConstants.DEFAULT_SIGN_FORMAT);
			signatureFormat = AOSignConstants.DEFAULT_SIGN_FORMAT;
		}

		// Para mantener la interfaz con el exterior intacta, traduciremos
		// cualquier nombre de formato antiguo a su nueva forma
		this.sigFormat = NormalizedNames.normalizeFormatName(signatureFormat);
	}

	/** {@inheritDoc} */
	@Override
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
	@Override
	public String getSignatureMode() {
		LOGGER.info("Invocando getSignatureMode"); //$NON-NLS-1$
		return this.sigMode == null ? AOSignConstants.DEFAULT_SIGN_MODE : this.sigMode;
	}

	/** {@inheritDoc} */
	@Override
	public void setKeyStore(final String filename, final String password, final String type) {

		setError(null);

		if (filename != null && filename.trim().length() > 0 &&
				!normalizePath(filename).equals(this.userSelectedPath) &&
					!checkUserPermision(
						AppletMessages.getString("SignApplet.19") + CR + //$NON-NLS-1$
							AOFileUtils.pathLengthShortener(filename, MAX_PATHNAME_LENGTH) + CR +
								AppletMessages.getString("SignApplet.12") //$NON-NLS-1$
						)
					)
		{
			setError(AppletMessages.getString("SignApplet.494", filename)); //$NON-NLS-1$
			return;
		}

		LOGGER.info("Invocando setKeyStore de tipo '" + type + "' con el path '" + filename + "'"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
			/** {@inheritDoc} */
			@Override
			public Void run() {
				final AOKeyStore newStore = AOKeyStore.getKeyStore(NormalizedNames.normalizeKeyStoreName(type));
				if (newStore == null) {
					setError(AppletMessages.getString("SignApplet.3")); //$NON-NLS-1$
					return null;
				}

				// Guardamos la contrasena porque nos es necesaria tanto
				// para abrir el almacen como
				// para utilizar el certificado
				SignApplet.this.getKsConfigManager().setKsPath(filename);
				SignApplet.this.getKsConfigManager().setKsPassword(password);
				SignApplet.this.getKsConfigManager().changeKeyStore(newStore);

				if (SignApplet.this.getShowMozillaSmartCardWarning() && (newStore == AOKeyStore.MOZ_UNI || newStore == AOKeyStore.PKCS11)) {
					SignApplet.this.getKsConfigManager().setLoadingWarning(true);
				}

				setError(null);
				return null;
			}
		});
	}

	/** {@inheritDoc} */
	@Override
	public void setPolicy(final String identifier,
			final String description,
			final String qualifier,
			final String hashB64) {

		LOGGER.info("Invocando setPolicy con el identificador: " + identifier); //$NON-NLS-1$

		// El identificador puede ser un OID o una URI (incluyendo una URN de tipo OID)
		if (identifier != null) {
			// Probamos primero si es un OID
			try {
				this.policyId = "urn:oid:" + new Oid(identifier.replace("urn:oid:", "")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			}
			catch (final Exception e1) {
				// No es un OID, probamos una URL
				try {
					this.policyId = new URI(identifier).toString();
					//                    this.policyId = AOUtil.createURI(identifier).toURL().toString();
				}
				catch (final Exception e) {
					LOGGER.warning("No se ha indicado un OID o una URI valida para la politica(" + identifier + "), pero se establecera el valor de todas formas: " + e); //$NON-NLS-1$ //$NON-NLS-2$
					this.policyId = identifier;
				}
			}
		}
		// Configuramos Oid calificador
		if (qualifier != null) {
			// Miramos a ver si es un OID directamente, en cuyo caso lo pasamos
			// a URN
			try {
				this.policyQualifier = new URI("urn:oid:" + new Oid(qualifier.replace("urn:oid:", ""))); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
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

		// Y la huella digital del identificador
		this.policyHashB64 = hashB64;
	}

	/** {@inheritDoc} */
	@Override
	public boolean sign() {
		LOGGER.info("Invocando sign"); //$NON-NLS-1$
		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			/** {@inheritDoc} */
			@Override
			public Boolean run() {

				// Si no esta establecido el algoritmo usamos el por
				// defecto, pero solo para esta ocasion,
				// no lo establecemos para posteriores
				final String algorithm = SignApplet.this.getSigAlgo() == null ? AOSignConstants.DEFAULT_SIGN_ALGO : SignApplet.this.getSigAlgo();

				// Si no esta establecido el formato usamos el por
				// defecto, pero solo para esta ocasion,
				// no lo establecemos para posteriores
				final String format = SignApplet.this.getSigFormat() == null ? AOSignConstants.DEFAULT_SIGN_FORMAT : SignApplet.this.getSigFormat();

				// Si no esta establecido el modo usamos el por defecto,
				// pero solo para esta ocasion,
				// no lo establecemos para posteriores
				final String mode = SignApplet.this.getSigMode() == null ? AOSignConstants.DEFAULT_SIGN_MODE : SignApplet.this.getSigMode();

				// Para mantener las formas de la version 2.4 del
				// cliente, se mostrara una
				// ventana modal, en caso de solicitarse una firma
				// Enveloped en modo explicito,
				// informando de que esta configuracion es imposible
				if ((format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED) || format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)) && mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT)) {

					getLogger().severe("El formato Enveloped es incompatible con el modo de firma explicito"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.139")); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				// Tomamos el Signer adecuado
				final AOSigner signer = AOSignerFactory.getSigner(format);
				if (signer == null) {
					getLogger().severe("El formato de firma '" + format + "' no esta soportado"); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.95") + format); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				// Como condicion excepcional, si se nos ha introducido un hash para firmar
				// estableceremos el algoritmo de hash del algoritmo de firma configurado
				// como el algoritmo del hash introducido
				if (SignApplet.this.getHash() != null && mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT)) {
					final int withPos = algorithm.indexOf("with"); //$NON-NLS-1$
					if (withPos == -1) {
						getLogger().severe("El formato del algoritmo de firma no es valido: " + algorithm); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.197") + algorithm); //$NON-NLS-1$
					}
					SignApplet.this.getGenericConfig().setProperty("precalculatedHashAlgorithm", algorithm.substring(0, withPos)); //$NON-NLS-1$
				}

				// -----------------------
				// Evitamos las configuraciones de firma de hashes no
				// soportadas
				// -----------------------

				// La firma de hashes solo esta soportada por los
				// formatos de firma: CMS, CAdES, XMLdSig y XAdES
				if (SignApplet.this.getHash() != null && (SignApplet.this.getSigFormat().equals(AOSignConstants.SIGN_FORMAT_PDF) || SignApplet.this.getSigFormat().equals(AOSignConstants.SIGN_FORMAT_ODF)
						|| SignApplet.this.getSigFormat().equals(AOSignConstants.SIGN_FORMAT_OOXML) || SignApplet.this.getSigFormat().equals(AOSignConstants.SIGN_FORMAT_PKCS1)
						|| SignApplet.this.getSigFormat().equals(AOSignConstants.SIGN_FORMAT_FACTURAE))) {

					getLogger().severe("La firma de hash no esta soportada para el formato " + SignApplet.this.getSigFormat()); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.198", SignApplet.this.getSigFormat())); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				// La firma implicita de hash exige que se introduzcan
				// los datos a los que corresponde el hash.
				// Deben haberse introducido los datos, no se permite el
				// fichero por incompatibilidad con
				// las funciones de soporte del estilo
				// "getFileBase64Encoded",
				// "getFileHashBase64Encoded",...
				if (SignApplet.this.getHash() != null && SignApplet.this.getInternalData() == null && mode.equals(AOSignConstants.SIGN_MODE_IMPLICIT)) {
					getLogger().severe("La firma implicita de hash exige que se introduzcan los datos a los que corresponde el hash"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.216")); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				getLogger().info("Firma con algoritmo " + algorithm + ", formato " + format + " y modo " + mode); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$


				// Si no se nos ha introducido un listado de hashes
				// entendemos que debe realizarse
				// una firma corriente. En este caso, se tomara
				// cualquier parametro de entrada establecido
				// como origen de los datos (dato, fichero y hash).
				byte[] dataToSign = null;
				try {
					dataToSign = SignApplet.this.getInData();
				}
				catch (final AOException e) {
					// El metodo getInDataStream ya se habra
					// encargado de establecer el mensaje en caso de
					// error (Incluido el OutOfMemoryError)
					return Boolean.FALSE;
				}

				// En el caso de las firmas XAdES de fichero o datos, si se configura el
				// modo explicito, se realizara el calculo de hash desde fuera (siempre sera el por defecto)
				if (SignApplet.this.getSigFormat().toLowerCase().startsWith(AOSignConstants.SIGN_FORMAT_XADES.toLowerCase()) &&
						(SignApplet.this.getInternalFileUri() != null || SignApplet.this.getInternalData() != null) &&
						mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT)) {

					final String digestAlgo = DEFAULT_MESSAGE_DIGEST_ALGORITHM;
					SignApplet.this.getGenericConfig().setProperty("mimeType", ("hash/" + digestAlgo).toLowerCase());  //$NON-NLS-1$//$NON-NLS-2$
					try {
						dataToSign = CryptoUtils.getMessageDigest(dataToSign, digestAlgo);
					}
					catch (final NoSuchAlgorithmException e) {
						getLogger().severe("Error en la configuracion del algoritmo de hash interno para el calculo de las firmas explicitas XAdES: " + e); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.464") + digestAlgo); //$NON-NLS-1$
						return null;
					}
				}

				// Configuramos el OID del tipo de datos
				configureDataTypeExtraParams(SignApplet.this.getGenericConfig());

				// Configuramos el certificado
				final PrivateKeyEntry ke = SignApplet.this.configureCertificate();
				if (ke == null) {
					getLogger().severe("Error en la seleccion del certificado"); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				// Si se han especificado atributos de firma los
				// agregamos. Esto solo sera efectivo
				// para los signers a los que aplique
				SignApplet.this.addAttributes(signer);

				// Configuramos la politica
				SignApplet.this.configurePolicy();

				// Configuramos las trasnformaciones XML
				SignApplet.this.configureXMLTransforms();

				// Agregamos las ultimas configuraciones y firmamos
				SignApplet.this.getGenericConfig().setProperty("mode", mode); //$NON-NLS-1$
				SignApplet.this.getGenericConfig().setProperty("format", format); //$NON-NLS-1$
				if (SignApplet.this.getInternalFileUri() != null) {
					SignApplet.this.getGenericConfig().setProperty("uri", SignApplet.this.getInternalFileUri()); //$NON-NLS-1$
				}

				final byte[] outputBuffer;
				try {
					outputBuffer = signer.sign(
							dataToSign,
							algorithm,
							ke.getPrivateKey(),
							ke.getCertificateChain(),
							SignApplet.this.getGenericConfig()
							);
				}
				catch (final UnsupportedOperationException e) {
					getLogger().severe(e.getMessage());
					SignApplet.this.setError(AppletMessages.getString("SignApplet.682")); //$NON-NLS-1$
					return Boolean.FALSE;
				}
				catch (final AOFormatFileException e) {
					getLogger().severe(e.getMessage());
					SignApplet.this.setError(AppletMessages.getString("SignApplet.11")); //$NON-NLS-1$
					return Boolean.FALSE;
				}
				catch (final AOInvalidFormatException e) {
					getLogger().severe(e.getMessage());
					if (e.getClass().getName().endsWith("InvalidEFacturaDataException")) { //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.13")); //$NON-NLS-1$
					} else {
						SignApplet.this.setError(AppletMessages.getString("SignApplet.11")); //$NON-NLS-1$
					}
					return Boolean.FALSE;
				}
				catch (final AOException e) {
					getLogger().severe(e.toString());
					SignApplet.this.setError(AppletMessages.getString("SignApplet.101")); //$NON-NLS-1$
					return Boolean.FALSE;
				}
				catch (final Exception e) {
					getLogger().severe("Error durante el proceso de firma: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.101")); //$NON-NLS-1$
					return Boolean.FALSE;
				}
				catch (final OutOfMemoryError e) {
					getLogger().severe("Error de falta de memoria durante la firma: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				// Ahora vamos a guardar el resultado en el fichero
				// de salida
				if (outputBuffer == null || outputBuffer.length < 1) {
					// No vaya a ser que saliese un resultado
					// vacio...
					getLogger().severe("El proceso de firma no genero ningun resultado"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.154")); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				SignApplet.this.setSignData(outputBuffer);

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
				this.genericConfig.setProperty("xmlTransform" + i + "Type", this.xmlTransforms.get(i).getType()); //$NON-NLS-1$ //$NON-NLS-2$
				// El subtipo y el cuerpo son opcionales
				if (this.xmlTransforms.get(i).getSubtype() != null) {
					this.genericConfig.setProperty("xmlTransform" + i + "Subtype", this.xmlTransforms.get(i).getSubtype()); //$NON-NLS-1$ //$NON-NLS-2$
				}
				if (this.xmlTransforms.get(i).getBody() != null) {
					this.genericConfig.setProperty("xmlTransform" + i + "Body", this.xmlTransforms.get(i).getBody()); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
		}
	}

	static void configureDataTypeExtraParams(final Properties extraParams) {
		if (extraParams.containsKey(EXTRAPARAM_KEY_MIMETYPE)) {
			final String oid;
			try {
				oid = MimeHelper.transformMimeTypeToOid(extraParams.getProperty(EXTRAPARAM_KEY_MIMETYPE));
			}
			catch (final IOException e) {
				getLogger().warning("No se ha podido identificar el OID asociado el MimeType de los datos: " + e); //$NON-NLS-1$
				return;
			}
			if (oid != null) {
				extraParams.setProperty("contentTypeOid", oid); //$NON-NLS-1$
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
				final Iterator<String> itOid = SignApplet.this.signedAttributes.keySet().iterator();
				while (itOid.hasNext()) {
					final String oid = itOid.next();
					((AOCMSSigner) signer).addSignedAttribute(oid, SignApplet.this.signedAttributes.get(oid).getBytes());
				}
			}

			// Agregamos los atributos sin firmar
			if (SignApplet.this.unsignedAttributes != null) {
				final Iterator<String> itOid = SignApplet.this.unsignedAttributes.keySet().iterator();
				while (itOid.hasNext()) {
					final String oid = itOid.next();
					for (final String value : SignApplet.this.unsignedAttributes.get(oid)) {
						((AOCMSSigner) signer).addUnsignedAttribute(oid, value.getBytes());
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
			this.genericConfig.setProperty("policyIdentifier", this.policyId); //$NON-NLS-1$
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
	@Override
	public boolean coSign() {
		LOGGER.info("Invocando cosign"); //$NON-NLS-1$

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			/** {@inheritDoc} */
			@Override
			public Boolean run() {

				// No establecemos algoritmo por defecto, si no esta
				// establecido usamos el por
				// defecto, pero solo para esta ocasion
				final String algorithm = SignApplet.this.getSigAlgo() == null ? AOSignConstants.DEFAULT_SIGN_ALGO : SignApplet.this.getSigAlgo();

				// No establecemos formato por defecto, si no esta
				// establecido usamos el por
				// defecto, pero solo para esta ocasion
				final String format = SignApplet.this.getSigFormat() == null ? AOSignConstants.DEFAULT_SIGN_FORMAT : SignApplet.this.getSigFormat();

				// No establecemos formato por defecto, si no esta
				// establecido usamos el por
				// defecto, pero solo para esta ocasion
				final String mode = SignApplet.this.getSigMode() == null ? AOSignConstants.DEFAULT_SIGN_MODE : SignApplet.this.getSigMode();

				// Para mantener las formas de la version 2.4 del
				// cliente, se mostrara una
				// ventana modal, en caso de solicitarse una firma
				// Enveloped en modo explicito,
				// informando de que esta configuracion es imposible
				if ((format.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED) || format.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)) && mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT)) {
					getLogger().severe("El formato Enveloped es incompatible con el modo de firma explicito"); //$NON-NLS-1$
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
					// (Incluido el OutOfMemoryError)
					return Boolean.FALSE;
				}

				// Tomamos la firma sobre la que se realiza la
				// contrafirma
				final byte[] originalSign;
				try {
					final GetSignatureAction getSignatureAction = new GetSignatureAction(
							SignApplet.this.getSignData(), SignApplet.this.getElectronicSignatureFile());
					getSignatureAction.setSelectFile(true, SignApplet.this.getSigFormat(), SignApplet.this);
					originalSign = AccessController.doPrivileged(getSignatureAction);
					SignApplet.this.setElectronicSignatureFile(getSignatureAction.getSelectedSignatureFile());
				}
				catch (final PrivilegedActionException e) {
					if (e.getCause() instanceof AOCancelledOperationException) {
						getLogger().info("Operacion cancelada por el usuario"); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					}
					else if (e.getCause() instanceof AOException) {
						getLogger().info("Error al recuperar los datos de firma: " + e.getCause()); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.64")); //$NON-NLS-1$
					}
					return Boolean.FALSE;
				}

				// Tomamos el manejador de firma asociado al formato
				final AOSigner signer = AOSignerFactory.getSigner(format);
				if (signer == null) {
					getLogger().severe("El formato de firma '" + format + "' no esta soportado"); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.95") + format); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				if (SignApplet.this.getHash() != null && mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT)) {
					final int withPos = algorithm.indexOf("with"); //$NON-NLS-1$
					if (withPos == -1) {
						getLogger().severe("El formato del algoritmo de firma no es valido: " + algorithm); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.197") + algorithm); //$NON-NLS-1$
						return Boolean.FALSE;
					}
					// Establecemos el algoritmo con el que se calculo
					// el hash externamente
					SignApplet.this.getGenericConfig().setProperty("precalculatedHashAlgorithm", algorithm.substring(0, withPos)); //$NON-NLS-1$
				}

				/*
				 * La firma de hashes solo esta soportada por los
				 * formatos de firma binaria CMS y CAdES. Las firmas
				 * PDF, ODF y OOXML requieren siempre los datos, ya que
				 * van empotradas. Las firmas XML los necesitan para
				 * hacer la referencia a los mismos.
				 */
				if (SignApplet.this.getHash() != null && !SignApplet.this.getSigFormat().equals(AOSignConstants.SIGN_FORMAT_CMS) && !SignApplet.this.getSigFormat().equals(AOSignConstants.SIGN_FORMAT_CADES)) {
					getLogger().severe("La firma de hashes solo esta soportada por los formatos de firma binaria CMS y CAdES"); //$NON-NLS-1$
					setError(AppletMessages.getString("SignApplet.277")); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				/*
				 * Evitamos las configuraciones de firma de hashes no
				 * soportadas
				 */

				// La firma implicita de hash exige que se introduzcan
				// los datos a los que corresponde el hash
				if (SignApplet.this.getHash() != null && SignApplet.this.getInternalData() == null && mode.equals(AOSignConstants.SIGN_MODE_IMPLICIT)) {
					getLogger().severe("La firma implicita de huella digital exige que se introduzcan los datos a los que corresponde la huella digital"); //$NON-NLS-1$
					setError(AppletMessages.getString("SignApplet.216")); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				// En el caso de las firmas XAdES de fichero o datos, si se configura el
				// modo explicito, se indica que el tipo de datos es un hash (siempre sera el algoritmo por defecto)
				if (SignApplet.this.getSigFormat().toLowerCase().startsWith(AOSignConstants.SIGN_FORMAT_XADES.toLowerCase()) &&
						(SignApplet.this.getInternalFileUri() != null || SignApplet.this.getInternalData() != null) &&
						mode.equals(AOSignConstants.SIGN_MODE_EXPLICIT)) {

					SignApplet.this.getGenericConfig().setProperty("mimeType", ("hash/" + DEFAULT_MESSAGE_DIGEST_ALGORITHM).toLowerCase());  //$NON-NLS-1$//$NON-NLS-2$
				}

				// Configuramos el mimetype de los datos
				configureDataTypeExtraParams(SignApplet.this.getGenericConfig());

				// Configuramos el certificado
				final PrivateKeyEntry ke = SignApplet.this.configureCertificate();
				if (ke == null) {
					getLogger().severe("Error en la seleccion del certificado"); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				// Si se han especificado atributos de firma los
				// agregamos. Esto solo sera efectivo
				// para los signers a los que aplique
				SignApplet.this.addAttributes(signer);

				// Configuramos la politica
				SignApplet.this.configurePolicy();

				// Finalmente, configuramos el modo, la URI y operamos
				SignApplet.this.getGenericConfig().setProperty("mode", mode); //$NON-NLS-1$
				if (SignApplet.this.getInternalFileUri() != null) {
					SignApplet.this.getGenericConfig().setProperty("uri", SignApplet.this.getInternalFileUri()); //$NON-NLS-1$
				}

				byte[] outputBuffer;
				try {
					outputBuffer = signer.cosign(
						dataToSign,
						originalSign,
						algorithm,
						ke.getPrivateKey(),
						ke.getCertificateChain(),
						SignApplet.this.getGenericConfig()
					);
				}
				catch (final UnsupportedOperationException e) {
					getLogger().severe(AppletMessages.getString("SignApplet.682") + ": " + e.getMessage()); //$NON-NLS-1$  //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.682")); //$NON-NLS-1$
					return Boolean.FALSE;
				}
				catch (final Exception e) {
					getLogger().severe("Error durante el proceso de cofirma: " + e); //$NON-NLS-1$
					setError(AppletMessages.getString("SignApplet.101")); //$NON-NLS-1$
					return Boolean.FALSE;
				}
				catch (final OutOfMemoryError e) {
					getLogger().severe("Error de falta de memoria durante la cofirma: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				// Ahora vamos a guardar el resultado en el fichero de
				// salida
				if (outputBuffer == null || outputBuffer.length < 1) {
					// No vaya a ser que saliese un resultado vacio...
					getLogger().severe("El proceso de firma no genero ningun resultado"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.154")); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				SignApplet.this.setError(null);

				SignApplet.this.setSignData(outputBuffer);

				return Boolean.TRUE;
			}
		}).booleanValue();
	}

	/** {@inheritDoc} */
	@Override
	public void setInIncludeExtensions(final String extensions) {
		LOGGER.info("Invocando setInIncludeExtensions: " + extensions); //$NON-NLS-1$
		this.massiveExtFiltered = extensions == null ? null : extensions.split(","); //$NON-NLS-1$
	}

	/** {@inheritDoc} */
	@Override
	public boolean signDirectory() {
		LOGGER.info("Invocando signDirectory"); //$NON-NLS-1$

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			/** {@inheritDoc} */
			@Override
			public Boolean run() {
				if (SignApplet.this.getSigFormat().equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED) && SignApplet.this.getSigMode().equals(AOSignConstants.SIGN_MODE_EXPLICIT) || SignApplet.this.getSigFormat().equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED) && SignApplet.this.getSigMode().equals(AOSignConstants.SIGN_MODE_EXPLICIT)) {
					SignApplet.this.setError(true);
					getLogger().severe("El formato Enveloped es incompatible con el modo de firma explicito"); //$NON-NLS-1$
					setError(AppletMessages.getString("SignApplet.139")); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				// Si no se ha establecido el directorio de entrada de
				// ficheros, lo solicitamos
				final String inputDir;
				if (SignApplet.this.getMassiveInputDirectory() != null) {
					inputDir = SignApplet.this.getMassiveInputDirectory();
				}
				else {
					try {
						inputDir = AOUIFactory.getLoadFiles(
								AppletMessages.getString("SignApplet.187"), null, null, null, //$NON-NLS-1$
								null, true, false, null, SignApplet.this)[0].getAbsolutePath();
					}
					catch(final AOCancelledOperationException e) {
						getLogger().info("Operacion cancelada por el usuario"); //$NON-NLS-1$
						return Boolean.FALSE;
					}
				}

				// Establecemos el directorio de salida, si no existe se
				// creara un directorio temporal
				// al que poder acceder para la posterior lectura de los
				// datos de firma
				String outputDir;
				if (SignApplet.this.getMassiveOutputDirectory() != null) {
					outputDir = SignApplet.this.getMassiveOutputDirectory();
				}
				else {
					getLogger().warning("No se ha indicado un directorio para el guardado de los firmas generadas, se almacenaran en el mismo directorio de entrada: " + inputDir); //$NON-NLS-1$
					outputDir = inputDir;
				}

				// Configuramos el certificado
				final PrivateKeyEntry ke = SignApplet.this.configureCertificate();
				if (ke == null) {
					getLogger().severe("Error en la seleccion del certificado"); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				// Creamos el manejador de firma masiva
				final DirectorySignatureHelper massiveSigner;
				try {
					massiveSigner = new DirectorySignatureHelper(SignApplet.this.getSigAlgo(), SignApplet.this.getSigFormat(), SignApplet.this.getSigMode());
				}
				catch (final Exception e) {
					getLogger().severe("No se pudo inicializar el modulo de firma masiva: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.200")); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				// Le introducimos el filtro al manejador de firma
				// masiva
				if (SignApplet.this.getMassiveExtFiltered() != null && SignApplet.this.getMassiveExtFiltered().length > 0) {
					final StringBuilder description = new StringBuilder(AppletMessages.getString("SignApplet.201")); //$NON-NLS-1$
					for (int i = 0; i < SignApplet.this.getMassiveExtFiltered().length; i++) {
						description.append("*.").append(SignApplet.this.getMassiveExtFiltered()[i]); //$NON-NLS-1$
						if (i + 1 != SignApplet.this.getMassiveExtFiltered().length) {
							description.append(","); //$NON-NLS-1$
						}
					}
					massiveSigner.setFileFilter(new ExtFilter(SignApplet.this.getMassiveExtFiltered(), description.toString()));
				}

				// Configuramos la operacion
				SignApplet.this.getGenericConfig().setProperty("format", SignApplet.this.getSigFormat()); //$NON-NLS-1$
				SignApplet.this.getGenericConfig().setProperty("mode", SignApplet.this.getSigMode()); //$NON-NLS-1$
				SignApplet.this.configurePolicy();
				SignApplet.this.configureXMLTransforms();

				boolean allOk = true;
				try {
					allOk =
						massiveSigner.massiveSign(SignApplet.this.getMassiveOperation(),
								inputDir,
								SignApplet.this.isRecursiveSignDir(),
								outputDir,
								true,
								SignApplet.this.isOriginalFormat(),
								ke,
								SignApplet.this.getGenericConfig());
				}
				catch (final Exception e) {
					getLogger().severe("Error grave durante la operacion de firma masiva: " + e); //$NON-NLS-1$
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
	@Override
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
		if (this.massiveSignatureHelper != null && this.massiveSignatureHelper.isEnabled()) {
			this.massiveSignatureHelper.setMassiveOperation(this.massiveOperation);
		}
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
		if (this.data == null || this.hash != null && SignApplet.this.sigMode.equals(AOSignConstants.SIGN_MODE_EXPLICIT)) {

			// Si no, comprobamos si se nos ha introducido un hash para firmar
			if (this.hash == null) {

				// Si no, comprobamos si se nos ha indicado un fichero de
				// entrada
				if (this.fileUri == null) {

					// Si no, le pedimos al usuario que seleccione un fichero y
					// lo configuramos
					final String fileName = AOUIFactory.getLoadFiles(
						AppletMessages.getString("SignApplet.356"), //$NON-NLS-1$
						null,
						null,
						null,
						null,
						false,
						false,
						null, // Icono
						this
					)[0].getAbsolutePath();
					if (fileName == null) {
						LOGGER.severe("Se ha cancelado la seleccion del fichero de entrada"); //$NON-NLS-1$
						this.setError(AppletMessages.getString("SignApplet.212")); //$NON-NLS-1$
						throw new AOException("Se ha cancelado la seleccion del fichero de entrada, se cancelara toda la operacion" //$NON-NLS-1$
						);
					}
					this.fileUri = fileName;
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
				catch (final OutOfMemoryError e) {
					LOGGER.severe("Error de falta de memoria al cargar los datos de entrada: " + e); //$NON-NLS-1$
					this.setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
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

		// Si no se nos ha establecido el MimeType y la entrada son datos o un fichero lo analizamos
		// para identificarlo
		if (!this.genericConfig.containsKey(EXTRAPARAM_KEY_MIMETYPE) || MimeHelper.DEFAULT_MIMETYPE.equals(this.genericConfig.getProperty(EXTRAPARAM_KEY_MIMETYPE))) {
			if (this.data != null || this.fileUri != null) {
				try {
					analizeMimeType(tempData);
				} catch (final IOException e) {
					LOGGER.severe("Error al leer los datos: " + e); //$NON-NLS-1$
					this.setError(AppletMessages.getString("SignApplet.407")); //$NON-NLS-1$
					throw new AOException(e.getMessage(), e);
				}
				catch (final OutOfMemoryError e) {
					LOGGER.severe("Error de falta de memoria al analizar los datos de entrada: " + e); //$NON-NLS-1$
					this.setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
					throw new AOException(e.getMessage(), e);
				}
			}
			else {
				this.genericConfig.setProperty(EXTRAPARAM_KEY_MIMETYPE, ("hash/" + DEFAULT_MESSAGE_DIGEST_ALGORITHM).toLowerCase()); //$NON-NLS-1$
			}
		}
		return tempData;
	}

	/** Analiza datos para establecer su MimeType y la descripci&oacute;n textual
	 * de su tipo.
	 * @param dataContent
	 *        Contenidos que se desean analizar.
	 * @throws IOException Cuando ocurre alg&uacute;n error al leer los datos.
	 */
	private void analizeMimeType(final byte[] dataContent) throws IOException {
		// Intentamos extraer el mimetype y su descripcion
		if (dataContent != null) {
			final MimeHelper mtHelper = new MimeHelper(dataContent);
			this.genericConfig.setProperty(EXTRAPARAM_KEY_MIMETYPE, mtHelper.getMimeType());
		}
	}

	// ==============================================
	// Funcionalidades de multifirma masiva programatica
	// ==============================================

	/** Manejador de firma masiva. */
	private MassiveSignatureHelper massiveSignatureHelper = null;

	MassiveSignatureHelper getMassiveSignatureHelper() {
		return this.massiveSignatureHelper;
	}

	void setMassiveSignatureHelper(final MassiveSignatureHelper msh) {
		this.massiveSignatureHelper = msh;
	}

	/** {@inheritDoc} */
	@Override
	public boolean initMassiveSignature() {
		LOGGER.info("Invocando initMassiveSignature"); //$NON-NLS-1$

		// Desactivamos la configuracion de error actual
		this.setError(null);

		// Anotamos que la proxima firma masiva de fichero sera la primera
		this.firstMassiveFile = true;

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			/** {@inheritDoc} */
			@Override
			public Boolean run() {

				// Configuramos el certificado
				final PrivateKeyEntry ke = SignApplet.this.configureCertificate();
				if (ke == null) {
					getLogger().severe("Error en la seleccion del certificado"); //$NON-NLS-1$
					return Boolean.FALSE;
				}

				// Configuramos el entorno
				SignApplet.this.configurePolicy();
				SignApplet.this.configureXMLTransforms();

				// Establecemos la configuracion que se usara para la
				// firma masiva
				final MassiveSignConfiguration massiveConfiguration = new MassiveSignConfiguration(ke);
				massiveConfiguration.setExtraParams(SignApplet.this.getGenericConfig());
				massiveConfiguration.setAlgorithm(SignApplet.this.getSigAlgo());
				massiveConfiguration.setDefaultFormat(SignApplet.this.getSigFormat());
				massiveConfiguration.setMode(SignApplet.this.getSigMode());
				massiveConfiguration.setOriginalFormat(SignApplet.this.isOriginalFormat());
				massiveConfiguration.setMassiveOperation(
						SignApplet.this.getMassiveOperation() != null ?
								SignApplet.this.getMassiveOperation() :
									MassiveType.valueOf(AOSignConstants.DEFAULT_MASSIVE_OPERATION));

				try {
					SignApplet.this.setMassiveSignatureHelper(new MassiveSignatureHelper(massiveConfiguration));
				}
				catch (final AOException e) {
					getLogger().severe("Error al inicializar el modulo de multifirma masiva: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.8")); //$NON-NLS-1$
					return Boolean.FALSE;
				}
				return Boolean.TRUE;
			}
		}).booleanValue();
	}

	/** {@inheritDoc} */
	@Override
	public void endMassiveSignature() {
		LOGGER.info("Invocando endMassiveSignature"); //$NON-NLS-1$
		if (this.massiveSignatureHelper == null || !this.massiveSignatureHelper.isEnabled()) {
			LOGGER.warning("No se habia inicializado la operacion de firma masiva"); //$NON-NLS-1$
			return;
		}
		this.massiveSignatureHelper.disable();
	}

	/** {@inheritDoc} */
	@Override
	public String massiveSignatureData(final String b64Data) {
		LOGGER.info("Invocando massiveSignatureData"); //$NON-NLS-1$
		this.setError(null);
		if (this.massiveSignatureHelper == null || !this.massiveSignatureHelper.isEnabled()) {
			this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
			return null;
		}
		if (b64Data == null) {
			this.setError(AppletMessages.getString("SignApplet.89")); //$NON-NLS-1$
			return null;
		}
		final byte[] dataToSign;
		try {
			dataToSign = Base64.decode(b64Data);
		} catch (final Exception e1) {
			this.setError(AppletMessages.getString("SignApplet.490")); //$NON-NLS-1$
			return null;
		}


		// Ejecutamos la operacion
		try {
			return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
				/** {@inheritDoc} */
				@Override
				public String run() {

					// Establecemos el formato de firma para las operaciones de firma masiva
					SignApplet.this.getMassiveSignatureHelper().setSignatureFormat(SignApplet.this.getSigFormat());

					final byte[] result = SignApplet.this.getMassiveSignatureHelper().signData(dataToSign);
					if (result == null) {
						SignApplet.this.setError(SignApplet.this.getMassiveSignatureHelper().getCurrentLogEntry());
					}
					return Base64.encode(result);
				}
			});
		} catch (final Exception e) {
			this.setError(AppletMessages.getString("SignApplet.205")); //$NON-NLS-1$
			return null;
		}
	}

	/** {@inheritDoc} */
	@Override
	public String massiveSignatureHash(final String b64Hash) {
		LOGGER.info("Invocando massiveSignatureHash"); //$NON-NLS-1$
		this.setError(null);
		if (this.massiveSignatureHelper == null || !this.massiveSignatureHelper.isEnabled()) {
			this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
			return null;
		}
		if (b64Hash == null) {
			this.setError(AppletMessages.getString("SignApplet.89")); //$NON-NLS-1$
			return null;
		}
		final byte[] dataHash;
		try {
			dataHash = Base64.decode(b64Hash);
		} catch (final Exception e1) {
			this.setError(AppletMessages.getString("SignApplet.491")); //$NON-NLS-1$
			return null;
		}

		try {
			return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
				/** {@inheritDoc} */
				@Override
				public String run() {

					// Establecemos el formato de firma para las operaciones de firma masiva
					SignApplet.this.getMassiveSignatureHelper().setSignatureFormat(SignApplet.this.getSigFormat());

					final byte[] result = SignApplet.this.getMassiveSignatureHelper().signHash(dataHash);
					if (result == null) {
						SignApplet.this.setError(SignApplet.this.getMassiveSignatureHelper().getCurrentLogEntry());
						return null;
					}
					return Base64.encode(result);
				}
			});
		} catch (final Exception e) {
			this.setError(AppletMessages.getString("SignApplet.205")); //$NON-NLS-1$
			return null;
		}
	}

	/** {@inheritDoc} */
	@Override
	public String massiveSignatureFile(final String filename) {

		LOGGER.info("Invocando massiveSignatureFile: " + filename); //$NON-NLS-1$
		this.setError(null);
		if (this.massiveSignatureHelper == null || !this.massiveSignatureHelper.isEnabled()) {
			this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
			return null;
		}

		if (this.firstMassiveFile && !checkUserPermision(AppletMessages.getString("SignApplet.1") + //$NON-NLS-1$
				CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.497")); //$NON-NLS-1$
			return null;
		}
		this.firstMassiveFile = false;

		if (filename == null || "".equals(filename)) { //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.48")); //$NON-NLS-1$
			return null;
		}

		try {
			return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
				/** {@inheritDoc} */
				@Override
				public String run() {

					// Establecemos el formato de firma para las operaciones de firma masiva
					SignApplet.this.getMassiveSignatureHelper().setSignatureFormat(SignApplet.this.getSigFormat());

					final byte[] result = SignApplet.this.getMassiveSignatureHelper().signFile(filename);
					if (result == null){
						SignApplet.this.setError(SignApplet.this.getMassiveSignatureHelper().getCurrentLogEntry());
					}
					return Base64.encode(result);
				}
			});
		} catch (final Exception e) {
			this.setError(AppletMessages.getString("SignApplet.205")); //$NON-NLS-1$
			return null;
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getMassiveSignatureCurrentLog() {
		LOGGER.info("Invocando getMassiveSignatureCurrentLog"); //$NON-NLS-1$
		this.setError(null);
		if (this.massiveSignatureHelper == null || !this.massiveSignatureHelper.isEnabled()) {
			this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
			return null;
		}
		return this.massiveSignatureHelper.getCurrentLogEntry();
	}

	/** {@inheritDoc} */
	@Override
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
	@Override
	public void saveMassiveSignatureLog() {
		LOGGER.info("Invocando saveMassiveSignatureLog"); //$NON-NLS-1$
		this.setError(null);
		if (this.massiveSignatureHelper == null) {
			this.setError(AppletMessages.getString("SignApplet.381")); //$NON-NLS-1$
			return;
		}

		// Guardamos la firma en fichero
		AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
			/** {@inheritDoc} */
			@Override
			public Void run() {
				SignApplet.this.saveFileAsinchronously(
						SignApplet.this.getMassiveSignatureLog().getBytes(),
						AppletMessages.getString("SignApplet.501"), //$NON-NLS-1$
						SignApplet.this.getOutputFile(), new String[] {"txt"}, //$NON-NLS-1$
						AppletMessages.getString("SignApplet.383") //$NON-NLS-1$
				);
				return null;
			}
		});
	}

	/** {@inheritDoc} */
	@Override
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

	private static void setLookAndFeel() {
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
	@Override
	public boolean isInitialized() {
		LOGGER.info("Invocando isInitialized"); //$NON-NLS-1$
		return this.initializedApplet;
	}

	/** {@inheritDoc} */
	@Override
	public boolean signData(final String b64data) {
		LOGGER.info("Invocando signData"); //$NON-NLS-1$
		if (b64data == null) {
			LOGGER.severe("No se han introducido los datos que se desean firmar"); //$NON-NLS-1$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.278")); //$NON-NLS-1$
			return false;
		}
		try {
			this.data = Base64.decode(b64data);
		} catch (final Exception e1) {
			this.setError(AppletMessages.getString("SignApplet.490")); //$NON-NLS-1$
			return false;
		}
		return sign();
	}

	/** {@inheritDoc} */
	@Override
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
	@Override
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
			return ""; //$NON-NLS-1$
		}
		catch (final OutOfMemoryError e) {
			LOGGER.severe("Error de falta de memoria al recuperar la firma electronica: " + e); //$NON-NLS-1$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
			return ""; //$NON-NLS-1$
		}

		try {
			return sign == null ? null : Base64.encode(sign);
		} catch (final OutOfMemoryError e) {
			LOGGER.severe("Error de falta de memoria al recuperar la firma electronica: " + e); //$NON-NLS-1$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
			return ""; //$NON-NLS-1$
		}
	}

	/** {@inheritDoc} */
	@Override
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
			return sign == null ?
					null : charsetName == null ?
							new String(sign) : new String(sign, charsetName);
		} catch (final UnsupportedEncodingException e) {
			LOGGER.warning("Codificacion no soportada (" + charsetName + //$NON-NLS-1$
			"), se devolvera la firma con la codificacion por defecto"); //$NON-NLS-1$
			return new String(sign);
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getFilePath() {
		LOGGER.info("Invocando getFilePath"); //$NON-NLS-1$
		if (this.outputFile == null) {
			LOGGER.warning("No se dispone de la direccion del fichero de firma, se devolvera una cadena vacia"); //$NON-NLS-1$
			return ""; //$NON-NLS-1$
		}
		return this.outputFile;
	}

	/** {@inheritDoc} */
	@Override
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
			path = path.substring("file://".length()); //$NON-NLS-1$
		}

		return path;
	}

	/** {@inheritDoc} */
	@Override
	public String getErrorMessage() {
		LOGGER.info("Invocando getErrorMessage"); //$NON-NLS-1$
		return this.error ? this.errorMsg : ""; //$NON-NLS-1$
	}

	/** {@inheritDoc} */
	@Override
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
		this.setError(errorMsg, false);
	}

	/** Establece un mensaje de error y almacena que este se ha producido o, en
	 * caso de introducirse <code>null</code> o cadena vac&iacute;a, se indica
	 * que no hay error. Tambi&eacute;n muestra este error por consola siempre y
	 * cuando no sea nulo. En caso de estar configurado el cliente para mostrar
	 * los errores, se mostrar&aacute; una ventana modal al usuario con el error
	 * establecido.
	 * @param errorMsg
	 *        Mensaje de error.
	 * @param forceToShow
	 * 		  Obliga a que se muestre el error mediante un di&aacute;logo modal.
	 *             */
	void setError(final String errorMsg, final boolean forceToShow) {
		if (errorMsg == null || errorMsg.length() < 1) {
			this.error = false;
			this.errorMsg = ""; //$NON-NLS-1$
		}
		else {
			this.error = true;
			this.errorMsg = errorMsg;
		}

		// Mostramos, si procede, el mensaje de error que corresponda
		if (this.error && (forceToShow || this.showErrors)) {
			AOUIFactory.showMessageDialog(
				this,
				this.errorMsg,
				AppletMessages.getString("SignApplet.156"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
		}
	}

	/** {@inheritDoc} */
	@Override
	@Deprecated
	public String getTextFileContent(final String filename) {
		LOGGER.info("Invocando getTextFileContent: " + filename); //$NON-NLS-1$
		return getTextFileContent(filename, null);
	}

	/** {@inheritDoc} */
	@Override
	public String getTextFileContent(final String filename, final String charset) {
		LOGGER.info("Invocando getTextFileContent: " + filename + " : " + charset); //$NON-NLS-1$ //$NON-NLS-2$

		setError(null);

		if (filename == null || filename.trim().length() < 1) {
			setError(AppletMessages.getString("SignApplet.54")); //$NON-NLS-1$
			return null;
		}

		if (!normalizePath(filename).equals(this.userSelectedPath)) {
			if (!checkUserPermision(
					AppletMessages.getString("SignApplet.19") + CR + //$NON-NLS-1$
						AOFileUtils.pathLengthShortener(filename, MAX_PATHNAME_LENGTH) + CR +
							AppletMessages.getString("SignApplet.12") //$NON-NLS-1$
			)) {
				setError(AppletMessages.getString("SignApplet.494", filename)); //$NON-NLS-1$
				return null;
			}
		}

		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			/** {@inheritDoc} */
			@Override
			public String run() {

				String resultText;
				byte[] result = null;
				try {
					final InputStream is = AOUtil.loadFile(AOUtil.createURI(filename));
					result = AOUtil.getDataFromInputStream(is);
					if (charset == null) {
						resultText = new String(result);
					} else {
						resultText = new String(result, charset);
					}
					is.close();
				}
				catch (final UnsupportedEncodingException e) {
					getLogger().warning("Codificacion no soportada (" + charset + //$NON-NLS-1$
							"), se devolvera la firma con la codificacion por defecto"); //$NON-NLS-1$
					resultText = new String(result);
				}
				catch (final Exception e) {
					getLogger().severe("El fichero indicado no existe o no es posible acceder a el: " + e); //$NON-NLS-1$
					setError(AppletMessages.getString("SignApplet.407")); //$NON-NLS-1$
					return null;
				}
				return resultText;
			}
		});
	}

	/** {@inheritDoc} */
	@Override
	public String getTextFromBase64(final String b64) {
		return getTextFromBase64(b64, null);
	}

	/** {@inheritDoc} */
	@Override
	public String getTextFromBase64(final String b64, final String charsetName) {
		LOGGER.info("Invocando getTextFromBase64"); //$NON-NLS-1$
		Charset charset = null;
		try {
			charset = Charset.forName(charsetName);
		} catch (final Exception e) {
			LOGGER.warning("Codificacion no soportada (" + charsetName + //$NON-NLS-1$
			"), se utilizara la codificacion por defecto"); //$NON-NLS-1$
		}
		try {
			if (charset == null) {
				return new String(Base64.decode(b64));
			}
			return new String(Base64.decode(b64), charset);
		} catch (final Exception e2) {
			this.setError(AppletMessages.getString("SignApplet.492")); //$NON-NLS-1$
			return null;
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getBase64FromText(final String plainText) {
		return getBase64FromText(plainText, null);
	}

	/** {@inheritDoc} */
	@Override
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
	@Override
	public String getFileBase64Encoded(final String filename, final boolean showProgress) {
		LOGGER.info("Invocando getFileBase64Encoded: " + filename); //$NON-NLS-1$

		setError(null);

		if (filename == null || filename.trim().length() < 1) {
			setError(AppletMessages.getString("SignApplet.58")); //$NON-NLS-1$
			return null;
		}

		if (!normalizePath(filename).equals(this.userSelectedPath)) {
			if (!checkUserPermision(
				AppletMessages.getString("SignApplet.19") + CR + //$NON-NLS-1$
					AOFileUtils.pathLengthShortener(filename, MAX_PATHNAME_LENGTH) + CR +
						AppletMessages.getString("SignApplet.12") //$NON-NLS-1$
			)) {
				setError(AppletMessages.getString("SignApplet.494", filename)); //$NON-NLS-1$
				return null;
			}
		}

		try {
			return getFileBase64Encoded(AOUtil.createURI(filename));
		}
		catch (final Exception e) {
			LOGGER.severe("Error al leer el fichero '" + filename + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			setError(AppletMessages.getString("SignApplet.81") + filename); //$NON-NLS-1$
			return null;
		}
		catch (final OutOfMemoryError e) {
			LOGGER.severe("Error de falta de memoria al recuperar los datos: " + e); //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
			return null;
		}
	}

	/** {@inheritDoc} */
	@Override
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
		catch (final OutOfMemoryError e) {
			LOGGER.severe("Error de falta de memoria al recuperar los datos: " + e); //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
			return null;
		}
	}

	/** Recupera el contenido de un fichero codificado en Base64.
	 * @param uri Ruta del fichero.
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

		setError(null);
		return this.fileBase64 ? new String(fileContent) : Base64.encode(fileContent);
	}

	/** {@inheritDoc} */
	@Override
	public String getFileHashBase64Encoded() {
		LOGGER.info("Invocando getFileHashBase64Encoded"); //$NON-NLS-1$
		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			/** {@inheritDoc} */
			@Override
			public String run() {
				if (SignApplet.this.getInternalFileUri() == null) {
					getLogger().severe("No se ha establecido el fichero del que calcular el Hash"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.348")); //$NON-NLS-1$
					return null;
				}
				final byte[] tmpData;
				try {
					final InputStream is = AOUtil.loadFile(AOUtil.createURI(SignApplet.this.getInternalFileUri()));
					tmpData = AOUtil.getDataFromInputStream(is);
					is.close();
				}
				catch (final Exception e) {
					getLogger().severe("Error al leer el fichero '" + SignApplet.this.getInternalFileUri() + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
					setError(AppletMessages.getString("SignApplet.85")); //$NON-NLS-1$
					return null;
				}
				final byte[] binaryData;
				try {
					if (SignApplet.this.isFileBase64()) {
						binaryData = Base64.decode(new String(tmpData));
					}
					else {
						binaryData = tmpData;
					}
				}
				catch (final IOException e) {
					getLogger().severe("Error en la decodificacion Base64 del fichero " + SignApplet.this.getInternalFileUri() + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.407") + SignApplet.this.getInternalFileUri()); //$NON-NLS-1$
					return null;
				}
				final String digestAlg = AOSignConstants.getDigestAlgorithmName(SignApplet.this.getSigAlgo());
				try {
					return Base64.encode(CryptoUtils.getMessageDigest(binaryData, digestAlg));
				}
				catch (final NoSuchAlgorithmException e) {
					getLogger().severe("El algoritmo de hash '" + digestAlg + "' no esta soportado: " + e); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.464") + digestAlg); //$NON-NLS-1$
					return null;
				}
			}
		});
	}

    /** {@inheritDoc} */
	@Override
	public String showCertSelectionDialog() {
		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			/** {@inheritDoc} */
			@Override
			public String run() {
				// Configuramos el certificado
				final PrivateKeyEntry ke = SignApplet.this.configureCertificate();
				if (ke == null) {
					getLogger().severe("Error en la seleccion del certificado"); //$NON-NLS-1$
					return null;
				}
				return SignApplet.this.getKsConfigManager().getSelectedAlias();
			}
		});
	}

	/** {@inheritDoc} */
	@Override
	public void setDataMimeType(final String mimeType) {
		LOGGER.info("Invocando setDataMimeType: " + mimeType); //$NON-NLS-1$

		if (mimeType != null && mimeType.length() > 0) {
			this.genericConfig.setProperty(EXTRAPARAM_KEY_MIMETYPE, mimeType);
		}
		else {
			this.genericConfig.remove(EXTRAPARAM_KEY_MIMETYPE);
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getB64Data() {
		LOGGER.info("Invocando getB64Data"); //$NON-NLS-1$
		if (this.data == null) {
			LOGGER.warning("No se dispone de datos de salida, se devolvera cadena vacia"); //$NON-NLS-1$
			return "";  //$NON-NLS-1$
		}
		try {
			return Base64.encode(this.data);
		} catch (final OutOfMemoryError e) {
			LOGGER.severe("Error de falta de memoria al recuperar los datos: " + e); //$NON-NLS-1$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
			return ""; //$NON-NLS-1$
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getData() {
		LOGGER.info("Invocando getData"); //$NON-NLS-1$
		if (this.data == null) {
			LOGGER.warning("No se dispone de datos de salida, se devolvera cadena vacia"); //$NON-NLS-1$
		}
		try {
			return this.data == null ? "" : new String(this.data); //$NON-NLS-1$
		} catch (final OutOfMemoryError e) {
			LOGGER.severe("Error de falta de memoria al recuperar los datos: " + e); //$NON-NLS-1$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
			return ""; //$NON-NLS-1$
		}
	}

	/** {@inheritDoc} */
	@Override
	public void setInRecursiveDirectorySign(final boolean recursiveSignDir) {
		LOGGER.info("Invocando setInRecursiveDirectorySign: " + recursiveSignDir); //$NON-NLS-1$
		this.recursiveSignDir = recursiveSignDir;
	}

	/** {@inheritDoc} */
	@Override
	public void setInputDirectoryToSign(final String directory) {
		LOGGER.info("Invocando setInputDirectoryToSign: " + directory); //$NON-NLS-1$

		setError(null);

		if (directory != null && directory.trim().length() > 0 && !checkUserPermision(
				AppletMessages.getString("SignApplet.103") + CR + //$NON-NLS-1$
				AOFileUtils.pathLengthShortener(directory, MAX_PATHNAME_LENGTH) +
				CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.495", directory)); //$NON-NLS-1$
			return;
		}

		this.massiveInputDirectory = directory;
	}

	/** {@inheritDoc} */
	@Override
	public String getInputDirectoryToSign() {
		LOGGER.info("Invocando getInputDirectoryToSign"); //$NON-NLS-1$
		return this.massiveInputDirectory;
	}

	/** {@inheritDoc} */
	@Override
	public void setOutputDirectoryToSign(final String directory) {
		LOGGER.info("Invocando setOutputDirectoryToSign: " + directory); //$NON-NLS-1$

		setError(null);

		if (directory != null && directory.trim().length() > 0 && !checkUserPermision(
				AppletMessages.getString("SignApplet.105") + CR + //$NON-NLS-1$
				AOFileUtils.pathLengthShortener(directory, MAX_PATHNAME_LENGTH) +
				CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.495", directory)); //$NON-NLS-1$
			return;
		}
		this.massiveOutputDirectory = directory;
	}

	/** {@inheritDoc} */
	@Override
	public String getOutputDirectoryToSign() {
		LOGGER.info("Invocando getOutputDirectoryToSign"); //$NON-NLS-1$
		return this.massiveOutputDirectory;
	}

	/** {@inheritDoc} */
	@Override
	public void setOriginalFormat(final boolean originalFormat) {
		LOGGER.info("Invocando setOriginalFormat: " + originalFormat); //$NON-NLS-1$
		this.originalFormat = originalFormat;
	}

	// *************************************************************************
	// ******************** METODOS PUBLICOS DEPRECADOS ************************
	// *************************************************************************
	/** {@inheritDoc} */
	@Override
	public void setShowErrors(final boolean showErrors) {
		LOGGER.info("Invocando setShowErrors: " + showErrors); //$NON-NLS-1$
		this.showErrors = showErrors;
	}

	/** {@inheritDoc} */
	@Override
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
		return AOUIFactory.OK_OPTION == AOUIFactory.showConfirmDialog(
			this,
			hashData != null ? AppletMessages.getString("SignApplet.655") + CR + digestAlgo + ": " + hashData : AppletMessages.getString("SignApplet.657"), //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			AppletMessages.getString("SignApplet.658"), //$NON-NLS-1$
			AOUIFactory.OK_CANCEL_OPTION,
			AOUIFactory.WARNING_MESSAGE
		);
	}

	/** {@inheritDoc} */
	@Override
	public boolean addSignedAttribute(final String oid, final String value) {
		LOGGER.info("Invocando addSignedAttribute: " + oid + " = " + value); //$NON-NLS-1$ //$NON-NLS-2$

		// Comprobaciones de seguridad
		if (oid == null || value == null) {
			LOGGER.severe("Ni el OID ni el valor del atributo firmado a agregar pueden ser nulos"); //$NON-NLS-1$
			this.setError(AppletMessages.getString("SignApplet.690")); //$NON-NLS-1$
			return false;
		}

		// Creamos primeramente el listado de atributos si no lo esta ya
		if (this.signedAttributes == null) {
			this.signedAttributes = new HashMap<String, String>();
		}

		final String newOid = oid;

		// Comprobamos que el OID no estuviese ya agregado
		if (this.signedAttributes.containsKey(newOid)) {
			LOGGER.warning("Ya existia un atributo con el OID especificado, se sustituira su valor por el nuevo"); //$NON-NLS-1$
		}

		// Agregamos el nuevo atributo
		this.signedAttributes.put(newOid, value);

		return true;
	}

	/** {@inheritDoc} */
	@Override
	public boolean removeSignedAttribute(final String oid) {
		LOGGER.info("Invocando removeSignedAttribute: " + oid); //$NON-NLS-1$

		// Comprobamos que el oid no sea nulo
		if (oid == null) {
			LOGGER.severe("El OID del atributo firmado que se desea eliminar no puede ser nulo"); //$NON-NLS-1$
			this.setError(AppletMessages.getString("SignApplet.698")); //$NON-NLS-1$
			return false;
		}

		final String oidToRemove = oid;

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
	@Override
	public boolean addUnsignedAttribute(final String oid, final String value) {
		LOGGER.info("Invocando addUnsignedAttribute: " + oid + " = " + value); //$NON-NLS-1$ //$NON-NLS-2$

		// Comprobaciones de seguridad
		if (oid == null || value == null) {
			LOGGER.severe("Ni el OID ni el valor del atributo no firmado a agregar pueden ser nulos"); //$NON-NLS-1$
			this.setError(AppletMessages.getString("SignApplet.690")); //$NON-NLS-1$
			return false;
		}

		// Creamos primeramente el listado de atributos si no lo esta ya
		if (this.unsignedAttributes == null) {
			this.unsignedAttributes = new HashMap<String, List<String>>();
		}

		final String newOid = oid;

		// Agregamos el valor del atributo, teniendo en cuenta que el OID
		// especificado ya podria tener otros atributos asignados
		List<String> values = null;
		if (this.unsignedAttributes.containsKey(newOid)) {
			values = this.unsignedAttributes.get(newOid);
		}
		else {
			values = new ArrayList<String>();
		}
		values.add(value);

		// Agregamos el atributo con todos sus valores
		this.unsignedAttributes.put(newOid, values);

		return true;
	}

	/** {@inheritDoc} */
	@Override
	public boolean removeUnsignedAttribute(final String oid, final String value) {
		LOGGER.info("Invocando removeUnsignedAttribute: " + oid); //$NON-NLS-1$

		// Comprobamos que el oid no sea nulo
		if (oid == null) {
			LOGGER.severe("El OID del atributo no firmado que se desea eliminar no puede ser nulo"); //$NON-NLS-1$
			this.setError(AppletMessages.getString("SignApplet.698")); //$NON-NLS-1$
			return false;
		}

		final String oidToRemove = oid;

		// Comprobamos que el atributo exista y si tiene mas valores asignados
		// para eliminar lo que corresponda
		if (this.unsignedAttributes != null && this.unsignedAttributes.containsKey(oidToRemove)) {
			final List<String> values = this.unsignedAttributes.get(oidToRemove);
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
	@Override
	public void addExtraParam(final String key, final String value) {
		LOGGER.info("Invocando addExtraParam: " + key); //$NON-NLS-1$

		// Si no se ha indicado una clave o valor, abortamos la operacion
		if (key == null || value == null) {
			return;
		}

		// Establecemos la propiedad
		this.genericConfig.setProperty(key, value);
	}

	/** {@inheritDoc} */
	@Override
	public void removeExtraParam(final String key) {
		LOGGER.info("Invocando removeExtraParam: " + key); //$NON-NLS-1$

		// Si no se ha indicado una clave, abortamos la operacion
		if (key == null) {
			return;
		}

		// Eliminamos la propiedad
		this.genericConfig.remove(key);
	}

	/** {@inheritDoc} */
	@Override
	public void addXMLTransform(final String type, final String subtype, final String body) {
		LOGGER.info("Invocando addXMLTransform"); //$NON-NLS-1$
		if (this.xmlTransforms == null) {
			this.xmlTransforms = new ArrayList<AOXMLTransform>();
		}

		this.xmlTransforms.add(new AOXMLTransform(type, subtype, body));
	}

	/** {@inheritDoc} */
	@Override
	public void resetXMLTransforms() {
		LOGGER.info("Invocando resetXMLTransforms"); //$NON-NLS-1$
		this.xmlTransforms = null;
	}

	/** {@inheritDoc} */
	@Override
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
	@Override
	public String loadFilePath(final String title, final String exts, final String description) {
		LOGGER.info("Invocando loadFilePath"); //$NON-NLS-1$
		try {
			final String selectedPath = AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
				/** {@inheritDoc} */
				@Override
				public String run() {
					return AOUIFactory.getLoadFiles(
						title,
						null,
						null,
						exts == null || exts.trim().length() == 0
							? null
								: AOUtil.split(exts, SignApplet.STRING_SEPARATOR),
						description,
						false,
						false,
						null, // Icono
						SignApplet.this
					)[0].getAbsolutePath();
				}
			});
			this.userSelectedPath = normalizePath(selectedPath);
			return this.userSelectedPath;
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

	/**
	 * Normaliza las barras de las rutas para evitar que fallen comparaciones con otras que s&oacute;lo
	 * se diferencian en ellas.
	 * @param path Ruta de fichero.
	 * @return Ruta normalizada.
	 */
	private static String normalizePath(final String path) {
		return path.replace('/', '\\');
	}

	/** {@inheritDoc} */
	@Override
	public String selectDirectory() {
		LOGGER.info("Invocando selectDirectory"); //$NON-NLS-1$
		try {
			return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
				/** {@inheritDoc} */
				@Override
				public String run() {
					return AOUIFactory.getLoadFiles(
							AppletMessages.getString("SignApplet.104"), null, null, null,  //$NON-NLS-1$
							null, true, false, null, SignApplet.this)[0].getAbsolutePath();
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
	@Override
	public X509Certificate getSignCertificate() {
		LOGGER.info("Invocando getSignCertificate"); //$NON-NLS-1$
		X509Certificate cert = null;
		try {
			cert = this.ksConfigManager.getSelectedCertificate();
		}
		catch (final AOKeyStoreManagerException e) {
			LOGGER.warning("No se ha inicializado el almacen de certificados: " + e); //$NON-NLS-1$
			return null;
		}
		catch (final AOKeystoreAlternativeException e) {
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
	 * @param c Componente del cual se desea obtener el padre.
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
	 * @param strings Cadenas de entrada.
	 * @param delimitator Delimitador a usar.
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
	@Override
	@Deprecated
	public void setCertFilterRFC2254(final String subjectFilter, final String issuerFilter, final boolean onlySignatureCertificates) {
		LOGGER.info("Invocando setCertFilterRFC2254"); //$NON-NLS-1$
		this.addRFC2254CertificateFilter(subjectFilter, issuerFilter, onlySignatureCertificates);
		this.ksConfigManager.setMandatoryCert(false);
	}

	/** {@inheritDoc} */
	@Override
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
				LOGGER.severe("No se puede identificar el algoritmo de la clave: " + e); //$NON-NLS-1$
				this.setError(AppletMessages.getString("SignApplet.114")); //$NON-NLS-1$
			}
			catch (final UnrecoverableEntryException e) {
				LOGGER.severe("Error al extraer la clave privada del certificado: " + e); //$NON-NLS-1$
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
				else {

					if (AOUIFactory.YES_OPTION == AOUIFactory.showConfirmDialog(
							SignApplet.this,
							AppletMessages.getString("SignApplet.82", this.ksConfigManager.getKeyStore().getName(), kst.getName()), //$NON-NLS-1$
							AppletMessages.getString("SignApplet.658"), //$NON-NLS-1$
							AOUIFactory.YES_NO_OPTION,
							AOUIFactory.WARNING_MESSAGE
							)) {
						setKeyStore(null, null, kst.toString());
						configureCertificate();
					}
					else {
						LOGGER.severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					}
				}
			}
		}
		return this.ksConfigManager.getCertificateKeyEntry();
	}

	/** Recupera la huella digital en hexadecimal de los datos de entrada del applet para
	 * firma.
	 * @param algorithm Algoritmo de huella digital a usar.
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

		return AOUIFactory.showConfirmDialog(
			this,
			message,
			AppletMessages.getString("SignApplet.16"), //$NON-NLS-1$
			AOUIFactory.YES_NO_OPTION,
			AOUIFactory.WARNING_MESSAGE
		) == AOUIFactory.YES_OPTION;
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
	 *        <li>"auto" indica que el programa seleccionar&aacute; autom&aacute;ticamente un certificado. Si se ha indicado uno o m&aacute;s nombres
	 *        de CA mediante los par&aacute;metros caNameN la selecci&oacute;n autom&aacute;tica se limita a los certificado emitidos por las CA
	 *        indicadas.</li>
	 *        <li>"ask" indica que se debe solicitar al usuario que seleccione un certificado. Si se ha indicado uno o m&aacute;s nombres de CA
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
			/** {@inheritDoc} */
			@Override
			public String run() {
				final AOKeyStoreManager ksManager;
				try {
					ksManager = SignApplet.this.getKsConfigManager().getKeyStoreManager();
				}
				catch (final AOCancelledOperationException e) {
					getLogger().severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					return "error:userCancel"; //$NON-NLS-1$
				}
				catch (final AOKeyStoreManagerException e) {
					getLogger().severe("Error inicializando el almacen de claves: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
					return "error:internalError"; //$NON-NLS-1$
				}
				catch (final AOKeystoreAlternativeException e) {
					final AOKeyStore kst = e.getAlternativeKsm();
					if (kst == null) {
						getLogger().severe("Error inicializando el almacen de claves: " + e); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
						return "error:internalError"; //$NON-NLS-1$
					}
					if (AOUIFactory.YES_OPTION == AOUIFactory.showConfirmDialog(
						SignApplet.this,
						AppletMessages.getString("SignApplet.80", kst.getName()), //$NON-NLS-1$
						AppletMessages.getString("SignApplet.658"), //$NON-NLS-1$
						AOUIFactory.YES_NO_OPTION,
						AOUIFactory.WARNING_MESSAGE
					)) {
						setKeyStore(null, null, kst.toString());
						return signText(stringToSign, caOption, caNameN);
					}
					getLogger().severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					return "error:userCancel"; //$NON-NLS-1$
				}

				final SignText signTextComponent =
					new SignText(getArrayCertificatesAlias(),
							ksManager,
							SignApplet.this,
							SignApplet.this.getKsConfigManager().getKeyStore().getCertificatePasswordCallback(SignApplet.this));

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
	 *        <li>"auto" indica que el programa seleccionar&aacute; autom&aacute;ticamente un certificado. Si se ha indicado uno o m&aacute;s nombres
	 *        de CA mediante los par&aacute;metros caNameN la selecci&oacute;n autom&aacute;tica se limita a los certificado emitidos por las CA
	 *        indicadas.</li>
	 *        <li>"ask" indica que se debe solicitar al usuario que seleccione un certificado. Si se ha indicado uno o m&aacute;s nombres de CA
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
	 * @param locale Idioma seg&uacute;n ISO-639. */
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

	/** Establece las propiedades del sistema indicadas como argumentos y separadas por '-D'.
	 * Las propiedades deben establecerse como clave=valor.
	 * @param javaArguments Argumentos de invocaci&oacute;n a la JVM. */
	private static void setSystemProperties(final String javaArguments) {

		LOGGER.info("setSystemProperties: " + javaArguments); //$NON-NLS-1$

		if (javaArguments != null && javaArguments.trim().length() > 0) {
			final String[] properties = javaArguments.split("-D");  //$NON-NLS-1$
			for (final String property : properties) {
				final int equalIndex = property.indexOf('=');
				if (equalIndex != -1) {
					LOGGER.info("Establecemos la propiedad del sistema: " + property.trim()); //$NON-NLS-1$
					System.setProperty(property.substring(0, equalIndex).trim(), property.substring(equalIndex + 1).trim());
				}
			}
		}
	}

	/** Establece la localizaci&oacute;n por defecto de la aplicaci&oacute;n.
	 * @param locale C&oacute;digo de idioma. */
	private static void setLocale(final String locale) {

		LOGGER.info("setLocale: " + locale); //$NON-NLS-1$

		if (locale == null || locale.trim().length() == 0) {
			return;
		}

		final String[] localeComponents = locale.replace('-', '_').split("_"); //$NON-NLS-1$
		if (localeComponents.length == 1) {
			Locale.setDefault(new Locale(localeComponents[0]));
		}
		else if (localeComponents.length == 2) {
			Locale.setDefault(new Locale(localeComponents[0], localeComponents[1]));
		}
		else if (localeComponents.length > 2) {
			Locale.setDefault(new Locale(localeComponents[0], localeComponents[1], localeComponents[2]));
		}
	}

	/** {@inheritDoc} */
	@Override
	public void setCipherData(final String data) {
		LOGGER.info("Invocando setCipherData"); //$NON-NLS-1$
		try {
			this.cipherManager.setCipheredData(data == null ? null : Base64.decode(data));
		} catch (final Exception e) {
			LOGGER.warning("Los datos insertados no estan en Base64: " + e); //$NON-NLS-1$

		} catch (final OutOfMemoryError e) {
			LOGGER.severe("Error de falta de memoria al establecer los datos cifrados: " + e); //$NON-NLS-1$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
		}
	}

	/** {@inheritDoc} */
	@Override
	public void setPlainData(final String data) {
		LOGGER.info("Invocando setPlainData"); //$NON-NLS-1$
		try {
			this.cipherManager.setPlainData(data == null ? null : Base64.decode(data));
		} catch (final Exception e) {
			LOGGER.warning("Los datos insertados no estan en Base64: " + e); //$NON-NLS-1$
			this.data = null;
		} catch (final OutOfMemoryError e) {
			LOGGER.severe("Error de falta de memoria al establecer los datos en claro: " + e); //$NON-NLS-1$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getCipherData() {
		LOGGER.info("Invocando getCipherData"); //$NON-NLS-1$
		try {
			return Base64.encode(this.cipherManager.getCipheredData());
		} catch (final OutOfMemoryError e) {
			LOGGER.severe("Error de falta de memoria al recuperar los datos cifrados: " + e); //$NON-NLS-1$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
			return ""; //$NON-NLS-1$
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getPlainData() {
		LOGGER.info("Invocando getPlainData"); //$NON-NLS-1$
		try {
			return this.cipherManager.getPlainData() == null ?
					null : Base64.encode(this.cipherManager.getPlainData());
		} catch (final OutOfMemoryError e) {
			LOGGER.severe("Error de falta de memoria al establecer los datos cifrados: " + e); //$NON-NLS-1$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
			return ""; //$NON-NLS-1$
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getKey() {
		LOGGER.info("Invocando getKey"); //$NON-NLS-1$
		if (this.cipherManager.getCipherKey() != null) {
			return Base64.encode(this.cipherManager.getCipherKey());
		}
		return null;
	}

	/** {@inheritDoc} */
	@Override
	public void setKey(final String newKey) {
		LOGGER.info("Invocando setKey"); //$NON-NLS-1$
		try {
			this.cipherManager.setCipherKey(newKey == null ? null : Base64.decode(newKey));
		} catch (final Exception e) {
			LOGGER.warning("La clave insertada no esta en Base64: " + e); //$NON-NLS-1$
			this.data = null;
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getPassword() {
		LOGGER.info("Invocando getPassword"); //$NON-NLS-1$
		return this.cipherManager.getCipherPassword() == null ? null : String.valueOf(this.cipherManager.getCipherPassword());
	}

	/** {@inheritDoc} */
	@Override
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
	@Override
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
	@Override
	public String getCipherAlgorithm() {
		LOGGER.info("Invocando getCipherAlgorithm"); //$NON-NLS-1$
		return this.cipherManager.getCipherAlgorithm().getName();
	}

	/** {@inheritDoc} */
	@Override
	public void setKeyMode(final String keyMode) {
		LOGGER.info("Invocando setKeyMode: " + keyMode); //$NON-NLS-1$
		this.cipherManager.setKeyMode(keyMode);
	}

	/** {@inheritDoc} */
	@Override
	public String getKeyMode() {
		LOGGER.info("Invocando getKeyMode"); //$NON-NLS-1$
		return this.cipherManager.getKeyMode();
	}

	/** {@inheritDoc} */
	@Override
	public void setUseCipherKeyStore(final boolean useKeyStore) {
		LOGGER.info("Invocando setUseCipherKeyStore con el valor: " + useKeyStore); //$NON-NLS-1$
		this.cipherManager.setUseCipherKeyStore(useKeyStore);
	}

	/** {@inheritDoc} */
	@Override
	public boolean cipherFile(final String filename) {
		LOGGER.info("Invocando cipherFile: " + filename); //$NON-NLS-1$

		setError(null);

		if (filename == null || filename.trim().length() < 1) {
			setError(AppletMessages.getString("SignApplet.78")); //$NON-NLS-1$
			return false;
		}

		if (!checkUserPermision(AppletMessages.getString("SignApplet.79") + CR + //$NON-NLS-1$
				AOFileUtils.pathLengthShortener(filename, MAX_PATHNAME_LENGTH) +
				CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.494", filename)); //$NON-NLS-1$
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
	@Override
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
	 */
	private boolean cipherData(final byte[] dat) {

		// Establecemos el fichero seleccionado (si es que lo hay) para que se procese
		// si no se indicaron los datos en claro para cifrar
		if (dat == null && getInternalFileUri() != null) {
			try {
				this.cipherManager.setFileUri(AOUtil.createURI(getInternalFileUri()), false);
			}
			catch (final URISyntaxException e) {
				setError(AppletMessages.getString("SignApplet.15") + getInternalFileUri()); //$NON-NLS-1$
				LOGGER.severe("Error: " + e.toString()); //$NON-NLS-1$
				return false;
			}
		}

		// El resultado queda almacenado en el objeto CipherManager
		try {
			AccessController.doPrivileged(new CipherAction(this.cipherManager, dat));
		} catch (final PrivilegedActionException e) {
			if (e.getCause() instanceof KeyException) {
				LOGGER.info("Ocurrio un error al decodificar la clave de cifrado: " + e.getCause()); //$NON-NLS-1$
				setError(AppletMessages.getString("SignApplet.113")); //$NON-NLS-1$
				return false;
			}
			LOGGER.severe("Error: " + e.toString()); //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.93")); //$NON-NLS-1$
			return false;
		}
		catch (final AOCancelledOperationException e) {
			LOGGER.severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
			return false;
		}
		catch (final OutOfMemoryError e) {
			LOGGER.severe("Error de falta de memoria durante el cifrado: " + e); //$NON-NLS-1$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
			return false;
		}
		return true;
	}

	/** {@inheritDoc} */
	@Override
	public boolean decipherFile(final String filename) {
		LOGGER.info("Invocando decipherFile: " + filename); //$NON-NLS-1$

		setError(null);

		if (filename == null || filename.trim().length() < 1) {
			setError(AppletMessages.getString("SignApplet.87")); //$NON-NLS-1$
			return false;
		}

		if (!checkUserPermision(AppletMessages.getString("SignApplet.88") + CR + //$NON-NLS-1$
				AOFileUtils.pathLengthShortener(filename, MAX_PATHNAME_LENGTH) +
				CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.494", filename)); //$NON-NLS-1$
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
	@Override
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
	 */
	private boolean decipherData(final byte[] dat) {

		// Establecemos el fichero seleccionado (si es que lo hay) para que se procese
		// si no se indicaron los datos en claro para descifrar
		if (getInternalFileUri() != null) {
			try {
				this.cipherManager.setFileUri(AOUtil.createURI(getInternalFileUri()), false);
			}
			catch (final URISyntaxException e) {
				setError(AppletMessages.getString("SignApplet.15") + getInternalFileUri()); //$NON-NLS-1$
				LOGGER.severe("Error: " + e.toString()); //$NON-NLS-1$
				return false;
			}
		}

		// El resultado quedara almacenado en el objeto CipherManager
		try {
			AccessController.doPrivileged(new DecipherAction(this.cipherManager, dat));
		} catch (final PrivilegedActionException e) {
			if (e.getCause() instanceof KeyException) {
				LOGGER.info("Ocurrio un error al decodificar la clave de cifrado: " + e.getCause()); //$NON-NLS-1$
				setError(AppletMessages.getString("SignApplet.111")); //$NON-NLS-1$
				return false;
			}
			LOGGER.info("Ocurrio un error en la operacion de descifrado: " + e.getCause()); //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.92")); //$NON-NLS-1$
			return false;
		}
		catch (final AOCancelledOperationException e) {
			LOGGER.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
			return false;
		}
		catch (final OutOfMemoryError e) {
			getLogger().severe("Error de falta de memoria durante el descifrado: " + e); //$NON-NLS-1$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
			return false;
		}
		return true;
	}

	/** {@inheritDoc} */
	@Override
	public boolean savePlainDataToFile(final String filename) {
		LOGGER.info("Invocando savePlainDataToFile: " + filename); //$NON-NLS-1$

		setError(null);

		if (this.cipherManager.getPlainData() == null) {
			LOGGER.severe("No hay datos en claro que guardar"); //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.394")); //$NON-NLS-1$
			return false;
		}
		if (filename == null || filename.trim().length() < 1) {
			LOGGER.severe("El fichero de salida para los datos no puede ser nulo"); //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.396")); //$NON-NLS-1$
			return false;
		}

		if (!checkUserPermision(AppletMessages.getString("SignApplet.66") + CR + //$NON-NLS-1$
				AOFileUtils.pathLengthShortener(filename, MAX_PATHNAME_LENGTH) +
				CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.496", filename)); //$NON-NLS-1$
			return false;
		}

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			/** {@inheritDoc} */
			@Override
			public Boolean run() {
				try {
					SignApplet.saveDataToStorage(SignApplet.this.getCipherManager().getPlainData(), filename);
				}
				catch (final Exception e) {
					getLogger().severe("No se pudo almacenar el texto plano (establecido o cifrado) en " + filename + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.392") + filename); //$NON-NLS-1$
					return Boolean.FALSE;
				}
				return Boolean.TRUE;
			}
		}).booleanValue();
	}

	/** {@inheritDoc} */
	@Override
	public boolean saveCipherDataToFile(final String filename) {
		LOGGER.info("Invocando saveCipherDataToFile: " + filename); //$NON-NLS-1$

		setError(null);

		if (this.cipherManager.getCipheredData() == null) {
			LOGGER.severe("No hay datos cifrados que guardar"); //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.395")); //$NON-NLS-1$
			return false;
		}

		if (filename == null || filename.trim().length() < 1) {
			LOGGER.severe("El fichero de salida para los datos no puede ser nulo"); //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.396")); //$NON-NLS-1$
			return false;
		}

		if (!checkUserPermision(AppletMessages.getString("SignApplet.71") + CR + //$NON-NLS-1$
				AOFileUtils.pathLengthShortener(filename, MAX_PATHNAME_LENGTH) +
				CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.496", filename)); //$NON-NLS-1$
			return false;
		}

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			/** {@inheritDoc} */
			@Override
			public Boolean run() {
				try {
					SignApplet.saveDataToStorage(SignApplet.this.getCipherManager().getCipheredData(), filename);
				}
				catch (final Exception e) {
					getLogger().severe("No se pudo almacenar el texto cifrado en" + filename + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.397") + filename); //$NON-NLS-1$
					return Boolean.FALSE;
				}
				return Boolean.TRUE;
			}
		}).booleanValue();
	}

	/** {@inheritDoc} */
	@Override
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
	@Override
	public void addRecipientToCMS(final String certB64) {
		LOGGER.info("Invocando addRecipientToCMS"); //$NON-NLS-1$
		if (certB64 == null || certB64.length() == 0) {
			LOGGER.warning("No se han introducido destinatarios"); //$NON-NLS-1$
		}
		try {
			addRecipientToCMS(Base64.decode(certB64));
		} catch (final Exception e) {
			LOGGER.warning("El certificado no esta en Base64: " + e); //$NON-NLS-1$
		}
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
	@Override
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
	@Override
	public void setCMSContentType(final String contentType) {
		LOGGER.info("Invocando setCMSContentType: " + contentType); //$NON-NLS-1$
		this.enveloperManager.setCmsContentType(contentType);
	}

	/** {@inheritDoc} */
	@Override
	public boolean buildCMSEncrypted() {
		LOGGER.info("Invocando buildCMSEncrypted"); //$NON-NLS-1$
		return this.doEnvelopOperation(null, AOSignConstants.CMS_CONTENTTYPE_ENCRYPTEDDATA);
	}

	/** {@inheritDoc} */
	@Override
	public boolean buildCMSEnveloped() {
		LOGGER.info("Invocando buildCMSEnveloped"); //$NON-NLS-1$
		return this.doEnvelopOperation(null, AOSignConstants.CMS_CONTENTTYPE_ENVELOPEDDATA);
	}

	/** {@inheritDoc} */
	@Override
	public boolean buildCMSAuthenticated() {
		LOGGER.info("Invocando buildCMSAuthenticated"); //$NON-NLS-1$
		return this.doEnvelopOperation(null, AOSignConstants.CMS_CONTENTTYPE_AUTHENVELOPEDDATA);
	}

	/** {@inheritDoc} */
	@Override
	public boolean buildCMSStructure() {
		LOGGER.info("Invocando buildCMSStructure"); //$NON-NLS-1$
		return this.doEnvelopOperation(null, null);
	}

	/** {@inheritDoc} */
	@Override
	public boolean signAndPackData() {
		LOGGER.info("Invocando signAndPackData"); //$NON-NLS-1$
		return this.doEnvelopOperation(null, AOSignConstants.CMS_CONTENTTYPE_SIGNEDANDENVELOPEDDATA);
	}

	/** {@inheritDoc} */
	@Override
	public boolean signAndPackFile(final String filename) {
		LOGGER.info("Invocando signAndPackFile: " + filename); //$NON-NLS-1$

		setError(null);

		if (filename == null || filename.trim().length() < 1) {
			setError(AppletMessages.getString("SignApplet.91")); //$NON-NLS-1$
			return false;
		}

		if (!checkUserPermision(AppletMessages.getString("SignApplet.94") + CR + //$NON-NLS-1$
				AOFileUtils.pathLengthShortener(filename, MAX_PATHNAME_LENGTH) +
				CR + AppletMessages.getString("SignApplet.12"))) { //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.494", filename)); //$NON-NLS-1$
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

		setError(null);

		final byte[] contentData;
		try {
			contentData = dat != null ? dat :
				(byte[]) AccessController.doPrivileged(new PrivilegedExceptionAction<Object>() {
					@Override
					public Object run() throws Exception {
						return getInData();
					}
				});
		}
		catch (final PrivilegedActionException e) {
			// El metodo getInData() establece el mensaje en caso de error (Incluido el OutOfMemoryError)
			return false;
		}
		catch (final AOCancelledOperationException e) {
			getLogger().severe("Operacion cancelada por el usuario: " + e); //$NON-NLS-1$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
			return false;
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
				LOGGER.severe(e.toString());
				setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
				return false;
			}
			LOGGER.severe(e.toString());
			setError(AppletMessages.getString("SignApplet.65")); //$NON-NLS-1$
			return false;
		}
		catch (final OutOfMemoryError e) {
			LOGGER.severe("Error de falta de memoria durante el ensobrado: " + e); //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
			return false;
		}
		catch (final Throwable e) {
			LOGGER.severe("Error durante el proceso de ensobrado electronico: " + e); //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.67")); //$NON-NLS-1$
			return false;
		}

		return true;
	}

	/** {@inheritDoc} */
	@Override
	public boolean coEnvelop() {
		LOGGER.info("Invocando coEnvelop"); //$NON-NLS-1$

		// Reiniciamos el mensaje de error
		this.setError(null);

		// Leemos los datos
		final byte[] envelop;
		try {
			envelop =
				(byte[]) AccessController.doPrivileged(new PrivilegedExceptionAction<Object>() {
					@Override
					public Object run() throws Exception {
						return getInData();
					}
				});
		}
		catch (final PrivilegedActionException e) {
			// El metodo getInData() establece el mensaje en caso de error (Incluido el OutOfMemoryError)
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
	@Override
	public boolean recoverCMS() {
		LOGGER.info("Invocando recoverCMS"); //$NON-NLS-1$

		// Reiniciamos el mensaje de error
		this.setError(null);

		final byte[] contentData;
		try {
			contentData = this.data != null ? this.data : getInData();
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
				LOGGER.severe(e.toString());
				setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
				return false;
			} else if (e.getCause() instanceof AOInvalidRecipientException) {
				LOGGER.severe(e.toString());
				setError(AppletMessages.getString("SignApplet.112")); //$NON-NLS-1$
				return false;
			} else if (e.getCause() instanceof AOInvalidFormatException) {
				LOGGER.severe(e.toString());
				setError(AppletMessages.getString("SignApplet.75")); //$NON-NLS-1$
				return false;
			} else {
				LOGGER.severe(e.toString());
				setError(AppletMessages.getString("SignApplet.77")); //$NON-NLS-1$
				return false;
			}
		}
		catch (final OutOfMemoryError e) {
			LOGGER.severe("Error de falta de memoria al recuperar desensobrar: " + e); //$NON-NLS-1$
			setError(AppletMessages.getString("SignApplet.493")); //$NON-NLS-1$
			return false;
		}
		return true;
	}

	/** {@inheritDoc} */
	@Override
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
	@Override
	public void setLdapCertificatePrincipal(final String ldapCertificatePrincipal) {
		LOGGER.info("Invocando setLdapCertificatePrincipal con el parametro: " + ldapCertificatePrincipal); //$NON-NLS-1$
		this.ldapCertificatePrincipal = ldapCertificatePrincipal != null && ldapCertificatePrincipal.length() > 0 ? ldapCertificatePrincipal : null;
	}

	/** {@inheritDoc} */
	@Override
	public String getLdapCertificate() {
		LOGGER.info("Invocando getLdapCertificate()"); //$NON-NLS-1$
		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			/** {@inheritDoc} */
			@Override
			public String run() {

				final X509Certificate cert;

				// Si se ha establecido la direccion LDAP del
				// certificado, se descarga; si no
				// se muestra un navegador LDAP para seleccionarlo y
				// descargarlo
				if (SignApplet.this.getLdapCertificatePrincipal() != null) {
					try {
						cert =
							LdapUtils.getCertificate(SignApplet.this.getLdapServerUrl(),
									SignApplet.this.getLdapServerPort(),
									SignApplet.this.getLdapCertificatePrincipal());
					}
					catch (final Exception e) {
						getLogger().severe("Error al recuperar el certificado '" + SignApplet.this.getLdapCertificatePrincipal() + "' del directorio LDAP: " + e); //$NON-NLS-1$ //$NON-NLS-2$
						setError(AppletMessages.getString("SignApplet.74") + SignApplet.this.getLdapCertificatePrincipal()); //$NON-NLS-1$
						return null;
					}
				}

				else {
					getLogger().severe("No se especifico el Principal del certificado que se desea seleccionar"); //$NON-NLS-1$
					return null;
				}

				// Devolvemos el certificado codificado en Base64
				try {
					return Base64.encode(cert.getEncoded());
				}
				catch (final Exception e) {
					getLogger().severe("Error al codificar el certificado recuperado del directorio LDAP : " + e); //$NON-NLS-1$
					setError(AppletMessages.getString("SignApplet.83")); //$NON-NLS-1$
					return null;
				}
			}
		});
	}
}

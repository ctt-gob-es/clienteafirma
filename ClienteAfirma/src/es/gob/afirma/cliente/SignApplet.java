/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */

package es.gob.afirma.cliente;

import java.awt.Component;
import java.awt.Container;
import java.awt.Frame;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.net.URI;
import java.net.URL;
import java.net.URLDecoder;
import java.security.AccessController;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Properties;
import java.util.Vector;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;
import javax.swing.JApplet;
import javax.swing.JOptionPane;
import javax.swing.UIManager;

import org.ietf.jgss.Oid;

import sun.misc.BASE64Decoder;
import es.gob.afirma.EntryPointsCrypto;
import es.gob.afirma.EntryPointsUtil;
import es.gob.afirma.beans.AOXMLTransform;
import es.gob.afirma.callbacks.CachePasswordCallback;
import es.gob.afirma.callbacks.NullPasswordCallback;
import es.gob.afirma.callbacks.UIPasswordCallback;
import es.gob.afirma.ciphers.AOAlgorithmConfig;
import es.gob.afirma.ciphers.AOCipher;
import es.gob.afirma.ciphers.AOCipherKeyStoreHelper;
import es.gob.afirma.ciphers.AOSunJCECipher;
import es.gob.afirma.cliente.utilidades.browser.Browser;
import es.gob.afirma.cliente.utilidades.browser.FirmadorWeb.FirmaWeb;
import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.exceptions.AOCantSaveDataException;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOFormatFileException;
import es.gob.afirma.exceptions.AOInvalidFormatException;
import es.gob.afirma.exceptions.AOInvalidKeyException;
import es.gob.afirma.exceptions.AOInvalidRecipientException;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.ldap.LdapUtils;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOInstallParameters;
import es.gob.afirma.misc.AOSignConstants;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.AsinchronousSaveData;
import es.gob.afirma.misc.CMSEnvelopManager;
import es.gob.afirma.misc.DirectorySignatureHelper;
import es.gob.afirma.misc.MassiveSignatureHelper;
import es.gob.afirma.misc.MimeHelper;
import es.gob.afirma.misc.SignText;
import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;
import es.gob.afirma.misc.AOConstants.AOCipherBlockMode;
import es.gob.afirma.misc.AOConstants.AOCipherPadding;
import es.gob.afirma.misc.AOConstants.AOKeyStore;
import es.gob.afirma.misc.AOCryptoUtil.RawBASE64Encoder;
import es.gob.afirma.misc.DirectorySignatureHelper.MassiveType;
import es.gob.afirma.misc.MassiveSignatureHelper.MassiveSignConfiguration;
import es.gob.afirma.signers.AOCADESSigner;
import es.gob.afirma.signers.AOCMSSigner;
import es.gob.afirma.signers.AOSigner;
import es.gob.afirma.signers.AOSignerFactory;
import es.gob.afirma.signers.aobinarysignhelper.CMSInformation;
import es.gob.afirma.ui.AOUIManager;
import es.gob.aoclasses.AOCertFilter;

/**
 * Reimplementaci&oacute;n del Applet original de firma del cliente AFirma.
 */
public class SignApplet extends JApplet implements EntryPointsCrypto, EntryPointsUtil {

	/** Separador utilizado para separar varios valores consecutivos en una cadena devuelta por el Applet. */
	public static final String STRING_SEPARATOR = "$%$"; //$NON-NLS-1$

	/** N&uacute;meto de puerto por defecto para la conexi&oacute;n LDAP. */
	private static final int DEFAULT_LDAP_PORT = 389;

	/** Logger para la visualizaci&oacute;n de los mensajes por consola. */
	private static final Logger logger = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Informaci&oacute;n del almac&eacute;n de datos que vamos a utilizar. */
	private AOConstants.AOKeyStore store = AOConstants.AOKeyStore.WINDOWS; // Windows CAPI por defecto

	/** Contrase&ntilde;a del keystore en uso, necesaria tambi&eacute;n para el uso den sus certificados. */
	private char[] keystorePassword = null;

	/** Alias de los certificados v&aacute;lidos del almac&eacute;n activo. */ 
	private String[] certAlias = null;

	/** Alias del certificado actual. */
	private String selectedAlias = null;

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

	/** Algoritmo de cifrado actual. **/
	private AOCipherAlgorithm cipAlgo = null;

	/** Modo de bloques actual para el cifrado. **/
	private AOCipherBlockMode cipBlockMode = null;

	/** Padding actual para el cifrado cifrado **/
	private AOCipherPadding cipPadding = null;

	/** Alias de la clave de cifrado que deseamos utilizar cuando el modo de clave es 'KEYSTORE'. */
	private String cipherKeyAlias = null;

	/** Contrase&ntilde;a del almac&eacute;n de claves de cifrado. */
	private char[] cipherKeystorePass = null;

	/** Indica si se debe dar la posibilidad de que el usuario almacene una clave generada
	 * autom&aacute;ticamente en su repositorio de claves. */
	private boolean useCipherKeyStore = true;

	/** Modo de generaci&oacute;n de clave */
	private String keyMode = AOConstants.DEFAULT_KEY_MODE;

	/**	Clave para el cifrado y desencriptado en base 64 */
	private String cipherB64Key = null;

	/** Contrase&ntilde;a para el cifrado de datos. */
	private char[] cipherPassword = null;

	/** Datos a operar criptogr&aacute;ficamente. */
	private byte[] data = null;

	/** Datos planos que van a ser cifrados o que han sido descifrados */
	private byte[] plainData = null;

	/** Datos cifrados o preparados para ser descifrados */
	private byte[] cipheredData = null;

	/** Firma electr&oacute;nica, tanto la generada por las distintas operaciones de firma como la
	 * utilizada en las operaciones de firma y cofirma.
	 */
	private byte[] signData = null;

	/** URL de identificaci&oacute;n de la pol&iacute;tica de firma. */
	private URL policyId = null;

	/** Descripcion de la pol&iacute;tica de firma. */
	private String policyDesc = null;

	/** Oid cualificador de la pol&iacute;tica de firma. */
	private Oid policyQualifier = null;

	/** Tipo de contenido de la estructura CMS que se desea generar. */
	private String cmsContentType = null;
	
	/** Certificado de los usuarios a los que va destinado un sobre digital. */
	private X509Certificate[] recipientsCMS = null;

	/** Listado de certificados externos de destinatarios para el sobre digital. */
	private HashMap<BigInteger, X509Certificate> recipientsCMSExt = null;

	/** URL del servidor LDAP. */  
	private String ldapServerUrl = null;

	/** Puerto del servidor LDAP. Por defecto, 389. */
	private int ldapServerPort = DEFAULT_LDAP_PORT;

	/** Principal del certificado que se desea recuperar del servidor LDAP. */
	private String ldapCertificatePrincipal = null;

	/**	Certificado utilizado para la firma de datos. */
	private X509Certificate certToSign = null;

	/** Firmantes o nodos que se desean contrafirmar. */
	private String[] signersToCounterSign = new String[0];

	/** Listado de hashes a firmar en una operaci&oacute;n de firma masiva. */
	private Vector<String> hashesToSign = null;

	/** Tipo de operaci&oacute;n masiva a realizar. Por defecto, multifirma masiva. */
	private MassiveType massiveOperation = MassiveType.SIGN;

	/** Indica si se deben firmar los ficheros de las subcarpetas del directorio seleccionado
	 * durante la operacion de firma masiva. */
	private boolean recursiveSignDir = false;

	/** Directorio de donde se toman los ficheros a firmar de forma masiva. */
	private String massiveInputDirectory = null;

	/** Directorio en donde se almacenar&aacute;n las firmas masivas. */
	private String massiveOutputDirectory = null;

	/** Indica si se debe respectar el formato de firma original para la multifirma masiva. */
	private boolean originalFormat = true;

	/** Extensiones por las que debe filtrarse durante la firma masiva. */
	private String[] massiveExtFiltered = null;

	/**	Almacena el resultado de la firma masiva, en donde todas las firmas est&aacute;n en base 64 y
	 * concatenadas con '!' (cierre de exclamaci&oacute;n). Este atributo almacena la firma expl&iacute;cita
	 * de los ficheros que se han seleccionado (no de sus Hashes como en versiones anteriores del cliente).
	 */
	private String massiveSignData = null;

	/** Indica si se ha producido alg&uacute;n error durante la &uacute;ltima operaci&oacute;n. */
	private boolean error = false;

	/**	Indica si se deben mostrar o no los hashes de los ficheros que se vayan a firmar. */
	private boolean showHashes = false;

	/** Indica si el comportamiento por defecto a la hora de mostrar los certificados para firmar,
	 * es mostrar tambi&eacute;n los caducados. */
	private boolean defaultShowExpiratedCertificates = true;

	/** Indica si se deben mostrar en la lista de certificados, aquellos que ya han expirado. */
	private boolean showExpiratedCertificates = defaultShowExpiratedCertificates;

	/**	Indica si se deben mostrar o no los mensajes de error mediante un di&aacute;logo de error. */
	private boolean showErrors = false;

	/** Mensaje asociado al &uacute;ltimo error producido. */
	private String errorMsg = ""; //$NON-NLS-1$

	/** Indica si el applet ha sido inicializado. */
	private boolean initializedApplet = false;

	/** Filtro de certificados. **/ 
	private AOCertFilter certFilter = null;

	/** Indica si se ha establecido un filtro que devuelve un &uacute;nico certificado. */
	private boolean mandatoryCert = false;

	/** Camino de un fichero de firma electronica. Se utiliza para la cofirma y contrafirma. */
	private URI electronicSignatureFile = null;

	/** Manejador del repositorio de certificados. */
	private AOKeyStoreManager aoKsManager = null;

	/** MimeType obtenido de los datos proporcionados. */
	private String dataMimeType = AOConstants.DEFAULT_MIMETYPE;

	/** MimeType establecido externamente para incorporar a la firma. */
	private String extMimeType = null;
	
	/** Descripci&oacute;n del tipo de datos firmados. */
	private String dataDescription = null;

	/** Listado de atributos firmados que se deben agregar a una firma. */
	private HashMap<org.ietf.jgss.Oid, String> signedAttributes = null;

	/** Listado de atributos sin firmar que se deben agregar a una firma. */
	private HashMap<org.ietf.jgss.Oid, Vector<String>> unsignedAttributes = null;

	/** Listado de propiedades gen&eacute;ricas establecidas para las firmas. */
	private Properties genericConfig = new Properties();

	/** Transformaciones XML que aplicar a los formatos de firma que las soporten. */
	private Vector<AOXMLTransform> xmlTransforms = null;

	/** Filtro de certificado RFC2254 del emisor del certificado. */
	private String rfc2254IssuerFilter = null;
	
	/** Filtro de certificado RFC2254 del titular del certificado. */
	private String rfc2254SubjectFilter = null;
	
	/** Filtro de certificado por KeyUsage. */
	private Boolean[] keyUsageFilter = null;

	private static final long serialVersionUID = 5692094082535848369L;

	public void initialize() {
		logger.info("Invocando initialize"); //$NON-NLS-1$
		keystorePassword = null;
		certAlias = null;
		selectedAlias = null;
		showExpiratedCertificates = defaultShowExpiratedCertificates;
		outputFile = null;
		fileUri = null;
		fileBase64 = false;
		hash = null;
		sigAlgo = AOConstants.DEFAULT_SIGN_ALGO;
		sigFormat = AOConstants.DEFAULT_SIGN_FORMAT;
		sigMode = AOConstants.DEFAULT_SIGN_MODE;
		cipAlgo = null;
		cipBlockMode = null;
		cipPadding = null;
		cipherKeyAlias = null;
		cipherKeystorePass = null; 
		useCipherKeyStore = true;
		cipherPassword = null;
		keyMode = AOConstants.DEFAULT_KEY_MODE;;
		cipherB64Key = null;
		data = null;
		plainData = null;
		cipheredData = null;
		signData = null;
		recipientsCMS = null;
		recipientsCMSExt = null;
		ldapServerUrl = null;
		ldapServerPort = DEFAULT_LDAP_PORT;
		ldapCertificatePrincipal = null;
		dataMimeType = AOConstants.DEFAULT_MIMETYPE;
		extMimeType = null;
		dataDescription = null;
		certToSign = null;
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
		certFilter = null;
		mandatoryCert = false;
		electronicSignatureFile = null;
		signedAttributes = null;
		unsignedAttributes = null;
		genericConfig = new Properties();
		xmlTransforms = null;
		rfc2254IssuerFilter = null;
		rfc2254SubjectFilter = null;
		keyUsageFilter = null;
	}

	public String getCertificatesAlias() {
		logger.info("Invocando getCertificatesAlias"); //$NON-NLS-1$
		return this.concatStrings(getArrayCertificatesAlias(), STRING_SEPARATOR);
	}

	public String[] getArrayCertificatesAlias() {

		logger.info("Invocando getArrayCertificatesAlias"); //$NON-NLS-1$

		if (certAlias != null) return certAlias;

		return AccessController.doPrivileged(new java.security.PrivilegedAction<String[]>() {
			public String[] run() {
				if (aoKsManager==null) {
					logger.info("El almacen de certificados no estaba inicializado, se inicializara ahora"); //$NON-NLS-1$
					try {
						initKeyStore();
					}
					catch (final AOCancelledOperationException e) {
						throw e;
					}
					catch(final Throwable e) {
						logger.severe("Error inicializando el almacen de claves, se devolvera una lista vacia de certificados: " + e); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.6")); //$NON-NLS-1$
						return new String[0];
					}
				}

				try {
					certAlias = aoKsManager.getAliases().clone();
				} 
				catch (Throwable e) {
					logger.severe("Error al recuperar los alias del repositorio, se devolvera una lista vacia: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.340")); //$NON-NLS-1$
					return new String[0];
				}

				// Indicamos que no se ha producido error.
				SignApplet.this.setError(null);

				return certAlias.clone();
			}
		});
	}

	public String getCertificates() {
		logger.info("Invocando getCertificates"); //$NON-NLS-1$
		return this.concatStrings(getArrayCertificates(), STRING_SEPARATOR);
	}

	public final String[] getArrayCertificates() {
		logger.info("Invocando getArrayCertificates"); //$NON-NLS-1$
		return AccessController.doPrivileged(new java.security.PrivilegedAction<String[]>() {
			public String[] run() {
				String[] certs = new String[0];
				try {
					String aliases[] = new String[0];
					try {
						aliases = getArrayCertificatesAlias();
					}
					catch(final AOCancelledOperationException e) {
						logger.info("El usuario cancelo la inicialicion del almacen, se devolvera null: " + e); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.5")); //$NON-NLS-1$
						return null;
					}
					certs = new String[aliases.length];
					for(int i=0;i<aliases.length;i++) certs[i] = getCertificate(aliases[i]);
				}
				catch(final Throwable e) {
					logger.severe("Error obteniendo los certificados, se devolvera null : " + e); //$NON-NLS-1$
					setError(AppletMessages.getString("SignApplet.9")); //$NON-NLS-1$
					return null;
				}
				return certs;
			}
		});
	}

	public final String getCertificate(final String alias) {
		logger.info("Invocando getCertificate: " + alias); //$NON-NLS-1$
		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			public String run() {
				X509Certificate cert = SignApplet.this.getCertificateBinary(alias);
				if(cert == null) return null;

				String b64CertEncode;
				try {
					b64CertEncode = new RawBASE64Encoder().encode(cert.getEncoded());
				} 
				catch (final Throwable e) {
					logger.severe("Error al codificar el certificado, se devolvera null: " + e); //$NON-NLS-1$
					return null;
				}

				return "Bag Attributes\r\n" + //$NON-NLS-1$
				"friendlyName: " + AOUtil.getCN(cert) + "\r\n" + //$NON-NLS-1$ //$NON-NLS-2$
				"-----BEGIN CERTIFICATE-----\r\n" + //$NON-NLS-1$
				b64CertEncode +
				"\r\n-----END CERTIFICATE-----"; //$NON-NLS-1$
			}
		});
	}

	public String getCertificatePublicKey(final String alias) {
		logger.info("Invocando getCertificatePublicKey: "+alias); //$NON-NLS-1$
		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			public String run() {
				X509Certificate cert = SignApplet.this.getCertificateBinary(alias);
				if(cert == null) return null;

				String b64PKEncoded;
				try {
					b64PKEncoded = new RawBASE64Encoder().encode(cert.getPublicKey().getEncoded());
				} 
				catch (final Throwable e) {
					logger.severe("Error al codificar la clave publica del certificado, se devolvera null: " + e); //$NON-NLS-1$
					return null;
				}

				return "-----BEGIN RSA PUBLIC KEY-----\r\n" + //$NON-NLS-1$
				b64PKEncoded +
				"\r\n-----END RSA PUBLIC KEY-----"; //$NON-NLS-1$
			}
		});
	}

	/**
	 * Recupera un certificado del repositorio activo.
	 * @param alias Alias del certificado que deseamos recuperar.
	 * @return Certificado.
	 */
	private final X509Certificate getCertificateBinary(final String alias) {
		return AccessController.doPrivileged(new java.security.PrivilegedAction<X509Certificate>() {
			public X509Certificate run() {
				if(SignApplet.this.aoKsManager == null) {
					try {
						SignApplet.this.initKeyStore();
					} 
					catch (final Throwable e) {
						logger.severe("Error al inicializar el repositorio de certificados, se devolvera null: " + e); //$NON-NLS-1$
						return null;
					}
				}
				final Certificate cert = SignApplet.this.aoKsManager.getCertificate(alias);
				if(cert == null) return null;
				return (X509Certificate)cert;
			}
		});
	}

	/**
	 * Inicializa el repositorio de certificados establecido.
	 * @throws AOException Cuando ocurre un error durante la inicializaci&oacute;n.
	 */
	private void initKeyStore() throws AOException {
		this.initKeyStore(null, null, null);
	}

	/**
	 * Inicializa el repositorio de certificados establecido.
	 * @param path Ruta al repositorio.
	 * @param description Descripci&oacute;n del almac&eacute;n de claves (en el caso
	 * de PKCS#11 se toma de aqu&iacute; el n&uacute;mero de z&oacute;calo).
	 * @param password Contrase&ntilde;a del almac&eacute;n de claves.
	 * @throws AOException Cuando ocurre un error durante la inicializaci&oacute;n.
	 */
	private void initKeyStore(final String path, final String description, final String password) throws AOCancelledOperationException, AOException {

		// Reiniciamos el listado de alias de los certificados validos del almacen activo
		certAlias = null;

		// Para evitar la perdida de las excepciones que se emitan se relanzaran estas cuando hereden de
		// RuntimeException y se devolvera como valor de retorno en caso contrario. Si la devolucion del 
		// metodo es null se entendera que la operacion finalizo correctamente
		SignApplet.this.aoKsManager = AOKeyStoreManagerFactory.getAOKeyStoreManager(
				SignApplet.this.store,
				path,
				description,
				(password != null ?
						new CachePasswordCallback(password.toCharArray()) :
							SignApplet.this.getPreferredPCB(store)),
							SignApplet.this
		);

		logger.info("El almacen de claves establecido es: " + aoKsManager.toString()); //$NON-NLS-1$
	}

	private void saveDataToStorage(final byte[] binaryData, final String filename) throws AOException {
		if(binaryData == null) throw new NullPointerException("Los datos que desea almacenar no pueden ser nulos"); //$NON-NLS-1$
		if(filename == null)   throw new NullPointerException("El nombre de fichero de salida no puede ser nulo"); //$NON-NLS-1$

		FileOutputStream fos = null;
		try {
			fos = new FileOutputStream(filename);
			fos.write(binaryData);
		}
		catch (Throwable e) {
			throw new AOCantSaveDataException("No se pudieron almacenar los datos en disco: " + e); //$NON-NLS-1$
		} finally {
			if(fos != null) {
				try {
					fos.close();
				}
				catch(Exception e) {
					logger.warning("No se ha podido cerrar el fichero de salida, es posible que se haya perdido parte de la informacion"); //$NON-NLS-1$
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
				URI uri;
				try {
					uri = AOUtil.createURI(strUri);
				}
				catch(Exception e) {
					logger.severe("La URI proporcionada no es valida (" + strUri + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.15") + strUri); //$NON-NLS-1$
					return false;
				}
				if (!uri.getScheme().equals("file")) { //$NON-NLS-1$
					logger.severe("Solo se permite grabar en almacenamiento local, no mediante el protocolo " + uri.getScheme()); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.17") + uri.getScheme()); //$NON-NLS-1$
					return false;
				}
				// OK, en este punto "fileUri" es un nombre de fichero, con lo que ignoramos la uri
				// y lo tratamos como tal
				try {
					saveDataToStorage(data, strUri);
				}
				catch(Throwable e) {
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

					extensions = new String[]{"sig"}; //$NON-NLS-1$
					description = AppletMessages.getString("SignApplet.25"); //$NON-NLS-1$

					if (AOConstants.SIGN_FORMAT_CMS.equals(sigFormat)) {
						description = AppletMessages.getString("SignApplet.29"); //$NON-NLS-1$
						extensions = new String[]{"csig", "p7s", "sig"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					}

					else if (AOConstants.SIGN_FORMAT_CADES.equals(sigFormat)) {
						description = AppletMessages.getString("SignApplet.26"); //$NON-NLS-1$
						extensions = new String[]{"csig"}; //$NON-NLS-1$
					}

					else if (AOConstants.SIGN_FORMAT_XADES.equals(sigFormat) || 
							AOConstants.SIGN_FORMAT_XADES_DETACHED.equals(sigFormat) ||
							AOConstants.SIGN_FORMAT_XADES_ENVELOPED.equals(sigFormat) ||
							AOConstants.SIGN_FORMAT_XADES_ENVELOPING.equals(sigFormat) ||
							AOConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED.equals(sigFormat) ||
							AOConstants.SIGN_FORMAT_XMLDSIG.equals(sigFormat) ||
							AOConstants.SIGN_FORMAT_XMLDSIG_DETACHED.equals(sigFormat) ||
							AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED.equals(sigFormat) ||
							AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING.equals(sigFormat) ||
							AOConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED.equals(sigFormat) ||
							AOConstants.SIGN_FORMAT_SOAP.equals(sigFormat)
					) {
						description = AppletMessages.getString("SignApplet.27"); //$NON-NLS-1$
						extensions = new String[]{"xsig", "xml"}; //$NON-NLS-1$ //$NON-NLS-2$
					}

					else if (AOConstants.SIGN_FORMAT_PDF.equals(sigFormat)) {
						description = AppletMessages.getString("SignApplet.28"); //$NON-NLS-1$
						extensions = new String[]{"pdf"}; //$NON-NLS-1$
					}

					else if (AOConstants.SIGN_FORMAT_OOXML.equals(sigFormat)) {
						description = AppletMessages.getString("SignApplet.30"); //$NON-NLS-1$
						extensions = new String[]{"docx", "xlsx", "pptx"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					}

					else if (AOConstants.SIGN_FORMAT_ODF.equals(sigFormat)) {
						description = AppletMessages.getString("SignApplet.32"); //$NON-NLS-1$
						extensions = new String[]{"odt", "ods", "odp"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					}
				}

				// Almacenamos en disco
				saveFileAsinchronously(signData, outputFile, extensions, description);

				SignApplet.this.setError(null);

				return true;
			}
		});
	}

	/**
	 * Guarda datos en disco as&iacute;ncronamente. Si no se indica la la ruta del fichero de salida
	 * se mostrar&aacute; un di&aacute;logo para su selecci&oacute, mostrando la descripci&oacute;n
	 * y las extensiones proporcionadas.<br/>
	 * Esta llamada asincrona se utiliza para evitar que Mozilla Firefox muestre al usuario una
	 * advertencia indic&aacute;ndole que hay un script boqueado y se le d&eacute; la posibilidad
	 * de detenerlo.
	 * @param dat Datos que deseamos almacenar.
	 * @param outputPath Ruta del fichero de salida.
	 * @param extensions Extensiones permitidas para el di&aacute;logo de guardado.
	 * @param description Descrìpci&oacute;n de los datos para el di&aacute;logo de guardado.
	 */
	private void saveFileAsinchronously(final byte[] dat, final String outputPath, final String[] extensions, final String description) {
		AsinchronousSaveData saveDataDialog = new AsinchronousSaveData(
				dat,
				outputPath,
				description,
				extensions,
				getParentFrame(SignApplet.this),
				true);

		new Thread(saveDataDialog).run();
	}

	public void setCertFilter(String certFilter) {
		logger.info("Invocando setCertFilter: " + certFilter); //$NON-NLS-1$

		// Si hay establecido un filtro de certificado unico y no se ha introducido ahora ningun
		// filtro, se ignorara la peticion para no pisar el otro filtro. Si se esta ahora estableciendo
		// un filtro, desactivamos la condicion de certificado unico 
		if(mandatoryCert && (certFilter == null || certFilter.length() < 1)) return;
		this.certFilter = (certFilter == null || certFilter.length() < 1) ? null : new AOCertFilter(certFilter);
		if(this.certFilter != null) this.mandatoryCert = false;
	}

	public void setMandatoryCertificateCondition(String mandatoryCertificateCondition) {
		logger.info("Invocando setMandatoryCertificateCondition: " + mandatoryCertificateCondition); //$NON-NLS-1$

		// Si ya estaba establecida una condicion unica y ahora se introduce un valor nulo o vacio,
		// la eliminamos. Si no habia condicion unica, la establecemos si se nos ha introducido o
		// la eliminamos si se indica que es nula 
		if(this.mandatoryCert && (mandatoryCertificateCondition == null || mandatoryCertificateCondition.length() < 1)) {
			this.mandatoryCert = false;
			this.certFilter = null;
			return;
		}
		this.certFilter = (mandatoryCertificateCondition == null || mandatoryCertificateCondition.length() < 1) ? null : new AOCertFilter(mandatoryCertificateCondition);
		this.mandatoryCert = this.certFilter != null;
	}

	public final void setData(final String data) {
		logger.info("Invocando setData"); //$NON-NLS-1$
		if(data == null) {
			this.data = null;
			return;
		}

		AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
			public Void run() {
				try {
					SignApplet.this.data = new sun.misc.BASE64Decoder().decodeBuffer(data); 
				}
				catch(Exception e) {
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
		if(fu == null || fu.trim().equals("")) { //$NON-NLS-1$
			this.fileUri = null;
			return;
		}

		AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
			public Void run() {
				try {
					SignApplet.this.fileUri = AOUtil.createURI(fu);
				}
				catch(final Throwable e) {
					logger.severe("El nombre de fichero indicado no es una URI valida:" + e); //$NON-NLS-1$
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
		if(fu == null || fu.trim().equals("")) { //$NON-NLS-1$
			this.fileUri = null;
			return;
		}

		AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
			public Void run() {
				try {
					SignApplet.this.fileUri = AOUtil.createURI(fu);
					SignApplet.this.fileBase64 = true;
				}
				catch(final Throwable e) {
					logger.severe("El nombre de fichero indicado no es una URI valida:" + e); //$NON-NLS-1$
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
		if(hash == null) {
			this.hash = null;
			return;
		}

		AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
			public Void run() {
				try {
					SignApplet.this.hash = new BASE64Decoder().decodeBuffer(hash);
				}
				catch (final Throwable e) {
					logger.warning("El hash establecido no estaba en base 64, posiblemente se genere una firma incorrecta: " + e); //$NON-NLS-1$
				}
				return null;
			}
		});
		
		SignApplet.this.dataMimeType = AOConstants.DEFAULT_MIMETYPE;
		SignApplet.this.fileUri = null;
		SignApplet.this.fileBase64 = false;
		SignApplet.this.data = null;
	}

	public void setElectronicSignature(final String inElectronicSignature) {
		logger.info("Invocando setElectronicSignature"); //$NON-NLS-1$
		AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
			public Void run() {
				if(inElectronicSignature == null || inElectronicSignature.length() == 0) {
					signData = null;
				}
				else {
					try {
						signData = new sun.misc.BASE64Decoder().decodeBuffer(inElectronicSignature);
					}
					catch(Exception e) {
						logger.severe("Error al establecer la firma sobre la que se realizara la operacion: " + e); //$NON-NLS-1$
						signData = null;
					}
				}
				return null;
			}
		});
	}

	public void setElectronicSignatureFile(String inElectronicSignatureFile) {
		logger.info("Invocando inElectronicSignatureFile: " + inElectronicSignatureFile); //$NON-NLS-1$
		if(inElectronicSignatureFile == null || inElectronicSignatureFile.length() == 0) {
			electronicSignatureFile = null;
		}
		else {
			try {
				electronicSignatureFile = AOUtil.createURI(inElectronicSignatureFile);
			}
			catch(Exception e) {
				logger.severe(
						"El nombre de fichero de firma indicado no es una URI valida: " + e //$NON-NLS-1$
				);
				electronicSignatureFile = null;
			}
		}
	}

	public void setSignersToCounterSign(String signers) {
		logger.info("Invocando setSignersToCounterSign" + (signers != null ? ": "+signers.trim().replace('\n', ' ').replace("\r\n", " ") : "")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
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

				// Probamos si la firma se corresponde con el formato establecido y si es asi
				// lo utilizamos
				AOSigner signer = AOCryptoUtil.getSigner(SignApplet.this.sigFormat);				
				if(signer != null && signer.isSign(new ByteArrayInputStream(originalSign))) {
					try {
						return AOUtil.showTreeAsString(signer.getSignersStructure(new ByteArrayInputStream(originalSign), false), null, null);
					} 
					catch (final Throwable e) {
						logger.severe("Arbol de firmas no valido: " + e); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.72")); //$NON-NLS-1$
						return null;
					}
				}

				// Si no se ajustaba la firma al formato establecido, comprobamos todos los formatos
				signer = AOCryptoUtil.getSigner(originalSign);
				if(signer == null) {
					logger.severe("La firma introducida no se ajusta a ningun formato soportado"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.63")); //$NON-NLS-1$
					return null;
				}
				try {
					return AOUtil.showTreeAsString(signer.getSignersStructure(new ByteArrayInputStream(originalSign), false), null, null);
				} 
				catch (final Throwable e) {
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
				// Si no hay establecido un algoritmo de firma, tomamos el por defecto pero 
				// solo para esta ocasion
				String algorithm = (sigAlgo == null ? AOConstants.DEFAULT_SIGN_ALGO : sigAlgo);

				// Si no hay establecido un formato de firma, tomamos el por defecto pero 
				// solo para esta ocasion
				String format = (sigFormat == null ? AOConstants.DEFAULT_SIGN_FORMAT : sigFormat);

				// Tomamos la firma sobre la que se realiza la contrafirma
				byte[] originalSign = null;
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

				// Si no hay certificado seleccionado mostramos la lista de seleccion
				String aliasToUse = null;
				try {
					if (selectedAlias == null) {
						if (certAlias == null) SignApplet.this.getCertificatesAlias();
						selectedAlias = aliasToUse = SignApplet.this.selectCertificate(true);
					}
					else aliasToUse = selectedAlias;
				} 
				catch(final AOCancelledOperationException e) {
					logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					return false;
				} 
				catch (final AOException e) {
					logger.severe("Error al seleccionar un certificado del repositorio: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.363")); //$NON-NLS-1$
					return false;
				}

				// Comprobamos que finalmente se haya seleccionado un alias
				if (aliasToUse == null) {
					logger.severe("Error al seleccionar un certificado del repositorio"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.363")); //$NON-NLS-1$
					return false;
				}

				// Comprobamos si se ha inicializado el keystore
				if (aoKsManager == null) {
					logger.severe("No se pudo recuperar el KeyStore asociado al alias"); //$NON-NLS-1$
					SignApplet.this.setError(
							AppletMessages.getString("SignApplet.91") + aliasToUse //$NON-NLS-1$
					);
					return false;
				}

				PrivateKeyEntry ke;
				try {
					ke = aoKsManager.getKeyEntry(aliasToUse, AOCryptoUtil.getCertificatePC(SignApplet.this.store));
				}
				catch(AOCancelledOperationException e) {
					logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					return false;
				}
				catch (Throwable e) {
					logger.severe("No se ha podido obtener el certicado con el alias " + aliasToUse + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.94") + aliasToUse); //$NON-NLS-1$
					return false;
				}

				// Recuperamos el certificado a traves del KeyEntry y lo almacenamos por si posteriormente
				// a la contrafirma se nos pide informacion sobre el certificado utilizado
				certToSign = aoKsManager.getCertificate(ke);

				// Tomamos el manejador del formato de firma
				AOSigner signer = AOCryptoUtil.getSigner(format);
				if(signer == null) {
					SignApplet.this.setError(AppletMessages.getString("SignApplet.95") + format); //$NON-NLS-1$
					logger.severe("El formato de firma '" + format +  //$NON-NLS-1$
							"' no esta soportado. Lo formatos soportados son:\n" +  //$NON-NLS-1$
							AOSignerFactory.getInstance().toString());
					return false;
				}

				// Establecemos el mimetype de los datos
				try {
					String mimeType = SignApplet.this.extMimeType != null ? SignApplet.this.extMimeType : SignApplet.this.dataMimeType;
					if (mimeType == null) signer.setDataObjectFormat(SignApplet.this.dataDescription, null, null, null);
					else {
						String oid = MimeHelper.transformMimeTypeToOid(mimeType);
						signer.setDataObjectFormat(SignApplet.this.dataDescription, oid == null ? null : new Oid(oid), new javax.activation.MimeType(mimeType), null);
					}
				} 
				catch (Throwable e) {
					logger.warning("MimeType mal formado, se tratara de detectar el mimetype de los datos: " + e); //$NON-NLS-1$
				}

				// Obtenemos los parametros necesarios segun tipo de contrafirma. Esto son los firmantes
				// para la contrafirma de firmantes, los nodos para la contrafirma de nodos y ninguno
				// para la contrafirma de todos los nodos o solo los nodos hoja.
				Object[] params = null;

				// Obtenemos los parametros para la contrafirma de firmantes
				if(target == AOSignConstants.CounterSignTarget.Signers) {
					if(SignApplet.this.signersToCounterSign == null || SignApplet.this.signersToCounterSign.length < 1) {
						try {
							params = AOUIManager.showSignersSelectionPane(signer.getSignersStructure(new ByteArrayInputStream(originalSign), false), SignApplet.this);
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
				else if(target == AOSignConstants.CounterSignTarget.Nodes) {
					// Si no se establecen los nodos de firma mediante setSignersToCounterSign(String)
					// mostramos el panel de seleccion de nodos. Este panel devolvera los nodos de firma
					// a partir del 0.
					if(SignApplet.this.signersToCounterSign == null || SignApplet.this.signersToCounterSign.length < 1) {
						int[] indexes;
						try {
							indexes = AOUIManager.showNodeSignSelectionPane(signer.getSignersStructure(new ByteArrayInputStream(originalSign), false), SignApplet.this);
						} 
						catch (final AOCancelledOperationException ex) {
							logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
							return false;
						}
						catch (final Throwable ex) {
							logger.severe("Error al seleccionar los nodos de firma: " + ex); //$NON-NLS-1$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.99")); //$NON-NLS-1$
							return false;
						}

						if(indexes.length == 0) {
							logger.severe("Se debe seleccionar al menos un nodo de firma para contrafirmar"); //$NON-NLS-1$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.100")); //$NON-NLS-1$
							return false;
						}

						// Ordenamos el array de indices (requisito del metodo de contrafirma de nodos)
						java.util.Arrays.sort(indexes);

						// Los indices de firma del panel de seleccion se reciben a partir del 0.
						params = new Object[indexes.length];
						for(int i = 0; i<indexes.length; i++) params[i] = Integer.valueOf(indexes[i]);
					}
					else {
						params = new Object[SignApplet.this.signersToCounterSign.length];
						for(int i=0; i<SignApplet.this.signersToCounterSign.length; i++)
							params[i] = Integer.valueOf(SignApplet.this.signersToCounterSign[i]);
					}
				}

				// Si se han especificado atributos de firma los agregamos. Esto solo sera efectivo
				// para los signers a los que aplique
				SignApplet.this.addAttributes(signer);

				// Si se nos pide que mostremos el hash de los datos a firmar, lo hacemos
				if(SignApplet.this.showHashes) {
					if(!SignApplet.this.showHashMessage()) {
						logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
						return false;
					}
				}

				// Contrafirmamos finalmente
				byte[] outputBuffer;
				try {
					outputBuffer = signer.countersign(
							new ByteArrayInputStream(originalSign),
							algorithm,
							target,
							params,
							ke,
							certToSign,
							null
					);
				}
				catch (UnsupportedOperationException e) {
					logger.severe(e.getMessage());
					SignApplet.this.setError(AppletMessages.getString("SignApplet.2")); //$NON-NLS-1$
					JOptionPane.showMessageDialog(
							SignApplet.this, 
							AppletMessages.getString(AppletMessages.getString("SignApplet.682")),  //$NON-NLS-1$
							AppletMessages.getString("SignApplet.156"), //$NON-NLS-1$ 
							JOptionPane.ERROR_MESSAGE
					);
					return false;
				}
				catch(final Throwable e) {
					logger.severe("Error durante el proceso de firma: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.101")); //$NON-NLS-1$
					return false;
				}

				// Ahora vamos a guardar el resultado en el fichero de salida
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


	public void setOutFilePath(String outFilePath) {
		logger.info("Invocando setOutFilePath: " + outFilePath); //$NON-NLS-1$
		if (outFilePath == null) {
			logger.info(
					"Se ha establecido el nombre de fichero de salida a null" //$NON-NLS-1$
			);
			outputFile = null;
			return;
		}
		if(outFilePath.length() < 1) {
			logger.warning(
					"El nombre de fichero de salida indicado esta vacio, se establecera a null" //$NON-NLS-1$
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
		signatureAlgorithm = this.translateToNewAlgorithmName(signatureAlgorithm);
		if(signatureAlgorithm == null) {
			logger.warning(
					"El algoritmo de firma no puede ser nulo, se establecera el algoritmo por defecto" //$NON-NLS-1$
			);
			signatureAlgorithm = AOConstants.DEFAULT_SIGN_ALGO;
		}

		// Localizamos el algoritmo indicado entre los soportados
		for (String algo : AOConstants.SUPPORTED_SIGN_ALGOS) {
			if(algo.equals(signatureAlgorithm)) {
				sigAlgo = signatureAlgorithm;
				return;
			}
		}
		
		// Si el algoritmo no esta soportado, indicamos los soportado y establecemos el por defecto
		StringBuilder exstr = new StringBuilder("El algoritmo de firma '") //$NON-NLS-1$
			.append(signatureAlgorithm).append("' no esta soportado, se establecera el algoritmo por defeco: ")
			.append(AOConstants.DEFAULT_SIGN_ALGO)
			.append("\nLos algoritmos de firma soportados son:\n"); //$NON-NLS-1$
		for (String algo : AOConstants.SUPPORTED_SIGN_ALGOS) {
			exstr.append(algo).append("\n");
		}
		logger.warning(exstr.toString());
		sigAlgo = AOConstants.DEFAULT_SIGN_ALGO;
	}

	public void setSignatureFormat(String signatureFormat) {
		logger.info("Invocando setSignatureFormat: " + signatureFormat); //$NON-NLS-1$

		// Si no se establece formato alguno, se mantiene el por defecto
		if(signatureFormat == null) {
			logger.warning(
					"El formato de firma no puede ser nulo, se establecera el formato por defecto: "+AOConstants.DEFAULT_SIGN_FORMAT //$NON-NLS-1$
			);
			signatureFormat = AOConstants.DEFAULT_SIGN_FORMAT;
		}
		
		// Para mantener la interfaz con el exterior intacta, traduciremos
		// cualquier nombre de formato antiguo a su nueva forma
		this.sigFormat = this.translateToNewFormatName(signatureFormat);
	}

	public void setSignatureMode(String mode) {
		logger.info("Invocando setSignatureMode: " + mode); //$NON-NLS-1$
		// Para mantener la interfaz con el exterior intacta, traduciremos
		// cualquier nombre de modo antiguo a su nueva forma
		mode = this.translateToNewModeName(mode);

		if(mode == null) {
			logger.warning(
					"El modo de firma no puede ser nulo, se establecera el modo por defecto" //$NON-NLS-1$
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

		logger.info("Invocando setKeyStore de tipo '"+type+"' con el path '"+path+"'"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
			public Void run() {
				AOKeyStore newStore = AOKeyStoreManager.getKeyStore(
						SignApplet.this.translateToKeyStoreDescription(type));

				if(newStore == null) {
					setError(AppletMessages.getString("SignApplet.3")); //$NON-NLS-1$ //$NON-NLS-2$
					return null;
				}
				
				String storePath = (path == null || path.trim().equals("")) ? null : path; //$NON-NLS-1$
				String storePass = (password == null || password.trim().equals("")) ? null : password; //$NON-NLS-1$

				// Guardamos la contrasena porque nos es necesaria tanto para abrir el almacen como
				// para utilizar el certificado
				SignApplet.this.keystorePassword =(storePass == null ? null : storePass.toCharArray());
				AOKeyStore oldStore = SignApplet.this.store;
				SignApplet.this.store = newStore;
				try {
					SignApplet.this.initKeyStore(storePath, null, storePass);
				}
				catch (final AOCancelledOperationException e) {
					logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
					setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					return null;
				}
				catch (final Throwable e) {
					logger.severe("Error al iniciar el repositorio de certificados, se volvera al almacen por defecto para el sistema/operativo navegador: " + e); //$NON-NLS-1$
					setError(AppletMessages.getString("SignApplet.1")); //$NON-NLS-1$
					SignApplet.this.store = oldStore;
					return null;
				}
				setError(null);
				return null;
			}
		});
	}

	public void setPolicy(String identifier, String description, String qualifier) {
		// Configuramos la URL identificadora 
		if(identifier != null) {
			try {
				this.policyId = AOUtil.createURI(identifier).toURL();
			} catch (Throwable e) {
				logger.severe("La politica indicada no es una URL valida"); //$NON-NLS-1$
			}
		}
		// Configuramos Oid cualificador
		if(qualifier != null) {
			try {
				this.policyQualifier = new Oid(qualifier);
			} catch (Throwable e) {
				logger.severe("El cualificador indicado no es un OID valido"); //$NON-NLS-1$
			}
		}
		// Configuramos la descripcion
		this.policyDesc = description;
	}

	public boolean sign() {
		logger.info("Invocando sign"); //$NON-NLS-1$

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {

				// Si no esta establecido el algoritmo usamos el por defecto, pero solo para esta ocasion,
				// no lo establecemos para posteriores
				String algorithm = (sigAlgo == null ? AOConstants.DEFAULT_SIGN_ALGO : sigAlgo);

				// Si no esta establecido el formato usamos el por defecto, pero solo para esta ocasion,
				// no lo establecemos para posteriores
				String format = (sigFormat == null ? AOConstants.DEFAULT_SIGN_FORMAT : sigFormat);

				// Si no esta establecido el modo usamos el por defecto, pero solo para esta ocasion,
				// no lo establecemos para posteriores
				String mode = (sigMode == null ? AOConstants.DEFAULT_SIGN_MODE : sigMode);

				// Para mantener las formas de la version 2.4 del cliente, se mostrara una
				// ventana modal, en caso de solicitarse una firma Enveloped en modo explicito,
				// informando de que esta configuracion es imposible 
				if((format.equals(AOConstants.SIGN_FORMAT_XADES_ENVELOPED) ||
						format.equals(AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)) &&
						mode.equals(AOConstants.SIGN_MODE_EXPLICIT)) {

					logger.severe("El formato Enveloped es incompatible con el modo de firma explicito"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.139")); //$NON-NLS-1$
					return false;
				}

				// Tomamos el Signer adecuado
				AOSigner signer = AOCryptoUtil.getSigner(format);
				if(signer == null) {
					logger.severe("El formato de firma '" + format +  //$NON-NLS-1$
							"' no esta soportado. Lo formatos soportados son:\n" +  //$NON-NLS-1$
							AOSignerFactory.getInstance().toString());
					SignApplet.this.setError(AppletMessages.getString("SignApplet.95") + format); //$NON-NLS-1$
					return false;
				}

				// Como condicion excepcional, si se nos ha introducido un hash para firmar 
				// estableceremos el algoritmo hash que se realizo el hash segun el algoritmo que
				// se nos solicitase
				if(hash != null && mode.equals(AOConstants.SIGN_MODE_EXPLICIT)) {
					int withPos = algorithm.indexOf("with");
					if(withPos == -1) { //$NON-NLS-1$
						logger.severe("El formato del algoritmo de firma no es valido: " + algorithm); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.197") + algorithm); //$NON-NLS-1$
					}
					genericConfig.setProperty("precalculatedHashAlgorithm", algorithm.substring(0, withPos));
				}

				// -----------------------
				// Evitamos las configuraciones de firma de hashes no soportadas
				// -----------------------

				// La firma de hashes solo esta soportada por los formatos de firma: CMS, CAdES, XMLdSig y XAdES
				if(hash != null && (sigFormat.equals(AOConstants.SIGN_FORMAT_PDF) 
						|| sigFormat.equals(AOConstants.SIGN_FORMAT_ODF)
						|| sigFormat.equals(AOConstants.SIGN_FORMAT_OOXML)
						|| sigFormat.equals(AOConstants.SIGN_FORMAT_PKCS1))) {

					logger.severe("La firma de hash no esta soportada para el formato " + sigFormat); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.198") + sigFormat); //$NON-NLS-1$
					return false;
				}

				// La firma implicita de hash exige que se introduzcan los datos a los que corresponde el hash.
				// Deben haberse introducido los datos, no se permite el fichero por incompatibilidad con
				// las funciones de soporte del estilo "getFileBase64Encoded", "getFileHashBase64Encoded",...
				if(hash != null && data == null && mode.equals(AOConstants.SIGN_MODE_IMPLICIT)) {
					logger.severe("La firma implicita de hash exige que se introduzcan los datos a los que corresponde el hash"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.216")); //$NON-NLS-1$
					return false;
				}

				logger.info("Firma con algoritmo " + algorithm + ", formato " + format + " y modo " + mode); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

				// Si no se nos ha introducido un listado de hashes entendemos que debe realizarse
				// una firma corriente. En este caso, se tomara cualquier parametro de entrada establecido
				// como origen de los datos (dato, fichero y hash).
				InputStream streamToSign = null;
				if(SignApplet.this.hashesToSign == null) {

					try {
						streamToSign = SignApplet.this.getInDataStream();
					} catch (AOException e) {
						// El metodo getInDataStream ya se habra encargado de establecer el mensaje en caso de error
						return false;
					}

					// Establecemos el formato de los datos
					String mimeType = SignApplet.this.extMimeType != null ? SignApplet.this.extMimeType : SignApplet.this.dataMimeType;
					if (mimeType == null) signer.setDataObjectFormat(SignApplet.this.dataDescription, null, null, null);
					else {
						try {
							String oid = MimeHelper.transformMimeTypeToOid(mimeType);
							signer.setDataObjectFormat(SignApplet.this.dataDescription, oid == null ? null : new Oid(oid), new javax.activation.MimeType(mimeType), null);
						} 
						catch (final Throwable e) {
							logger.warning("No se ha podido establecer el formato de los datos firmados: " + e); //$NON-NLS-1$
						}
					}
				}

				// Si no hay certificado seleccionado mostramos la lista de seleccion
				String aliasToUse = null;
				try {
					if (selectedAlias == null) {
						if (certAlias == null) SignApplet.this.getCertificatesAlias();
						selectedAlias = aliasToUse = SignApplet.this.selectCertificate(true);
					}
					else aliasToUse = selectedAlias;
				} 
				catch(final AOCancelledOperationException e) {
					logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					return false;
				} 
				catch (final AOException e) {
					logger.severe("Error al seleccionar un certificado del repositorio: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.363")); //$NON-NLS-1$
					return false;
				}

				// Comprobamos que finalmente se haya seleccionado un alias
				if (aliasToUse == null) {
					logger.severe("Error al seleccionar un certificado del repositorio"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.363")); //$NON-NLS-1$
					return false;
				}

				// Comprobamos que el repositorio de certificados este inicializado 
				if (aoKsManager == null) {
					logger.severe("No se pudo recuperar el KeyStore asociado al alias"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.91") + aliasToUse); //$NON-NLS-1$
					return false;
				}

				// Llegados a este punto necesitamos la clave y el certificado, que los obtenemos
				// a partir del alias...
				PrivateKeyEntry ke;
				try {
					ke = aoKsManager.getKeyEntry(aliasToUse,
							SignApplet.this.keystorePassword == null ?
									AOCryptoUtil.getCertificatePC(SignApplet.this.store) :
										new CachePasswordCallback(SignApplet.this.keystorePassword));
				}
				catch(final AOCancelledOperationException e) {
					logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					return false;
				}
				catch (final Throwable e) {
					logger.severe("No se ha podido obtener el certicado con el alias '" + aliasToUse + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.94") + ": " + aliasToUse); //$NON-NLS-1$ //$NON-NLS-2$
					return false;
				}

				// Recuperamos el certificado a traves del KeyEntry y lo almacenamos porque posteriormente
				// a la firma pueden pedirnos informacion sobre el certificado utilizado
				certToSign = aoKsManager.getCertificate(ke);

				// Si se nos ha introducido un listado de hashes entendemos que deben firmarse estos.
				// En este caso, se ignorara cualquier otro parametro de entrada de datos de firma (dato,
				// fichero y hash).
				if(SignApplet.this.hashesToSign != null && SignApplet.this.hashesToSign.size() > 0) {

					String[] signs = null;
					DirectorySignatureHelper massiveSigner = new DirectorySignatureHelper(algorithm, format, mode);


					// Recuperamos el signer que se utilizara para la operacion de firma masiva para poder agregarle
					// antes los atributos de firma que correspondan
					AOSigner configuredSigner = SignApplet.this.addAttributes(massiveSigner.getDefaultSigner());

					// Configuramos la politica
					SignApplet.this.configurePolicy();

					// Configuramos las transformaciones XML 
					SignApplet.this.configureXMLTransforms();

					// Configuramos y ejecutamos la operacion 
					genericConfig.setProperty("format", format);
					genericConfig.setProperty("mode", mode);
					genericConfig.setProperty("ignoreStyleSheets", "true");

					try {
						signs = massiveSigner.hashesMassiveSign(SignApplet.this.hashesToSign.toArray(new String[0]), ke, certToSign, configuredSigner, SignApplet.this.showHashes, genericConfig);
					} 
					catch (final AOException e) {
						logger.severe("Error durante la operacion de firma masiva de hashes: " + e); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.236")); //$NON-NLS-1$
						return false;
					}
					StringBuilder allSigns = new StringBuilder();
					for(int i=0; i<signs.length; i++) {
						allSigns.append(signs[i]);
						if(i<signs.length-1) {
							allSigns.append("!"); //$NON-NLS-1$
						}
					}
					SignApplet.this.massiveSignData = allSigns.toString();
				}

				// Realizamos la operacion corriente de firma.
				else {

					// Si se han especificado atributos de firma los agregamos. Esto solo sera efectivo
					// para los signers a los que aplique
					SignApplet.this.addAttributes(signer);

					// Configuramos la politica
					SignApplet.this.configurePolicy();

					// Configuramos las trasnformaciones XML
					SignApplet.this.configureXMLTransforms();

					// Si se nos pide que mostremos el hash de los datos a firmar, lo hacemos
					if(SignApplet.this.showHashes) {
						if(!SignApplet.this.showHashMessage()) {
							logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
							return false;
						}
					}

					// Agregamos las ultimas configuraciones y firmamos
					genericConfig.setProperty("mode", mode);
					genericConfig.setProperty("format", format);
					if(SignApplet.this.fileUri != null)
						genericConfig.setProperty("uri", SignApplet.this.fileUri.toASCIIString());

					byte[] outputBuffer;
					try {
						outputBuffer = signer.sign(
								streamToSign,
								algorithm,
								ke,
								certToSign,
								genericConfig
						);
					}
					catch (UnsupportedOperationException e) {
						logger.severe(e.getMessage()); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString(AppletMessages.getString("SignApplet.682"))); //$NON-NLS-1$
						return false;
					}
					catch (AOFormatFileException e) {
						logger.severe(e.getMessage()); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.11")); //$NON-NLS-1$
						return false;
					}
					catch(AOException e) {
						logger.severe(e.toString()); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.101")); //$NON-NLS-1$
						e.printStackTrace();
						return false;
					}
					catch(Throwable e) {
						logger.severe("Error durante el proceso de firma: " + e); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.101")); //$NON-NLS-1$
						e.printStackTrace();
						return false;
					} finally {
							closeStream(streamToSign);
					}

					// Ahora vamos a guardar el resultado en el fichero de salida
					if (outputBuffer == null || outputBuffer.length < 1) {
						// No vaya a ser que saliese un resultado vacio...
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

	/**
	 * Agrega las transformaciones XML configuradas en el cliente a la configuraci&oacute;n de firma.
	 */
	private void configureXMLTransforms() {
		if(xmlTransforms != null) {
			genericConfig.setProperty("xmlTransforms", Integer.toString(xmlTransforms.size()));
			for(int i=0; i< xmlTransforms.size(); i++) {
				genericConfig.setProperty("xmlTransform"+(i)+"Type", xmlTransforms.get(i).getType());
				// El subtipo y el cuerpo son opcionales
				if(xmlTransforms.get(i).getSubtype() != null)
					genericConfig.setProperty("xmlTransform"+(i)+"Subtype", xmlTransforms.get(i).getSubtype());
				if(xmlTransforms.get(i).getBody() != null)
					genericConfig.setProperty("xmlTransform"+(i)+"Body", xmlTransforms.get(i).getBody());
			}
		}
	}

	/**
	 * Agrega los atributos firmados y sin firmar definidos al manejador de firma cuando este
	 * corresponda a un formato de firma que los soporte.
	 * @param signer Manejador de firma.
	 * @return Manejador de firma configurado.
	 */
	private AOSigner addAttributes(AOSigner signer) {
		// Si el Signer soporta la agregacion de atributos
		if(signer instanceof AOCMSSigner || signer instanceof AOCADESSigner) {

			// Agregamos los atributos firmados
			if(SignApplet.this.signedAttributes != null) {
				Iterator<org.ietf.jgss.Oid> itOid = SignApplet.this.signedAttributes.keySet().iterator();
				while(itOid.hasNext()) {
					org.ietf.jgss.Oid oid = itOid.next();
					((AOCMSSigner)signer).addSignedAttribute(oid, SignApplet.this.signedAttributes.get(oid).getBytes());
				}
			}

			// Agregamos los atributos sin firmar
			if(SignApplet.this.unsignedAttributes != null) {
				Iterator<org.ietf.jgss.Oid> itOid = SignApplet.this.unsignedAttributes.keySet().iterator();
				while(itOid.hasNext()) {
					org.ietf.jgss.Oid oid = itOid.next();
					for(String value : SignApplet.this.unsignedAttributes.get(oid)) {
						((AOCMSSigner)signer).addUnsignedAttribute(oid, value.getBytes());
					}
				}
			}
		}
		return signer;
	}


	/**
	 * Agrega la pol&iacute;tica de firma a la configuraci&oacute;n de la operaci&oacute;n de firma.
	 * @see #setPolicy(String, String, String)
	 */
	private void configurePolicy() {

		if(this.policyId != null) genericConfig.setProperty("policyIdentifier", this.policyId.toString());
		if(this.policyDesc != null) genericConfig.setProperty("policyDescription", this.policyDesc);
		if(this.policyQualifier != null) genericConfig.setProperty("policyQualifier", this.policyQualifier.toString());
	}

	public boolean coSign() {
		logger.info("Invocando cosign"); //$NON-NLS-1$

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {

				// No establecemos algoritmo por defecto, si no esta establecido usamos el por
				// defecto, pero solo para esta ocasion
				String algorithm = (sigAlgo == null ? AOConstants.DEFAULT_SIGN_ALGO : sigAlgo);

				// No establecemos formato por defecto, si no esta establecido usamos el por
				// defecto, pero solo para esta ocasion
				String format = (sigFormat == null ? AOConstants.DEFAULT_SIGN_FORMAT : sigFormat);

				// No establecemos formato por defecto, si no esta establecido usamos el por
				// defecto, pero solo para esta ocasion
				String mode = (sigMode == null ? AOConstants.DEFAULT_SIGN_MODE : sigMode);

				// Para mantener las formas de la version 2.4 del cliente, se mostrara una
				// ventana modal, en caso de solicitarse una firma Enveloped en modo explicito,
				// informando de que esta configuracion es imposible 
				if((format.equals(AOConstants.SIGN_FORMAT_XADES_ENVELOPED) ||
						format.equals(AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)) &&
						mode.equals(AOConstants.SIGN_MODE_EXPLICIT)) {
					logger.severe("El formato Enveloped es incompatible con el modo de firma explicito");  //$NON-NLS-1$
					setError(AppletMessages.getString("SignApplet.139"));  //$NON-NLS-1$
					return false;
				}

				// Tomamos los datos que debemos firmar
				InputStream streamToSign = null;
				try {
					streamToSign = SignApplet.this.getInDataStream();
				} catch (AOException e) {
					// El metodo getInDataStream ya se habra encargado de establecer el mensaje en caso de error
					return false;
				}

				// Tomamos la firma sobre la que se realiza la contrafirma
				InputStream originalSign = null;
				try {
					originalSign = new ByteArrayInputStream(SignApplet.this.getSelectedSignature(true));
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
				if(signer == null) {
					logger.severe("El formato de firma '" + format +  //$NON-NLS-1$
							"' no esta soportado. Lo formatos soportados son:\n" +  //$NON-NLS-1$
							AOSignerFactory.getInstance().toString());
					SignApplet.this.setError(AppletMessages.getString("SignApplet.95") + format); //$NON-NLS-1$
					return false;
				}

				if(hash != null && mode.equals(AOConstants.SIGN_MODE_EXPLICIT)) {
					int withPos = algorithm.indexOf("with"); 
					if(withPos == -1) { //$NON-NLS-1$
						logger.severe("El formato del algoritmo de firma no es valido: " + algorithm); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.197") + algorithm); //$NON-NLS-1$
						return false;
					}
					// Establecemos el algoritmo con el que se calculo el hash externamente
					genericConfig.setProperty("precalculatedHashAlgorithm", algorithm.substring(0, withPos));
				}

				/*
				 * La firma de hashes solo esta soportada por los formatos de firma binaria CMS y CAdES.
				 * Las firmas PDF, ODF y OOXML requieren siempre los datos, ya que van empotradas. Las
				 * firmas XML los necesitan para hacer la referencia a los mismos.
				 */
				if(hash != null && !sigFormat.equals(AOConstants.SIGN_FORMAT_CMS) && 
						!sigFormat.equals(AOConstants.SIGN_FORMAT_CADES)) {
					logger.severe("La firma de hashes solo esta soportada por los formatos de firma binaria CMS y CAdES"); //$NON-NLS-1$
					setError(AppletMessages.getString("SignApplet.277")); //$NON-NLS-1$			//TODO: Permitir cofirma de hashes en XML
					return false;
				}

				/*
				 * Evitamos las configuraciones de firma de hashes no soportadas
				 */

				// La firma implicita de hash exige que se introduzcan los datos a los que corresponde el hash
				if(hash != null && data == null && mode.equals(AOConstants.SIGN_MODE_IMPLICIT)) {
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
						signer.setDataObjectFormat(SignApplet.this.dataDescription, oid == null ? null : new Oid(oid), new javax.activation.MimeType(mimeType), null);
					} 
					catch (final Throwable e) {
						logger.warning("No se ha podido establecer el formato de los datos firmados: " + e); //$NON-NLS-1$
					}
				}

				// Si no hay certificado seleccionado mostramos la lista de seleccion
				String aliasToUse = null;
				try {
					if (selectedAlias == null) {
						if (certAlias == null) SignApplet.this.getCertificatesAlias();
						selectedAlias = aliasToUse = SignApplet.this.selectCertificate(true);
					}
					else aliasToUse = selectedAlias;
				} 
				catch(AOCancelledOperationException e) {
					logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					return false;
				} 
				catch (AOException e) {
					logger.severe(e.getMessage()+": "+e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.340")); //$NON-NLS-1$
					return false;
				}

				// Comprobamos que finalmente se haya seleccionado un alias
				if (aliasToUse == null) {
					logger.severe("Error al seleccionar un certificado del repositorio"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.363")); //$NON-NLS-1$
					return false;
				}

				// Comprobamos que este inicializado el repositorio
				if (aoKsManager == null) {
					logger.severe("No se pudo recuperar el almacen de certificados asociado al alias"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.91") + aliasToUse); //$NON-NLS-1$
					return false;
				}

				// Llegados a este punto necesitamos la clave y el certificado, que los obtenemos
				// a partir del alias...
				PrivateKeyEntry ke;
				try {
					ke = aoKsManager.getKeyEntry(aliasToUse, AOCryptoUtil.getCertificatePC(SignApplet.this.store));
				}
				catch(AOCancelledOperationException e) {
					logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					return false;
				}
				catch (Throwable e) {
					logger.severe("No se ha podido obtener el certicado con el alias " + aliasToUse + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.94") + aliasToUse); //$NON-NLS-1$
					return false;
				}

				byte[] outputBuffer;

				// Recuperamos el certificado a traves del KeyEntry y lo almacenamos porque posteriormente
				// pueden pedirnos informacion sobre el certificado utilizado
				certToSign = aoKsManager.getCertificate(ke);

				// Si se han especificado atributos de firma los agregamos. Esto solo sera efectivo
				// para los signers a los que aplique
				SignApplet.this.addAttributes(signer);

				// Si se nos pide que mostremos el hash de los datos a firmar, lo hacemos
				if(SignApplet.this.showHashes) {
					if(!SignApplet.this.showHashMessage()) {
						logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
						return false;
					}
				}

				// Finalmente, configuramos y operamos
				genericConfig.setProperty("mode", mode);
				if(SignApplet.this.fileUri != null)
					genericConfig.setProperty("uri", SignApplet.this.fileUri.toASCIIString());

				try {
					outputBuffer = signer.cosign (
							streamToSign,
							originalSign,
							algorithm,
							ke,
							certToSign,
							genericConfig
					);
				}
				catch (UnsupportedOperationException e) {
					logger.severe(AppletMessages.getString("SignApplet.682") + ": " + e.getMessage());  //$NON-NLS-1$  //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.682")); //$NON-NLS-1$
					return false;
				}
				catch(Throwable e) {
					logger.severe("Error durante el proceso de firma: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.101")); //$NON-NLS-1$
					e.printStackTrace();
					return false;
				} finally {
					// Cerramos lo flujos de datos
					try { streamToSign.close(); } catch (Exception e) { }
					try { originalSign.close(); } catch (Exception e) { }
				}

				// Ahora vamos a guardar el resultado en el fichero de salida
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

	/**
	 * Muestra un di&aacute;logo para la selecci&oacute;n de un fichero de firma. En caso de que se
	 * indique un formato, se usaran filtros para las extensiones predeterminadas para el formato de
	 * firma concreto. Si no se selecciona ningun fichero, se devolver&aacute; {@code null}.
	 * @param signFormat Formato de la firma que se desea seleccionar.
	 * @return Fichero de firma.
	 */
	private String selectSignFile(String signFormat) {

		String[] exts = null;
		String desc = null;

		if(signFormat != null) {
			if(signFormat.equals(AOConstants.SIGN_FORMAT_CMS)) {
				exts = new String[]{"csig", "p7s", "sig"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				desc = AppletMessages.getString("SignApplet.29"); //$NON-NLS-1$
			}
			else if(signFormat.equals(AOConstants.SIGN_FORMAT_CADES)) {
				exts = new String[]{"csig", "sig"}; //$NON-NLS-1$ //$NON-NLS-2$
				desc = AppletMessages.getString("SignApplet.26"); //$NON-NLS-1$
			}
			else if(signFormat.equals(AOConstants.SIGN_FORMAT_XMLDSIG_DETACHED) ||
					signFormat.equals(AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED) ||
					signFormat.equals(AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING) ||
					signFormat.equals(AOConstants.SIGN_FORMAT_XADES_DETACHED) ||
					signFormat.equals(AOConstants.SIGN_FORMAT_XADES_ENVELOPED) ||
					signFormat.equals(AOConstants.SIGN_FORMAT_XADES_ENVELOPING)) {
				exts = new String[]{"xsig", "sig", "xml"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				desc = AppletMessages.getString("SignApplet.27"); //$NON-NLS-1$
			}
			else if(signFormat.equals(AOConstants.SIGN_FORMAT_PKCS1)) {
				exts = new String[]{"sig"}; //$NON-NLS-1$
				desc = AppletMessages.getString("SignApplet.318"); //$NON-NLS-1$
			}
			else if(signFormat.equals(AOConstants.SIGN_FORMAT_PDF)) {
				exts = new String[]{"pdf"}; //$NON-NLS-1$
				desc = AppletMessages.getString("SignApplet.28"); //$NON-NLS-1$
			}
			else if(signFormat.equals(AOConstants.SIGN_FORMAT_ODF)) {
				exts = new String[]{"odt", "ods", "odp"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				desc = AppletMessages.getString("SignApplet.32"); //$NON-NLS-1$
			}
			else if(signFormat.equals(AOConstants.SIGN_FORMAT_OOXML)) {
				exts = new String[]{"docx", "xlsx", "pptx"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				desc = AppletMessages.getString("SignApplet.30"); //$NON-NLS-1$
			}
		}

		return AOUIManager.getLoadFileName(
				AppletMessages.getString("SignApplet.163"),  //$NON-NLS-1$
				exts,
				desc,
				SignApplet.this
		);
	}

	public void setInIncludeExtensions(String extensions) {
		logger.info("Invocando setInIncludeExtensions: " + extensions); //$NON-NLS-1$
		this.massiveExtFiltered = (extensions == null ? null : extensions.split(",")); //$NON-NLS-1$
	}

	public boolean signDirectory() {
		logger.info("Invocando signDirectory"); //$NON-NLS-1$

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {
				if (
						(sigFormat.equals(AOConstants.SIGN_FORMAT_XADES_ENVELOPED) && sigMode.equals(AOConstants.SIGN_MODE_EXPLICIT)) ||
						(sigFormat.equals(AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED) && sigMode.equals(AOConstants.SIGN_MODE_EXPLICIT))
				) {
					error = true;
					logger.severe("El formato Enveloped es incompatible con el modo de firma explicito");  //$NON-NLS-1$
					setError(AppletMessages.getString("SignApplet.139")); //$NON-NLS-1$
					return false;
				}

				// Si no se ha establecido el directorio de entrada de ficheros, lo solicitamos
				String inputDir;
				if(SignApplet.this.massiveInputDirectory != null) {
					inputDir = SignApplet.this.massiveInputDirectory;
				}
				else {
					inputDir = AOUIManager.selectDirectory(SignApplet.this, AppletMessages.getString("SignApplet.187")); //$NON-NLS-1$
					if(inputDir.equals("")) { //$NON-NLS-1$
						logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
						return false;
					}
				}

				// Establecemos el directorio de salida, si no existe se creara un directorio temporal
				// al que poder acceder para la posterior lectura de los datos de firma
				String outputDir;
				if(SignApplet.this.massiveOutputDirectory != null) {
					outputDir = SignApplet.this.massiveOutputDirectory;
				}
				else {
					logger.warning("No se ha indicado un directorio para el guardado de los firmas generadas, se almacenaran en el mismo directorio de entrada: " + inputDir); //$NON-NLS-1$
					outputDir = inputDir;

					//					File tempDir;
					//					try {
					//						tempDir = File.createTempFile("afirma5_firmamasiva", "_tmp"); //$NON-NLS-1$ //$NON-NLS-2$
					//						// File.createTempFile(String) crea un fichero temporal, como nosotros queremos un directorio
					//						// lo eliminamos y nos quedamos con el objeto File
					//						tempDir.delete();
					//						outputDir = tempDir.getAbsolutePath();
					//					} catch (IOException e) {
					//						logger.severe(AppletMessages.getString("SignApplet.193")+": " + e);
					//						SignApplet.this.setError(AppletMessages.getString("SignApplet.193")); //$NON-NLS-1$
					//						return false;
					//					}
				}

				// Si no hay certificado seleccionado mostramos la lista de seleccion
				String aliasToUse = null;
				try {
					if (selectedAlias == null) {
						if (certAlias == null) getCertificatesAlias();
						selectedAlias = aliasToUse = SignApplet.this.selectCertificate(true);
					}
					else aliasToUse = selectedAlias;
				} 
				catch(AOCancelledOperationException e) {
					logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					return false;
				} 
				catch (AOException e) {
					logger.severe(e.getMessage()+": "+e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.340")); //$NON-NLS-1$
					return false;
				}

				if (aliasToUse == null) {
					logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					return false;
				}

				// Llegados a este punto necesitamos la clave y el certificado, que los obtenemos
				// a partir del alias...
				if (aoKsManager == null) {
					logger.severe("No se pudo recuperar el almacen de certificados asociado al alias: " +aliasToUse); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.91") + aliasToUse); //$NON-NLS-1$
					return false;
				}

				PrivateKeyEntry ke;
				try {
					ke = aoKsManager.getKeyEntry(aliasToUse, AOCryptoUtil.getCertificatePC(SignApplet.this.store));
				}
				catch(AOCancelledOperationException e) {
					logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					return false;
				}
				catch(Throwable e) {
					logger.severe("No se ha podido obtener el certicado con el alias '" + aliasToUse + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.94") + aliasToUse); //$NON-NLS-1$
					return false;
				}

				// Creamos el manejador de firma masiva
				DirectorySignatureHelper massiveSigner;
				try {
					massiveSigner = new DirectorySignatureHelper(
							SignApplet.this.sigAlgo,
							SignApplet.this.sigFormat,
							SignApplet.this.sigMode
					);
				} 
				catch (Throwable e) {
					logger.severe("No se pudo inicializar el modulo de firma masiva: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.200")); //$NON-NLS-1$
					return false;
				}

				// Le introducimos el filtro al manejador de firma masiva
				if(SignApplet.this.massiveExtFiltered != null && SignApplet.this.massiveExtFiltered.length > 0) {
					StringBuilder description = new StringBuilder(AppletMessages.getString("SignApplet.201")); //$NON-NLS-1$
					for(int i=0; i < massiveExtFiltered.length; i++) {
						description.append("*.").append(massiveExtFiltered[i]); //$NON-NLS-1$
						if(i+1 != massiveExtFiltered.length) description.append(","); //$NON-NLS-1$
					}
					massiveSigner.setFileFilter(new AOUIManager.ExtFilter(massiveExtFiltered, description.toString()));
				}

				// Recuperamos el certificado a traves del KeyEntry y lo almacenamos porque posteriormente
				// pueden pedirnos informacion sobre el certificado utilizado
				certToSign = aoKsManager.getCertificate(ke);

				// Configuramos la operacion
				SignApplet.this.genericConfig.setProperty("format", SignApplet.this.sigFormat);
				SignApplet.this.genericConfig.setProperty("mode", SignApplet.this.sigMode);
				SignApplet.this.configurePolicy();
				SignApplet.this.configureXMLTransforms();

				boolean allOk = true;
				try {
					allOk = massiveSigner.massiveSign(
							SignApplet.this.massiveOperation,
							inputDir,
							SignApplet.this.recursiveSignDir,
							outputDir,
							true,
							SignApplet.this.originalFormat,
							ke,
							certToSign,
							SignApplet.this.genericConfig
					);
				} catch (Throwable e) {
					logger.severe("Error grave durante la operacion de firma masiva: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.205")); //$NON-NLS-1$
					return false;
				}

				if(allOk) SignApplet.this.setError(null);

				return allOk;
			}
		});
	}

	public void setMassiveOperation(String massiveOperationName) {
		logger.info("Invocando setMassiveOperation: " + massiveOperationName); //$NON-NLS-1$

		if(massiveOperationName == null || massiveOperationName.equals("")) { //$NON-NLS-1$
			massiveOperationName = AOConstants.DEFAULT_MASSIVE_OPERATION;
		}

		if(massiveOperationName.equals(AOConstants.MASSIVE_OPERATION_SIGN)) {
			this.massiveOperation = MassiveType.SIGN;
		}
		else if(massiveOperationName.equals(AOConstants.MASSIVE_OPERATION_COSIGN)) {
			this.massiveOperation = MassiveType.COSIGN;
		}
		else if(massiveOperationName.equals(AOConstants.MASSIVE_OPERATION_COUNTERSIGN_TREE)) {
			this.massiveOperation = MassiveType.COUNTERSIGN_ALL;
		}
		else if(massiveOperationName.equals(AOConstants.MASSIVE_OPERATION_COUNTERSIGN_LEAFS)) {
			this.massiveOperation = MassiveType.COUNTERSIGN_LEAFS;
		}
		else {
			logger.warning(
					"Operacion masiva no reconocida, se realizara la operacion: "+ //$NON-NLS-1$
					AOConstants.DEFAULT_MASSIVE_OPERATION
			);
			setMassiveOperation(AOConstants.DEFAULT_MASSIVE_OPERATION);
		}

		// Si ya hay una configuracion de firma masiva establecida, la actualizamos con la nueva operacion
		if(this.massiveSignatureHelper != null && this.massiveSignatureHelper.isInitialized()) {
			this.massiveSignatureHelper.setMassiveOperation(this.massiveOperation);
		}
	}

	public void addMassiveHash(String hashData) {
		if(this.hashesToSign == null) {
			this.hashesToSign = new Vector<String>();
		}
		this.hashesToSign.add(hashData);
	}

	/**
	 * Obtiene un flujo de entrada de datos a partir de los datos o el fichero especificado
	 * con {@link #setData(String)} y {@link #setFileuri(String)}.
	 * @return Flujo de entrada de datos
	 * @throws AOException Si ocurren errores al crear el flujo de datos
	 */
	private InputStream getInDataStream() throws AOException {
		InputStream streamToWork = null;

		// Comprobamos si se nos han introducido los datos directamente. Aun en caso de que se nos haya
		// introducido directamente, en caso de que tambien se haya introducido el hash y el modo de firma
		// sea explicito, hacemos una firma del hash.
		if (data == null || (hash != null && SignApplet.this.sigMode.equals(AOConstants.SIGN_MODE_EXPLICIT))) {

			// Si no, comprobamos si se nos ha introducido un hash para firmar 
			if (hash == null) {

				// Si no, comprobamos si se nos ha indicado un fichero de entrada
				if (fileUri == null) {

					// Si no, le pedimos al usuario que seleccione un fichero y lo configuramos
					String fileName = AOUIManager.getLoadFileName(AppletMessages.getString("SignApplet.356"), null, null, this); //$NON-NLS-1$
					if (fileName == null) {
						logger.severe("Se ha cancelado la seleccion del fichero de entrada"); //$NON-NLS-1$
						this.setError(AppletMessages.getString("SignApplet.212")); //$NON-NLS-1$
						throw new AOException(
								"Se ha cancelado la seleccion del fichero de entrada, se cancelara toda la operacion" //$NON-NLS-1$
						);
					}
					try {
						fileUri = AOUtil.createURI(fileName);
					}
					catch(Exception e) {
						logger.severe("Se ha proporcionado un nombre de fichero no valido '" + fileName + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
						this.setError(AppletMessages.getString("SignApplet.214") + fileName); //$NON-NLS-1$ //$NON-NLS-2$
						throw new AOException("Se ha proporcionado un nombre de fichero no valido: "+ fileName, e); //$NON-NLS-1$
					}
					fileBase64 = false;
				}
				// Cargamos los datos de la URI configurada
				streamToWork = this.loadConfiguredFile();
			} // Se nos ha introducido un hash
			else streamToWork = new ByteArrayInputStream(hash);
		} // Se nos ha introducido un dato
		else streamToWork = new ByteArrayInputStream(data);

		// Si la entrada son datos o un fichero lo analizamos
		if(data != null || fileUri != null) {
			byte[] tmpData = null;
			try {
				tmpData = AOUtil.getDataFromInputStream(streamToWork);
			} catch (Exception e) {
				return streamToWork;
			}
			analizeMimeType(tmpData);
			streamToWork = new ByteArrayInputStream(tmpData);
		}

		return streamToWork;
	}

	/**
	 * Devuelve un stream con el contenido del fichero configurado en el cliente teniendo
	 * en cuenta si se especific&oacute; si este estaba configurado en base64 o no. Si
	 * no hay ning&uacute;n fichero configurado devuelve {@code null}.
	 * @return Stream al contenido del fichero.
	 * @throws AOException Cuando ocurre algun error leyendo el fichero configurado.
	 */
	private InputStream loadConfiguredFile() throws AOException {

		InputStream dataStream = null;

		if(this.fileUri == null) {
			return null;
		}

		try {
			dataStream = AOUtil.loadFile(this.fileUri, SignApplet.this, true);
		} catch (FileNotFoundException e) {
			logger.severe("No se encuentra el fichero de datos " + this.fileUri + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
			this.setError(AppletMessages.getString("SignApplet.408")); //$NON-NLS-1$
			throw new AOException("No se encuentra el fichero de datos: " + this.fileUri.toString(), e); //$NON-NLS-1$
		} catch (Throwable e) {
			logger.severe("Error durante la lectura del fichero de datos de entrada: " + e); //$NON-NLS-1$
			this.setError(AppletMessages.getString("SignApplet.407")); //$NON-NLS-1$
			throw new AOException("Error durante la lectura del fichero de datos de entrada: " + this.fileUri.toString(), e); //$NON-NLS-1$
		}

		if(this.fileBase64) {
			try {
				dataStream = new ByteArrayInputStream(new sun.misc.BASE64Decoder().decodeBuffer(dataStream));
			} catch (IOException e) {
				logger.severe("Error en la decodificacion del Base64 del fichero de datos de entrada: " + e); //$NON-NLS-1$
				this.setError(AppletMessages.getString("SignApplet.409")); //$NON-NLS-1$
				throw new AOException("Error en la decodificacion del Base64 del fichero de datos de entrada: " + this.fileUri.toString(), e); //$NON-NLS-1$
			}
		}

		return dataStream;
	}
	
	/**
	 * Analiza datos para establecer su MimeType y la descripci&oacute;n textual de su tipo. 
	 * @param dataContent Contenidos que se desean analizar.
	 */
	private void analizeMimeType(byte[] dataContent) {
		// Intentamos extraer el mimetype y su descripcion
		if(dataContent != null) {
			MimeHelper mtHelper = new MimeHelper(dataContent);
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

				// Si no hay certificado seleccionado mostramos la lista de seleccion
				String aliasToUse = null;
				try {
					if (selectedAlias == null) {
						if (certAlias == null) SignApplet.this.getCertificatesAlias();
						selectedAlias = aliasToUse = SignApplet.this.selectCertificate(true);
					}
					else aliasToUse = selectedAlias;
				} catch(AOCancelledOperationException e) {
					logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					return false;
				} catch (AOException e) {
					logger.severe(e.getMessage()+": "+e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.340")); //$NON-NLS-1$
					return false;
				}

				// Comprobamos que finalmente se haya seleccionado un alias
				if (aliasToUse == null) {
					logger.severe("Error al seleccionar un certificado del repositorio"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.363")); //$NON-NLS-1$
					return false;
				}

				// Llegados a este punto necesitamos la clave y el certificado, que los obtenemos a partir del alias...
				// Hacemos esta comprobacion por si se nos ha indicado externamente el alias del certificado a usar 
				if (aoKsManager == null) {
					logger.severe("Se cancelara la operacion de firma, no se encuentra el almacen con el certicado: " + aliasToUse); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.146") + " " + aliasToUse); //$NON-NLS-1$ //$NON-NLS-2$
					return false;
				}

				PrivateKeyEntry ke;
				try {
					ke = aoKsManager.getKeyEntry(aliasToUse, AOCryptoUtil.getCertificatePC(SignApplet.this.store));
				}
				catch(AOCancelledOperationException e) {
					logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					return false;
				}
				catch (Throwable e) {
					logger.severe("No se ha podido obtener el certicado con el alias '" +aliasToUse + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.94") + aliasToUse); //$NON-NLS-1$
					return false;
				}

				// Certificado con el que vamos a firmar
				certToSign = aoKsManager.getCertificate(ke);

				// Configuramos el entorno
				SignApplet.this.configurePolicy();
				SignApplet.this.configureXMLTransforms();

				// Establecemos la configuracion que se usara para la firma masiva
				MassiveSignConfiguration massiveConfiguration = new MassiveSignConfiguration(ke, certToSign);
				massiveConfiguration.setExtraParams(SignApplet.this.genericConfig);
				massiveConfiguration.setAlgorithm(SignApplet.this.sigAlgo);
				massiveConfiguration.setDefaultFormat(SignApplet.this.sigFormat);
				massiveConfiguration.setMode(SignApplet.this.sigMode);
				massiveConfiguration.setOriginalFormat(SignApplet.this.originalFormat);
				massiveConfiguration.setMassiveOperation(
						SignApplet.this.massiveOperation != null ?
								SignApplet.this.massiveOperation :
									MassiveType.valueOf(AOConstants.DEFAULT_MASSIVE_OPERATION)
				);

				try {
					SignApplet.this.massiveSignatureHelper = new MassiveSignatureHelper(massiveConfiguration);
				} catch (AOException e) {
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
		if(this.massiveSignatureHelper == null) {
			logger.warning("No se ha inicializado la operacion de firma masiva"); //$NON-NLS-1$
			return;
		}
		this.massiveSignatureHelper.release();
	}

	public String massiveSignatureData(final String b64Data) {
		logger.info("Invocando massiveSignatureData"); //$NON-NLS-1$
		this.setError(null);
		if(this.massiveSignatureHelper == null || !this.massiveSignatureHelper.isInitialized()) {
			this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
			return null;
		}

		// Ejecutamos la operacion
		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			public String run() {
				String result = SignApplet.this.massiveSignatureHelper.signData(b64Data);
				if(result == null)
					SignApplet.this.setError(SignApplet.this.massiveSignatureHelper.getCurrentLogEntry());
				return result;		
			}
		});
	}

	public String massiveSignatureHash(final String b64Hash) {
		logger.info("Invocando massiveSignatureHash"); //$NON-NLS-1$
		this.setError(null);
		if(this.massiveSignatureHelper == null || !this.massiveSignatureHelper.isInitialized()) {
			this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
			return null;
		}

		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			public String run() {
				String result = SignApplet.this.massiveSignatureHelper.signHash(b64Hash);
				if(result == null)
					SignApplet.this.setError(SignApplet.this.massiveSignatureHelper.getCurrentLogEntry());

				return result;
			}
		});
	}

	public String massiveSignatureFile(final String fileuri) {
		logger.info("Invocando massiveSignatureFile: "+fileuri); //$NON-NLS-1$
		this.setError(null);
		if(this.massiveSignatureHelper == null || !this.massiveSignatureHelper.isInitialized()) {
			this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
			return null;
		}

		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			public String run() {
				String result = SignApplet.this.massiveSignatureHelper.signFile(fileuri);
				if(result == null)
					SignApplet.this.setError(SignApplet.this.massiveSignatureHelper.getCurrentLogEntry());

				return result;
			}
		});
	}

	public String getMassiveSignatureCurrentLog() {
		logger.info("Invocando getMassiveSignatureCurrentLog"); //$NON-NLS-1$
		this.setError(null);
		if(this.massiveSignatureHelper == null) {
			this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
			return null;
		}
		return this.massiveSignatureHelper.getCurrentLogEntry();
	}

	public String getMassiveSignatureLog() {
		logger.info("Invocando getMassiveSignatureLog"); //$NON-NLS-1$
		this.setError(null);
		if(this.massiveSignatureHelper == null) {
			this.setError(AppletMessages.getString("SignApplet.375")); //$NON-NLS-1$
			return null;
		}
		return this.massiveSignatureHelper.getAllLogEntries();	
	}

	public void saveMassiveSignatureLog() {
		logger.info("Invocando saveMassiveSignatureLog"); //$NON-NLS-1$
		this.setError(null);
		if(this.massiveSignatureHelper == null) {
			this.setError(AppletMessages.getString("SignApplet.381")); //$NON-NLS-1$
			return;
		}

		// Guardamos la firma en fichero
		AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
			public Void run() {
				SignApplet.this.saveFileAsinchronously(
						SignApplet.this.getMassiveSignatureLog().getBytes(),
						SignApplet.this.outputFile,
						new String[]{"txt"},  //$NON-NLS-1$
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
				try
				{
					String signAlgorithm = SignApplet.this.sigAlgo == null ? AOConstants.DEFAULT_SIGN_ALGO : SignApplet.this.sigAlgo;

					// Recuperamos el algoritmo de Hash de la firma
					int p = signAlgorithm.toLowerCase().indexOf("with"); //$NON-NLS-1$
					String hashAlg = p != -1 ? signAlgorithm.substring(0, p) : signAlgorithm;

					// Realizamos la firma Web
					firmaWeb = new Browser().browse(html, hashAlg);
					if(firmaWeb!= null)
					{
						SignApplet.this.setFileuri(firmaWeb.tmpWebDataFile.getAbsolutePath());
						SignApplet.this.sign();
					}
					else
					{
						throw new AOCancelledOperationException("Operacion cancelada por el usuario"); //$NON-NLS-1$
					}
				}
				catch(Exception e)
				{
					SignApplet.this.setError(AppletMessages.getString("SignApplet.389")); //$NON-NLS-1$
					logger.severe("Error durante el proceso de firma Web: " + e); //$NON-NLS-1$
					firmaWeb= null;
				}

				if(firmaWeb!=null) return firmaWeb.tmpWebDataFile.getAbsolutePath();
				return "ERROR"; //$NON-NLS-1$
			}
		});
	}

	public final void setSelectedCertificateAlias(final String cAlias) {
		logger.info("Invocando setSelectedCertificateAlias: " + cAlias); //$NON-NLS-1$

		AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
			public Void run() {
				if (cAlias==null) logger.info(
						"Se ha establecido nulo como alias del certificado activo" //$NON-NLS-1$
				);
				if(SignApplet.this.aoKsManager == null) {
					try {
						SignApplet.this.initKeyStore();
					} 
					catch (final AOException e) {
						logger.severe("Error inicializando el almacen de claves, no se podra acceder a los certificados del sistema: " + e); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.242")); //$NON-NLS-1$
					}
				}
				selectedAlias = cAlias;
				return null;
			}
		});
	}

	@Override
	public void init() {

		logger.info("Cliente @firma V3"); //$NON-NLS-1$
		logger.info("Versi\u00F3n: " + getVersion()); //$NON-NLS-1$

		logger.info("Sistema Operativo: "+getSystemProperty("os.name"));
		logger.info("Version del Sistema Operativo: "+getSystemProperty("os.version"));
		logger.info("Arquitectura del JRE: "+getSystemProperty("sun.arch.data.model"));

		this.setLookAndFeel();

		// Establecemos el directorio de instalacion (si no se especifica se tomara el directorio por defecto)
		AOInstallParameters.setInstallationDirectory(getParameter("installDirectory"));	 //$NON-NLS-1$
		logger.info("Directorio de instalacion: " + AOInstallParameters.getInstallationDirectory()); //$NON-NLS-1$

		// Configuramos el almacen de claves que corresponda
		this.configureDefaultStore(this.getSystemProperty("os.name"), getParameter("userAgent")); //$NON-NLS-1$ //$NON-NLS-2$

		logger.info("Almacen de certificados preestablecido: " + this.store.getDescription());
		
		// Configuramos si se deben mostrar los certificados caducados (por defecto, true)
		String paramValue = getParameter("showExpiratedCertificates"); //$NON-NLS-1$
		showExpiratedCertificates = defaultShowExpiratedCertificates =
			(paramValue == null || !paramValue.trim().equalsIgnoreCase("false")); //$NON-NLS-1$

		// Indicamos que el cliente ya se ha inicializado
		this.initializedApplet = true;

		// Imprimimos por consola el acuerdo de licencia por si el applet se ha cargado sin la
		// ayuda del bootloader, que ya se encarga de mostrarsela al usuario.
		this.printLicence();

		// Configuramos si se debe mostrar en los navegadores Mozilla un dialogo de abvertencia
		// acerca de los token externos. Por defecto, se mostraran.
		paramValue = getParameter("showMozillaSmartCardWarning"); //$NON-NLS-1$
		if ((paramValue == null || !paramValue.trim().equalsIgnoreCase("false")) && //$NON-NLS-1$
				(store == AOConstants.AOKeyStore.MOZ_UNI || store == AOConstants.AOKeyStore.PKCS11)) {
			JOptionPane.showMessageDialog(
					SignApplet.this, 
					AppletMessages.getString("SignApplet.13"), //$NON-NLS-1$
					AppletMessages.getString("SignApplet.658"),  //$NON-NLS-1$
					JOptionPane.WARNING_MESSAGE 
			);
		}
	}

	/**
	 * Imprime por consola el acuerdo de licencia del cliente.
	 */
	private void printLicence() {

		try {
			InputStream is = SignApplet.this.getClass().getResourceAsStream("/resources/licenses.txt");
			System.out.println(new String(AOUtil.getDataFromInputStream(is), "utf-8"));
			try {is.close();} catch (Exception e) {}
		} catch (Throwable e) {
			logger.warning("No se ha podido mostrar el acuerdo de licencia por consola");
		}
	}

	/**
	 * Preconfigura (pero no inicializa) un almac&eacute;n de certificados a partir del sistema
	 * operativo y el navegador en el que se est&eacute; ejecutando el applet.
	 * @param currentOS Sistema operativo indicaso segun System.getProperty("os.name").
	 * @param currentBrowser Navegador indicado por el userAgent del mismo.
	 */
	private void configureDefaultStore(String currentOS, String currentBrowser) {

		logger.info("User Agent: " + currentBrowser); //$NON-NLS-1$

		// Cualquier Linux o Solaris usara Mozilla NSS
		if (currentOS.contains("inux") ||  //$NON-NLS-1$
				currentOS.contains("olaris") ||  //$NON-NLS-1$
				currentOS.contains ("SunOS")) {
			this.store = AOConstants.AOKeyStore.MOZ_UNI; //$NON-NLS-1$

			// Cualquier navegador en MacOSX usara el proveedor de Apple
		} else if (currentOS.startsWith("Mac OS X")) {
			this.store = AOConstants.AOKeyStore.APPLE; //$NON-NLS-1$

		} else {

			if(currentBrowser != null) {
				currentBrowser = currentBrowser.toLowerCase();

				// Para Mozilla en Windows y Mac OS X, multiples acepciones para tolerar errores de nomenclatura
				if(currentBrowser.contains("firefox") || currentBrowser.contains("seamonkey")) //$NON-NLS-1$ //$NON-NLS-2$
					this.store = AOConstants.AOKeyStore.MOZ_UNI;
			}

			// Si no es nada de lo anterior, se quedara el valor que estuviese configurado,
			// por defecto CAPI (Windows / Internet Explorer)
		}
	}

	/**
	 * Recupera una propiedad del sistema. En caso de producirse un error, se devuelve <code>null</code>.
	 * @param property Propiedad del sistema.
	 * @return Valor de la propiedad.
	 */
	private final String getSystemProperty(final String property) {
		try {
			return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
				public String run() {
					return System.getProperty(property);
				}
			});
		} 
		catch (final Throwable e) {
			logger.severe("No se pudo recuperar la propiedad del sistema '" + property + "', se devolvera null"); //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
	}

	private final void setLookAndFeel() {
		String lookandfeel = UIManager.getSystemLookAndFeelClassName(); 
		try {
			UIManager.setLookAndFeel(lookandfeel);
		}
		catch(final Throwable e) {
			logger.warning(
					"No se ha podido establecer el Look&Feel '" + lookandfeel + "', las " + //$NON-NLS-1$ //$NON-NLS-2$
					"ventanas careceran de decoracion: " + e //$NON-NLS-1$
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

	/**
	 * Traduce el nombre identificador de un formato de la version 2.4 del cliente y entre distintas
	 * alternativas soportaas por el Cliente v3 al nombre identificador de la actual versi&oacute;n.
	 * En caso de no encontrar coincidencia con un nombre de formato conocido,
	 * se devuelve el mismo nombre que se le pas&oacute; a la funci&oacute;n. Si el formato introducido
	 * es <code>null</code> se devolver&aacute; <code>null</code> para que sea el m&eacute;todo que
	 * lo invoca quien ser encargue del error.
	 * @param oldName Antiguo nombre de formato.
	 * @return Nuevo nombre que se le ha asignado al formato indicado.
	 */
	private final String translateToNewFormatName(final String oldName) {

		// Dejamos que el objeto que llame al metodo se encargue del error
		if(oldName == null)	return null;

		if(oldName.equalsIgnoreCase("CMS") || oldName.equalsIgnoreCase("CMS-BES") || //$NON-NLS-1$ //$NON-NLS-2$
				oldName.equalsIgnoreCase("PKCS7") || oldName.equalsIgnoreCase("PKCS#7")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_CMS;
		} else if(oldName.equalsIgnoreCase("CADES") || oldName.equalsIgnoreCase("CADES-BES")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_CADES;
		} else if(oldName.equalsIgnoreCase("NONE") || oldName.equalsIgnoreCase("RAW")  || //$NON-NLS-1$ //$NON-NLS-2$
				oldName.equalsIgnoreCase("PKCS1") || oldName.equalsIgnoreCase("PKCS#1")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_PKCS1;
		} else if(oldName.equalsIgnoreCase("XADES") || oldName.equalsIgnoreCase("XADES-BES") || //$NON-NLS-1$ //$NON-NLS-2$
				oldName.equalsIgnoreCase("XAdES Detached") || oldName.equalsIgnoreCase("XADES_DETACHED")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_XADES_DETACHED;
		} else if(oldName.equalsIgnoreCase("XAdES Externally Detached") || oldName.equalsIgnoreCase("XADES_EXTERNALLY_DETACHED")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED;
		} else if(oldName.equalsIgnoreCase("XAdES Enveloped") || oldName.equalsIgnoreCase("XAdES_Enveloped")) {  //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_XADES_ENVELOPED;
		} else if(oldName.equalsIgnoreCase("XAdES Enveloping") || oldName.equalsIgnoreCase("XAdES_Enveloping")) {  //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_XADES_ENVELOPING;
		} else if(oldName.equalsIgnoreCase("XMLDSIGN") || oldName.equalsIgnoreCase("XMLDSIGN-BES") || //$NON-NLS-1$ //$NON-NLS-2$
				oldName.equalsIgnoreCase("XMLDSIG") || oldName.equalsIgnoreCase("XMLDSIG-BES") || //$NON-NLS-1$ //$NON-NLS-2$
				oldName.equalsIgnoreCase("XMLDSign Detached") || oldName.equalsIgnoreCase("XMLDSig Detached") || //$NON-NLS-1$ //$NON-NLS-2$
				oldName.equalsIgnoreCase("XMLDSIGN_DETACHED") || oldName.equalsIgnoreCase("XMLDSIG_DETACHED")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_XMLDSIG_DETACHED;
		} else if(oldName.equalsIgnoreCase("XMLdSig Externally Detached") || oldName.equalsIgnoreCase("XMLDSIG_EXTERNALLY_DETACHED") || //$NON-NLS-1$ //$NON-NLS-2$
				oldName.equalsIgnoreCase("XMLdSign Externally Detached") || oldName.equalsIgnoreCase("XMLDSIGN_EXTERNALLY_DETACHED")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED;
		} else if(oldName.equalsIgnoreCase("XMLDSign Enveloped") || oldName.equalsIgnoreCase("XMLDSig Enveloped") ||  //$NON-NLS-1$ //$NON-NLS-2$
				oldName.equalsIgnoreCase("XMLDSIGN_Enveloped") || oldName.equalsIgnoreCase("XMLDSIG_Enveloped")) { //$NON-NLS-1$ //$NON-NLS-2$ 
			return AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED;
		} else if(oldName.equalsIgnoreCase("XMLDSign Enveloping") || oldName.equalsIgnoreCase("XMLDSig Enveloping") || //$NON-NLS-1$ //$NON-NLS-2$
				oldName.equalsIgnoreCase("XMLDSIGN_Enveloping") || oldName.equalsIgnoreCase("XMLDSIG_Enveloping")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING;
		} else if(oldName.equalsIgnoreCase("PDF") || oldName.equalsIgnoreCase("Adobe PDF")) {  //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_PDF;
		} else if(oldName.equalsIgnoreCase("ODF (Open Document Format)") || oldName.equalsIgnoreCase("ODF") || oldName.equalsIgnoreCase("ODT") || //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				oldName.equalsIgnoreCase("ODS") || oldName.equalsIgnoreCase("ODP")  //$NON-NLS-1$ //$NON-NLS-2$
				|| oldName.equalsIgnoreCase("OpenOffice") || oldName.equalsIgnoreCase("OOo") || oldName.equalsIgnoreCase("OpenOffice.org")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			return AOConstants.SIGN_FORMAT_ODF;
		} else if(oldName.equalsIgnoreCase("OOXML (Office Open XML)") || oldName.equalsIgnoreCase("OOXML") || oldName.equalsIgnoreCase("DOCX") || //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				oldName.equalsIgnoreCase("XSLX") || oldName.equalsIgnoreCase("PPTX") || oldName.equalsIgnoreCase("PPSX")  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				|| oldName.equalsIgnoreCase("Office") || oldName.equalsIgnoreCase("Microsoft Office")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_FORMAT_OOXML;
		}

		// Si no es conocido el formato, dejamos el nombre tal cual
		return oldName;
	}

	/**
	 * Traduce el nombre identificador de un modo de firma de la version 2.4 del cliente al nombre
	 * identificador de la actual versi&oacute;n. En caso de no encontrar coincidencia con un nombre
	 * de modo conocido, se devuelve el mismo nombre que se le pas&oacute; a la funci&oacute;n.
	 * Si el modo introducido es <code>null</code> se devolver&aacute; <code>null</code> para que sea
	 * el m&eacute;todo que lo invoca quien se encargue del error.
	 * @param oldName Antiguo nombre de modo.
	 * @return Nuevo nombre que se le ha asignado al modo indicado.
	 */
	private String translateToNewModeName(String oldName) {

		// Dejamos que el objeto que llame al metodo se encargue del error
		if(oldName == null)	return null;

		String modeLw = oldName.toLowerCase();
		if(modeLw.equals("explicit")) { //$NON-NLS-1$
			return AOConstants.SIGN_MODE_EXPLICIT;
		} else if(modeLw.equals("implicit")) { //$NON-NLS-1$
			return AOConstants.SIGN_MODE_IMPLICIT;
		}
		return oldName;
	}

	/**
	 * Traduce el nombre identificador de un algoritmo de firma de la version 2.4 del cliente al nombre
	 * identificador de la actual versi&oacute;n. En caso de no encontrar coincidencia con un nombre
	 * de algoritmo conocido, se devuelve el mismo nombre que se le pas&oacute; a la funci&oacute;n.
	 * Si el algoritmo introducido es <code>null</code> se devolver&aacute; <code>null</code> para que sea
	 * el m&eacute;todo que lo invoca quien se encargue del error.
	 * @param oldName Antiguo nombre de algoritmo.
	 * @return Nuevo nombre que se le ha asignado al algoritmo indicado.
	 */
	private String translateToNewAlgorithmName(String oldName) {

		// Dejamos que el objeto que llame al metodo se encargue del error
		if(oldName == null) return null;

		if(oldName.equalsIgnoreCase("sha1WithRsaEncryption") || oldName.equalsIgnoreCase("SHA1withRSA") || oldName.equalsIgnoreCase("SHA-1withRSA") || oldName.equalsIgnoreCase("SHA1RSA")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			return AOConstants.SIGN_ALGORITHM_SHA1WITHRSA;
		} else if(oldName.equalsIgnoreCase("md5WithRsaEncryption") || oldName.equalsIgnoreCase("MD5withRSA")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOConstants.SIGN_ALGORITHM_MD5WITHRSA;
		}
		return oldName;
	}

	/**
	 * Transforma un nombre identificativo de un keystore en la descripci&oacute;n de
	 * ese mismo keystore para as&iacute; poder obtenerlo a trav&eacute;s del
	 * {@link AOKeyStoreManager}. Si no se consigue identificar el keystore se devuelve
	 * la misma cadena que se ha indicado.
	 * @param type Tipo de keystore.
	 * @return Descripci&oacute;n del keystore indicado.
	 */
	private String translateToKeyStoreDescription(String type) {

		String typeLw = type.toLowerCase();
		if(typeLw.equals("windows") || typeLw.equals("internet explorer") //$NON-NLS-1$ //$NON-NLS-2$
				|| typeLw.equals("ie") || typeLw.equals("microsoft") //$NON-NLS-1$ //$NON-NLS-2$
				|| typeLw.equals("windows-my") || typeLw.equals("windowsmy")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOKeyStore.WINDOWS.getDescription();
		} else if(typeLw.equals("winaddressbook") || typeLw.equals("addressbook") //$NON-NLS-1$ //$NON-NLS-2$
				|| typeLw.equals("win-others") || typeLw.equals("winothers") //$NON-NLS-1$ //$NON-NLS-2$
				|| typeLw.equals("windows-others") || typeLw.equals("windowsothers")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOKeyStore.WINADDRESSBOOK.getDescription();
		} else if(typeLw.equals("mac os x") || typeLw.equals("macos x") //$NON-NLS-1$ //$NON-NLS-2$
				|| typeLw.equals("macosx") || typeLw.equals("safari") //$NON-NLS-1$ //$NON-NLS-2$
				|| typeLw.equals("apple") || typeLw.equals("apple safari") //$NON-NLS-1$ //$NON-NLS-2$ 
				|| typeLw.equals("keychainstore")) { //$NON-NLS-1$
			return AOKeyStore.APPLE.getDescription();
		} else if(typeLw.equals("mozilla") || typeLw.equals("firefox") || typeLw.equals("ff")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ 
			return AOKeyStore.MOZ_UNI.getDescription();
		} else if(typeLw.equals("pkcs#12") || typeLw.equals("pkcs12") || typeLw.equals("p12") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				|| typeLw.equals("pfx")) { //$NON-NLS-1$
			return AOKeyStore.PKCS12.getDescription();
		} else if(typeLw.equals("pkcs#11") || typeLw.equals("pkcs11") || typeLw.equals("p11")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			return AOKeyStore.PKCS11.getDescription();
		} else if(typeLw.equals("java") || typeLw.equals("jks") || typeLw.equals("java keystore") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				|| typeLw.equals("javakeystore")) { //$NON-NLS-1$
			return AOKeyStore.JAVA.getDescription();
		} else if(typeLw.equals("single") || typeLw.equals("pkcs7") || typeLw.equals("pkcs#7") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				|| typeLw.equals("x509") || typeLw.equals("x.509") || typeLw.equals("cer")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			return AOKeyStore.SINGLE.getDescription();
		} else if(typeLw.equals("jceks") || typeLw.equals("java cryptography extension keystore")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOKeyStore.JCEKS.getDescription();
		} else if(typeLw.equals("javace") || typeLw.equals("caseexactjks") //$NON-NLS-1$ //$NON-NLS-2$
				|| typeLw.equals("java keystore (case exact)") || typeLw.equals("jks (case exact)")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOKeyStore.JAVACE.getDescription();
		} else if(typeLw.equals("win-ca") || typeLw.equals("winca") //$NON-NLS-1$ //$NON-NLS-2$
				|| typeLw.equals("windows-ca") || typeLw.equals("windowsca")) { //$NON-NLS-1$ //$NON-NLS-2$
			return AOKeyStore.WINCA.getDescription();
		} else if(typeLw.equals("windows-root") || typeLw.equals("windowsroot") //$NON-NLS-1$ //$NON-NLS-2$
				|| typeLw.equals("winroot")) { //$NON-NLS-1$
			return AOKeyStore.WINROOT.getDescription();
		}
		return type;
	}

	public final boolean signData(final String b64data) {
		logger.info("Invocando signData"); //$NON-NLS-1$
		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {
				if(b64data == null) {
					logger.severe("No se han introducido los datos que se desean firmar"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.278")); //$NON-NLS-1$
					return false;
				}

				try {
					data = new sun.misc.BASE64Decoder().decodeBuffer(b64data);
				} 
				catch (Throwable e) {
					logger.severe("Error al transformar los datos de base 64 a array de bytes: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.279")); //$NON-NLS-1$
					return false;
				}
				return sign();
			}
		});
	}

	public String getSignCertificateBase64Encoded() {
		logger.info("Invocando getSignCertificateBase64Encoded"); //$NON-NLS-1$
		if(certToSign == null) {
			logger.warning("No se dispone del certificado de firma, se devolvera una cadena vacia"); //$NON-NLS-1$
			return ""; //$NON-NLS-1$
		}

		byte[] certEnconded;
		try {
			certEnconded = certToSign.getEncoded();
		} catch (CertificateEncodingException e) {
			logger.warning("La codificacion del certificado no es valida, se devolvera una cadena vacia: "+e); //$NON-NLS-1$
			return ""; //$NON-NLS-1$
		}

		return new RawBASE64Encoder().encode(certEnconded);
	}

	public String getSignatureBase64Encoded() {
		logger.info("Invocando getSignatureBase64Encoded"); //$NON-NLS-1$
		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			public String run() {

				byte[] sign = null;
				try {
					sign = SignApplet.this.getSelectedSignature(false);
				} catch (Throwable e) {
					logger.severe(
							"No se ha podido recuperar la firma electr&oacute;nica: "+e //$NON-NLS-1$
					);
					SignApplet.this.setError(AppletMessages.getString("SignApplet.64")); //$NON-NLS-1$
				}
				return (sign ==  null) ? null :new RawBASE64Encoder().encode(sign);
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
				} catch (Throwable e) {
					logger.severe(
							"No se ha podido recuperar la firma electr&oacute;nica: "+e //$NON-NLS-1$
					);
					SignApplet.this.setError(AppletMessages.getString("SignApplet.64")); //$NON-NLS-1$
				}
				return (sign == null) ? null : new String(sign);
			}
		});
	}

	public String getFilePath() {
		logger.info("Invocando getFilePath"); //$NON-NLS-1$
		if(this.outputFile == null)	{
			logger.warning("No se dispone de la direccion del fichero de firma, se devolvera una cadena vacia"); //$NON-NLS-1$
			return ""; //$NON-NLS-1$
		}
		return this.outputFile;
	}

	public String getFileUsedPath() {
		logger.info("Invocando getFileUsedPath"); //$NON-NLS-1$
		if(this.fileUri == null) {
			logger.warning("No se dispone de la direccion del fichero de datos de entrada, se devolvera una cadena vacia"); //$NON-NLS-1$
			return ""; //$NON-NLS-1$
		}

		String path = ""; //$NON-NLS-1$
		try {
			path = URLDecoder.decode(this.fileUri.toASCIIString(), "UTF-8"); //$NON-NLS-1$
		} catch (Throwable e) {
			logger.warning("Codificacion de caracteres no valida: "+e); //$NON-NLS-1$
		}

		// Si es un fichero local eliminamos el esquema de la ruta
		if(path.startsWith("file://")) path = path.substring(7); //$NON-NLS-1$

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

	/**
	 * Establece un mensaje de error y almacena que este se ha producido o, en caso de introducirse
	 * <code>null</code> o cadena vac&iacute;a, se indica que no hay error. Tambi&eacute;n muestra
	 * este error por consola siempre y cuando no sea nulo. En caso de estar configurado el cliente
	 * para mostrar los errores, se mostrar&aacute; una ventana modal al usuario con el error establecido.
	 * @param errorMsg Mensaje de error.
	 */
	private void setError(String errorMsg) {
		if(errorMsg == null || errorMsg.length() < 1) {
			this.error = false;
			this.errorMsg = ""; //$NON-NLS-1$
		} else {
			this.error = true;
			this.errorMsg = errorMsg;
		}

		// Mostramos, si procede, el mensaje de error que corresponda
		if(this.showErrors && this.error) {
			JOptionPane.showMessageDialog(this, this.errorMsg, AppletMessages.getString("SignApplet.156"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$
		}
	}

	public String getTextFileContent(final String url) {
		logger.info("Invocando getTextFileContent: " + url); //$NON-NLS-1$

		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			public String run() {
				InputStream is;
				try {
					is = AOUtil.loadFile(AOUtil.createURI(url), SignApplet.this, true);
				} 
				catch (final Throwable e) {
					logger.severe("El fichero indicado no existe o no es posible acceder a el: " + e); //$NON-NLS-1$
					return ""; //$NON-NLS-1$
				}
				String result = null;
				try {
					result = new String(AOUtil.getDataFromInputStream(is));
				} catch (Throwable e) {
					logger.severe("No se pudo leer el contenido del fichero indicado: " + e); //$NON-NLS-1$
					return ""; //$NON-NLS-1$
				}
				try {is.close();} catch (Throwable e) { logger.warning("Error al cerrar un flujo de datos: " + e); } //$NON-NLS-1$

				return result;
			}
		});
	}

	public String getTextFromBase64(final String b64) {
		logger.info("Invocando getTextFromBase64"); //$NON-NLS-1$
		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			public String run() {
				try {
					return new String((new sun.misc.BASE64Decoder()).decodeBuffer(b64));
				} catch (Throwable e) {
					logger.severe("Error al decodificar el texto en Base 64, se devolver&aacute; null"); //$NON-NLS-1$
				}
				return null;
			}
		});
	}

	public String getBase64FromText(final String plainText) {
		logger.info("Invocando getBase64FromText"); //$NON-NLS-1$
		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			public String run() {
				try {
					return new String(new RawBASE64Encoder().encode(plainText.getBytes()));
				} catch (Throwable e) {
					logger.severe("Error al codificar el texto plano en base 64, se devolver&aacute; null"); //$NON-NLS-1$
				}
				return null;
			}
		});
	}

	public String getFileBase64Encoded(final String strUri, final boolean showProgress) {
		logger.info("Invocando getFileBase64Encoded: " + strUri); //$NON-NLS-1$
		try {
			return getFileBase64Encoded(AOUtil.createURI(strUri), showProgress);
		} catch (Throwable e) {
			logger.severe("Error al leer el fichero '" + strUri + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			setError("Error al leer el fichero " + strUri);
			return null;
		}
	}

	public String getFileBase64Encoded(final boolean showProgress) {
		logger.info("Invocando getFileBase64Encoded"); //$NON-NLS-1$
		return getFileBase64Encoded(this.fileUri, showProgress);
	}
	
	/**
	 * Recupera el contenido de un fichero codificado en base 64.
	 * @param uri Ruta del fichero.
	 * @param showProgress Si desea mostrarse una barra de progreso para la carga.
	 * @return Contentido en base 64.
	 */
	private String getFileBase64Encoded(final URI uri, final boolean showProgress) {
		if(uri == null) {
			logger.severe("No se ha establecido un fichero que desea obtener en base 64"); //$NON-NLS-1$
			return null; //$NON-NLS-1$
		}
		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			public String run() {
				InputStream is = null;
				try {
					is = AOUtil.loadFile(uri, SignApplet.this, showProgress);
					
					// Si el contenido del fichero estaba en base 64, no es necesario codificarlo
					if(fileBase64) {
						return new String(AOUtil.getDataFromInputStream(is));
					} else {
						return AOCryptoUtil.getBase64Encoded(is);
					}
				} catch (FileNotFoundException e) {
					logger.severe("No se encuentra el fichero '"+uri+"': "+e); //$NON-NLS-1$ //$NON-NLS-2$
					setError("No se encuentra el fichero " + uri);
					return null;
				} catch (Throwable e) {
					logger.severe("Error al leer el fichero '"+uri+"': "+e); //$NON-NLS-1$ //$NON-NLS-2$
					setError("Error al leer el fichero " + uri);
					return null;
				} finally {
					if(is != null) {
						try {
							is.close();
						} catch (Exception e) {}
					}
				}
			}
		});
	}
	
	public String getFileHashBase64Encoded(final boolean showProgress) {
		logger.info("Invocando getFileHashBase64Encoded"); //$NON-NLS-1$

		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			public String run() {
				if(SignApplet.this.fileUri == null) {
					logger.severe("No se ha establecido el fichero del que calcular el Hash"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.348")); //$NON-NLS-1$
					return null;
				}
				
				InputStream is;
				try {
					is = loadConfiguredFile();
				} catch (Throwable e) {
					return null;		// Ya se establece el error en loadConfigureFile
				}

				byte[] binaryData;
				try {
					binaryData = AOUtil.getDataFromInputStream(is);
				} catch (Throwable e) {
					logger.severe("Error durante la lectura del fichero " + SignApplet.this.fileUri + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.407") + SignApplet.this.fileUri); //$NON-NLS-1$
					return null;
				}

				try { is.close(); } catch (IOException e) {
					logger.warning("Error al cerrar el stream de entrada de datos: " + e); //$NON-NLS-1$
				}

				String digestAlg = AOUtil.getDigestAlgorithm(SignApplet.this.sigAlgo);
				try {
					return new RawBASE64Encoder().encode(AOCryptoUtil.getMessageDigest(binaryData, digestAlg));
				} catch (NoSuchAlgorithmException e) {
					logger.severe("El algoritmo de hash '"+digestAlg+"' no esta soportado: "+e); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.464") + digestAlg); //$NON-NLS-1$
					return null;
				}
			}
		});
	}

	public void setCipherData(final String data) {
		logger.info("Invocando setCipherData"); //$NON-NLS-1$
		AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
			public Void run() {
				if(data == null) {
					logger.severe("Se ha intentad establecer unos datos cifrados nulos"); //$NON-NLS-1$
					SignApplet.this.cipheredData = null;
				}
				else {
					try {
						SignApplet.this.cipheredData = new sun.misc.BASE64Decoder().decodeBuffer(data);
					} catch (Throwable e) {
						logger.severe("Los datos introducidos no estan en base 64: "+e); //$NON-NLS-1$
						SignApplet.this.cipheredData = null;
					}
				}
				return null;
			}
		});
	}

	public void setPlainData(String data) {
		logger.info("Invocando setPlainData"); //$NON-NLS-1$
		if(data == null) {
			logger.severe("Se ha intentado establecer unos datos planos nulos"); //$NON-NLS-1$
			this.plainData = null;
		}
		else this.plainData = data.getBytes();
	}

	public String getCipherData() {
		logger.info("Invocando getCipherData"); //$NON-NLS-1$
		if(this.cipheredData == null) {
			return null;
		}
		return new RawBASE64Encoder().encode(this.cipheredData);
	}

	public String getPlainData() {
		logger.info("Invocando getPlainData"); //$NON-NLS-1$
		return this.plainData == null ? null : new String(this.plainData);
	}

	public String getKey() {
		logger.info("Invocando getKey"); //$NON-NLS-1$
		return this.cipherB64Key;
	}

	public void setKey(String newKey) {
		logger.info("Invocando setKey: " + newKey); //$NON-NLS-1$
		this.cipherB64Key = newKey;
	}

	public String getPassword() {
		logger.info("Invocando getPassword"); //$NON-NLS-1$
		return this.cipherPassword == null ? null : String.valueOf(this.cipherPassword);
	}

	public boolean setPassword(String password) {
		logger.info("Invocando setPassword"); //$NON-NLS-1$
		for (char c : password.toCharArray())
			if (c < 32 || c > 126) {
				logger.warning("La contrasena introducida no es una cadena ASCII"); //$NON-NLS-1$
				return false;
			}
		this.cipherPassword = password.toCharArray();
		return true;
	}

	public void setCipherAlgorithm(String algorithm) {
		logger.info("Invocando setCipherAlgorithm: " + algorithm); //$NON-NLS-1$

		// Si se introduce null o cadena vacia, eliminamos la configuracion actual
		if(algorithm == null || algorithm.length() == 0) {
			this.cipAlgo = null;
			this.cipBlockMode = null;
			this.cipPadding = null;
			return;
		}

		// Desmenbramos el algoritmo por si se ha indicado el modo de bloque y el padding
		String[] algoConfig = algorithm.split("/"); //$NON-NLS-1$
		this.cipAlgo = AOCipherAlgorithm.getValueOf(algoConfig[0]);
		if(cipAlgo == null) {
			logger.warning(
					"Algoritmo de cifrado no reconocido, se eliminara el algoritmo de cifrado establecido" //$NON-NLS-1$
			);
			return;
		}

		// Establecemos el resto de la configuracion 
		if(algoConfig.length == 3) {
			this.cipBlockMode = AOCipherBlockMode.getValueOf(algoConfig[1]);
			this.cipPadding = AOCipherPadding.getValueOf(algoConfig[2]);
		} else {
			this.cipBlockMode = null;
			this.cipPadding = null;
		}
	}

	public String getCipherAlgorithm() {
		logger.info("Invocando getCipherAlgorithm"); //$NON-NLS-1$

		String cipherAlgorithm = (this.cipAlgo == null ? AOConstants.AOCipherAlgorithm.DEFAULT_CIPHER_ALGO : this.cipAlgo.getName());
		if(this.cipBlockMode != null && this.cipPadding != null) {
			cipherAlgorithm += "/" + cipBlockMode.getName() + "/" + cipPadding.getName();  //$NON-NLS-1$ //$NON-NLS-2$
		}
		return cipherAlgorithm;
	}

	public void setKeyMode(String keyMode) {
		logger.info("Invocando setKeyMode: " + keyMode); //$NON-NLS-1$
		this.keyMode = (keyMode == null ? AOConstants.DEFAULT_KEY_MODE : keyMode);
	}

	public String getKeyMode() {
		logger.info("Invocando getKeyMode"); //$NON-NLS-1$
		return this.keyMode;
	}

	public boolean savePlainDataToFile(final String strUri) {
		logger.info("Invocando savePlainDataToFile: " + strUri); //$NON-NLS-1$

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {
				try {
					SignApplet.this.saveDataToStorage(SignApplet.this.plainData, strUri);
				} catch (Throwable e) {
					logger.severe("No se pudo almacenar el texto plano (establecido o cifrado) en " + strUri + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.392") + strUri); //$NON-NLS-1$
					return false;
				}
				return true;
			}
		});
	}

	public void setUseCipherKeyStore(boolean useKeyStore) {
		logger.info("Invocando setUseCipherKeyStore con el valor: " + useKeyStore); //$NON-NLS-1$
		this.useCipherKeyStore = useKeyStore;
	}

	public boolean saveCipherDataToFile(final String strUri) {
		logger.info("Invocando saveCipherDataToFile: " + strUri); //$NON-NLS-1$

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {
				if(SignApplet.this.cipheredData == null) {
					logger.severe("No hay datos cifrados que guardar"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.395")); //$NON-NLS-1$
					return false;
				}
				if(strUri == null) {
					logger.severe("El fichero de salida para los datos no puede ser nulo"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.396")); //$NON-NLS-1$
					return false;
				}
				try {
					SignApplet.this.saveDataToStorage(SignApplet.this.cipheredData, strUri);
				} catch (Throwable e) {
					logger.severe("No se pudo almacenar el texto cifrado en" + strUri + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.397") + strUri); //$NON-NLS-1$
					return false;
				}
				return true;
			}
		});
	}

	public boolean cipherFile(final String strUri) {
		logger.info("Invocando cipherFile: " + strUri); //$NON-NLS-1$
		
		this.plainData = null;

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {
				InputStream is = null;
				
				try {
					is = AOUtil.loadFile(AOUtil.createURI(strUri), SignApplet.this, true);
					return cipherData(AOUtil.getDataFromInputStream(is));
				} catch (Throwable e) {
					logger.severe("No se pudo acceder al fichero '" + strUri + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
					setError("No se pudo acceder al fichero que se desea encriptar");
					return false;
				} finally {
					if(is != null) {
						try {
							is.close();
						} catch (Exception e) {}
					}
				}
			}
		});
	}

	public boolean decipherFile(final String strUri) {
		logger.info("Invocando decipherFile: " + strUri); //$NON-NLS-1$

		this.cipheredData = null;

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {
				InputStream is = null;
				
				try {
					is = AOUtil.loadFile(AOUtil.createURI(strUri), SignApplet.this, true);
					return decipherData(AOUtil.getDataFromInputStream(is));
				} catch (Throwable e) {
					logger.severe("No se pudo acceder al fichero '" + strUri + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
					setError(AppletMessages.getString("SignApplet.402")); //$NON-NLS-1$
					return false;
				} finally {
					if(is != null) {
						try {
							is.close();
						} catch (Exception e) {}
					}
				}
			}
		});
	}

	public boolean cipherData() {
		logger.info("Invocando cipherData"); //$NON-NLS-1$

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {
				byte[] dataToCipher = null;
				if (plainData == null) {

					// Fichero de entrada
					if (fileUri == null) {
						String fileName = AOUIManager.getLoadFileName(null, null, SignApplet.this);
						if (fileName == null) {
							logger.severe("Se ha cancelado la seleccion del fichero de entrada, se cancelara toda la operacion de cifrado"); //$NON-NLS-1$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.405")); //$NON-NLS-1$
							return false;
						}						

						try {
							fileUri = AOUtil.createURI(fileName);
						}
						catch(Exception e) {
							logger.severe("Se ha proporcionado un nombre de fichero no valido: " + e); //$NON-NLS-1$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.214") + fileName); //$NON-NLS-1$
							return false;
						}
					}

					// En este punto, tenemos la URI de los datos de entrada
					InputStream is = null;
					try {
						is = loadConfiguredFile();
						dataToCipher = AOUtil.getDataFromInputStream(is);
					} catch (Throwable e) {
						logger.severe("Error al leer el fichero de datos '" + fileUri.toASCIIString() + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.407")); //$NON-NLS-1$
						return false;
					}
					try {
						is.close();
					} catch (Throwable e) { }
				}
				else {
					dataToCipher = plainData;
				}
				
				return cipherData(dataToCipher);
			}
		});
	}

	/**
	 * Cifra los datos indicados aplicando la configuraci&oacute;n de cifrado actual y
	 * establece el resultado en la configuraci&oacute;n del cliente.
	 * @param dataToCipher Datos que se desean cifrar.
	 * @return Devuelve {@code true} si los datos se cifraron correctamente, {@code false}
	 * en caso contrario.
	 */
	private boolean cipherData(final byte[] dataToCipher) {
	// Si no esta establecido el algoritmo de cifrado usamos el por
		// defecto, pero solo para esta ocasion
		AOCipherAlgorithm algorithm = (cipAlgo == null ? AOCipherAlgorithm.getDefault() : cipAlgo);
		AOAlgorithmConfig config = new AOAlgorithmConfig(algorithm, SignApplet.this.cipBlockMode, SignApplet.this.cipPadding);

		// Ya tenemos el stream con los datos, vamos a ver que Cipher uso
		AOCipher cipher = new AOSunJCECipher();
		Key cipherKey = null;

		// Tomamos o generamos la clave, segun nos indique el modo de clave.
		String keyModeTemp = SignApplet.this.keyMode;
		if(keyModeTemp == null) {
			keyModeTemp = AOConstants.DEFAULT_KEY_MODE;
		}

		if(keyModeTemp.equals(AOConstants.KEY_MODE_GENERATEKEY)) {
			try {
				cipherKey = cipher.generateKey(config);
			} catch (Throwable e) {
				logger.severe("Error al generar una clave para el algoritmo" + algorithm + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
				SignApplet.this.setError(AppletMessages.getString("SignApplet.411") + algorithm); //$NON-NLS-1$
				return false;
			}
			SignApplet.this.cipherB64Key = new RawBASE64Encoder().encode(cipherKey.getEncoded());

			// Si se permite el almacenamiento de las claves, le damos la posibilidad al usuario
			// Si se selecciona "Si" se almacenara la clave, si se selecciona "No" no se almacenara
			// y si se selecciona "Cancelar" o se cierra el dialogo, se cancelara toda la operacion
			// de cifrado.
			if(SignApplet.this.useCipherKeyStore) {
				try {
					SignApplet.this.saveCipherKey(config, cipherKey);
				} catch (AOCancelledOperationException e) {
					logger.info("El usuario cancelo la operacion de cifrado desde el dialogo de almacenamiento de clave: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					return false;
				}
			}
		}
		else if(keyModeTemp.equals(AOConstants.KEY_MODE_USERINPUT)) {

			// Cuando se selecciono introducir una clave:
			// 	- Si el usuario la ha introducido:
			// 		- Se usa la clave configurada.
			// 	- Si no la ha introducido:
			// 		- Se comprueba si se permite el uso del almacen de claves de cifrado
			//			- Si se permite
			//				- Se comprueba que existe el almacen
			//					- Si existe
			//						- Se muestra el dialogo para acceder a el
			//					- Si no existe
			//						- Se indica que no se ha introducido ninguna clave para el cifrado
			//			- Si no se permite
			//				- Se indica que no se ha introducido ninguna clave para el cifrado
			if(SignApplet.this.cipherB64Key != null) {
				try {
					cipherKey = cipher.decodeKey(SignApplet.this.cipherB64Key, config, null);
				} catch (Throwable e) {
					logger.severe(AppletMessages.getString("SignApplet.413")+algorithm+": " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.413") + algorithm); //$NON-NLS-1$
					return false;
				}
			}
			else if(SignApplet.this.useCipherKeyStore && AOCipherKeyStoreHelper.storeExists()) {
				try {
					cipherKey = SignApplet.this.getKeyFromCipherKeyStore();
				} catch (AOCancelledOperationException e) {
					logger.info("Operacion cancelada por el usuario");
					setError(AppletMessages.getString("SignApplet.415"));
					return false;
				} catch (Throwable e) {
					logger.severe("Ocurrio un error recuperando la clave de cifrado del almacen de claves del usuario"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.592")); //$NON-NLS-1$
					return false;
				}
			} else {
				logger.severe(AppletMessages.getString("SignApplet.412")+" "+AOConstants.KEY_MODE_USERINPUT); //$NON-NLS-1$ //$NON-NLS-2$
				SignApplet.this.setError(AppletMessages.getString("SignApplet.412")+" "+AOConstants.KEY_MODE_USERINPUT); //$NON-NLS-1$
				return false;
			}
		}
		else if(keyModeTemp.equals(AOConstants.KEY_MODE_PASSWORD)) {
			if(SignApplet.this.cipherPassword == null || SignApplet.this.cipherPassword.length == 0) {
				SignApplet.this.cipherPassword = AOUIManager.getPassword(AppletMessages.getString("SignApplet.414"), AOConstants.ACCEPTED_CHARS, true, getParentFrame(SignApplet.this));  //$NON-NLS-1$
			}
			try {
				cipherKey = cipher.decodePassphrase(
						new String(SignApplet.this.cipherPassword),
						config,
						null
				);
			} catch (final AOCancelledOperationException e) {
				logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
				SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
				return false;
			} catch (final Throwable e) {
				logger.severe("No se pudo obtener una clave valida a partir de la contrasena introducida: " + e); //$NON-NLS-1$
				SignApplet.this.setError(AppletMessages.getString("SignApplet.416")); //$NON-NLS-1$
				return false;
			}
		}
		else {
			logger.severe(AppletMessages.getString("SignApplet.417")); //$NON-NLS-1$ //$NON-NLS-2$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.417")); //$NON-NLS-1$
			return false;
		}

		// realizamos la operacion de cifrado
		try {
			SignApplet.this.cipheredData = cipher.cipher(
					dataToCipher,
					config,
					cipherKey
			);
		}
		catch(AOInvalidKeyException e) {
			logger.severe("La clave o contrasena introducida no es valida: " + e); //$NON-NLS-1$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.550")); //$NON-NLS-1$
			SignApplet.this.plainData = null;
			return false;
		}
		catch(Exception e) {
			logger.severe("Error durante el proceso de cifrado: " + e); //$NON-NLS-1$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.418")); //$NON-NLS-1$
			SignApplet.this.cipheredData = null;
			return false;
		}

		// Ahora vamos a guardar el resultado en el fichero de salida
		if (SignApplet.this.cipheredData == null || SignApplet.this.cipheredData.length < 1) {
			// No vaya a ser que saliese un resultado vacio...
			logger.severe("El proceso de cifrado no genero ningun resultado"); //$NON-NLS-1$ //$NON-NLS-2$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.419")); //$NON-NLS-1$
			SignApplet.this.cipheredData = null;
			return false;
		}

		SignApplet.this.setError(null);
		return true;
	}
	
	public boolean decipherData() {
		logger.info("Invocando decipherData"); //$NON-NLS-1$

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {
				byte[] dataToDecipher = null;
				if (cipheredData == null) {

					// Si no hay una informacion cofrada establecida, la tratamos de leer desde fichero
					if (fileUri == null) {
						String fileName = AOUIManager.getLoadFileName(null, null, SignApplet.this);
						if (fileName == null) {
							logger.severe("Se ha cancelado la seleccion del fichero de entrada, se cancelara toda la operacion de desencriptado"); //$NON-NLS-1$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.422")); //$NON-NLS-1$
							return false;
						}
						try {
							fileUri = AOUtil.createURI(fileName);
						}
						catch(Exception e) {
							logger.severe("Se ha proporcionado un nombre de fichero no valido (" + fileName + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.214") + fileName); //$NON-NLS-1$
							return false;
						}
					}

					// En este punto, tenemos la URI de los datos de entrada
					InputStream is = null;
					try {
						is = loadConfiguredFile();
						dataToDecipher = AOUtil.getDataFromInputStream(is);
					} catch (Throwable e) {
						logger.severe("Error al leer el fichero de datos '" + fileUri.toASCIIString() + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.407")); //$NON-NLS-1$
						return false;
					} finally {
						try {
							is.close();
						} catch (Throwable e) { } // Aunque no se pueda cerrar el fichero, no hacemos nada
					}
				}
				else {
					dataToDecipher = cipheredData;
				}
				
				return decipherData(dataToDecipher);
			}
		});
	}

	/**
	 * Descifra los datos indicados aplicando la configuraci&oacute;n de cifrado actual y
	 * establece el resultado en la configuraci&oacute;n del cliente.
	 * @param dataToDecipher Datos que se desean descifrar.
	 * @return Devuelve {@code true} si los datos se descifraron correctamente, {@code false}
	 * en caso contrario.
	 */
	private boolean decipherData(final byte[] dataToDecipher) {

		// Si no esta establecido el algoritmo de cifrado usamos el por
		// defecto, pero solo para esta ocasion
		AOCipherAlgorithm algorithm = (cipAlgo == null ? AOCipherAlgorithm.getDefault() : cipAlgo);
		AOAlgorithmConfig config = new AOAlgorithmConfig(algorithm, SignApplet.this.cipBlockMode, SignApplet.this.cipPadding);
		AOCipher decipher = new AOSunJCECipher();
		Key decipherKey = null;

		// Si el modo de clave es por password, generamos la clave a partir de el.
		// En caso contrario se supone que el usuario ha establecido la clave de
		// desencriptado.
		if(SignApplet.this.keyMode.equals(AOConstants.KEY_MODE_PASSWORD)) {
			try {
				decipherKey = decipher.decodePassphrase(
						new String(SignApplet.this.cipherPassword != null && SignApplet.this.cipherPassword.length > 0 ?
								SignApplet.this.cipherPassword :
									AOUIManager.getPassword(AppletMessages.getString("SignApplet.414"), getParentFrame(SignApplet.this)) //$NON-NLS-1$
						),
						config,
						null
				);
			} catch (AOCancelledOperationException e) {
				logger.severe("Operacion cancelada por el usuario"); //$NON-NLS-1$
				SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
				return false;
			} catch (Throwable e) {
				logger.severe("No se pudo obtener una clave valida a partir de la contrasena introducida: " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				SignApplet.this.setError(AppletMessages.getString("SignApplet.416")); //$NON-NLS-1$
				return false;
			}
		}
		else {
			// Para el caso de trabajar con claves, si no se indico cual debe usarse,
			// se ofrecera al usuario la posibilidad de tomar una del almacen de claves
			// de cifrado. Si no existe el almacen, se le indica que es obligatorio
			// introducir la clave.
			if(SignApplet.this.cipherB64Key == null) {
				if(AOCipherKeyStoreHelper.storeExists()) {
					try {
						decipherKey = SignApplet.this.getKeyFromCipherKeyStore();
					} catch (AOCancelledOperationException e) {
						SignApplet.this.setError(e.getMessage());
						return false;
					} catch (AOException e) {
						SignApplet.this.setError(e.getMessage());
						return false;
					} catch (Throwable e) {
						logger.severe("Ocurrio un error al configurar la clave de cifrado/descifrado desde el repositorio de claves: "+e); //$NON-NLS-1$
						SignApplet.this.setError("Ocurri\u00F3 un error al configurar la clave de cifrado/descifrado desde el repositorio de claves");
						return false;
					}
				} else {
					logger.severe("No se ha indicado la clave para el descifrado de datos y no hay almacen de claves");  //$NON-NLS-1$
					SignApplet.this.setError("No se ha indicado la clave para el descifrado de datos");
					return false;
				}
			} else {
				try {
					decipherKey = decipher.decodeKey(
							SignApplet.this.cipherB64Key,
							config,
							null
					);
				} catch (Throwable e) {
					logger.severe("No se pudo obtener una clave valida a partir de la codificacion de clave en base 64 introducida: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.432")); //$NON-NLS-1$
					return false;
				}
			}
		}

		// Realizamos la operacion de desencriptado
		try {
			SignApplet.this.plainData = decipher.decipher(
					dataToDecipher,
					config,
					decipherKey
			);
		}
		catch(AOInvalidKeyException e) {
			logger.severe("La clave o contrasena introducida no es valida: " + e); //$NON-NLS-1$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.550")); //$NON-NLS-1$
			SignApplet.this.plainData = null;
			return false;
		}
		catch(AOException e) {
			logger.severe(e.getMessage()+": "+e); //$NON-NLS-1$
			SignApplet.this.setError(e.getMessage());
			SignApplet.this.plainData = null;
			return false;
		}
		catch(Exception e) {
			logger.severe("Error durante el proceso de desencriptado: " + e); //$NON-NLS-1$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.433")); //$NON-NLS-1$
			SignApplet.this.plainData = null;
			return false;
		}


		if (SignApplet.this.plainData == null || SignApplet.this.plainData.length < 1) {
			logger.severe("El proceso de desencriptado no genero ningun resultado"); //$NON-NLS-1$
			SignApplet.this.setError(AppletMessages.getString("SignApplet.434")); //$NON-NLS-1$
			SignApplet.this.plainData = null;
			return false;
		}

		SignApplet.this.setError(null);

		return true;
	}
	
	/**
	 * Guarda una clave de cifrado en el repositorio de claves del usuario.
	 * @param config Configuraci&oacute;n de la clave que se desea almacenar.
	 * @param cipherKey Clave de cifrado que se desea almacenar.
	 * @return Devuelve <code>true</code> si se almacena la clave de cifrado,
	 * <code>false</code>
	 */
	private boolean saveCipherKey(AOAlgorithmConfig config, Key cipherKey) {
		// Preguntamos si se desea almacenar en el almacen de claves de cifrado y
		// si se acepta y no existe este almacen, lo creamos
		int selectedOption = JOptionPane.showConfirmDialog(
				SignApplet.this, 
				AppletMessages.getString("SignApplet.40"), //$NON-NLS-1$
				AppletMessages.getString("SignApplet.41"), //$NON-NLS-1$
				JOptionPane.YES_NO_CANCEL_OPTION);

		// Si se pulsa Cancelar o se cierra el dialogo, se cancela toda la operacion de cifrado
		if(selectedOption == JOptionPane.CANCEL_OPTION || selectedOption == JOptionPane.CLOSED_OPTION) {
			throw new AOCancelledOperationException("Se ha cancelado la operacion de cifrado"); //$NON-NLS-1$
		} else if (selectedOption == JOptionPane.YES_OPTION) {

			// Controlamos un maximo de 3 intentos para abrir el almacen cuando no se
			// establecio la contrasena
			AOCipherKeyStoreHelper cKs = null;
			int numTries = 0;
			do {
				numTries++;
				try {
					cKs = new AOCipherKeyStoreHelper(
							SignApplet.this.cipherKeystorePass != null ?
									SignApplet.this.cipherKeystorePass :
										new UIPasswordCallback("Inserte la contrase\u00F1a del almac\u00E9n de claves de usuario", SignApplet.this).getPassword());
				} catch (AOCancelledOperationException e) {
					return false;
				} catch (AOException e) {
					logger.warning("No se pudo abrir el repositorio de claves de cifrado: " + e); //$NON-NLS-1$
					if(SignApplet.this.cipherKeystorePass != null) {
						logger.warning("Se cancelara el almacenamiento de la clave"); //$NON-NLS-1$
					}
				} catch (Throwable e) {
					logger.severe("Error grave al abrir el repositorio de claves de cifrado: " + e); //$NON-NLS-1$
					return false;
				}
			} while (SignApplet.this.cipherKeystorePass == null && cKs == null && numTries < 3); 

			// Si se agotaron los intentos sin exito se aborta la operacion 
			if(SignApplet.this.cipherKeystorePass == null && cKs == null) {
				JOptionPane.showMessageDialog(
						SignApplet.this,
						AppletMessages.getString("SignApplet.43"), //$NON-NLS-1$
						AppletMessages.getString("SignApplet.156"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE);
				return false;
			}

			try {
				cKs.storeKey(SignApplet.this.cipherKeyAlias != null ?
						SignApplet.this.cipherKeyAlias :
							JOptionPane.showInputDialog(
									SignApplet.this,
									AppletMessages.getString("SignApplet.46"), //$NON-NLS-1$
									AppletMessages.getString("SignApplet.47"), //$NON-NLS-1$
									JOptionPane.QUESTION_MESSAGE)
									+ " (" + config.getAlgorithm().getName() + "/" //$NON-NLS-1$ //$NON-NLS-2$
									+ config.getBlockMode().getName() + "/" //$NON-NLS-1$
									+ config.getPadding().getName() + ")", //$NON-NLS-1$
									cipherKey);
			} catch (AOCancelledOperationException e) {
				return false;
			} catch (Throwable e) {
				logger.severe("Error al almacenar la clave de cifrado, la clave quedara sin almacenar"); //$NON-NLS-1$
				return false;
			}
		}
		return true;
	}
	
	/**
	 * Obtiene una clave de cifrado a partir de una clave del almac&eacute;n del usuario.
	 * Si no se ha establecido la contrase&ntilde;a del almac&eacute;n y/o no se ha seleccionado
	 * un alias, se solicita al usuario mediante una ventana modal.
	 * @return Clave de cifrado/descifrado.
	 * @throws AOException Ocurri&oacute; un error durate el proceso de configuraci&oacute;n. 
	 */
	private Key getKeyFromCipherKeyStore() throws AOException {
		// Si el almacen no existe devolvemos un error
		if(!AOCipherKeyStoreHelper.storeExists()) {
			logger.severe("No existe un almacen de claves de cifrado asociado al usuario"); //$NON-NLS-1$
			throw new AOException(AppletMessages.getString("SignApplet.51")); //$NON-NLS-1$
		}
		// Abrimos el Almacen de claves de cifrado preguntandole al usuario la clave si no
		// la indico
		AOCipherKeyStoreHelper cKs = null;
		try {
			cKs = new AOCipherKeyStoreHelper(
					SignApplet.this.cipherKeystorePass != null ?
							SignApplet.this.cipherKeystorePass :
								AOUIManager.getPassword(AppletMessages.getString("SignApplet.52"), SignApplet.this) //$NON-NLS-1$
			);
		} catch (AOCancelledOperationException e) {
			throw e;
		} catch (Throwable e) {
			logger.severe("Error al abrir el repositorio de claves del usuario: " + e); //$NON-NLS-1$
			throw new AOException("Error al abrir el repositorio de claves del usuario", e); //$NON-NLS-1$
		}

		// Si no se establecio el alias de la clave de cifrado, se la pedimos al usuario
		String alias = null;
		if(SignApplet.this.cipherKeyAlias == null) {
			try {
				alias = AOUIManager.showCertSelectionDialog(
						cKs.getAliases(),
						null,
						null, 
						SignApplet.this, 
						true, 
						true, 
						SignApplet.this.showExpiratedCertificates
				);
			} 
			catch (final AOCancelledOperationException e) {
				throw e;
			} catch (Throwable e) {
				logger.severe("Error seleccionar la clave de cifrado: " + e); //$NON-NLS-1$
				throw new AOException("Error seleccionar la clave de cifrado", e); //$NON-NLS-1$
			}
		} else {
			alias = SignApplet.this.cipherKeyAlias;
		}

		return cKs.getKey(alias);
	}

	/**
	 * Recupera el PasswordCallback que t&iacute;picamente se requiere para el acceso a un
	 * almac&eacute;n de claves.  
	 * @param kStore Almac&eacuten de claves
	 */
	private PasswordCallback getPreferredPCB(AOConstants.AOKeyStore kStore) {

		if(kStore == null)
			throw new NullPointerException("No se ha indicado el KeyStore del que desea " + //$NON-NLS-1$
			"obtener le PasswordCallBack"); //$NON-NLS-1$

		PasswordCallback pssCallback;
		if(kStore == AOConstants.AOKeyStore.WINDOWS || kStore == AOConstants.AOKeyStore.WINROOT
				|| kStore == AOConstants.AOKeyStore.PKCS11)
			pssCallback = new NullPasswordCallback();
		else {
			pssCallback = new UIPasswordCallback(
					AppletMessages.getString("SignApplet.439")+" "+kStore.getDescription(), this); //$NON-NLS-1$ //$NON-NLS-2$   
		}
		return pssCallback;
	}

	/**
	 * Recupera los alias de los certificados del KeyStore establecido que coincidan con alguno de
	 * los alias del array introducido y cumplan la condicion del filtro existente. Si no hay un filtro
	 * establecido se devolver&aacute;n todos los alias que pertenezcan a un certificado del KeyStore.
	 * Si no hay establecido un KeyStore se devolvera un array vac&iacute;o.
	 * @param alias Alias de los certificados a comprobar.
	 * @return Alias de los certificados que pasan el filtro.
	 * @see #setCertFilter(String) Establece el filtro de certificados.
	 */
	private String[] filtCert(String[] aliasList) {

		if(aliasList == null || aliasList.length == 0 || this.aoKsManager == null) {
			logger.info("Aun no se ha establecido el almacen de claves, se devolvera una lista de alias vacia"); //$NON-NLS-1$
			return new String[0];
		}

		// Si no hay establecido un filtro, todos los alias son validos
		if(this.certFilter == null) return aliasList;

		Hashtable<X509Certificate, String> currentCerts = new Hashtable<X509Certificate, String>();
		Certificate tmpCert;
		for (String alias : aliasList) {
			try {
				tmpCert = this.aoKsManager.getCertificate(alias);
				if (tmpCert != null && tmpCert instanceof X509Certificate) {
					currentCerts.put((X509Certificate)tmpCert, alias);
				}
			}
			catch(Throwable e) {
				logger.info("Eliminado el alias '" + alias + "' en el filtrado por expresion: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}

		X509Certificate[] acceptedCerts = certFilter.filter(currentCerts.keySet().toArray(new X509Certificate[0]));

		// Recuperamos los alias de los certificados que han pasado el filtro
		Vector<String> acceptedAlias = new Vector<String>();
		String tmpAl;
		for (X509Certificate c : acceptedCerts) {
			tmpAl = currentCerts.get(c);
			if (tmpAl!=null) acceptedAlias.add(tmpAl);
		}

		return acceptedAlias.toArray(new String[0]);
	}

	/**
	 * Muestra el di&aacute;logo de selecci&oacute;n de certificados del almac&eacute;n seleccionado,
	 * aplicando los filtros de certificado de ser necesario.
	 * @param checkPrivateKey Indica si se deben mostrar &uacute;nicamente los certificados con clave privada.
	 * @return Alias real (con el que fue dado de alta en el almac&eacute;n de claves) del certificado seleccionado.
	 * @throws AOException Cuando se ha pedido un certificado concreto y no se ha obtenido ninguno
	 * o m&aacute;s de un resultado.
	 * @throws AOCancelledOperationException Cuando el usuario cancel&oacute; la operaci&oacute;n.
	 */
	private String selectCertificate(boolean checkPrivateKey) throws AOException, AOCancelledOperationException {
		
		String[] optionAlias = this.filtCert(certAlias);
		if(optionAlias.length == 0) {
			logger.severe("No se ha encontrado ningun certificado que cumpla el patron requerido. Se cancelara la operacion"); //$NON-NLS-1$
			throw new AOException("No se ha encontrado ning\u00FAn certificado que cumpla el patr\u00F3n requerido. Se cancelar\u00E1 la operaci\u00F3n.");
		}
		
		// Si se ha pedido que se seleccione un certificado y se aplico un filtro (por expresiones),
		// se seleccionara el certificado si solo hay uno tras el filtrado o se devolvera un erro si
		// hay mas de uno
		if(this.mandatoryCert && this.certFilter != null) {
			if(optionAlias.length > 1) {
				logger.severe("Se han encontrado mas de un certificado que cumple el patron requerido. Se cancelara la operacion."); //$NON-NLS-1$
				throw new AOException("Se han encontrado m\u00E1s de un certificado que cumple el patr\u00F3n requerido.\nSe cancelar\u00E1 la operaci\u00F3n.");
			}
			return optionAlias[0];
		}

		String ret = null;
		try {
			ret = AOUIManager.showCertSelectionDialog(
					optionAlias, // Aliases
					(this.aoKsManager == null) ? null : this.aoKsManager.getKeyStores(), // KeyStores
							this.keyUsageFilter,
							getParentFrame(this),
							checkPrivateKey, // Comprobar accesibilidad de claves privadas 
							true,						 // Comprobar caducidad del certificado
							this.showExpiratedCertificates,
							this.rfc2254IssuerFilter,
							this.rfc2254SubjectFilter,
							this.mandatoryCert
			);
		}
		catch(final AOCancelledOperationException e) {
			throw e;
		}
		catch(final Throwable e) {
			throw new AOException("No se ha podido mostrar el di\u00E1logo de selecci\u00F3n de certificados."); //$NON-NLS-1$
		}

		return ret;

	}

	public String showCertSelectionDialog() {
		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			public String run() {
				
				// Si no esta inicializada la lista de alias del almacen, la inicializamos
				if(certAlias == null)
					getCertificatesAlias();
				
				try {
					return (selectedAlias = selectCertificate(false));
				} catch (final AOCancelledOperationException e) {
					SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
					return null;
				} catch (final AOException e) {
					SignApplet.this.setError("Ocurri\u00F3 un error al seleccionar el certificado de usuario");
					logger.severe("Ocurrio un error al seleccionar el certificado de usuario: "+e);  //$NON-NLS-1$
					return null;
				}
			}
		});
	}

	/**
	 * Recupera la informaci&oacute;n de firma establecida o generada por el cliente. Si no
	 * hay firma establecida y se ha pedido que no se le deje seleccionar al usuario, se
	 * devolver&aacute; {@code null}.
	 * @param select Indica si mostrar un di&aacute;logo al usuario para la selecci&oacute;n
	 * de la firma cuando no est&eacute; establecida.
	 * @return Firma electr&oacute;nica.
	 * @throws AOCancelledOperationException Cuando el usuario cancela la operaci&oacute;n.
	 * @throws AOException Cuando ocurre un error en la lectura de la informaci&oacute;n de firma.
	 */
	private byte[] getSelectedSignature(final boolean select) throws AOCancelledOperationException, AOException {
		final byte[] originalSign;
		if (signData != null) {
			originalSign = signData;
		} else if (electronicSignatureFile == null && !select) {
			originalSign = null;
		} else {
			if (electronicSignatureFile == null) {
				final String fileName = SignApplet.this.selectSignFile(SignApplet.this.sigFormat);
				if (fileName == null) throw new AOCancelledOperationException("Operacion cancelada por el usuario");  //$NON-NLS-1$
				try {
					electronicSignatureFile = AOUtil.createURI(fileName);
				}
				catch(final Throwable e) {
					logger.severe("Se ha proporcionado un nombre de fichero no valido (" + fileName + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
					throw new AOException("El nombre de fichero '" + fileName + "' no es valido ", e); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}	// Fin 'else': Si no habia fichero seleccionado

			// Cargamos el fichero que estaba seleccionado o recien elegido por el usuario
			try {
				final InputStream is = AOUtil.loadFile(electronicSignatureFile, this, true);
				originalSign = AOUtil.getDataFromInputStream(is);
				try { is.close(); } catch (Exception e) { }
			} 
			catch (final FileNotFoundException e) {
				logger.severe("No se encuentra el fichero de firma '" + electronicSignatureFile.getPath() + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				throw new AOException("No se encuentra el fichero de firma '" + electronicSignatureFile.getPath() + "'"); //$NON-NLS-1$ //$NON-NLS-2$
			} 
			catch (final Throwable e) {
				logger.severe("Error tratando de leer el fichero de firma original (" + electronicSignatureFile.getPath() + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				throw new AOException("Error tratando de leer el fichero de firma '" + electronicSignatureFile.getPath() + "'", e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}

		return originalSign;
	}

	public void setRecipientsToCMS(final String s) {
		logger.info("Invocando setRecipientsToCMS: " + (s != null ? s.trim() : "null")); //$NON-NLS-1$ //$NON-NLS-2$
		this.setError(null);

		if(s == null || s.equals("")) { //$NON-NLS-1$
			this.recipientsCMS = new X509Certificate[0];
			logger.info("Se han eliminado las referencias a los certificados destino de los sobres digitales"); //$NON-NLS-1$
			return;
		}

		String[] recipientsCertFiles = s.split("\n");  //$NON-NLS-1$
		final Vector<X509Certificate> recipientsCerts = new Vector<X509Certificate>(recipientsCertFiles.length); 
		for(final String recipientsCertFile: recipientsCertFiles) {
			try {
				final ByteArrayOutputStream baos = new ByteArrayOutputStream();
				AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
					public Void run() {
						InputStream is = null;
						try {
							is = AOUtil.loadFile(AOUtil.createURI(recipientsCertFile), SignApplet.this, true);
							baos.write(AOUtil.getDataFromInputStream(is));
						} catch (Throwable e) {
							logger.severe("Error al leer el fichero de certificado del destinatario del sobre: " + recipientsCertFile); //$NON-NLS-1$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.468")+recipientsCertFile); //$NON-NLS-1$
						}
						if(is != null) {
							try { is.close(); } catch (Throwable e) {
								logger.warning("No se pudo liberar el flujo de datos de entrada de uno de los certificados de usuario"); //$NON-NLS-1$
							}
						}
						return null;
					}
				});
				recipientsCerts.add((X509Certificate)
						CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
								new ByteArrayInputStream(baos.toByteArray())
						)
				);
			} 
			catch (final Throwable e) {
				logger.severe("Error al cargar el certificado de destinatario (" + recipientsCertFile + "): "+e); //$NON-NLS-1$ //$NON-NLS-2$
				this.setError(AppletMessages.getString("SignApplet.572") + recipientsCertFile); //$NON-NLS-1$
				continue;
			}
		}
		this.recipientsCMS = recipientsCerts.toArray(new X509Certificate[recipientsCerts.size()]);
	}

	public void addRecipientToCMS(final String certB64) {

		if(certB64 == null || certB64.length() == 0)
			throw new NullPointerException("No se ha introducido un certificado de destinatario"); //$NON-NLS-1$

		if(recipientsCMSExt == null) recipientsCMSExt = new HashMap<BigInteger, X509Certificate>();

		// Convertimos a tipo certificado
		X509Certificate x509Cert;
		
		x509Cert = AccessController.doPrivileged(new java.security.PrivilegedAction<X509Certificate>() {
			public X509Certificate run() {
				X509Certificate cert = null;
				try {
					cert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
							new ByteArrayInputStream(new BASE64Decoder().decodeBuffer(certB64))
					);
				} 
				catch (final Throwable e) {
					logger.severe("Error al cargar el certificado de destinatario: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.572")); //$NON-NLS-1$
				}
				return cert;
			}
		});

		if (x509Cert != null) {
			// Si no esta en la lista de certificados, lo agregamos
			if(!recipientsCMSExt.containsKey(x509Cert.getSerialNumber())) {
				recipientsCMSExt.put(x509Cert.getSerialNumber(), x509Cert);   
			}
		}
	}

	public void removeRecipientToCMS(final String certB64) {

		if(certB64 == null || certB64.length() == 0)
			throw new NullPointerException("No se ha introducido el certificado que se desea eliminar"); //$NON-NLS-1$

		if(recipientsCMSExt == null) return;

		// Convertimos a tipo certificado
		X509Certificate x509Cert = 
			AccessController.doPrivileged(new java.security.PrivilegedAction<X509Certificate>() {
				public X509Certificate run() {
					X509Certificate cert = null;
					try {
						cert = (X509Certificate) CertificateFactory.getInstance("X.509").generateCertificate( //$NON-NLS-1$
								new ByteArrayInputStream(new BASE64Decoder().decodeBuffer(certB64))
						);
					} 
					catch (final Throwable e) {
						logger.severe("Error al cargar el certificado de destinatario: " + e); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.572")); //$NON-NLS-1$
					}
					return cert;
				}
			});

		if (x509Cert != null) {
			// Si esta en la lista de certificados, lo eliminamos
			if (recipientsCMSExt.containsKey(x509Cert.getSerialNumber()))
				recipientsCMSExt.remove(x509Cert.getSerialNumber());
		}

		if (recipientsCMSExt.isEmpty()) recipientsCMSExt = null;

	}

	public void setLdapConfiguration(final String address, String port, final String root) {
		logger.info("Invocando setLdapConfiguration (addess=" + address + "; port=" + port + "; root=" + root + ")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		if(address == null) {
			throw new NullPointerException("No se ha indicado la URL del directorio LDAP"); //$NON-NLS-1$
		}

		// Si no se indica el puerto se toma el por defecto
		if(port == null) {
			logger.warning("No se ha indicado el puerto para la configuracion del LDAP, se utilizara el puerto "+DEFAULT_LDAP_PORT); //$NON-NLS-1$
			port = Integer.toString(DEFAULT_LDAP_PORT);
		}

		try {
			ldapServerPort = Integer.parseInt(port);
			if(ldapServerPort < 1 || ldapServerPort > 65535)
				throw new IllegalArgumentException("Numero de puerto no valido, el numero de puerto debe estar entre 1 y 65535"); //$NON-NLS-1$
		} 
		catch (final Throwable e) {
			logger.warning("No se ha insertado un numero de puerto valido para el LDAP, se usara el puerto " + DEFAULT_LDAP_PORT + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
			ldapServerPort = DEFAULT_LDAP_PORT;
		}

		ldapServerUrl = ""; //$NON-NLS-1$
		ldapServerUrl += address;
		//    	ldapRootPath = (root == null || root.trim().length() == 0) ? null : root;
	}

	public void setLdapCertificatePrincipal(final String ldapCertificatePrincipal) {
		logger.info("Invocando setLdapCertificatePrincipal con el parametro: "+ldapCertificatePrincipal); //$NON-NLS-1$
		this.ldapCertificatePrincipal =
			(ldapCertificatePrincipal != null && ldapCertificatePrincipal.length() > 0 ? ldapCertificatePrincipal : null);
	}

	public String getLdapCertificate() {
		logger.info("Invocando getLdapCertificate()"); //$NON-NLS-1$
		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			public String run() {

				final X509Certificate cert;

				// Si se ha establecido la direccion LDAP del certificado, se descarga; si no
				// se muestra un navegador LDAP para seleccionarlo y descargarlo
				if(ldapCertificatePrincipal != null) {
					try {
						cert = LdapUtils.getCertificate(
								SignApplet.this.ldapServerUrl,
								SignApplet.this.ldapServerPort,
								SignApplet.this.ldapCertificatePrincipal
						);
					} catch (final Throwable e) {
						logger.severe("Error al recuperar el certificado '" + ldapCertificatePrincipal + "' del directorio LDAP: " + e); //$NON-NLS-1$ //$NON-NLS-2$
						setError(AppletMessages.getString("SignApplet.74") + ldapCertificatePrincipal); //$NON-NLS-1$
						return null;
					}
				}

				else {
					logger.severe("No se especifico el Principal del certificado que se desea seleccionar");
					return null;
				}

				// Devolvemos el certificado codificado en Base64
				try {
					return new RawBASE64Encoder().encode(cert.getEncoded());
				} 
				catch (final Throwable e) {
					logger.severe("Error al codificar el certificado recuperado del directorio LDAP : " + e); //$NON-NLS-1$
					setError(AppletMessages.getString("SignApplet.83")); //$NON-NLS-1$
					return null;
				}
			}
		});
	}


	public void setCMSContentType(String contentType) {
		logger.info("Invocando setCMSContentType: " + contentType); //$NON-NLS-1$
		this.cmsContentType = contentType;
	}
	
	public boolean buildCMSEncrypted() {
		logger.info("Invocando buildCMSEncrypted"); //$NON-NLS-1$
		return this.doEnvelopOperation(AOConstants.BINARY_ENVELOP_ENCRYPTEDDATA);
	}

	public boolean buildCMSEnveloped() {
		logger.info("Invocando buildCMSEnveloped"); //$NON-NLS-1$
		return this.doEnvelopOperation(AOConstants.BINARY_ENVELOP_ENVELOPEDDATA);
	}

	public boolean buildCMSAuthenticated() {
		logger.info("Invocando buildCMSAuthenticated"); //$NON-NLS-1$
		return this.doEnvelopOperation(AOConstants.BINARY_ENVELOP_AUTHENTICATEDENVELOPEDDATA);
	}
	
	public boolean buildCMSStructure() {
		logger.info("Invocando buildCMSStructure"); //$NON-NLS-1$
		return this.doEnvelopOperation(
				this.cmsContentType == null ?
						AOConstants.DEFAULT_BINARY_ENVELOP
						: this.cmsContentType
		);
	}
	
	public boolean signAndPackData() {
		logger.info("Invocando signAndPackData"); //$NON-NLS-1$
		return this.doEnvelopOperation(AOConstants.BINARY_ENVELOP_SIGNEDANDENVELOPEDDATA);
	}
	
	public boolean signAndPackFile(final String uri) {
		logger.info("Invocando signAndPackFile: " + uri); //$NON-NLS-1$

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {
				InputStream is;
				try {
					is = AOUtil.loadFile(AOUtil.createURI(uri), SignApplet.this, true);
				} catch (Exception e) {
					logger.severe(e.getMessage() + ": " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.407")); //$NON-NLS-1$
					return false;
				}

				return doEnvelopOperation(is, AOConstants.BINARY_ENVELOP_SIGNEDANDENVELOPEDDATA);
			}
		});
	}
	
	private boolean doEnvelopOperation(final String envelopOperation) {
		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {
				InputStream streamToEnvelop = null;
				try {
					streamToEnvelop = getInDataStream();
				} 
				catch (final Throwable e) {
					// El error ya lo ha establecido getInDataStream() con el setError(). 
					return false;
				}
				return doEnvelopOperation(streamToEnvelop, envelopOperation);
			}
		});
	}

	private boolean doEnvelopOperation(final InputStream streamToEnvelop, final String envelopOperation) {
		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {
				SignApplet.this.setError(null);

				// Si el envoltorio solicitado obliga a introducir los datos del remitente, nos aseguramos
				// de que haya un certificado seleccionado.
				PrivateKeyEntry ke = null;
				if(envelopOperation.equals(AOConstants.BINARY_ENVELOP_SIGNEDANDENVELOPEDDATA) ||
						envelopOperation.equals(AOConstants.BINARY_ENVELOP_AUTHENTICATEDENVELOPEDDATA)) {
					if (selectedAlias == null) {
						try {
							if (certAlias == null) getCertificatesAlias();
							selectedAlias = SignApplet.this.selectCertificate(true);
						}
						catch(final AOCancelledOperationException e) {
							logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
							closeStream(streamToEnvelop);
							return false;
						} 
						catch (final AOException e) {
							logger.severe(e.getMessage()+": " + e); //$NON-NLS-1$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.340")); //$NON-NLS-1$
							closeStream(streamToEnvelop);
							return false;
						}
					}
					
					if (selectedAlias == null) {
						logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
						closeStream(streamToEnvelop);
						return false;
					}
				}

				// Aqui solo entraremos si es un envoltorio firmado o un envoltorio al que se le ha indicado
				// el certificado del emisor del mensaje
				if(selectedAlias != null) {
					// Comprobamos que se ha inicializado el repositorio de certificados
					if (aoKsManager == null) {
						logger.severe("No se pudo recuperar el almacen de certificados asociado al alias: " + selectedAlias); //$NON-NLS-1$ //$NON-NLS-2$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.91") + selectedAlias); //$NON-NLS-1$
						closeStream(streamToEnvelop);
						return false;
					}

					// Establecemos la clave del certificado
					try {
						ke = aoKsManager.getKeyEntry(selectedAlias, AOCryptoUtil.getCertificatePC(SignApplet.this.store)); //$NON-NLS-1$
					}
					catch(final AOCancelledOperationException e) {
						logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
						closeStream(streamToEnvelop);
						return false;
					}
					catch (final Throwable e) {
						logger.severe("No se ha podido obtener el certicado con el alias " + selectedAlias + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.482") + selectedAlias); //$NON-NLS-1$
						closeStream(streamToEnvelop);
						return false;
					}

					// Recuperamos el certificado a traves del KeyEntry y lo almacenamos porque posteriormente
					// pueden pedirnos informacion sobre el certificado utilizado
					certToSign = aoKsManager.getCertificate(ke);
				}

				AOCMSSigner cmsEnveloper = new AOCMSSigner();
				if(envelopOperation.equals(AOConstants.BINARY_ENVELOP_ENCRYPTEDDATA)) {
					try {
						// Si no esta establecido el algoritmo de cifrado usamos el por
						// defecto, pero solo para esta ocasion
						AOCipherAlgorithm cipAlgoTemp = (SignApplet.this.cipAlgo == null ? AOCipherAlgorithm.getDefault() : SignApplet.this.cipAlgo);
						cmsEnveloper.setCipherAlgorithm(cipAlgoTemp);

						String passKey = null;
						// Tomamos la clave (si el algoritmo de cifrado la soporta), o mostramos el
						// dialogo para la insercion de contrasena en caso contrario
						if(cipAlgoTemp.supportsKey() && SignApplet.this.keyMode.equals(AOConstants.KEY_MODE_GENERATEKEY)) {
							passKey = new RawBASE64Encoder().encode(
									(new AOSunJCECipher()).generateKey(new AOAlgorithmConfig(SignApplet.this.cipAlgo, null, null)).getEncoded()
							);
							SignApplet.this.cipherB64Key = passKey;
						}
						else if(cipAlgoTemp.supportsKey() && SignApplet.this.keyMode.equals(AOConstants.KEY_MODE_USERINPUT)) {
							if(SignApplet.this.cipherB64Key == null) {
								logger.severe("Debe introducirse una clave para realizar el cifrado de datos"); //$NON-NLS-1$
								SignApplet.this.setError(AppletMessages.getString("SignApplet.483")); //$NON-NLS-1$
								return false;
							}
							passKey = SignApplet.this.cipherB64Key;
						}
						else if(cipAlgoTemp.supportsPassword() && SignApplet.this.keyMode.equals(AOConstants.KEY_MODE_PASSWORD)) {
							if(SignApplet.this.cipherPassword == null || SignApplet.this.cipherPassword.length == 0) {
								try {
									passKey = new String(AOUIManager.getPassword(AppletMessages.getString("SignApplet.414"), getParentFrame(SignApplet.this))); //$NON-NLS-1$
								} catch (AOCancelledOperationException e) {
									logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
									SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
									closeStream(streamToEnvelop);
									return false;
								} 
								catch (Throwable e) {
									logger.severe("Error al solicitar la contrasena al usuario: " + e); //$NON-NLS-1$
									SignApplet.this.setError(AppletMessages.getString("SignApplet.486")); //$NON-NLS-1$
									closeStream(streamToEnvelop);
									return false;
								}
							} else {
								passKey = String.valueOf(SignApplet.this.cipherPassword);
							}
						}
						else {
							logger.severe(AppletMessages.getString("La configuración de algoritmo y modo de clave indicada no esta permitida: " + cipAlgoTemp + "/" + SignApplet.this.keyMode)); //$NON-NLS-1$ //$NON-NLS-2$
							SignApplet.this.setError(
									AppletMessages.getString("SignApplet.487") + cipAlgoTemp + "/" + SignApplet.this.keyMode //$NON-NLS-1$ //$NON-NLS-2$
							);
							closeStream(streamToEnvelop);
							return false;
						}
						SignApplet.this.data = cmsEnveloper.encrypt(
								streamToEnvelop,
								SignApplet.this.sigAlgo == null ? AOConstants.DEFAULT_SIGN_ALGO : SignApplet.this.sigAlgo,
										passKey
						);
					} catch (Throwable e) {
						logger.severe("Error al generar los datos encriptados en una estructura CMS (EncryptedData): " + e); //$NON-NLS-1$ //$NON-NLS-2$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.489")); //$NON-NLS-1$
						closeStream(streamToEnvelop);
						return false;
					}
				}
				else if(envelopOperation.equals(AOConstants.BINARY_ENVELOP_SIGNEDANDENVELOPEDDATA) || 
						envelopOperation.equals(AOConstants.BINARY_ENVELOP_ENVELOPEDDATA) ||
						envelopOperation.equals(AOConstants.BINARY_ENVELOP_AUTHENTICATEDENVELOPEDDATA)) {

					if((SignApplet.this.recipientsCMS == null || SignApplet.this.recipientsCMS.length == 0)
							&& SignApplet.this.recipientsCMSExt == null) {
						logger.severe("No se han especificado los certificados de los destinatarios del sobre digital"); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.86")); //$NON-NLS-1$
						closeStream(streamToEnvelop);
						return false;
					}

					// Cargamos el vector de certificados destinatarios 
					Vector<X509Certificate> recipientsList = new Vector<X509Certificate>();
					if(SignApplet.this.recipientsCMS != null) {
						for(X509Certificate x509Cert : SignApplet.this.recipientsCMS) 
							recipientsList.add(x509Cert);
					}
					if(SignApplet.this.recipientsCMSExt != null) {
						for(X509Certificate x509Cert : SignApplet.this.recipientsCMSExt.values().toArray(new X509Certificate[0]))
							recipientsList.add(x509Cert);
					}

					// Creamos el envoltorio
					try {
						SignApplet.this.data = cmsEnveloper.envelop(
								streamToEnvelop,
								SignApplet.this.sigAlgo == null ? AOConstants.DEFAULT_SIGN_ALGO : SignApplet.this.sigAlgo,
										envelopOperation,
										ke,
										recipientsList.toArray(new X509Certificate[0])
						);
					} 
					catch (final Throwable e) {
						logger.severe("Error al generar el envoltorio firmado CMS de los datos (" + envelopOperation + "): " + e); //$NON-NLS-1$ //$NON-NLS-2$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.87") + envelopOperation); //$NON-NLS-1$
						closeStream(streamToEnvelop);
						return false;
					}
				}
				else {
					logger.severe("Operacion de envoltorio no reconocida: " + envelopOperation); //$NON-NLS-1$
					closeStream(streamToEnvelop);
					SignApplet.this.setError(AppletMessages.getString("SignApplet.494")); //$NON-NLS-1$
					return false;
				}

				// Cerramos el flujo de datos del sobre digital
				closeStream(streamToEnvelop);

				SignApplet.this.setError(null);
				return true;
			}
		});
	}

	public boolean coEnvelop() {
		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {

				// Leemos los datos
				InputStream is;
				try {
					is = SignApplet.this.getInDataStream();
				} catch (AOException e) {
					logger.severe("No se ha podido obtener el certicado con el alias " + selectedAlias + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.482") + selectedAlias); //$NON-NLS-1$
					return false;
				}
				
				// Si el envoltorio solicitado obliga a introducir los datos del remitente, nos aseguramos
				// de que haya un certificado seleccionado.
				PrivateKeyEntry ke = null;
				if (selectedAlias == null) {
					try {
						if (certAlias == null) getCertificatesAlias();
						selectedAlias = SignApplet.this.selectCertificate(true);
						if (selectedAlias == null) {
							throw new AOCancelledOperationException();
						}
					}
					catch(final AOCancelledOperationException e) {
						logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
						return false;
					}
					catch (final AOException e) {
						logger.severe(e.getMessage()+": " + e); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.340")); //$NON-NLS-1$
						return false;
					}
				}

				// Comprobamos que este inicializado el repositorio
				if (aoKsManager == null) {
					logger.severe("No se pudo recuperar el almacen de certificados asociado al alias"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.91") + selectedAlias); //$NON-NLS-1$
					return false;
				}
				
				// Establecemos la clave del certificado
				try {
					ke = aoKsManager.getKeyEntry(selectedAlias, AOCryptoUtil.getCertificatePC(SignApplet.this.store)); //$NON-NLS-1$
				}
				catch(final AOCancelledOperationException e) {
					logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
					return false;
				}
				catch (final Throwable e) {
					logger.severe("No se ha podido obtener el certicado con el alias " + selectedAlias + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.482") + selectedAlias); //$NON-NLS-1$
					return false;
				}

				// Almacenamos el certificado por si se solicita posteriormente
				certToSign = aoKsManager.getCertificate(ke);
				
				// Generamos el nuevo envoltorio
				CMSEnvelopManager enveloper = new CMSEnvelopManager();
				try {
					SignApplet.this.data = enveloper.doCoEnvelop(is, (X509Certificate[])ke.getCertificateChain());
				} catch (IOException e) {
					Logger.getLogger("es.gob.afirma").severe("Ocurrio un error al leer el sobre digital al que desea agrega un nuevo remitente: " + e);
					SignApplet.this.setError("Ocurrio un error al leer el sobre digital al que desea agrega un nuevo remitente");
					return false;
				} catch (Throwable e) {
					Logger.getLogger("es.gob.afirma").severe("Ocurrio un error al agregar el remitente indicado: " + e);
					SignApplet.this.setError("Ocurrio un error al agregar el remitente indicado");
					return false;
				}

				SignApplet.this.setError(null);
				return true;
			}
		});
	}
	
	/**
	 * Cierra un flujo de datos y muestra un mensaje de advertencia en caso de no poder
	 * hacerlo.
	 * @param stream Flujo de datos a cerrar.
	 */
	private void closeStream(final Closeable stream) {
		try { stream.close();} catch (final Exception e) {
			logger.warning("No se ha podido cerrar un stream: " + e); //$NON-NLS-1$
		}
	}

	public boolean recoverCMS() {
		logger.info("Invocando recoverCMS"); //$NON-NLS-1$

		// Reiniciamos el mensaje de error
		this.setError(null);

		return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
			public Boolean run() {

				final org.bouncycastle.asn1.ASN1InputStream is;
				try {
					is = new org.bouncycastle.asn1.ASN1InputStream(SignApplet.this.getInDataStream());
				} 
				catch (final AOException e) {
					logger.severe("No se pudieron leer los datos del envoltorio CMS: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.617")); //$NON-NLS-1$
					return null;
				}

				// Leemos los datos
				final org.bouncycastle.asn1.ASN1Sequence dsq;
				try {
					dsq=(org.bouncycastle.asn1.ASN1Sequence)is.readObject();
				} 
				catch (final IOException e) {
					logger.severe("Se produjo un error durante la lectura de un Objeto ASN.1: " + e); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.620")); //$NON-NLS-1$
					return null;
				} finally {
					try {
						is.close();
					} 
					catch (final Throwable e) {
						logger.warning("No se pudo cerrar el flujo de datos del sobre digital: " + e); //$NON-NLS-1$
					}
				}

				final Enumeration<?> objects = dsq.getObjects();

				// Elementos que contienen los elementos OID Data
				final org.bouncycastle.asn1.DERObjectIdentifier doi = (org.bouncycastle.asn1.DERObjectIdentifier)objects.nextElement();

				final byte[] datos;
				if(doi.equals(org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers.encryptedData)){

					String pass = null;
					// Si se cifra con contrasena
					if(SignApplet.this.keyMode.equals(AOConstants.KEY_MODE_PASSWORD)) {
						if(SignApplet.this.cipherPassword == null || SignApplet.this.cipherPassword.length == 0) {
							pass = String.valueOf(AOUIManager.getPassword(AppletMessages.getString("SignApplet.621"), SignApplet.this)); //$NON-NLS-1$
						} 
						else {
							pass = String.valueOf(SignApplet.this.cipherPassword);
						}
					}
					// Si se encripto con una clave
					else {
						if(SignApplet.this.cipherB64Key == null) {
							pass = String.valueOf(AOUIManager.getPassword(AppletMessages.getString("SignApplet.622"), SignApplet.this)); //$NON-NLS-1$
						} else {
							pass = SignApplet.this.cipherB64Key;
						}
					}

					try {
						datos = new es.gob.afirma.signers.aobinarysignhelper.CMSDecipherEncryptedData().dechiperEncryptedData(SignApplet.this.getInDataStream(), pass);
					} catch (final AOCancelledOperationException e) {
						logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
						return false;
					} catch (final InvalidKeyException e) {
						logger.severe("La clave para descifrado no es valida: " + e); //$NON-NLS-1$
						SignApplet.this.setError("La clave para descifrado no es valida");
						return false;
					} catch (final Throwable e) {
						logger.severe("Error al recuperar el envoltorio de cifrado: " + e); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.624")); //$NON-NLS-1$
						return false;
					}
				}
				else if(doi.equals(org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers.envelopedData) ||
								doi.equals(org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers.signedAndEnvelopedData) ||
								doi.equals(org.bouncycastle.asn1.cms.CMSObjectIdentifiers.authEnvelopedData)) {

					// Si no hay certificado seleccionado mostramos la lista de seleccion
					if (selectedAlias == null) {
						try {
							if (certAlias == null) getCertificatesAlias();
							selectedAlias = SignApplet.this.selectCertificate(true);
						} 
						catch(final AOCancelledOperationException e) {
							logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
							return false;
						} 
						catch (final AOException e) {
							logger.severe("Error: " + e); //$NON-NLS-1$
							SignApplet.this.setError("No se pudo mostrar el listado de certificados del usuario"); //$NON-NLS-1$
							return false;
						}
					}

					if (selectedAlias == null) {
						logger.warning("Error al seleccionar un certificado del repositorio"); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.363")); //$NON-NLS-1$
						return false;
					}

					// Llegados a este punto necesitamos la clave y el certificado, que los obtenemos
					// a partir del alias...
					if (aoKsManager == null) {
						logger.severe("No se pudo recuperar el almacen de certificados asociado al alias: " + selectedAlias); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.91") + selectedAlias); //$NON-NLS-1$
						return false;
					}

					final PrivateKeyEntry ke;
					try {
						ke = aoKsManager.getKeyEntry(selectedAlias, AOCryptoUtil.getCertificatePC(SignApplet.this.store));
					}
					catch(final AOCancelledOperationException ce) {
						logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
						return false;
					}
					catch (final Throwable e) {
						logger.severe("No se ha podido obtener el certicado con el alias '" + selectedAlias + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.94") + selectedAlias); //$NON-NLS-1$
						return false;
					}

					// Recuperamos el certificado a traves del KeyEntry y lo almacenamos porque posteriormente
					// a la firma pueden pedirnos informacion sobre el certificado utilizado
					certToSign = aoKsManager.getCertificate(ke);

					// Obtenemos los datos segun sean de tipo EnvelopedData, SignedAndEnvelopedData
					// o AuthenticationAndEnvelopedData
					try {
						if(doi.equals(org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers.envelopedData)) {
							datos = new es.gob.afirma.signers.aobinarysignhelper.CMSDecipherEnvelopData().dechiperEnvelopData(SignApplet.this.getInDataStream(), certToSign, ke);
						} else if (doi.equals(org.bouncycastle.asn1.cms.CMSObjectIdentifiers.authEnvelopedData)) {
							datos = new es.gob.afirma.signers.aobinarysignhelper.CMSDecipherAuthenticatedEnvelopedData().dechiperAuthenticatedEnvelopedData(SignApplet.this.getInDataStream(), certToSign, ke);
						} else {
							datos = new es.gob.afirma.signers.aobinarysignhelper.CMSDecipherSignedAndEnvelopedData().dechiperSignedAndEnvelopData(SignApplet.this.getInDataStream(), certToSign, ke);
						}
					}
					catch (final AOCancelledOperationException e) {
						logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
						return false;
					}
					catch (final AOInvalidRecipientException e) {
						logger.severe("El usuario indicado no es uno de los destinatarios del sobre digital: " + e); //$NON-NLS-1$
						SignApplet.this.setError("El usuario indicado no es uno de los destinatarios del sobre digital");
						return false;
					}
					catch (final InvalidKeyException e) {
						logger.severe("La clave para descifrado no es valida: " + e); //$NON-NLS-1$
						SignApplet.this.setError("La clave para descifrado no es valida");
						return false;
					}
					catch (final Throwable e) {
						logger.severe("Error durante el extraccion del objeto del envoltorio CMS: " + e); //$NON-NLS-1$
						SignApplet.this.setError(AppletMessages.getString("SignApplet.617")); //$NON-NLS-1$
						return false;
					}
				}
				else {
					logger.severe("Los datos introducidos no se corresponden con un tipo de objeto CMS soportado"); //$NON-NLS-1$
					SignApplet.this.setError(AppletMessages.getString("SignApplet.654")); //$NON-NLS-1$
					return false;
				}

				// Establecemos los datos recuperados para que el cliente los obtenga mediante un #getData
				SignApplet.this.data = datos;

				return true;
			}
		});
	}

	public String formatEncryptedCMS(String b64) {
		logger.info("Invocando formatEncryptedCMS"); //$NON-NLS-1$
		return this.getCMSInfo(b64);
	}

	public String formatEnvelopedCMS(String b64) {
		logger.info("Invocando formatEnvelopedCMS"); //$NON-NLS-1$
		return this.getCMSInfo(b64);
	}

	/**
	 * Recupera la informaci&oacute;n de un objeto CMS reconocido. Si ocurre un
	 * error durante el proceso, se devuelve cadena vac&iacute;a.
	 * @param b64 Objeto CMS en base 64.
	 * @return Informaci&oacute;n del objeto CMS introducido. 
	 */
	private String getCMSInfo(final String b64) {
		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
			public String run() {
				ByteArrayInputStream bais = null;
				try {
					bais = new ByteArrayInputStream(new sun.misc.BASE64Decoder().decodeBuffer(b64)); 
				} catch (Throwable e) {
					logger.severe("Error al decodificar la cadena en base64: " + e); //$NON-NLS-1$
					return ""; //$NON-NLS-1$
				}
				String cmsInformation;
				try {
					cmsInformation = new CMSInformation().getInformation(bais);
				} catch (AOInvalidFormatException e) {
					logger.severe("Formato de dato no valido: " + e); //$NON-NLS-1$
					return ""; //$NON-NLS-1$
				} catch (Throwable e) {
					logger.severe("Error al obtener la informacion del objeto CMS: " + e); //$NON-NLS-1$
					return ""; //$NON-NLS-1$
				}
				try { bais.close(); } catch (IOException e) {}

				return cmsInformation;
			}
		});
	}

	public void setDataMimeType(String mimetype) {
		logger.info("Invocando setDataMimeType: " + mimetype); //$NON-NLS-1$
		if(mimetype == null || mimetype.length() == 0) {
			this.extMimeType = AOConstants.DEFAULT_MIMETYPE;
		}
		this.extMimeType = mimetype;
	}

	public String getB64Data() {
		logger.info("Invocando getB64Data"); //$NON-NLS-1$
		if(this.data == null) logger.warning("No se dispone de datos de salida, se devolvera cadena vacia"); //$NON-NLS-1$
		return (this.data == null ? "" : new RawBASE64Encoder().encode(this.data)); //$NON-NLS-1$
	}

	@Deprecated
	public String getCMSData(){
		logger.info("Invocando getCMSData"); //$NON-NLS-1$
		return getB64Data();
	}

	public String getData() {
		logger.info("Invocando getData"); //$NON-NLS-1$
		if(this.data == null) logger.warning("No se dispone de datos de salida, se devolvera cadena vacia"); //$NON-NLS-1$
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
		this.showExpiratedCertificates = showExpiratedCerts;
	}

	/**
	 * Muestra un di&aacute;logo pidiendo confirmaci&oacute;n para la firma de los datos representados
	 * por un hash en base 64. La entrada de datos para la operaci&oacute;n de firma, cofirma o contrafirma
	 * debe ser v&aacute;lida.
	 * @return <code>true</code> cuando se confirma la operaci&oacute;n <code>false</code> en caso contrario.
	 */
	private boolean showHashMessage() {
		String digestAlgo = AOUtil.getDigestAlgorithm(this.sigAlgo);
		String hashData = getHexDigestData(digestAlgo);

		// Mostramos el mensaje informando del hash de los datos o, en caso de no haber podido calcularlo,
		// lo informamos.
		int result = JOptionPane.showConfirmDialog(
				this,
				(hashData != null ? 
						AppletMessages.getString("SignApplet.655")+ "\n" + digestAlgo+ ": "+hashData : //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
							AppletMessages.getString("SignApplet.657")), //$NON-NLS-1$
							AppletMessages.getString("SignApplet.658"), //$NON-NLS-1$
							JOptionPane.OK_CANCEL_OPTION,
							JOptionPane.WARNING_MESSAGE
		);
		return result == JOptionPane.OK_OPTION;
	}

	/**
	 * Recupera el hash en hexadecimal de los datos de entrada del applet para firma.
	 * @return Hash en hexadecimal formateado o <code>null</code> si no se introdujeron o no pudieron leerse los datos.
	 */
	private String getHexDigestData(String algorithm) {

		byte[] hashData = null;
		// Si estamos firmando datos o si es una firma de hash implicita (contamos con los datos)
		if(this.data != null && (this.hash == null || this.sigMode == AOConstants.SIGN_MODE_IMPLICIT)) {
			try {
				hashData = AOCryptoUtil.getMessageDigest(this.data, algorithm);
			} 
			catch (Throwable e) {
				hashData = null;
			}
		}
		// Si estamos firmando un fichero
		else if(this.fileUri != null) {
			try {
				hashData = AOCryptoUtil.getMessageDigest(
						AOUtil.getDataFromInputStream(AOUtil.loadFile(this.fileUri, null, false)),
						algorithm
				);
			} catch (Throwable e) {
				hashData = null;
			}
		}
		// Si estamos firmando un hash en modo explicito (el caso de modo implicito ya se trato)
		else if(this.hash != null){
			hashData = this.hash;
		}
		// Si no se esta firmando nada de lo anterior (que deberia abarcar todos los casos de firmar y cofirma)
		// comprobamos si se ha introducido la informacion de una firma electronica (caso unicamente de
		// contrafirma ya que la cofirma debe mostrar la informacion de los datos, no de la firma ya existente).
		// Comprobamos si se ha introducido la firma directamente 
		else if(this.signData != null) {
			try {
				hashData = AOCryptoUtil.getMessageDigest(this.signData, algorithm);
			} 
			catch (Throwable e) {
				hashData = null;
			}
		}
		// Comprobamos si se ha introducido la localizacion de la firma
		else if(this.electronicSignatureFile != null) {
			try {
				hashData = AOCryptoUtil.getMessageDigest(
						AOUtil.getDataFromInputStream(AOUtil.loadFile(this.electronicSignatureFile, null, false)),
						algorithm
				);
			} catch (Throwable e) {
				hashData = null;
			}
		}

		if(hashData == null) {
			logger.severe("No se han indicado o no se han podido leer los datos para el calculo de la huella digital"); //$NON-NLS-1$
			return null;
		}

		return AOUtil.hexify(hashData, ":"); //$NON-NLS-1$
	}

	public String Firma(final String datos) {
		logger.info("Invocando Firma"); //$NON-NLS-1$

		//Inicializamos los valores por codigo
		initialize();

		// Descodificamos los datos introducidos, que son los parametros separados por '#' que
		// definen la operacion a realizar
		String[] params = null;
		try {
			params =
				AccessController.doPrivileged(new java.security.PrivilegedAction<String[]>() {
					public String[] run() {
						try {
							return (new String(new sun.misc.BASE64Decoder().decodeBuffer(datos))).split("#"); //$NON-NLS-1$
						} 
						catch (IOException e) {
							logger.severe("La cadena introducida no es un Base 64 valido: " + e); //$NON-NLS-1$
							SignApplet.this.setError(AppletMessages.getString("SignApplet.665")); //$NON-NLS-1$
							throw new RuntimeException("Error al decodificar el Base64: " + e); //$NON-NLS-1$
						}
					}
				});
		} 
		catch (RuntimeException e) {
			return null;
		}

		// Comprobamos que hemos podido extraer todos los parametros
		if(params == null || params.length < 3) {
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
		if(!this.addSignedAttribute("2.5.4.45", params[1])) { //$NON-NLS-1$
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
			//Establecemos la firma
			setElectronicSignature(params[3]);
			//Cofirmamos
			allOK = this.coSign();
			logger.info("Se ha realizado una operacion de cofirma"); //$NON-NLS-1$
			break;

			// Contrafirma (Firma en cascada)
		case 2:
			//Establecemos la firma
			setElectronicSignature(params[2]);
			//Establecemos los firmantes que se desean contrafirmar
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

		if(!allOK){
			logger.severe("No se pudo completar la firma de datos"); //$NON-NLS-1$
			this.setError(AppletMessages.getString("SignApplet.101")); //$NON-NLS-1$
			return null;
		}

		// Indicamos que no se ha producido error.
		SignApplet.this.setError(null);

		//Devolvemos "cert=<CERTIFICADO>;enc=<FIRMA>"
		return "cert="+getSignCertificateBase64Encoded()+";enc="+getSignatureBase64Encoded(); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public boolean addSignedAttribute(String oid, String value) {
		logger.warning("Invocando addSignedAttribute: " + oid + " = " + value); //$NON-NLS-1$ //$NON-NLS-2$

		// Comprobaciones de seguridad
		if(oid == null || value == null) {
			logger.severe("Ni el OID ni el valor del atributo firmado a agregar pueden ser nulos"); //$NON-NLS-1$
			this.setError(AppletMessages.getString("SignApplet.690")); //$NON-NLS-1$
			return false;
		}

		// Creamos primeramente el listado de atributos si no lo esta ya
		if (this.signedAttributes == null)
			this.signedAttributes = new HashMap<org.ietf.jgss.Oid, String>();

		// Comprobamos que OID se valido
		org.ietf.jgss.Oid newOid = null;
		try {
			newOid = new org.ietf.jgss.Oid(oid);
		} catch (Exception e) {
			logger.severe("El OID especificado no tiene un formato de OID valido: " + e); //$NON-NLS-1$
			this.setError(AppletMessages.getString("SignApplet.693")); //$NON-NLS-1$
			return false;
		}

		// Comprobamos que el OID no estuviese ya agregado
		if(this.signedAttributes.containsKey(newOid)) {
			logger.warning("Ya existia un atributo con el OID especificado, se sustituira su valor por el nuevo"); //$NON-NLS-1$
		}

		// Agregamos el nuevo atributo
		this.signedAttributes.put(newOid, value);

		return true;
	}


	public boolean removeSignedAttribute(String oid) {
		logger.warning("Invocando removeSignedAttribute: " + oid); //$NON-NLS-1$

		// Comprobamos que el oid no sea nulo
		if(oid == null) {
			logger.severe("El OID del atributo firmado que se desea eliminar no puede ser nulo"); //$NON-NLS-1$
			this.setError(AppletMessages.getString("SignApplet.698")); //$NON-NLS-1$
			return false;
		}

		// Comprobamos la validez del OID a eliminar
		org.ietf.jgss.Oid oidToRemove = null;
		try {
			oidToRemove = new org.ietf.jgss.Oid(oid);
		} catch (Exception e) {
			logger.severe("El OID especificado no tiene un formato valido: " + e); //$NON-NLS-1$
			this.setError(AppletMessages.getString("SignApplet.693")); //$NON-NLS-1$
			return false;
		}

		// Comprobamos que el atributo exista
		if(this.signedAttributes == null || !this.signedAttributes.containsKey(oidToRemove)) {
			logger.severe("El OID especificado no se agrego previamente a la firma"); //$NON-NLS-1$
			this.setError(AppletMessages.getString("SignApplet.704")); //$NON-NLS-1$
			return false;
		}

		// Eliminamos el atributo
		this.signedAttributes.remove(oidToRemove);

		// Si esta vacio el listado de atributos, liberamos memoria
		if(this.signedAttributes.isEmpty()) {
			this.signedAttributes = null;
		}
		return true;
	}

	public boolean addUnsignedAttribute(String oid, String value) {
		logger.warning("Invocando addUnsignedAttribute: " + oid + " = " + value); //$NON-NLS-1$ //$NON-NLS-2$

		// Comprobaciones de seguridad
		if(oid == null || value == null) {
			logger.severe("Ni el OID ni el valor del atributo no firmado a agregar pueden ser nulos"); //$NON-NLS-1$
			this.setError(AppletMessages.getString("SignApplet.690")); //$NON-NLS-1$
			return false;
		}

		// Creamos primeramente el listado de atributos si no lo esta ya
		if (this.unsignedAttributes == null)
			this.unsignedAttributes = new HashMap<org.ietf.jgss.Oid, Vector<String>>();

		// Comprobamos que OID se valido
		org.ietf.jgss.Oid newOid = null;
		try {
			newOid = new org.ietf.jgss.Oid(oid);
		} catch (Exception e) {
			logger.severe("El OID especificado no tiene un formato valido: " + e); //$NON-NLS-1$
			this.setError(AppletMessages.getString("SignApplet.693")); //$NON-NLS-1$
			return false;
		}

		// Agregamos el valor del atributo, teniendo en cuenta que el OID especificado ya podria tener otros atributos asignados
		Vector<String> values = null;
		if(this.unsignedAttributes.containsKey(newOid)) {
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
		if(oid == null) {
			logger.severe("El OID del atributo no firmado que se desea eliminar no puede ser nulo"); //$NON-NLS-1$
			this.setError(AppletMessages.getString("SignApplet.698")); //$NON-NLS-1$
			return false;
		}

		// Comprobamos la validez del OID a eliminar
		org.ietf.jgss.Oid oidToRemove = null;
		try {
			oidToRemove = new org.ietf.jgss.Oid(oid);
		} catch (Exception e) {
			logger.severe("El OID especificado no tiene un formato valido: " + e); //$NON-NLS-1$
			this.setError(AppletMessages.getString("SignApplet.693")); //$NON-NLS-1$
			return false;
		}

		// Comprobamos que el atributo exista y si tiene mas valores asignados para eliminar lo que corresponda
		if(this.unsignedAttributes != null && this.unsignedAttributes.containsKey(oidToRemove)) {
			Vector<String> values = this.unsignedAttributes.get(oidToRemove);
			if(values.contains(value)) {
				// Si el atributo existe y solo tiene ese valor, eliminamos el atributo completo
				if(values.size() == 1) {
					this.unsignedAttributes.remove(oidToRemove);
				} else {
					// Si el atributo tiene mas valores, eliminamos solo el que corresponda
					values.remove(value);
					this.unsignedAttributes.put(oidToRemove, values);
				}
			}
		}

		// Si esta vacio el listado de atributos, liberamos memoria
		if(this.unsignedAttributes.isEmpty()) this.unsignedAttributes = null;

		return true;
	}

	public void addExtraParam(String key, String value) {
		logger.warning("Invocando addExtraParam: " + key); //$NON-NLS-1$

		// Si no se ha indicado una clave o valor, abortamos la operacion
		if(key == null || value == null) return;

		// Establecemos la propiedad
		genericConfig.setProperty(key, value);
	}

	public void removeExtraParam(String key) {
		logger.warning("Invocando removeExtraParam: " + key); //$NON-NLS-1$

		// Si no se ha indicado una clave, abortamos la operacion
		if(key == null) return;

		// Eliminamos la propiedad
		genericConfig.remove(key);
	}

	public void addXMLTransform(String type, String subtype, String body) {
		if(this.xmlTransforms == null)
			this.xmlTransforms = new Vector<AOXMLTransform>();

		this.xmlTransforms.add(new AOXMLTransform(type, subtype, body));
	}

	public void resetXMLTransforms() {
		this.xmlTransforms = null;
	}

	public String getVersion() {

		Properties p = new Properties();
		try {
			p.load(SignApplet.class.getResourceAsStream("/version.properties")); //$NON-NLS-1$
		} catch (Throwable e) {
			logger.warning("No se han podido obtener los datos de version del cliente de firma"); //$NON-NLS-1$
		}
		StringBuilder version = new StringBuilder();
		version.append(p.getProperty("version.mayor", "0")).append(".") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		.append(p.getProperty("version.minor", "0")).append(".") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		.append(p.getProperty("version.build", "0")); //$NON-NLS-1$ //$NON-NLS-2$

		String desc = p.getProperty("build"); //$NON-NLS-1$
		if(desc != null && !desc.trim().equals("")) { //$NON-NLS-1$
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
					return AOUIManager.getLoadFileName(
							title,
							((exts == null ||  exts.trim().length() == 0) ?
									null : exts.split(SignApplet.STRING_SEPARATOR)),
									description,
									SignApplet.this);
				}
			});
		} 
		catch (AOCancelledOperationException e) {
			logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
			this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
			return null;
		} 
		catch (Throwable e) {
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
		catch (AOCancelledOperationException e) {
			logger.info("Operacion cancelada por el usuario"); //$NON-NLS-1$
			this.setError(AppletMessages.getString("SignApplet.68")); //$NON-NLS-1$
			return null;
		} 
		catch (Throwable e) {
			logger.warning("Error al seleccionar un directorio: " + e); //$NON-NLS-1$
			this.setError(AppletMessages.getString("SignApplet.333")); //$NON-NLS-1$
			return null;
		}
	}

	public X509Certificate getSignCertificate() {
		logger.info("Invocando getSignCertificateBase64Encoded"); //$NON-NLS-1$
		if(certToSign == null) {
			logger.warning("No se dispone del certificado de firma, se devolvera una cadena vacia"); //$NON-NLS-1$
			return null;
		}

		return certToSign;
	}

	// *************************************************************************
	// *************************************************************************

	/**
	 * Recupera el frame del que cuelga un componente.
	 * @return Frame padre del componente.
	 */
	private Frame getParentFrame(Component c) {
		if(c == null) return null;
		Container cont  = c.getParent(); 
		if (cont instanceof Frame) return (Frame) cont; 
		return getParentFrame(cont); 
	}

	/**
	 * Toma un array de cadenas y las concatena separ&aacute;ndolas con un delimitador.
	 * @return Cadena concatenada.
	 */
	private String concatStrings(String[] strings, String delimitator) {
		StringBuilder sb = new StringBuilder();
		for(int i=0; i<strings.length; i++) {
			if(i>0) sb.append(delimitator);
			sb.append(strings[i]);
		}
		return sb.toString();
	}

	public void setCertFilterRFC2254(String subjectFilter, String issuerFilter, boolean onlySignatureCertificates) {
		rfc2254IssuerFilter = issuerFilter;
		rfc2254SubjectFilter = subjectFilter;
		if (onlySignatureCertificates) keyUsageFilter = AOConstants.SIGN_CERT_USAGE;
		this.mandatoryCert = false;
	}

	public void setMandatoryCertificateConditionRFC2254(String subjectFilter, String issuerFilter, boolean onlySignatureCertificates) {
		logger.info("Invocando setMandatoryCertificationCondition"); //$NON-NLS-1$
		rfc2254IssuerFilter = issuerFilter;
		rfc2254SubjectFilter = subjectFilter;
		if (onlySignatureCertificates) keyUsageFilter = AOConstants.SIGN_CERT_USAGE;
		
		// Si se establecio alguno de los tres filtros, activamos la seleccion de un unico certificado
		this.mandatoryCert = 
			(rfc2254IssuerFilter != null || rfc2254SubjectFilter != null || onlySignatureCertificates);
	}
	
	/**
	 * Firma una cadena de texto simulando el funcionamiento del m&eacute;todo {@code [window.]crypto
	 * .signText(stringToSign, caOption, [caNameString1, [caNameString2, . . . ]])} de JavaScript en 
	 * navegadores Mozilla / Firefox.
	 * @param stringToSign El texto a firmar. Si se especifica "ask" en el par&aacute;metro caOption 
	 * se presenta el texto al usuario en un di&aacute;logo de una forma legible
	 * @param caOption Puede ser una de las siguientes opciones: 
	 * <ul><li>"auto" indica que el programa seleccionar&aacute; autom&aacute;ticamente un certificado. 
	 * Si se ha indicado uno o m&aacute;s nombre de CA mediante los par&aacute;metros caNameN la 
	 * selecci&oacute;n autom&aacute;tica se limita a los certificado emitidos por las CA indicadas.</li> 
	 * <li>"ask" indica que se debe solicitar al usuario que seleccione un certificado. Si se ha indicado 
	 * uno o m&aacute;s nombre de CA mediante los par&aacute;metros caNameN la selecci&oacute;n 
	 * autom&aacute;tica se limita a los certificado emitidos por las CA indicadas. </li></ul>
	 * @param caNameN DN de las CA cuyos certificados deben tenerse en cuenta para la selección de firmante, 
	 * debe proporcionarse un par&aacute;metro por cada CA. Para mayor información sobre el formato DN consulte 
	 * <a href="http://www.faqs.org/rfcs/rfc1485.html">String Representation of Distinguished Names</a>.
	 * @return Si el usuario aprob&oacute; la operaci&oacute;n y esta termin&oacute; correctamente se 
	 * devuelve el objeto firmado en formato CMS codificado en Base64. En caso contrario devuelve uno 
	 * de los siguientes c&oacute;digos de error: 
	 * <ul><li>error:noMatchingCert si el usuario no dispone de ning&uacute;n certificado emitido por las 
	 * CA indicadas.</li> 
	 * <li>error:userCancel si el usuario cancela la operaci&oacute;n. </li>
	 * <li>error:internalError si ocurre cualquier error durante el proceso.</li></ul>
	 */
	public String signText(final String stringToSign, final String caOption, final String[] caNameN) {
		return AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {

			public String run() {

				if(SignApplet.this.aoKsManager == null)
					try {
						SignApplet.this.initKeyStore();
					} catch (AOException e) {
						logger.severe("Error al inicializar el almacen de claves");
						SignApplet.this.setError("Error al inicializar el almacen de claves");
						return null;
					}

					SignText signTextComponent = new SignText(
							getArrayCertificatesAlias(),
							SignApplet.this.aoKsManager,
							SignApplet.this,
							AOCryptoUtil.getCertificatePC(SignApplet.this.store)
					);

					// El metodo signText muestra el dialogo propio de la funcion document.signText() de Mozilla
					// ejecuta la operacion de firma y, si ocurre algun error, establece como resultado el codigo
					// de error correspondiente
					signTextComponent.signText(stringToSign, caOption, caNameN);

					return signTextComponent.getResult();
			}
		});
	}

	/**
	 * Firma una cadena de texto simulando el funcionamiento del m&eacute;todo {@code [window.]crypto
	 * .signText(stringToSign, caOption, [caNameString1, [caNameString2, . . . ]])} de JavaScript en 
	 * navegadores Mozilla / Firefox.
	 * @param stringToSign El texto a firmar. Si se especifica "ask" en el par&aacute;metro caOption 
	 * se presenta el texto al usuario en un di&aacute;logo de una forma legible
	 * @param caOption Puede ser una de las siguientes opciones: 
	 * <ul><li>"auto" indica que el programa seleccionar&aacute; autom&aacute;ticamente un certificado. 
	 * Si se ha indicado uno o m&aacute;s nombre de CA mediante los par&aacute;metros caNameN la 
	 * selecci&oacute;n autom&aacute;tica se limita a los certificado emitidos por las CA indicadas.</li> 
	 * <li>"ask" indica que se debe solicitar al usuario que seleccione un certificado. Si se ha indicado 
	 * uno o m&aacute;s nombre de CA mediante los par&aacute;metros caNameN la selecci&oacute;n 
	 * autom&aacute;tica se limita a los certificado emitidos por las CA indicadas. </li></ul>
	 * @return Si el usuario aprob&oacute; la operaci&oacute;n y esta termin&oacute; correctamente se 
	 * devuelve el objeto firmado en formato CMS codificado en Base64. En caso contrario devuelve uno 
	 * de los siguientes c&oacute;digos de error: 
	 * <ul><li>error:noMatchingCert si el usuario no dispone de ning&uacute;n certificado emitido por las 
	 * CA indicadas.</li> 
	 * <li>error:userCancel si el usuario cancela la operaci&oacute;n. </li>
	 * <li>error:internalError si ocurre cualquier error durante el proceso.</li></ul>
	 */
	public String signText(final String stringToSign, final String caOption) {
		return signText(stringToSign, caOption, null);
	}
}

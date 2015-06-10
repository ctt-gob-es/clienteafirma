package es.gob.afirma.signers.tsp.pkcs7;

import java.io.IOException;
import java.net.URI;
import java.util.Properties;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;

/** Par&aacute;metros de configuraci&oacute;n de una Autoridad de Sellado de Tiempo.
 * @author Tomas Garc&iacute;a-Mer&aacute;s. */
public final class TsaParams {

	private static final String DEFAULT_DIGEST_ALGO = "SHA-512"; //$NON-NLS-1$
	private static final String POLICY = "0.4.0.2023.1.1"; //$NON-NLS-1$

	private final boolean tsaRequireCert;
	private final String tsaPolicy;
	private final URI tsaURL;
	private final String tsaUsr;
	private final String tsaPwd;
	private final TsaRequestExtension[] extensions;
	private final String tsaHashAlgorithm;
	private final byte[] sslKeyStore;
	private final String sslKeyStorePassword;
	private final String sslKeyStoreType;
	private final byte[] sslTrustStore;
	private final String sslTrustStorePassword;
	private final String sslTrustStoreType;
	private final boolean verifyHostname;


	/** Sello de tiempo a nivel de firma. */
	public static final String TS_SIGN = "1";  //$NON-NLS-1$

	/** Sello de tiempo a nivel de documento. */
	public static final String TS_DOC = "2";  //$NON-NLS-1$

	/** Sello de tiempo doble, a nivel de firma y a nivel de documento. */
	public static final String TS_SIGN_DOC = "3";  //$NON-NLS-1$

	@Override
	public String toString() {

		// Extensiones
		final StringBuilder exts = new StringBuilder("["); //$NON-NLS-1$
		if (this.extensions != null) {
			for(final TsaRequestExtension ex : this.extensions) {
				exts.append(ex);
				exts.append("; "); //$NON-NLS-1$
			}
		}
		exts.append("]"); //$NON-NLS-1$

		final String ret = "Parametros TSA [" + //$NON-NLS-1$
					"URL=" + getTsaUrl() + "; " + //$NON-NLS-1$ //$NON-NLS-2$
					"User=" + getTsaUsr() + ": " + //$NON-NLS-1$ //$NON-NLS-2$
					"Policy=" + getTsaPolicy() + "; " + //$NON-NLS-1$ //$NON-NLS-2$
					"Extensions" + exts.toString() + "; " + //$NON-NLS-1$ //$NON-NLS-2$
					"Digest=" + getTsaHashAlgorithm() + "; " + //$NON-NLS-1$ //$NON-NLS-2$
					"SSLKeyStore=" + (getSslKeyStore() != null ? "Yes" : "No") + "; " + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
					"SSLKeyStorePwd=" + getSslKeyStorePassword() + //$NON-NLS-1$
					"SSLKeyStoreType=" + getSslKeyStoreType() + //$NON-NLS-1$
					"SSLTrustStore=" + (getSslTrustStore() != null ? "Yes" : "No") + "; " + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
					"SSLTrustStorePwd=" + getSslTrustStorePassword() + //$NON-NLS-1$
					"SSLTrustStoreType=" + getSslTrustStoreType() + //$NON-NLS-1$
					"VerifyHostname=" + isVerifyHostname() + //$NON-NLS-1$
			"]"; //$NON-NLS-1$

		// Quitamos el punto y coma de la ultima extension
		return ret.replace("]; ]", "]]"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Construye los par&aacute;metros de configuraci&oacute;n de una Autoridad de Sellado de Tiempo.
	 * @param requireCert Indicar <code>true</code>Si es necesario incluir el certificado de la TSA,
	 *                    <code>false</code> en caso contrario.
	 * @param policy OID de la pol6iacute;tica de sellado de tiempo.
	 * @param url URL de la TSA.
	 * @param usr Nombre de usuario para la TSA.
	 * @param pwd Contrase&ntilde;a para el usuario de la TSA.
	 * @param exts Extensiones de la petici&oacute;n a la TSA.
	 * @param hashAlgorithm Algoritmo de huella digital a usar.
	 * @param sslKeyStoreFile Almac&eacute;n que contiene el certificado SSL cliente que pedir&aacute; la TSA al
	 *                  establecer la coneci&oacute;s HTTPS.
	 * @param sslKeyStorePassword Contrase&ntilde;a del almac&eacute;n que contiene el certificado SSL
	 *                          cliente para las conexiones HTTPS.
	 * @param sslKeyStoreType Tipo de almac&eacute;n que contiene el certificado SSL cliente.
	 * @param sslTrustStore Almac&eacute;n para los certificados SSL.
	 * @param sslTrustStore Almac&eacute;n que contiene el certificado SSL cliente que pedir&aacute; la TSA al
	 *                  establecer la coneci&oacute;s HTTPS.
	 * @param sslTrustStorePassword Contrase&ntilde;a del almac&eacute;n que contiene el certificado SSL
	 *                          cliente para las conexiones HTTPS.
	 * @param sslTrustStoreType Tipo de almac&eacute;n que contiene los certificado de CA reconocidas.
	 * @param verifyHostname Verifica el nombre de dominio contra los certificados SSL.
	 *                          */
	public TsaParams(final boolean requireCert,
			         final String policy,
			         final URI url,
			         final String usr,
			         final String pwd,
			         final TsaRequestExtension[] exts,
			         final String hashAlgorithm,
			         final byte[] sslKeyStoreFile,
			         final String sslKeyStorePassword,
			         final String sslKeyStoreType,
			         final byte[] sslTrustStore,
			         final String sslTrustStorePassword,
			         final String sslTrustStoreType,
			         final boolean verifyHostname) {
        if (url == null) {
        	throw new IllegalArgumentException(
				"La URL del servidor de sello de tiempo no puede ser nula" //$NON-NLS-1$
			);
        }
        this.tsaURL = url;
        this.tsaPolicy = policy != null ? policy : POLICY;
        this.tsaUsr = usr;
        this.tsaPwd = pwd;
        this.extensions = exts != null ? exts.clone() : null;
        this.tsaHashAlgorithm = hashAlgorithm != null ? hashAlgorithm : DEFAULT_DIGEST_ALGO;
        this.sslKeyStore = sslKeyStoreFile != null ? sslKeyStoreFile.clone() : null;
        this.sslKeyStorePassword = sslKeyStorePassword;
        this.sslKeyStoreType = sslKeyStoreType;
        this.sslTrustStore = sslTrustStore != null ? sslTrustStore.clone() : null;
        this.sslTrustStorePassword = sslTrustStorePassword;
        this.sslTrustStoreType = sslTrustStoreType;
        this.tsaRequireCert = requireCert;
        this.verifyHostname = verifyHostname;
	}

	/** Construye los par&aacute;metros de configuraci&oacute;n de una Autoridad de Sellado de Tiempo.
	 * En caso de ausencia o error en las propiedades de entrada lanza una <code>IllegalArgumentException</code>.
	 * @param extraParams Propiedades que contienen los par&aacute;metros de configuraci&oacute;n necesarios. */
	public TsaParams(final Properties extraParams) {
		if (extraParams == null) {
			throw new IllegalArgumentException(
				"La propiedades de configuracion de la TSA no pueden ser nulas" //$NON-NLS-1$
			);
		}
		final String tsa = extraParams.getProperty("tsaURL"); //$NON-NLS-1$
        if (tsa == null) {
        	throw new IllegalArgumentException(
				"La URL del servidor de sello de tiempo no puede ser nula" //$NON-NLS-1$
			);
        }
        try {
    		this.tsaURL = new URI(tsa);
    	}
    	catch(final Exception e) {
    		throw new IllegalArgumentException(
				"Se ha indicado una URL de TSA invalida (" + tsa + "): " + e, e //$NON-NLS-1$ //$NON-NLS-2$
			);
    	}
        this.tsaPolicy = extraParams.getProperty("tsaPolicy") != null ? //$NON-NLS-1$
    		extraParams.getProperty("tsaPolicy") : //$NON-NLS-1$
    			POLICY;
        this.tsaHashAlgorithm = extraParams.getProperty("tsaHashAlgorithm") != null ? //$NON-NLS-1$
        		AOSignConstants.getDigestAlgorithmName(extraParams.getProperty("tsaHashAlgorithm")) : //$NON-NLS-1$
        			DEFAULT_DIGEST_ALGO;
        this.tsaRequireCert = !Boolean.FALSE.toString().equalsIgnoreCase(extraParams.getProperty("tsaRequireCert")); //$NON-NLS-1$
        this.tsaUsr = extraParams.getProperty("tsaUsr"); //$NON-NLS-1$
        this.tsaPwd = extraParams.getProperty("tsaPwd"); //$NON-NLS-1$

        // Almacen para el SSL cliente
        final String keyStoreDataB64 = extraParams.getProperty("tsaSslKeyStore"); //$NON-NLS-1$
        if (keyStoreDataB64 != null) {
        	try {
				this.sslKeyStore = Base64.decode(keyStoreDataB64);
			}
			catch(final Exception e) {
				throw new IllegalArgumentException(
					"No se ha proporcionado en el parametro 'tsaSslKeyStore' el almacen de claves del SSL de la TSA en base 64: " + e, e  //$NON-NLS-1$
				);
			}
        }
        else {
        	this.sslKeyStore = null;
        }
        this.sslKeyStorePassword = extraParams.getProperty("tsaSslKeyStorePassword", ""); //$NON-NLS-1$ //$NON-NLS-2$
        this.sslKeyStoreType = extraParams.getProperty("tsaSslKeyStoreType", "PKCS12"); //$NON-NLS-1$ //$NON-NLS-2$

        // TrustStore con los certificados de confianza para el SSL
        final String trustStoreDataB64 = extraParams.getProperty("tsaSslTrustStore"); //$NON-NLS-1$
        if (trustStoreDataB64 != null) {
			try {
				this.sslTrustStore = Base64.decode(trustStoreDataB64);
			}
			catch(final Exception e) {
				throw new IllegalArgumentException(
						"No se ha proporcionado en el parametro 'tsaSslTrustStore' el almacen de confianza del SSL de la TSA en base 64: " + e, e  //$NON-NLS-1$
					);
			}
        }
        else {
        	this.sslTrustStore = null;
        }
        this.sslTrustStorePassword = extraParams.getProperty("tsaSslTrustStorePassword", ""); //$NON-NLS-1$ //$NON-NLS-2$
        this.sslTrustStoreType = extraParams.getProperty("tsaSslTrustStoreType", "PKCS12"); //$NON-NLS-1$ //$NON-NLS-2$

        try {
	        this.extensions = extraParams.getProperty("tsaExtensionOid") != null && extraParams.getProperty("tsaExtensionValueBase64") != null ? //$NON-NLS-1$ //$NON-NLS-2$
				new TsaRequestExtension[] {
					new TsaRequestExtension(
						extraParams.getProperty("tsaExtensionOid"), //$NON-NLS-1$
						Boolean.parseBoolean(extraParams.getProperty("tsaExtensionCritical", "false")), //$NON-NLS-1$ //$NON-NLS-2$
						Base64.decode(extraParams.getProperty("tsaExtensionValueBase64")) //$NON-NLS-1$
					)
				} : null;
        }
        catch(final IOException e) {
        	throw new IllegalArgumentException("Las extensiones del sello de tiempo no estan adecuadamente codificadas: " + e, e); //$NON-NLS-1$
        }

        this.verifyHostname = Boolean.parseBoolean(
    		extraParams.getProperty("verifyHostname", Boolean.TRUE.toString()) //$NON-NLS-1$
		);
	}

	boolean doTsaRequireCert() {
		return this.tsaRequireCert;
	}

	String getTsaPolicy() {
		return this.tsaPolicy;
	}

	URI getTsaUrl() {
		return this.tsaURL;
	}

	String getTsaUsr() {
		return this.tsaUsr;
	}

	String getTsaPwd() {
		return this.tsaPwd;
	}

	TsaRequestExtension[] getExtensions() {
		return this.extensions;
	}

	/** Obtiene el algoritmo de huella digital a usar en el sellado de tiempo.
	 * @return Algoritmo de huella digital a usar en el sellado de tiempo. */
	public String getTsaHashAlgorithm() {
		return this.tsaHashAlgorithm;
	}

	/** Obtiene el fichero PKCS#12 que contiene el certificado SSL cliente que pedir&aacute; la TSA al
	 * establecer la coneci&oacute;s HTTPS.
	 * @return Fichero PKCS#12 que contiene el certificado SSL cliente para las conexiones HTTPS, o
	 *         <code>null</code> si no se ha establecido ninguno. */
	public byte[] getSslKeyStore() {
		return this.sslKeyStore;
	}

	/** Obtiene la contrase&ntilde;a del fichero PKCS#12 que contiene el certificado SSL cliente para las conexiones HTTPS.
	 * @return Contrase&ntilde;a del fichero PKCS#12 que contiene el certificado SSL cliente para las conexiones HTTPS o
	 *         cadena vac&iacute;a si no se ha establecido ninguna. */
	public String getSslKeyStorePassword() {
		return this.sslKeyStorePassword;
	}

	/** Obtiene el tipo de almac&eacute;n que contiene el certificado SSL cliente para las conexiones HTTPS.
	 * @return Tipo de almac&eacute;n (JKS o PKCS12). Si no se indica ninguno, se establecer&aacute; JKS. */
	public String getSslKeyStoreType() {
		return this.sslKeyStoreType;
	}

	/** Obtiene el TrustStore que contiene los certificados de CA que se permiten para la generacion del certificado SSL
	 * de la TSA a trav&eacute;s de los que se puede establecer la coneci&oacute;s HTTPS.
	 * @return Almac&eacute;n que contiene los certificados de CA para las conexiones HTTPS, o
	 *         <code>null</code> si no se ha establecido ninguno. */
	public byte[] getSslTrustStore() {
		return this.sslTrustStore;
	}

	/** Obtiene la contrase&ntilde;a del almac&eacute;n que contiene el certificado con las CA de confianza para las conexiones HTTPS.
	 * @return Contrase&ntilde;a del almac&eacute;n o cadena vac&iacute;a si no se ha establecido ninguna. */
	public String getSslTrustStorePassword() {
		return this.sslTrustStorePassword;
	}

	/** Obtiene el tipo de almac&eacute;n que contiene los certificados de CA de confianza para las conexiones HTTPS.
	 * @return Tipo de almac&eacute;n (JKS o PKCS12). Si no se indica ninguno, se establecer&aacute; JKS. */
	public String getSslTrustStoreType() {
		return this.sslTrustStoreType;
	}

	/** Indica si se verifica o no el nombre de <i>host</i> en el SSL.
	 * @return <code>true</code> si se verifica el nombre de <i>host</i> en el SSL,
	 *         <code>false</code> en caso contrario. */
	public boolean isVerifyHostname() {
		return this.verifyHostname;
	}
}

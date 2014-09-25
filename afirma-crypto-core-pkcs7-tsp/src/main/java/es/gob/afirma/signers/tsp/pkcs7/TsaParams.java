package es.gob.afirma.signers.tsp.pkcs7;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.Properties;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;

/** Par&aacute;metros de configuraci&oacute;n de una Autoridad de Sellado de Tiempo.
 * @author Tomas Garc&iacute;a-Mer&aacute;s. */
public final class TsaParams {

	private static final String DEFAULT_DIGEST_ALGO = "SHA-512"; //$NON-NLS-1$
	private static final String POLICY = "0.4.0.2023.1.1"; //$NON-NLS-1$

	@XmlElement(name = "tsaRequireCert")
	private final boolean tsaRequireCert;

	@XmlElement(name = "tsaPolicy")
	private final String tsaPolicy;

	@XmlElement(name = "tsaURL")
	private final URI tsaURL;

	@XmlElement(name = "tsaUsr")
	private final String tsaUsr;

	@XmlElement(name = "tsaPwd")
	private final String tsaPwd;

	@XmlElementWrapper(name = "extensions")
	@XmlElement(name = "extension")
	private final TsaRequestExtension[] extensions;

	@XmlElement(name = "tsaHashAlgorithm")
	private final String tsaHashAlgorithm;

	@XmlElement(name = "sslPkcs12File")
	private final byte[] sslPkcs12File;

	@XmlElement(name = "sslPkcs12FilePassword")
	private final String sslPkcs12FilePassword;

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
					"SSLP12=" + (getSslPkcs12File() != null ? "Yes" : "No") + "; " + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
					"SSLP12Pwd=" + getSslPkcs12FilePassword() + //$NON-NLS-1$
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
	 * @param sslPkcs12 Fichero PKCS#12 que contiene el certificado SSL cliente que pedir&aacute; la TSA al
	 *                  establecer la coneci&oacute;s HTTPS.
	 * @param sslPkcs12Password Contrase&ntilde;a del fichero PKCS#12 que contiene el certificado SSL
	 *                          cliente para las conexiones HTTPS. */
	public TsaParams(final boolean requireCert,
			         final String policy,
			         final URI url,
			         final String usr,
			         final String pwd,
			         final TsaRequestExtension[] exts,
			         final String hashAlgorithm,
			         final byte[] sslPkcs12,
			         final String sslPkcs12Password) {
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
        this.sslPkcs12File = sslPkcs12 != null ? sslPkcs12.clone() : null;
        this.sslPkcs12FilePassword = sslPkcs12Password;
        this.tsaRequireCert = requireCert;

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

        // PKCS#12 / PFX para el SSL cliente
        final String p12FileName = extraParams.getProperty("tsaSslPkcs12File"); //$NON-NLS-1$
        if (p12FileName != null) {
        	final File p12File = new File(p12FileName);
        	if (!p12File.exists()) {
        		throw new IllegalArgumentException("El fichero PKCS#12 para el SSL de la TSA no existe: " + p12File); //$NON-NLS-1$
        	}
			try {
				final InputStream is = new FileInputStream(p12File);
				this.sslPkcs12File = AOUtil.getDataFromInputStream(is);
				is.close();
			}
			catch(final Exception e) {
				throw new IllegalArgumentException(
					"El fichero PKCS#12 (" + p12File + ") para el SSL de la TSA no ha podido leerse: " + e, e  //$NON-NLS-1$//$NON-NLS-2$
				);
			}
        }
        else {
        	this.sslPkcs12File = null;
        }
        this.sslPkcs12FilePassword = extraParams.getProperty("tsaSslPkcs12FilePassword", ""); //$NON-NLS-1$ //$NON-NLS-2$

        try {
	        this.extensions = extraParams.getProperty("tsaExtensionOid") != null && extraParams.getProperty("tsaExtensionValueBase64") != null ? //$NON-NLS-1$ //$NON-NLS-2$
				new TsaRequestExtension[] {
					new TsaRequestExtension(
						extraParams.getProperty("tsaExtensionOid"), //$NON-NLS-1$
						Boolean.getBoolean(extraParams.getProperty("tsaExtensionCritical", "false")), //$NON-NLS-1$ //$NON-NLS-2$
						Base64.decode(extraParams.getProperty("tsaExtensionValueBase64")) //$NON-NLS-1$
					)
				} : null;
        }
        catch(final IOException e) {
        	throw new IllegalArgumentException("Las extensiones del sello de tiempo no estan adecuadamente codificadas: " + e, e); //$NON-NLS-1$
        }
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
	public byte[] getSslPkcs12File() {
		return this.sslPkcs12File;
	}

	/** Obtiene la contrase&ntilde;a del fichero PKCS#12 que contiene el certificado SSL cliente para las conexiones HTTPS.
	 * @return Contrase&ntilde;a del fichero PKCS#12 que contiene el certificado SSL cliente para las conexiones HTTPS o
	 *         cadena vac&iacute;a si no se ha establecido ninguna. */
	public String getSslPkcs12FilePassword() {
		return this.sslPkcs12FilePassword;
	}
}

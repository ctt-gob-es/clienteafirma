package es.gob.afirma.crypto.handwritten;

import java.net.URI;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;

import es.gob.afirma.signers.tsp.pkcs7.TsaParams;
import es.gob.afirma.signers.tsp.pkcs7.TsaRequestExtension;

/** Par&aacute;metros de configuraci&oacute;n de una Autoridad de Sellado de Tiempo.
 * Es un envoltorio de <code>es.gob.afirma.signers.tsp.pkcs7.TsaParams</code> con soporte de JAXB,
 * que no lo soporta por compatibilidad con Android.
 * @author Tomas Garc&iacute;a-Mer&aacute;s. */
final class SerializableTsaParams {

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
	private final SerializableTsaRequestExtension[] extensions;

	@XmlElement(name = "tsaHashAlgorithm")
	private final String tsaHashAlgorithm;

	@XmlElement(name = "sslPkcs12File")
	private final byte[] sslPkcs12File;

	@XmlElement(name = "sslPkcs12FilePassword")
	private final String sslPkcs12FilePassword;

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
	public SerializableTsaParams(final boolean requireCert,
			         final String policy,
			         final URI url,
			         final String usr,
			         final String pwd,
			         final SerializableTsaRequestExtension[] exts,
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

	public TsaParams getTsaParams() {
		return new TsaParams(
			this.tsaRequireCert,
			this.tsaPolicy,
			this.tsaURL,
			this.tsaUsr,
			this.tsaPwd,
			convertExtensions(this.extensions),
			this.tsaHashAlgorithm,
			this.sslPkcs12File,
			this.sslPkcs12FilePassword,
			"PKCS12", //$NON-NLS-1$
			null,
			null,
			null,
			false
		);
	}

	private static TsaRequestExtension[] convertExtensions(final SerializableTsaRequestExtension[] serializableExts) {
		if (serializableExts == null) {
			return null;
		}
		final TsaRequestExtension[] ret = new TsaRequestExtension[serializableExts.length];
		for (int i=0;i<serializableExts.length;i++) {
			ret[i] = serializableExts[i].getTsaRequestExtension();
		}
		return ret;
	}

	/** Constructor de uso restringido a la serializaci&oacute;n JAXB. */
	@SuppressWarnings("unused")
	private SerializableTsaParams() {
		// Para la serializacion JAXB
        this.tsaURL = null;
        this.tsaPolicy = POLICY;
        this.tsaUsr = null;
        this.tsaPwd = null;
        this.extensions = null;
        this.tsaHashAlgorithm = DEFAULT_DIGEST_ALGO;
        this.sslPkcs12File = null;
        this.sslPkcs12FilePassword = null;
        this.tsaRequireCert = true;
	}

	@Override
	public String toString() {
		return getTsaParams().toString();
	}

	/** Extensi&oacute;n para una solicitud de TSA seg&uacute;n RFC 2161. */
	static final class SerializableTsaRequestExtension {

		@XmlElement(name = "oid")
		private final String oid;

		@XmlElement(name = "critical")
		private final boolean critical;

		@XmlElement(name = "value")
		private final byte[] value;

		/** Constructor de uso restringido a la serializaci&oacute;n JAXB. */
		@SuppressWarnings("unused")
		private SerializableTsaRequestExtension() {
			this.oid = null;
			this.critical = false;
			this.value = null;
		}

		/** Crea una extensi&oacute;n para una solicitud de TSA seg&uacute;n RFC 2161.
		 * @param oid OID de la extensi&oacute;n
		 * @param isCritical <code>true</code> si la extensi&oacute;n es cr&iacute;tica, <code>false</code> en caso contrario
		 * @param value Valor de la extensi&oacute;n */
		public SerializableTsaRequestExtension(final String oid, final boolean isCritical, final byte[] value) {
			if (oid == null || "".equals(oid)) { //$NON-NLS-1$
				throw new IllegalArgumentException("Las extensiones TSA necesitan obligatoriamente un OID"); //$NON-NLS-1$
			}
			if (value == null || value.length < 1) {
				throw new IllegalArgumentException("Las extensiones TSA necesitan obligatoriamente un valor"); //$NON-NLS-1$
			}
			this.oid = oid;
			this.critical = isCritical;
			this.value = value.clone();
		}

		TsaRequestExtension getTsaRequestExtension() {
			return new TsaRequestExtension(this.oid, this.critical, this.value);
		}

		@Override
		public String toString() {
			return getTsaRequestExtension().toString();
		}
	}

}

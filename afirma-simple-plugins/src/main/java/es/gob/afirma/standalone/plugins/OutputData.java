package es.gob.afirma.standalone.plugins;

import java.io.File;
import java.security.cert.X509Certificate;

/**
 * Datos resultantes del proceso de firma.
 */
public class OutputData {

	private boolean success;

	private File dataFile;

	private String signatureFormat;

	private X509Certificate[] certs;

	/**
	 * Indica si la operaci&oacute;n finaliz&oacute; correctamente.
	 * @return {@code true} si finaliz&oacute; correctamente, {@code false}
	 * en caso contrario.
	 */
	public boolean isSuccess() {
		return this.success;
	}

	/**
	 * Establece si la operaci&oacute;n finaliz&oacute; correctamente.
	 * @param success {@code true} si finaliz&oacute; correctamente, {@code false}
	 * en caso contrario.
	 */
	public void setSuccess(final boolean success) {
		this.success = success;
	}

	/**
	 * Recupera el fichero de datos generado.
	 * @return Fichero de datos o {@code null} si la operaci&oacute;n finaliz&oacute;
	 * con errores.
	 */
	public File getDataFile() {
		return this.dataFile;
	}

	/**
	 * Establece el fichero de datos generado.
	 * @param dataFile Fichero de datos.
	 */
	public void setDataFile(final File dataFile) {
		this.dataFile = dataFile;
	}

	/**
	 * Recupera el nombre del formato de firma utilizado.
	 * @return Formato de firma.
	 */
	public String getSignatureFormat() {
		return this.signatureFormat;
	}

	/**
	 * Establece el nombre del formato de firma utilizado.
	 * @param signatureFormat Formato de firma.
	 */
	public void setSignatureFormat(final String signatureFormat) {
		this.signatureFormat = signatureFormat;
	}

	/**
	 * Recupera un listado con los certificados encontrados dentro de la firma.
	 * @return Certificados de firma.
	 */
	public X509Certificate[] getCerts() {
		return this.certs != null ? this.certs.clone() : null;
	}

	/**
	 * Establece un listado con los certificados encontrados dentro de la firma.
	 * @param certs Certificados de firma.
	 */
	public void setCerts(final X509Certificate[] certs) {
		this.certs = certs != null ? certs.clone() : null;
	}
}

package es.gob.afirma.standalone.plugins;

import java.io.File;

/**
 * Datos seleccionados como entrada de la aplicaci&oacute;n.
 */
public class InputData {

	/** Fichero de datos de entrada. */
	private File dataFile;

	/** Formato de firma con el que se va a firmar. */
	private String signatureFormat;

	/**
	 * Recupera el fichero de datos.
	 * @return Fichero de datos o {@code null} si no se trata de un fichero.
	 */
	public File getDataFile() {
		return this.dataFile;
	}

	/**
	 * Establece el fichero de datos.
	 * @param dataFile Fichero de datos.
	 */
	public void setDataFile(File dataFile) {
		this.dataFile = dataFile;
	}

	/**
	 * Recupera el formato de firma con el que se firmaran los datos.
	 * @return Formato de firma.
	 */
	public String getSignatureFormat() {
		return this.signatureFormat;
	}

	/**
	 * Establece el formato de firma con el que se firmaran los datos.
	 * @param signatureFormat Formato de firma.
	 */
	public void setSignatureFormat(String signatureFormat) {
		this.signatureFormat = signatureFormat;
	}
}

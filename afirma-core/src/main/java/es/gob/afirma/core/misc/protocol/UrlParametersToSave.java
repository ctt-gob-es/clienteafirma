package es.gob.afirma.core.misc.protocol;

import java.net.URL;
import java.util.Arrays;

/** Par&aacute;metros para el guardado de datos. */
public final class UrlParametersToSave {

	private byte[] data = null;
	private String fileId = null;
	private byte[] desKey = null;
	private URL retrieveServletUrl = null;
	private String title = null;
	private String filename = null;
	private String extensions = null;
	private String fileTypeDescription = null;

	/** Establece la descripci&oacute;n del tipo de fichero a guardar.
	 * @param desc Descripci&oacute;n del tipo de fichero a guardar */
	void setFileTypeDescription(final String desc) {
		this.fileTypeDescription = desc;
	}

	/** Establece las extensiones recomendadas para el fichero a guardar.
	 * Deben indicarse como una lista separada por comas
	 * @param exts Extensiones recomendadas, indicadas como una lista separada por comas */
	void setExtensions(final String exts) {
		this.extensions = exts;
	}

	/** Establece el nombre de fichero propuesto para guardar los datos.
	 * @param filename Nombre de fichero propuesto para guardar los datos */
	void setFilename(final String filename) {
		this.filename = filename;
	}

	/** Establece el t&iacute;tulo del di&aacute;logo de guardado de datos.
	 * @param title T&iacute;tulo del di&aacute;logo de guardado de datos */
	void setTitle(final String title) {
		this.title = title;
	}

	/** Establece los datos a guardar.
	 * @param dat Datos a guardar */
	void setData(final byte[] dat) {
		this.data = dat != null ? Arrays.copyOf(dat, dat.length) : null;
	}

	/** Establece el identificador de los datos en el servidor intermedio.
	 * @param fileId Identificador de los datos en el servidor intermedio */
	void setFileId(final String fileId) {
		this.fileId = fileId;
	}

	/** Establece la clave DES de cifrado de los datos a subir al servidor intermedio.
	 * @param key Clave DES de cifrado de los datos a subir al servidor intermedio */
	void setDesKey(final byte[] key) {
		this.desKey = key != null ? Arrays.copyOf(key, key.length) : null;
	}

	/** Establece la URL de subida al servidor intermedio.
	 * @param retrieveServletUrl URL de subida al servidor intermedio */
	void setRetrieveServletUrl(final URL retrieveServletUrl) {
		this.retrieveServletUrl = retrieveServletUrl;
	}

	/** Obtiene la descripci&oacute;n del tipo de fichero a guardar.
	 * @return Descripci&oacute;n del tipo de fichero a guardar */
	public String getFileTypeDescription() {
		return this.fileTypeDescription;
	}

	/** Obtiene, como una lista separada por comas, las extensiones recomendadas para el
	 * fichero de salida.
	 * @return Lista separada por comas con las extensiones para el fichero de salida */
	public String getExtensions() {
		return this.extensions;
	}

	/** Obtiene el nombre de fichero propuesto para guardar los datos.
	 * @return Nombre de fichero propuesto para guardar los datos */
	public String getFileName() {
		return this.filename;
	}

	/** Obtiene el t&iacute;tulo del di&aacute;logo de guardado de datos.
	 * @return T&iacute;tulo del di&aacute;logo de guardado de datos */
	public String getTitle() {
		return this.title;
	}

	/** Obtiene los datos a guardar
	 * @return Datos a guardar */
	public byte[] getData() {
		return this.data;
	}

	/** Obtiene el identificador de los datos en el servidor intermedio.
	 * @return Identificador de los datos en el servidor intermedio */
	public String getFileId() {
		return this.fileId;
	}

	/** Obtiene la clave DES de cifrado de los datos a subir al servidor intermedio.
	 * @return Clave DES de cifrado de los datos a subir al servidor intermedio */
	public byte[] getDesKey() {
		return this.desKey;
	}

	/** Obtiene la URL de subida al servidor intermedio.
	 * @return URL de subida al servidor intermedio */
	public URL getRetrieveServletUrl() {
		return this.retrieveServletUrl;
	}

}

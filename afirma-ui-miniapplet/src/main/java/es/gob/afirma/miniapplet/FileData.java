package es.gob.afirma.miniapplet;

/**
 * Datos contenidos en un fichero.
 */
public class FileData {

	private final byte[] data;

	private String filename;

	/**
	 * Construye los datos del fichero con su contenido.
	 * @param data Datos contenidos en el fichero.
	 */
	public FileData(final byte[] data) {
		this.data = data;
		this.filename = null;
	}

	/**
	 * Recupera los datos del fichero.
	 * @return Contenido del fichero.
	 */
	public byte[] getData() {
		return this.data;
	}

	/**
	 * Recupera el nombre de fichero.
	 * @return Nombre del fichero.
	 */
	public String getFilename() {
		return this.filename;
	}

	/**
	 * Establece el nombre del fichero.
	 * @param filename Nombre del fichero.
	 */
	public void setFilename(final String filename) {
		this.filename = filename;
	}
}

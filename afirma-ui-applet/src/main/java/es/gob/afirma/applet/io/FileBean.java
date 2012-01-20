package es.gob.afirma.applet.io;

/**
 * Clase que almacena el nombre y contenido de un fichero.
 */
public class FileBean {

	private final String path;

	private final byte[] content;

	/**
	 * Crea un objeto con ruta de fichero y contenido.
	 * @param path Ruta al fichero.
	 * @param content Contenido del fichero.
	 */
	public FileBean(final String path, final byte[] content) {
		this.path = path;
		this.content = content;
	}

	/**
	 * Recupera la ruta indicada del fichero.
	 * @return Ruta del fichero.
	 */
	public String getPath() {
		return this.path;
	}

	/**
	 * Recupera el contenido del fichero.
	 * @return Contenido del fichero.
	 */
	public byte[] getContent() {
		return this.content;
	}
}

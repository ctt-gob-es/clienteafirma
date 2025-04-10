package es.gob.afirma.triphase.signer.xades;

/**
 * Conjunto de las cadenas de apertura y cierre que delimitan un texto.
 */
public class NodeDelimiter {

	/** Nombre del nodo. */
	private String nodeName;

	/** Cadena de apertura. */
	private String openTag;

	/** Cadena de cierre. */
	private String closeTag;

	/**
	 * Construye el delimitador con las etiquetas de apertura y cierre.
	 * @param nodeName Nombre del nodo.
	 * @param openTag Etiqueta de apertura.
	 * @param closeTag Etiqueta de cierre.
	 */
	public NodeDelimiter(final String nodeName, final String openTag, final String closeTag) {
		this.nodeName = nodeName;
		this.openTag = openTag;
		this.closeTag = closeTag;
	}

	/**
	 * Establece el nombre del nodo.
	 * @param nodeName Nombre del nodo.
	 */
	public void setNodeName(final String nodeName) {
		this.nodeName = nodeName;
	}

	/**
	 * Recupera el nombre del nodo.
	 * @return Nombre del nodo.
	 */
	public String getNodeName() {
		return this.nodeName;
	}

	/**
	 * Establece la etiqueta de apertura.
	 * @param openTag Etiqueta de apertura.
	 */
	public void setOpenTag(final String openTag) {
		this.openTag = openTag;
	}

	/**
	 * Recupera la etiqueta de apertura.
	 * @return Etiqueta de apertura.
	 */
	public String getOpenTag() {
		return this.openTag;
	}

	/**
	 * Establece la etiqueta de cierre.
	 * @param closeTag Etiqueta de cierre.
	 */
	public void setCloseTag(final String closeTag) {
		this.closeTag = closeTag;
	}

	/**
	 * Recupera la etiqueta de cierre.
	 * @return Etiqueta de cierre.
	 */
	public String getCloseTag() {
		return this.closeTag;
	}
}

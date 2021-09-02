package es.gob.afirma.triphase.signer.xades;

/**
 * Conjunto de las cadenas de apertura y cierre que delimitan un texto.
 */
public class NodeDelimiter {

	/** Cadena de apertura. */
	private String openTag;

	/** Cadena de cierre. */
	private String closeTag;

	/**
	 * Construye el delimitador con las etiquetas de apertura y cierre.
	 * @param openTag Etiqueta de apertura.
	 * @param closeTag Etiqueta de cierre.
	 */
	public NodeDelimiter(final String openTag, final String closeTag) {
		this.openTag = openTag;
		this.closeTag = closeTag;
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

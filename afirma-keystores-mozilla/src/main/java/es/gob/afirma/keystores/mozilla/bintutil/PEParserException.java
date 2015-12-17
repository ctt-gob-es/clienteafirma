package es.gob.afirma.keystores.mozilla.bintutil;

/** Error en el an&aacute;lisis de un fichero PE de Microsoft.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class PEParserException extends Exception {

	private static final long serialVersionUID = -1893004047399996424L;

	PEParserException(final String msg) {
		super(msg);
	}

}

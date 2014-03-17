package es.gob.afirma.signers.xml.style;

/** Excepci&oacute;n relativa a los errores de firma de hojas de estilo XML. */
public abstract class StyleException extends Exception {

	private static final long serialVersionUID = 1922584585798106575L;

	StyleException(final String msg) {
		super(msg);
	}

	StyleException(final String msg, final Throwable e) {
		super(msg, e);
	}

	StyleException(final Throwable e) {
		super(e);
	}
}

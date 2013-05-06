package es.gob.afirma.signers.xadestri.server;

/** Error en una prefirma XML. Habitualmente causado por la imposibilidad de hacer la sustituci&oacute;n del PKCS#1.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class XmlPreSignException extends Exception {
	private static final long serialVersionUID = 3950771737016341242L;

	XmlPreSignException(final String msg) {
		super(msg);
	}

	XmlPreSignException() {
		super();
	}
}

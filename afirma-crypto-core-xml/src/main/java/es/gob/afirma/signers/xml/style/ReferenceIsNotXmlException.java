package es.gob.afirma.signers.xml.style;

/** La referencia de hoja de estilo apunta a un no-XML. */
public final class ReferenceIsNotXmlException extends StyleException {

    private static final long serialVersionUID = 8076672806350530425L;

    ReferenceIsNotXmlException(final Throwable e) {
        super(e);
    }
}

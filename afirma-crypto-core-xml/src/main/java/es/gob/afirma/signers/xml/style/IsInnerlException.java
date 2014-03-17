package es.gob.afirma.signers.xml.style;

/** Hoja de estilo local (rutal local no dereferenciable) a un XML */
public final class IsInnerlException extends StyleException {

    private static final long serialVersionUID = -8769490831203570286L;

    /** Construye la excepci&oacute;n que indica que una referencia apunta al interior del mismo XML.
     * @param e Excepci&oacute;n anterior en la cadena */
    public IsInnerlException(final Throwable e) {
        super(e);
    }

}

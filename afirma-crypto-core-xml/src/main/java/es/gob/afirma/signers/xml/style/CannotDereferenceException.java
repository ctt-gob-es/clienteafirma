package es.gob.afirma.signers.xml.style;

/** No se puede dereferenciar la hoja de estilo. */
public final class CannotDereferenceException extends StyleException {

    private static final long serialVersionUID = 5883820163272098664L;

    /** Construye una excepci&oacute;n que indica la imposibilidad de
     * dereferenciar una hoja de estilo.
     * @param s
     *        Mesaje de excepci&oacute;n */
    CannotDereferenceException(final String s) {
        super(s);
    }

    /** Construye una excepci&oacute;n que indica la imposibilidad de
     * dereferenciar una hoja de estilo.
     * @param s
     *        Mesaje de excepci&oacute;n
     * @param e
     *        Excepci&oacute;n anterior en la cadena */
    CannotDereferenceException(final String s, final Exception e) {
        super(s, e);
    }
}

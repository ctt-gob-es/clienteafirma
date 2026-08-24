package es.gob.afirma.standalone.ui.restoreconfig;

/**
 * Excepci&oacute;n que indica que el fichero de perfil está protegido por contrase&ntilde;a.
 */
public class PasswordProtectedException extends Exception {

    /**
     * Serial version UID
     */
    private static final long serialVersionUID = 1L;

    /**
     * Crea una nueva excepci&oacute;n.
     * @param message Mensaje de la excepci&oacute;n.
     */
    public PasswordProtectedException(String message) {
        super(message);
    }

    /**
     * Crea una nueva excepci&oacute;n.
     * @param message Mensaje de la excepci&oacute;n.
     * @param cause Causa de la excepci&oacute;n.
     */
    public PasswordProtectedException(String message, Throwable cause) {
        super(message, cause);
    }
}

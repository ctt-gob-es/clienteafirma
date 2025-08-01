package es.gob.afirma.core;

public interface AOControlledException {

    /**
     * Recupera el c&oacute;digo de error asociado a la excepci&oacute;n,
     * @return C&oacute;digo de error o {@code null} si no se defini&oacute;n.
     */
    ErrorCode getErrorCode();
}

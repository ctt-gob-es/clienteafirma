/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.applet;

import java.security.PrivilegedAction;

/** Clase privilegiada b&aacute;sica. Implementa las funciones necesarias para
 * conocer si la operaci&oacute;n produjo alg&uacute;n error durante su
 * ejecuci&oacute;n.
 * @param <T>
 *        Tipo de retorno de la acci&oacute;n
 * @param <U>
 *        Tipo del resultado de la acci&oacute;n */
public abstract class BasicPrivilegedAction<T, U> implements PrivilegedAction<T> {

    /** Resultado de la operaci&oacute;n. */
    private U result = null;

    /** Mensaje de error almacenado. */
    private String errorMsg = null;

    /** Excepci&oacute;n que produjo el problema. */
    private Exception exception = null;

    /** Establece el resultado de la operaci&oacute;n.
     * @param result
     *        Resultado de la operaci&oacute;n. */
    protected void setResult(final U result) {
        this.result = result;
    }

    /** Recupera el resultado de la operaci&oacute;n. Si se produjo alg&uacute;n
     * error o si la operaci&oacute;n no produjo ning&uacute;n error, devuelve {@code null}.
     * @return Resultado de la operaci&oacute;n. */
    public U getResult() {
        return this.result;
    }

    /** Indica si ocurri&oacute; alg&uacute;n error.
     * @return <code>true</code> si ocurri&oacute; alg&uacute;n error, <code>false</code> en caso contrario */
    public boolean isError() {
        return this.errorMsg != null;
    }

    /** Establece el mensaje del error ocurrido. Si el mensaje es {@code null} se
     * entender&aacute;a que no ocurri&oacute; ning&uacute;n error.
     * @param errorMsg
     *        Mensaje de error. */
    protected void setError(final String errorMsg) {
        this.errorMsg = errorMsg;
    }

    /** Establece el mensaje del error ocurrido y la excepci&oacute;n que lo
     * caus&oacute;. Si el mensaje es {@code null} se entender&aacute;a que no
     * ocurri&oacute; ning&uacute;n error.
     * @param errorMsg
     *        Mensaje de error.
     * @param e
     *        Excepci&oacute;n que caus&oacute; el error. */
    protected void setError(final String errorMsg, final Exception e) {
        this.errorMsg = errorMsg;
        this.exception = e;
    }

    /** Recupera el mensaje del error ocurrido.
     * @return Mensaje de error. */
    public String getErrorMessage() {
        return this.errorMsg;
    }

    /** Recupera la excepci&oacute;n que produjo el error. Si no se
     * estableci&oacute; se devolver&aacute; {@code null}
     * @return Excepci&oacute;n que produjo el error. */
    public Exception getException() {
        return this.exception;
    }
}

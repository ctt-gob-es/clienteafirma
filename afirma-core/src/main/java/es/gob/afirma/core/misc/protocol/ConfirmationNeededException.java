package es.gob.afirma.core.misc.protocol;

import java.util.Properties;

/**
 * Excepci&oacute;n que indica cuando no se puede completar un proceso de firma porque se requiere
 * confirmaci&oacute;n por parte del usuario.
 */
public abstract class ConfirmationNeededException extends Exception {

	/** Serial Id. */
	private static final long serialVersionUID = 7016348225696172844L;

	/** Crea la excepci&oacute;n con un mensaje determinado.
     * @param msg
     *        Mensaje descriptivo de la excepci&oacute;n. */
    public ConfirmationNeededException(final String msg) {
        super(ProtocoloMessages.getString(msg));
    }

    /** Crea la excepci&oacute;n con un mensaje determinado.
     * @param msg
     *        Mensaje descriptivo de la excepci&oacute;n.
     * @param e
     *        Excepci&oacute;n que ha causado el lanzamiento de esta. */
    public ConfirmationNeededException(final String msg, final Throwable e) {
        super(ProtocoloMessages.getString(msg), e);
    }

    /**
     * Indica que par&aacute;metros se deben establecer cuando se pulse en el bot&oacute;n
     * "S&iacute;"
     * @return
     */
    public abstract Properties getYesParamsOptions();

}

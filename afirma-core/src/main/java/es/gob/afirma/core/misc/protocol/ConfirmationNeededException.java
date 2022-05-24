package es.gob.afirma.core.misc.protocol;

import java.util.HashMap;
import java.util.Map;

public class ConfirmationNeededException extends Exception {

	private static final long serialVersionUID = 6018641473192074251L;

	public static final Map<String, String> PARAMS_YES_OPTION;

	static {
		PARAMS_YES_OPTION = new HashMap<String, String>();
		PARAMS_YES_OPTION.put("allowShadowAttack", "true");  //$NON-NLS-1$//$NON-NLS-2$
	}

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
    public ConfirmationNeededException(final String msg, final Exception e) {
        super(ProtocoloMessages.getString(msg), e);
    }



}

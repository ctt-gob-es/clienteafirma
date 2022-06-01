package es.gob.afirma.signvalidation;

import java.util.Properties;

import es.gob.afirma.core.misc.protocol.ConfirmationNeededException;
import es.gob.afirma.signers.pades.PdfExtraParams;

/**
 * Se&ntilde;ala que un documento PDF es susceptible de haber sufrido un
 * PDF Shadow Attack tras una firma anterior.
 */
public class SuspectedPSAException extends ConfirmationNeededException {


	/** Serial Id. */
	private static final long serialVersionUID = 3307433810679035701L;

	private static final Properties PARAMS_YES_OPTION;
	static {
		PARAMS_YES_OPTION = new Properties();
		PARAMS_YES_OPTION.setProperty(PdfExtraParams.ALLOW_SHADOW_ATTACK, Boolean.TRUE.toString());
	}

	public SuspectedPSAException(final String msg) {
		super(msg);
	}

	public SuspectedPSAException(final String msg, final Throwable cause) {
		super(msg, cause);
	}

	@Override
	public Properties getYesParamsOptions() {
		final Properties copy = new Properties();
		for (final String k : PARAMS_YES_OPTION.keySet().toArray(new String[0])) {
			copy.setProperty(k, PARAMS_YES_OPTION.getProperty(k));
		}
		return copy;
	}
}

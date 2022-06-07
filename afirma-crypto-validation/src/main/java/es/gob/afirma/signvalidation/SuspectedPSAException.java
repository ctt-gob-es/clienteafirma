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

	public SuspectedPSAException(final String msg) {
		super(msg);
	}

	public SuspectedPSAException(final String msg, final Throwable cause) {
		super(msg, cause);
	}

	@Override
	public Properties getYesParamsOptions() {
		final Properties options = new Properties();
		options.setProperty(PdfExtraParams.ALLOW_SHADOW_ATTACK, Boolean.TRUE.toString());
		return options;
	}

	@Override
	public Properties getDefaultParamsOptions() {
		final Properties options = new Properties();
		options.setProperty(PdfExtraParams.ALLOW_SHADOW_ATTACK, Boolean.FALSE.toString());
		return options;
	}
}

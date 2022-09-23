package es.gob.afirma.signers.pades.common;

import es.gob.afirma.core.RuntimeConfigNeededException;

/**
 * Se&ntilde;ala que un documento PDF es susceptible de haber sufrido un
 * PDF Shadow Attack tras una firma anterior.
 */
public class SuspectedPSAException extends RuntimeConfigNeededException {

	/** Serial Id. */
	private static final long serialVersionUID = 3307433810679035701L;

	public static final String REQUESTOR_MSG_CODE = "pdfShadowAttackSuspect"; //$NON-NLS-1$

	public SuspectedPSAException(final String msg) {
		super(msg, RequestType.CONFIRM, REQUESTOR_MSG_CODE, PdfExtraParams.ALLOW_SHADOW_ATTACK);
	}
}

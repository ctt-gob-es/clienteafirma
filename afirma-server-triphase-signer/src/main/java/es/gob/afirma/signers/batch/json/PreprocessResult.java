package es.gob.afirma.signers.batch.json;

import es.gob.afirma.core.signers.TriphaseData;

/**
 * Resultado de la prefirma de una firma individual, que puede ser la prefirma generada
 * o el error resultante.
 */
public class PreprocessResult {

	private final ResultSingleSign signResult;

	private final TriphaseData presign;

	public PreprocessResult(final TriphaseData presign) {
		this.signResult = null;
		this.presign = presign;
	}

	public PreprocessResult(final ResultSingleSign result) {
		this.signResult = result;
		this.presign = null;
	}

	public TriphaseData getPresign() {
		return this.presign;
	}

	public ResultSingleSign getSignResult() {
		return this.signResult;
	}
}

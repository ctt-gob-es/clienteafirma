package es.gob.afirma.signers.batch.json;

import es.gob.afirma.signers.batch.ProcessResult;

public class ResultSingleSign {

	private final String id;

	private final boolean correct;

	private final ProcessResult result;

	public ResultSingleSign(final String id, final boolean correct, final ProcessResult result) {
		this.id = id;
		this.correct = correct;
		this.result = result;
	}

	public String getId() {
		return this.id;
	}

	public boolean isCorrect() {
		return this.correct;
	}

	public ProcessResult getResult() {
		return this.result;
	}
}

package es.gob.afirma.signers.batch.client;

import java.util.List;

import es.gob.afirma.core.signers.TriphaseData;

public class PresignBatch {

	private TriphaseData triphaseData;

	private List<BatchDataResult> errors;

	public PresignBatch(final TriphaseData triphaseData, final List<BatchDataResult> errors) {
		this.triphaseData = triphaseData;
		this.errors = errors;
	}

	public TriphaseData getTriphaseData() {
		return this.triphaseData;
	}

	public List<BatchDataResult> getErrors() {
		return this.errors;
	}

	public void setTriphaseData(final TriphaseData triphaseData) {
		this.triphaseData = triphaseData;
	}

	public void setErrors(final List<BatchDataResult> errors) {
		this.errors = errors;
	}
}

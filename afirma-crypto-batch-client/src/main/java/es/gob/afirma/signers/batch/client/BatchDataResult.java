package es.gob.afirma.signers.batch.client;

public class BatchDataResult {

	public enum Result {
		NOT_STARTED,
		DONE_AND_SAVED,
		DONE_BUT_NOT_SAVED_YET,
		DONE_BUT_SAVED_SKIPPED,
		DONE_BUT_ERROR_SAVING,
		ERROR_PRE,
		ERROR_POST,
		SKIPPED,
		SAVE_ROLLBACKED;
	}

	private final Result result;
	private final String description;
	private final String signId;

	public boolean wasSaved() {
		return Result.DONE_AND_SAVED.equals(this.result);
	}

	public BatchDataResult(final String signId, final Result r, final String d) {
		if (signId == null) {
			throw new IllegalArgumentException("El identificador no puede ser nulo"); //$NON-NLS-1$
		}
		if (r == null) {
			throw new IllegalArgumentException("El resultado no puede ser nulo"); //$NON-NLS-1$
		}
		this.signId = signId;
		this.result = r;
		this.description = d;
	}

	public String getId() {
		return this.signId;
	}

	public Result getResult() {
		return this.result;
	}

	public String getDescription() {
		return this.description;
	}
}

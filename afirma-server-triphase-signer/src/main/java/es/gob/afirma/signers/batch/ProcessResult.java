package es.gob.afirma.signers.batch;

public class ProcessResult {

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
	private String signId;
	private final boolean finished;

	public boolean wasSaved() {
		return Result.DONE_AND_SAVED.equals(this.result);
	}

	public static final ProcessResult PROCESS_RESULT_OK_UNSAVED = new ProcessResult(Result.DONE_BUT_NOT_SAVED_YET, null, false);
	public static final ProcessResult PROCESS_RESULT_SKIPPED    = new ProcessResult(Result.SKIPPED,                null, true);
	public static final ProcessResult PROCESS_RESULT_DONE_SAVED = new ProcessResult(Result.DONE_AND_SAVED,         null, true);
	public static final ProcessResult PROCESS_RESULT_ROLLBACKED = new ProcessResult(Result.SAVE_ROLLBACKED,        null, true);

	public ProcessResult(final Result r, final String d) {
		if (r == null) {
			throw new IllegalArgumentException(
					"El resultado no puede ser nulo" //$NON-NLS-1$
					);
		}
		this.result = r;
		this.description = d;
		this.finished = true;
	}

	public ProcessResult(final Result r, final String d, final boolean finished) {
		if (r == null) {
			throw new IllegalArgumentException(
					"El resultado no puede ser nulo" //$NON-NLS-1$
					);
		}
		this.result = r;
		this.description = d;
		this.finished = finished;
	}

	public String getId() {
		return this.signId;
	}

	public void setId(final String id) {
		this.signId = id;
	}

	public Result getResult() {
		return this.result;
	}

	public String getDescription() {
		return this.description;
	}

	public boolean isFinished() {
		return this.finished;
	}
}

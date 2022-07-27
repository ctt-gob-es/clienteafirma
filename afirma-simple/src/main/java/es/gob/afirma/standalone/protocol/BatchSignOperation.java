package es.gob.afirma.standalone.protocol;

import java.util.List;

/**
 * Configuraci&oacute;n de una operaci&oacute;n de firma de lote.
 */
public class BatchSignOperation {

	private final boolean stopOnError;
	private final List<SingleSignOperation> signs;

	public BatchSignOperation(final boolean stopOnError, final List<SingleSignOperation> signs) {
		this.stopOnError = stopOnError;
		this.signs = signs;
	}

	public boolean isStopOnError() {
		return this.stopOnError;
	}

	public List<SingleSignOperation> getSigns() {
		return this.signs;
	}
}

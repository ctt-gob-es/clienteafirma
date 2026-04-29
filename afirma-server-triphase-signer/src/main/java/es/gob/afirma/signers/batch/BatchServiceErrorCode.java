package es.gob.afirma.signers.batch;

import es.gob.afirma.core.ErrorCode;

public class BatchServiceErrorCode {

	public static class Internal {

		public static final ErrorCode INTERNAL_JSON_BATCH_ERROR				= new ErrorCode("200410", "Error interno al procesar un lote de firmas JSON"); //$NON-NLS-1$ //$NON-NLS-2$

	}
}
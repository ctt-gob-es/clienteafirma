package es.gob.afirma.signers.batch;

import es.gob.afirma.core.ErrorCode;

public class BatchServiceErrorCode {

	public static class Internal {

		public static final ErrorCode INTERNAL_JSON_BATCH_ERROR				= new ErrorCode("200410", "Errro interno al procesar un lote de firmas JSON"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INTERNAL_XML_BATCH_ERROR				= new ErrorCode("200411", "Errro interno al procesar un lote de firmas XML"); //$NON-NLS-1$ //$NON-NLS-2$

	}
}
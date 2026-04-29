package es.gob.afirma.signers.batch.client;

import es.gob.afirma.core.ErrorCode;

public class BatchErrorCode {


	public static class ThirdParty {

		public static final ErrorCode JSON_BATCH_PRESIGN_ERROR						= new ErrorCode("300601", "El servicio de prefirma de lotes JSON devolvio un error"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode JSON_BATCH_POSTSIGN_ERROR						= new ErrorCode("300602", "El servicio de postfirma de lotes JSON devolvio un error"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode XML_BATCH_PRESIGN_ERROR						= new ErrorCode("300701", "El servicio de prefirma de lotes XML devolvio un error"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode XML_BATCH_POSTSIGN_ERROR						= new ErrorCode("300702", "El servicio de postfirma de lotes XML devolvio un error"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	public static class Communication {

		public static final ErrorCode JSON_BATCH_PRESIGN_CONNECTION_ERROR			= new ErrorCode("401500", "Error de conexion con el servidor trifasico al hacer la prefirma Batch JSON"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode JSON_BATCH_PRESIGN_COMMUNICATION_ERROR		= new ErrorCode("401501", "Error de comunicacion con el servidor trifasico al hacer la prefirma Batch JSON"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode JSON_BATCH_PRESIGN_TIMEOUT					= new ErrorCode("401502", "Se excedio el tiempo maximo de espera en la llamada a la prefirma de lote JSON del servidor trifasico"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode JSON_BATCH_POSTSIGN_CONNECTION_ERROR			= new ErrorCode("401600", "Error de conexion con el servidor trifasico al hacer la posfirma Batch JSON"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode JSON_BATCH_POSTSIGN_COMMUNICATION_ERROR		= new ErrorCode("401601", "Error de comunicacion con el servidor trifasico al hacer la posfirma Batch JSON"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode JSON_BATCH_POSTSIGN_TIMEOUT					= new ErrorCode("401602", "Se excedio el tiempo maximo de espera en la llamada a la postfirma de lote JSON del servidor trifasico"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode XML_BATCH_PRESIGN_CONNECTION_ERROR			= new ErrorCode("401700", "Error de conexion con el servidor trifasico al hacer la prefirma Batch XML"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode XML_BATCH_PRESIGN_COMMUNICATION_ERROR			= new ErrorCode("401701", "Error de comunicacion con el servidor trifasico al hacer la prefirma Batch XML"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode XML_BATCH_PRESIGN_TIMEOUT						= new ErrorCode("401702", "Se excedio el tiempo maximo de espera en la llamada a la prefirma de lote XML del servidor trifasico"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode XML_BATCH_POSTSIGN_CONNECTION_ERROR			= new ErrorCode("401800", "Error de conexion con el servidor trifasico al hacer la posfirma Batch XML"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode XML_BATCH_POSTSIGN_COMMUNICATION_ERROR		= new ErrorCode("401801", "Error de comunicacion con el servidor trifasico al hacer la posfirma Batch XML"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode XML_BATCH_POSTSIGN_TIMEOUT					= new ErrorCode("401802", "Se excedio el tiempo maximo de espera en la llamada a la postfirma de lote XML del servidor trifasico"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	public static class Request {

		public static final ErrorCode INVALID_PARAMS_TO_PRESIGN_JSON_BATCH			= new ErrorCode("600417", "El servicio de prefirma de lote JSON informo de un error en los parametros enviados"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_PARAMS_TO_POSTSIGN_JSON_BATCH			= new ErrorCode("600418", "El servicio de postfirma de lote JSON informo de un error en los parametros enviados"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode MALFORMED_JSON_BATCH							= new ErrorCode("600419", "El lote no es un JSON valido o esta mal formado"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode INVALID_PARAMS_TO_PRESIGN_XML_BATCH			= new ErrorCode("600501", "El servicio de prefirma de lote XML informo de un error en los parametros enviados"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_PARAMS_TO_POSTSIGN_XML_BATCH			= new ErrorCode("600502", "El servicio de postfirma de lote XML informo de un error en los parametros enviados"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode MALFORMED_XML_BATCH							= new ErrorCode("600503", "El lote no es un XML valido o esta mal formado"); //$NON-NLS-1$ //$NON-NLS-2$

	}

}

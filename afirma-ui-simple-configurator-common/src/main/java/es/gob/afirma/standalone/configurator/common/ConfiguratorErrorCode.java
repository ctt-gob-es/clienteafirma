package es.gob.afirma.standalone.configurator.common;

import es.gob.afirma.core.ErrorCode;

public class ConfiguratorErrorCode {

	public static class Internal {
		public static final ErrorCode IMPORTING_PREFERENCES_ERROR		= new ErrorCode("230300", "Error desconocido importando las preferencias"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode CANT_READ_PREFERENCES_FILE		= new ErrorCode("230301", "No se pudo leer el fichero de preferencias"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_XML_PREFERENCES_FILE		= new ErrorCode("230302", "El fichero de preferencia no es un XML"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_FORMAT_PREFERENCES_FILE	= new ErrorCode("230303", "El fichero de preferencias no tiene un formato valido"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public static class Functional {

		public static final ErrorCode INVALID_CERTIFICATE_SIGNER		= new ErrorCode("522001", "El certificado firmante del fichero de configuracion no es valido"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_PREFERENCES_FILE_SIGNATURE= new ErrorCode("522002", "La firma del fichero de configuracion no es valida"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_PREFERENCES_FILE_SIGNER	= new ErrorCode("522003", "El certificado firmante del fichero de configuracion no esta autorizado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode UNSIGNED_PREFERENCES_FILE			= new ErrorCode("522004", "No se permite la importacion de ficheros de preferencias sin firmar"); //$NON-NLS-1$ //$NON-NLS-2$


	}

}

package es.gob.afirma.standalone;

import es.gob.afirma.core.ErrorCode;

public class SimpleErrorCode {
	
	public static class Internal {
		
		public static final ErrorCode CANT_LOAD_HELP				= new ErrorCode("200004", "No se ha podido cargar correctamente la ayuda"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode NOT_VALID_PLUGIN				= new ErrorCode("200005", "Plugin no valido"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode NEEDS_UPDATED_VERSION				= new ErrorCode("200701", "Se necesita actualizar la aplicacion"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode NO_DETECTED_PROXIES					= new ErrorCode("200702", "No se han detectado proxies en el sisema"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode ERROR_LOAD_TRUSTED_CERT				= new ErrorCode("200800", "Error al cargar el certificado de confianza"); //$NON-NLS-1$ //$NON-NLS-2$	
	}
	
	public static class Communication {
		public static final ErrorCode PROXY_CONNECTION					= new ErrorCode("400002", "Conexion incorrecta con proxy"); //$NON-NLS-1$ //$NON-NLS-2$
	}

}

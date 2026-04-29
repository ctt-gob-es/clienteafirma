package es.gob.afirma.standalone;

import es.gob.afirma.core.ErrorCode;

public class SimpleErrorCode {

	public static class Internal {

		public static final ErrorCode CANT_LOAD_HELP					= new ErrorCode("200004", "No se ha podido cargar correctamente la ayuda"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode NOT_VALID_PLUGIN					= new ErrorCode("200005", "Plugin no valido"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode ENCRIPTING_SIGNATURE				= new ErrorCode("200100", "Error al cifrar la firma para enviarla al servidor intermedio"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode ENCRIPTING_SIGNING_CERT			= new ErrorCode("200101", "Error al cifrar el certificado de firma para enviarlo al servidor intermedio"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode ENCRIPTING_SIGNATURE_EXTRA_DATA	= new ErrorCode("200116", "Error al cifrar la informacion extra de firma para enviarla al servidor intermedio"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode POSTPROCESING_SIGNATURE			= new ErrorCode("200117", "Error en el postproceso de la firma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode UNKNOWN_SIGNING_BY_SOCKETS_ERROR	= new ErrorCode("200118", "Error desconocido durante la operacion de firma usando la comunicacion por sockets"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOAD_FILE_TO_VIEW					= new ErrorCode("200119", "Error al cargar el fichero local para visualizar la firma"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode ENCRIPTING_SELECTED_CERT			= new ErrorCode("200200", "Error al cifrar el certificado seleccionado para enviarlo al servidor intermedio"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode UNKNOWN_SELECTING_CERT_ERROR		= new ErrorCode("200201", "Error desconocido durante la operacion de seleccion de certificado"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode CANT_SAVE_FILE					= new ErrorCode("200301", "No se ha podido guardar el fichero en disco"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode UNKNOWN_SAVING_DATA_ERROR			= new ErrorCode("200305", "Error general durante la operacion de guardado de datos"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode ENCRIPTING_BATCH_RESULT			= new ErrorCode("200400", "Error al cifrar la informacion de las firmas del lote para enviarla al servidor intermedio"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode ENCRIPTING_BATCH_SIGNING_CERT		= new ErrorCode("200401", "Error al cifrar el certificado de firma para enviarlo al servidor intermedio"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INTERNAL_JSON_BATCH_ERROR			= new ErrorCode("200410", "Error interno al procesar un lote de firmas JSON"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INTERNAL_XML_BATCH_ERROR			= new ErrorCode("200411", "Error interno al procesar un lote de firmas XML"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INTERNAL_LOCAL_BATCH_ERROR		= new ErrorCode("200412", "Error interno realizando la firma batch local"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode UNKNOWN_BATCH_ERROR				= new ErrorCode("200413", "Error desconocido al procesar un lote de firmas"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode UNKNOWN_LOADING_DATA_ERROR		= new ErrorCode("200600", "Error general durante la operacion de carga de fichero"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode CANT_LOAD_FILE					= new ErrorCode("200601", "Error al cargar el fichero local"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode NEEDS_UPDATED_VERSION				= new ErrorCode("200701", "Se necesita actualizar la aplicacion"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode NO_DETECTED_PROXIES				= new ErrorCode("200702", "No se han detectado proxies en el sisema"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode ERROR_LOAD_TRUSTED_CERT			= new ErrorCode("200800", "Error al cargar el certificado de confianza"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRUSTSTORE_INCORRECT_INSTALLATION	= new ErrorCode("200801", "El almacen de confianza de Autofirma no se encuentra instalado correctamente"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode SOCKET_INITIALIZING_ERROR			= new ErrorCode("220001", "No se pudo abrir el socket de comunicacion"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode DECRYPTING_PARAMS_ERROR			= new ErrorCode("220002", "Error desencriptando los datos del servidor"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode ERROR_RECIVED_FROM_CLIENT			= new ErrorCode("220003", "El servicio cliente ya notifico un error durante la peticion"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode ENCRYPTING_PARAMS_ERROR			= new ErrorCode("220004", "Error encriptando los datos del servidor"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOADING_SSL_KEYSTORE_ERROR		= new ErrorCode("220008", "No se ha podido cargar el almacen de claves SSL"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode GENERIC_LANGUAGE_IMPORT_ERROR		= new ErrorCode("230100", "Error importando el idioma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode CANT_READ_FILE					= new ErrorCode("230101", "No se ha podido leer correctamente el archivo ZIP"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode CANT_CREATE_DIRECTORY				= new ErrorCode("230102", "Se ha producido un error al crear el directorio del idioma"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public static class Communication {
		public static final ErrorCode PROXY_CONNECTION					= new ErrorCode("400002", "Conexion incorrecta con proxy"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode RECIVING_DATA_OF_SIGN_OPERATION	= new ErrorCode("401100", "Error en la descarga de la operacion de firma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode RECIVING_DATA_OF_CERT_OPERATION	= new ErrorCode("401101", "Error en la descarga de la operacion de seleccion de certificado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode RECIVING_DATA_OF_BATCH_OPERATION	= new ErrorCode("401102", "Error en la descarga de la operacion de lote"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode RECIVING_DATA_OF_SAVE_OPERATION	= new ErrorCode("401103", "Error en la descarga de la operacion de guardado de datos"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode RECIVING_DATA_OF_LOAD_OPERATION	= new ErrorCode("401104", "Error en la descarga de la operacion de carga de datos"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode RECIVING_DATA_OF_SIGN_AND_SAVE_OPERATION	= new ErrorCode("401105", "Error en la descarga de la operacion de firma y guardado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode RECIVING_DATA_OF_SIGN_TIMEOUT	= new ErrorCode("401106", "Se excedio el tiempo maximo del servicio de recuperacion de datos del servidor intermedio con los datos de firma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode RECIVING_DATA_OF_CERT_TIMEOUT	= new ErrorCode("401107", "Se excedio el tiempo maximo del servicio de recuperacion de datos del servidor intermedio con los datos de seleccion de certificados"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode RECIVING_DATA_OF_BATCH_TIMEOUT	= new ErrorCode("401108", "Se excedio el tiempo maximo del servicio de recuperacion de datos del servidor intermedio con los datos de firma de lote"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode RECIVING_DATA_OF_SAVE_TIMEOUT	= new ErrorCode("401109", "Se excedio el tiempo maximo del servicio de recuperacion de datos del servidor intermedio con los datos de guardado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode RECIVING_DATA_OF_LOAD_TIMEOUT	= new ErrorCode("401110", "Se excedio el tiempo maximo del servicio de recuperacion de datos del servidor intermedio con los datos de carga de datos"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode RECIVING_DATA_OF_SIGN_AND_SAVE_TIMEOUT	= new ErrorCode("401111", "Se excedio el tiempo maximo del servicio de recuperacion de datos del servidor intermedio con los datos de firma y guardado"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode SENDING_RESULT_OPERATION			= new ErrorCode("401200", "Error en el envio del resultado de la operacion"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode SENDING_RESULT_TIMEOUT			= new ErrorCode("401201", "Se excedio el tiempo maximo de espera en la llamada al servicio de guardado del servidor intermedio"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode EXTERNAL_REQUEST					= new ErrorCode("420001", "Se ha realizado la peticion desde un puerto externo"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode SENDING_RESULT_OPERATION_BY_SOCKET	= new ErrorCode("420501", "Error en el envio del resultado de la operacion por socket"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode READING_FROM_SOCKET					= new ErrorCode("420502", "No se pudieron leer los datos del socket"); //$NON-NLS-1$ //$NON-NLS-2$


	}

	public static class Functional {

		public static final ErrorCode MINIMUM_VERSION_NON_SATISTIED		= new ErrorCode("500005", "La web solicito una version posterior de Autofirma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode SIGNER_REQUIREMENT_NOT_SUPPORTED	= new ErrorCode("500006", "El firmador hizo una peticion de datos no soportada"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode VALIDATOR_REQUIREMENT_NOT_SUPPORTED	= new ErrorCode("500007", "El validador hizo una peticion de datos no soportada"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode NO_CERTS_FOUND_SIGNING			= new ErrorCode("501001", "No se han encontrado certificados para realizar la seleccion"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode CANT_IDENTIFY_SIGNATURE_FORMAT	= new ErrorCode("501009", "No se ha podido identificar un formato de firma para los datos"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode VISIBLE_SIGNATURE_IS_MANDATORY	= new ErrorCode("501010", "La operacion marco como obligatoria la firma visible PDF"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode NO_CERTS_FOUND_SELECTING_CERT		= new ErrorCode("502001", "No se han encontrado certificados para realizar la seleccion"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode NO_CERTS_FOUND_SIGNING_BATCH		= new ErrorCode("504001", "No se han encontrado certificados para realizar la firma del lote"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode POLICY_DEFAULT_ERROR                  = new ErrorCode("521000", "Ha ocurrido un error configurando la politica"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode INVALID_POLICY_IDENTIFIER             = new ErrorCode("521001", "El identificador de politica no puede ser nulo ni vacio"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode INVALID_POLICY_IDENTIFIER_URI         = new ErrorCode("521002", "El identificador debe ser una URI valida"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode MISSING_DIGEST_ALGORITHM              = new ErrorCode("521003", "Si se indica la huella digital es obligatorio indicar tambien el algoritmo"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode UNREACHABLE_POLICY_IDENTIFIER_URI		= new ErrorCode("521004", "Si no se especifica la huella digital, el identificador debe ser una URI accesible"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode INVALID_POLICY_IDENTIFIER_HASH_BASE64 = new ErrorCode("521005", "La huella digital debe estar en formato Base64"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode INVALID_POLICY_QUALIFIER_URI 			= new ErrorCode("521006", "El calificador debe ser una URI valida"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode INVALID_POLICY_IDENTIFIER_OID 		= new ErrorCode("521007", "El identificador debe tener un formato OID valido"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode EMPTY_POLICY_HASH 					= new ErrorCode("521008", "La huella digital no puede ser nula ni vacia"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode INVALID_POLICY_QUALIFIER_URL 			= new ErrorCode("521009", "El calificador debe ser una URL valida"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode INVALID_PROXY_CONFIG		 			= new ErrorCode("521010", "La configuracion de proxy no es valida"); //$NON-NLS-1$ //$NON-NLS-2$

        public static final ErrorCode INVALID_PLUGIN_FILE   				= new ErrorCode("530007", "El plugin no es valido para ser importado"); //$NON-NLS-1$ //$NON-NLS-2$
        public static final ErrorCode PLUGIN_FILE_NOT_FOUND   				= new ErrorCode("530009", "No se ha encontrado el fichero del plugin"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	public static class Request {

		public static final ErrorCode UNSUPPORTED_OPERATION				= new ErrorCode("600002", "La operacion no esta soportada"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_FORMAT_SIGNATURE_PARAM	= new ErrorCode("600121", "Alguna de las propiedades de firma es incorrecta"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_FORMAT_SIGN_BATCH_PARAM	= new ErrorCode("600420", "Alguna de las propiedades de firma del lote es incorrecta"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode UNSUPPORTED_REQUEST_SCHEME		= new ErrorCode("620009", "El esquema de la URL de invocacion no esta soportado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_SESSION_ID				= new ErrorCode("620010", "El identificador de sesion del websocket es incorrecto"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode UNSUPPORTED_PROTOCOL_VERSION		= new ErrorCode("620011", "Version de protocolo de comunicacion con el navegador no soportada"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode REQUEST_URI_NOT_FOUND				= new ErrorCode("620012", "No se ha proporcionado la URI de invocacion"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode PORTS_NOT_FOUND					= new ErrorCode("620013", "No se han proporciona los puertos de conexion"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode MALFORMED_REQUEST_TO_SOCKET		= new ErrorCode("620016", "Los parametros recibidos en el socket no son correctos"); //$NON-NLS-1$ //$NON-NLS-2$

	}
}

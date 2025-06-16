package es.gob.afirma.core;

public class ErrorCode {

	public static final String ERROR_HARDWARE = "1"; //$NON-NLS-1$
	public static final String ERROR_INTERNAL = "2"; //$NON-NLS-1$
	public static final String ERROR_THIRD_PARTY = "3"; //$NON-NLS-1$
	public static final String ERROR_COMMUNICATION = "4"; //$NON-NLS-1$
	public static final String ERROR_FUNCTIONAL = "5"; //$NON-NLS-1$
	public static final String ERROR_REQUEST = "6"; //$NON-NLS-1$

	public static final String ERROR_REQUEST_FROM_BROWSER = "62"; //$NON-NLS-1$

	public static final String ERROR_TRIPHASE_SERVICE  = "3004"; //$NON-NLS-1$

	private final String code;

	private String description;

	public ErrorCode(final String code) {
		this(code, null);
	}

	public ErrorCode(final String code, final String description) {
		checkCode(code);
		this.code = code;
		this.description = description;
	}

	private static void checkCode(final String errorCode) {

		boolean error = false;
		if (errorCode == null || errorCode.length() != 6) {
			error = true;
		}
		else {
			for (int i = 0; !error && i < errorCode.length(); i++) {
				if (!Character.isDigit(errorCode.charAt(i))) {
					error = true;
				}
			}
		}
		if (error) {
			throw new IllegalArgumentException("Los codigos de error deben ser un numero de 6 digitos"); //$NON-NLS-1$
		}
	}

	public String getCode() {
		return this.code;
	}

	public String getDescription() {
		return this.description;
	}

	public void setDescription(final String description) {
		this.description = description;
	}

	@Override
	public String toString() {
		return this.code + ": " + this.description; //$NON-NLS-1$
	}

	public boolean checkType(final String type) {
		return this.code.startsWith(type);
	}

	@Override
	public boolean equals(final Object obj) {
		return obj instanceof ErrorCode ? this.code.equals(((ErrorCode) obj).code) : false;
	}

	@Override
	public int hashCode() {
		return this.code.hashCode();
	}

//	public static class Hardware {
//	}

	public static class Internal {

		public static final ErrorCode LIBRARY_NOT_FOUND							= new ErrorCode("200002", "No se encuentran dependencias necesarias para la ejecucion de la operacion"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOADING_DATA_ERROR						= new ErrorCode("200003", "Error durante la carga de datos"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode ERROR_IMPORT_LANGUAGE						= new ErrorCode("200006", "Error al importar idioma"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode LOADING_LOCAL_FILE_ERROR					= new ErrorCode("200102", " Error al cargar el fichero local para realizar la firma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode SIGNINIG_KEY_AUTHENTICATION_ERROR			= new ErrorCode("200108", "No se pudo acceder a la clave de firma"); //$NON-NLS-1$ //$NON-NLS-2$
		//public static final ErrorCode INVALID_SIGNATURE_ALGORITHM				= new ErrorCode("200109", "Algoritmo de firma no soportado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_SIGNING_KEY						= new ErrorCode("200110", "La clave de firma no es valida o no es compatible con el algoritmo seleccionado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode SIGNING_PKCS1_ERROR						= new ErrorCode("200111", "Error al generar la firma PKCS#1"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_PKCS1_VALUE						= new ErrorCode("200112", "La firma no se ha generado con el certificado seleccionado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode VERIFING_PKCS1_ERROR						= new ErrorCode("200113", "No se pudo verificar la firma generada"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode ENCODING_SIGNING_CERTIFICATE				= new ErrorCode("200114", "Error codificando el certificado de firma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode UNKNOWN_SIGNING_ERROR						= new ErrorCode("200115", "Error desconocido durante la operacion de firma"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode UNSUPPORTED_HASH_ALGORITHM				= new ErrorCode("200118", "No se soporta un algoritmo de hash establecido internamente"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode BUILDING_ASIC_CONTAINER_ERROR				= new ErrorCode("214001", "Error al construir el contenedor ASiC de la nueva firma"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode PLUGIN_ERROR								= new ErrorCode("240000", "Error con el plugin"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public static class ThirdParty {
		public static final ErrorCode INVALID_OPERATION_XML						= new ErrorCode("300202", " La respuesta del servidor intermedio al descargar la informacion no es un XML valido"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode PRESIGN_HTTP_ERROR		 				= new ErrorCode("300400", "El servicio de prefirma devolvio un error HTTP"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode MALFORMED_PRESIGN_RESPONSE		 		= new ErrorCode("300401", "La respuesta del servidor trifasico al hacer la prefirma no es valida"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode PRESIGNS_NOT_FOUND				 		= new ErrorCode("300402", "No se han obtenido prefirmas del servicio de prefirma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode POSTSIGN_HTTP_ERROR		 				= new ErrorCode("300404", "El servicio de postfirma devolvio un error HTTP"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode POSTSIGN_ERROR					 		= new ErrorCode("300405", "La respuesta del servidor trifasico al hacer la postfirma no es valida"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode MALFORMED_POSTSIGN_RESPONSE				= new ErrorCode("300407", "El servidor trifasico devolvio una respuesta con texto OK pero no llega el formato correcto"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_1				= new ErrorCode("300409", "ERR-1 en la prefirma: No se ha indicado la operacion a realizar"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_2				= new ErrorCode("300410", "ERR-2 en la prefirma: No se ha indicado el identificador del documento"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_3				= new ErrorCode("300411", "ERR-3 en la prefirma: No se ha indicado el algoritmo de firma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_4				= new ErrorCode("300412", "ERR-4 en la prefirma: No se ha indicado el formato de firma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_5				= new ErrorCode("300413", "ERR-5 en la prefirma: No se ha indicado el certificado de usuario"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_6				= new ErrorCode("300414", "ERR-6 en la prefirma: El formato de los parametros adicionales suministrados es erroneo"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_7				= new ErrorCode("300415", "ERR-7 en la prefirma: El certificado de usuario no esta en formato X.509"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_8				= new ErrorCode("300416", "ERR-8 en la prefirma: Formato de firma no soportado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_9				= new ErrorCode("300417", "ERR-9 en la prefirma: Error realizando la prefirma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_10				= new ErrorCode("300418", "ERR-10 en la prefirma: Error al almacenar el documento"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_11				= new ErrorCode("300419", "ERR-11 en la prefirma: Operacion desconocida"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_12				= new ErrorCode("300420", "ERR-12 en la prefirma: Error realizando la postfirma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_13				= new ErrorCode("300421", "ERR-13 en la prefirma: No se indicado una sub-operacion valida a realizar (firma, cofirma,...)"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_14				= new ErrorCode("300422", "ERR-14 en la prefirma: Error al recuperar el documento"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_15				= new ErrorCode("300423", "ERR-15 en la la prefirma: El formato de los datos de sesion suministrados es erroneo"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_16				= new ErrorCode("300424", "ERR-16 en la la prefirma: Error al generar el codigo de verificacion de las firmas"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_17				= new ErrorCode("300425", "ERR-17 en la la prefirma: Error al comprobar el codigo de verificacion de las firmas"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_18				= new ErrorCode("300426", "ERR-18 en la la prefirma: Error de integridad en la firma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_19				= new ErrorCode("300427", "ERR-19 en la la prefirma: El formato de los datos de operacion suministrados es erroneo"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_PRESIGN_ERROR_20				= new ErrorCode("300428", "ERR-20 en la la prefirma: Algoritmo de firma no soportado"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_1				= new ErrorCode("300430", "ERR-1 en la postfirma: No se ha indicado la operacion a realizar"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_2				= new ErrorCode("300431", "ERR-2 en la postfirma: No se ha indicado el identificador del documento"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_3				= new ErrorCode("300432", "ERR-3 en la postfirma: No se ha indicado el algoritmo de firma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_4				= new ErrorCode("300433", "ERR-4 en la postfirma: No se ha indicado el formato de firma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_5				= new ErrorCode("300434", "ERR-5 en la postfirma: No se ha indicado el certificado de usuario"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_6				= new ErrorCode("300435", "ERR-6 en la postfirma: El formato de los parametros adicionales suministrados es erroneo"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_7				= new ErrorCode("300436", "ERR-7 en la postfirma: El certificado de usuario no esta en formato X.509"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_8				= new ErrorCode("300437", "ERR-8 en la postfirma: Formato de firma no soportado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_9				= new ErrorCode("300438", "ERR-9 en la postfirma: Error realizando la prefirma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_10				= new ErrorCode("300439", "ERR-10 en la postfirma: Error al almacenar el documento"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_11				= new ErrorCode("300440", "ERR-11 en la postfirma: Operacion desconocida"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_12				= new ErrorCode("300441", "ERR-12 en la postfirma: Error realizando la postfirma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_13				= new ErrorCode("300442", "ERR-13 en la postfirma: No se indicado una sub-operacion valida a realizar (firma, cofirma,...)"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_14				= new ErrorCode("300443", "ERR-14 en la postfirma: Error al recuperar el documento"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_15				= new ErrorCode("300444", "ERR-15 en la la postfirma: El formato de los datos de sesion suministrados es erroneo"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_16				= new ErrorCode("300445", "ERR-16 en la la postfirma: Error al generar el codigo de verificacion de las firmas"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_17				= new ErrorCode("300446", "ERR-17 en la la postfirma: Error al comprobar el codigo de verificacion de las firmas"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_18				= new ErrorCode("300447", "ERR-18 en la la postfirma: Error de integridad en la firma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_19				= new ErrorCode("300448", "ERR-19 en la la postfirma: El formato de los datos de operacion suministrados es erroneo"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_POSTSIGN_ERROR_20				= new ErrorCode("300449", "ERR-20 en la la postfirma: Algoritmo de firma no soportado"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode TRI_SERVER_UNKNOWN_PRESIGN_ERROR			= new ErrorCode("300451", "No se reconoce el error remitido por el servicio de prefirma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode TRI_SERVER_UNKNOWN_POSTSIGN_ERROR			= new ErrorCode("300452", "No se reconoce el error remitido por el servicio de postfirma"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	public static class Communication {
		public static final ErrorCode PRESIGN_SERVICE_CONNECTION_ERROR			= new ErrorCode("401300", "Error de conexion con el servidor trifasico al hacer la prefirma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode PRESIGN_SERVICE_COMMUNICATION_ERROR			= new ErrorCode("401301", "Error de comunicacion con el servidor trifasico al hacer la prefirma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode POSTSIGN_SERVICE_CONNECTION_ERROR			= new ErrorCode("401400", "Error de conexion con el servidor trifasico al hacer la posfirma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode POSTSIGN_SERVICE_COMMUNICATION_ERROR			= new ErrorCode("401401", "Error de comunicacion con el servidor trifasico al hacer la posfirma"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode TSA_CONNECTION_ERROR						= new ErrorCode("410001", "Error en la comuicacion con la autoridad de sellado de tiempo"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	public static class Functional {
		public static final ErrorCode CANCELLED_OPERATION						= new ErrorCode("500001", "Operacion cancelada por el usuario"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_SIGNATURE							= new ErrorCode("500002", "La firma que se trata de procesar es invalida"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INCOMPATIBLE_OPERATING_SYSTEM				= new ErrorCode("500003", "Sistema operativo no compatible con la operacion"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode CERTIFICATE_NEEDED 						= new ErrorCode("501001", "Error en la operacion, no hay certificados"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_FORMAT_FILE						= new ErrorCode("501002", "El formato de fichero no es valido para la operacion que se intenta realizar"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_SMARTCARD_PIN						= new ErrorCode("501003", "El PIN del la tarjeta inteligente es incorrecto"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode DOCUMENT_TOO_LARGE						= new ErrorCode("501005", "El documento supera el tamano maximo permitido"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode COMPATIBLE_SIGNATURE_NOT_FOUND			= new ErrorCode("501006", "La firma no es compatible con el formato de firma utilizado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode SIGNING_LTS								= new ErrorCode("501007", "Se esta intentando firmar una firma longeva que quedara invalidada"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode SIGNING_WITH_POLICY_INCOMPATIBILITY		= new ErrorCode("501008", "La firma resultante sera incompatible con la politica de firma"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode KEYSTORE_LOCKED							= new ErrorCode("511003", "El almacen de claves esta bloqueado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode SMARTCARD_LOCKED							= new ErrorCode("511004", "La tarjeta inteligente esta bloqueada"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode SIGNING_MALFORMED_SIGNATURE				= new ErrorCode("510006", "La firma no es compatible o esta mal formada"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public static class Request {
		public static final ErrorCode UNSUPPORTED_CIPHER_KEY				= new ErrorCode("600009", "La clave de cifrado recibida no esta soportada"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOCAL_RETRIEVE_URL					= new ErrorCode("600010", "La URL del servlet de recuperacion no puede ser local"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode DATA_NOT_FOUND						= new ErrorCode("600100", "No se ha recibido los datos en la peticion ni el id del fichero a descargar para la operacion de firma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode RETRIEVE_URL_TO_SIGN_NOT_FOUND		= new ErrorCode("600101", "No se ha recibido la URL del servlet para descargar la informacion de la operacion de firma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode STORAGE_URL_TO_SIGN_NOT_FOUND			= new ErrorCode("600102", "No se ha recibido la URL del servlet para guardar la firma en la operacion de firma"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode SIGNATURE_FORMAT_NOT_FOUND			= new ErrorCode("600104", "No se ha recibido el formato de firma para la operacion de firma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode UNSUPPORTED_SIGNATURE_FORMAT			= new ErrorCode("600105", "El formato de firma indicado en la operacion de firma no esta soportado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode SIGNATURE_ALGORITHM_NOT_FOUND			= new ErrorCode("600106", "No se ha recibido el algoritmo de firma para la operacion de firma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode UNSUPPORTED_SIGNATURE_ALGORITHM		= new ErrorCode("600107", "El algoritmo de firma indicado en la operacion de firma no esta soportado"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode UNSUPPORTED_COUNTERSIGN_CONFIG		= new ErrorCode("600110", "No se soporta la configuracion de nodos a contrafirmar"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode UNSUPPORTED_POLICY_HASH_ALGORITHM		= new ErrorCode("600111", "El algoritmo de huella indicado en la politica de firma no esta soportado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_COUNTERSIGNATURE_INDEX		= new ErrorCode("600112", "El numero indicado no se corresponde con la posicion de ninguna contrafirma"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_TIMESTAMP_HASH_ALGORITHM		= new ErrorCode("600113", "El algoritmo de huella para el sello de tiempo no esta soportado"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode INVALID_RETRIEVE_URL_TO_SIGN			= new ErrorCode("600116", "La URL del servlet de descarga para la operacion de firma esta mal formado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode RETRIEVE_URL_TO_SIGN_CANT_BE_LOCAL	= new ErrorCode("600117", "La URL del servlet de descarga para la operacion de firma no puede ser local"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_STORAGE_URL_TO_SIGN			= new ErrorCode("600118", "La URL del servlet de guardado para la operacion de firma esta mal formada"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOCAL_STORAGE_URL_TO_SIGN				= new ErrorCode("600119", "La URL del servlet de guardado para la operacion de firma no puede ser local"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_SESSION_ID_TO_SIGN			= new ErrorCode("600120", "El id de sesion recibido para la firma no es valido"); //$NON-NLS-1$ //$NON-NLS-2$


		public static final ErrorCode STORAGE_URL_TO_SELECT_CERT_NOT_FOUND	= new ErrorCode("600200", "No se ha recibido la URL del servlet para guardar el certificado en la operacion de enviar certificado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_SESSION_ID_TO_SELECT_CERT		= new ErrorCode("600201", "El id de sesion recibido para la seleccion de certificado no es valido"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_STORAGE_URL_TO_SELECT_CERT	= new ErrorCode("600202", "La URL del servlet de guardado para la operacion de seleccion de certificado esta mal formada"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOCAL_STORAGE_URL_TO_SELECT_CERT		= new ErrorCode("600203", "La URL del servlet de guardado para la operacion de seleccion de certificado no puede ser local"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode DATA_TO_SAVE_NOT_FOUND				= new ErrorCode("600300", "No se ha recibido los datos en la peticion ni el id del fichero a descargar para la operacion de guardado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode RETRIEVE_URL_TO_SAVE_NOT_FOUND		= new ErrorCode("600301", "No se ha recibido la URL del servlet para descargar la informacion de la operacion de guardado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode STORAGE_URL_TO_SAVE_NOT_FOUND			= new ErrorCode("600302", "No se ha recibido la URL del servlet para guardar la firma en la operacion de guardado"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_SESSION_ID_TO_SAVE			= new ErrorCode("600303", "El id de sesion recibido para el guardado de datos no es valido"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_STORAGE_URL_TO_SAVE			= new ErrorCode("600304", "La URL del servlet de guardado para la operacion de guargado esta mal formada"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOCAL_STORAGE_URL_TO_SAVE				= new ErrorCode("600305", "La URL del servlet de guardado para la operacion de guardado no puede ser local"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode FILENAME_TO_SAVE_NOT_FOUND			= new ErrorCode("600306", "No se ha recibido el nombre por defecto para el fichero de datos a guardar"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode FILE_EXTENSION_TO_SAVE_NOT_FOUND		= new ErrorCode("600307", "No se ha recibido la extension por defecto para el fichero de datos a guardar"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode STORAGE_URL_TO_SIGN_BATCH_NOT_FOUND	= new ErrorCode("600402", "No se ha recibido la URL del servlet para guardar la firma en la operacion de firma de lotes"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode ID_SESSION_TO_SIGN_BATCH_NOT_FOUND	= new ErrorCode("600403", "No se ha recibido el id del fichero a guardar para la operacion de firma de lotes"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode PRESIGN_BATCH_URL_NOT_FOUND			= new ErrorCode("600405", "No se ha recibido el URL de prefirma de la firma de lotes"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode POSTSIGN_BATCH_URL_NOT_FOUND			= new ErrorCode("600406", "No se ha recibido el URL de postfirma de la firma de lotes"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode INVALID_SESSION_ID_TO_SIGN_BATCH		= new ErrorCode("600410", "El id de sesion recibido no es valido"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_STORAGE_URL_TO_SIGN_BATCH		= new ErrorCode("600411", "La URL del servlet de guardado para la operacion de firma de lotes esta mal formada"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOCAL_STORAGE_URL_TO_SIGN_BATCH		= new ErrorCode("600412", "La URL del servlet de guardado para la operacion de firma de lotes no puede ser local"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode INVALID_PRESIGN_BATCH_URL				= new ErrorCode("600413", "La URL de prefirma de la firma de lotes esta mal formada"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOCAL_PRESIGN_BATCH_URL				= new ErrorCode("600414", "La URL de prefirma de la firma de lotes no puede ser local"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_POSTSIGN_BATCH_URL			= new ErrorCode("600415", "La URL de postfirma de la firma de lotes esta mal formada"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode LOCAL_POSTSIGN_BATCH_URL				= new ErrorCode("600416", "La URL de postfirma de la firma de lotes no puede ser local"); //$NON-NLS-1$ //$NON-NLS-2$

		public static final ErrorCode INVALID_PARAMS_TO_PRESIGN				= new ErrorCode("600701", "El servicio de prefirma informo de un error en los parametros enviados"); //$NON-NLS-1$ //$NON-NLS-2$
		public static final ErrorCode INVALID_PARAMS_TO_POSTSIGN			= new ErrorCode("600702", "El servicio de postfirma informo de un error en los parametros enviados"); //$NON-NLS-1$ //$NON-NLS-2$
	}
}

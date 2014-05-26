using es.gob.afirma.core.signers;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AfirmaWMetro
{
    class ConstantsAfirmaMetro
    {
        // Dominios aceptados en el protocolo
       // public const string CUSTOM_PROTOCOL_SIGN = "afirma://sign?";
        //public const string CUSTOM_PROTOCOL_SAVE = "afirma://save?";

        // Parámetros comunes a distintas URI
        public const string DAT = "dat"; // Datos (a firmar o a obtener desde un Servlet)

        // Parámetros comunes a las URI de entrada
        public const string SERVLET_RETIREVE = "rtservlet"; // URL del Servlet de obtención de datos

        public const string SERVLET_STORAGE = "stservlet";  // URL del Servlet de deposición de datos
        public const string ID = "id";                      // Identificador de los datos a firmar
        public const string KEY = "key";                    // Clave DES de cifrado para los datos firmados
        public const string FORMAT = "format";              // Formato de firma (CAdES, XAdES, PAdES)
        public const string ALGORITHM = "algorithm";        // Algoritmo de firma (SHA1withRSA, SHA512withRSA, etc.)
        public const string FROM = "metro";

        // Parámetros en URI de salida hacia los Servlets
        public const string OP = "op";               // Nombre del parámetro de tipo de operacion
        public const string OP_DEFAULT_SEND = "put"; // Valor del parámetro de tipo de operacion: Depositar datos
        public const string OP_DEFAULT_GET = "get";  // Valor del parámetro de tipo de operacion: Obtener datos
        public const string FILEID = "fileid";       // Nombre del parámetro de identificador de datos a depositar u obtener

        public const string SIGN_OPERATION = "sign";
        public const string SAVE_OPERATION = "save";
        
        public const string OP_DEFAULT = SIGN_OPERATION;
        
        public const string FORMAT_DEFAULT = "CADES";
        public const string ALGORITHM_DEFAULT = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;
        
        
        public const string RESPONSE_SERVLET_OK = "OK";
        public const string VERSION = "v";
        public const string VERSION_DEFAULT = "1_0";
        
        public const string ERROR_SEPARATOR = ":=";


        //CONSTANTES DE ERROR
        public const String ERROR_MISSING_OPERATION_NAME     = "ERR-00"; 
        public const String ERROR_UNSUPPORTED_OPERATION_NAME = "ERR-01"; 
        public const String ERROR_MISSING_DATA               = "ERR-02"; 
        public const String ERROR_BAD_XML                    = "ERR-03"; 
        public const String ERROR_BAD_CERTIFICATE            = "ERR-04"; 
        public const String ERROR_MISSING_DATA_ID            = "ERR-05"; 
        public const String ERROR_INVALID_DATA_ID            = "ERR-06"; 
        public const String ERROR_INVALID_DATA               = "ERR-07"; 
        public const String ERROR_MISSING_SERVLET            = "ERR-08"; 
        public const String ERROR_INVALID_SERVLET            = "ERR-09"; 
        public const String ERROR_NOT_SUPPORTED_FORMAT       = "ERR-10"; 
        public const String ERROR_CANCELLED_OPERATION        = "ERR-11"; 
        public const String ERROR_CODING_BASE64              = "ERR-12"; 
        public const String ERROR_PKE                        = "ERR-13"; 
        public const String ERROR_SIGNING                    = "ERR-14"; 
        public const String ERROR_INVALID_CIPHER_KEY         = "ERR-15"; 
        public const String ERROR_CIPHERING                  = "ERR-16"; 
        public const String ERROR_NO_CERT_SELECTED           = "ERR-17"; 
        public const String ERROR_COMMUNICATING_WITH_WEB     = "ERR-18"; 
        public const String ERROR_CONFIGURATION_FILE_PROBLEM = "ERR-19"; 
        public const String ERROR_MISSING_SYNTAX_VERSION     = "ERR-20";

        public const String DESC_ERROR_MISSING_OPERATION_NAME = "No se ha indicado c\u00F3digo de operaci\u00F3n";
        public const String DESC_ERROR_UNSUPPORTED_OPERATION_NAME = "C\u00F3digo de operaci\u00F3n no soportado";
        public const String DESC_ERROR_MISSING_DATA = "No se han proporcionado los datos de la operaci\u00F3n"; 
        public const String DESC_ERROR_BAD_XML = "Se ha recibido un XML mal formado"; 
        public const String DESC_ERROR_BAD_CERTIFICATE = "Se ha recibido un certificado corrupto"; 
        public const String DESC_ERROR_MISSING_DATA_ID = "No se ha proporcionado un identificador para los datos"; 
        public const String DESC_ERROR_INVALID_DATA_ID = "El identificador para los datos es inválido"; 
        public const String DESC_ERROR_INVALID_DATA = "Los datos solicitados o enviados son inválidos";
        public const String DESC_ERROR_MISSING_SERVLET = "No se ha proporcionado el sevlet para la comunicaci\u00F3n de los datos"; 
        public const String DESC_ERROR_INVALID_SERVLET = "La ruta del servlet es inválida"; 
        public const String DESC_ERROR_NOT_SUPPORTED_FORMAT = "Se ha configurado un formato de firma no soportado"; 
        public const String DESC_ERROR_CANCELLED_OPERATION = "Operaci\u00F3n cancelada"; 
        public const String DESC_ERROR_CODING_BASE64 = "Error en la codificaci\u00F3n del base 64"; 
        public const String DESC_ERROR_PKE = "No se seleccion\u00F3 ningún certificado de firma"; 
        public const String DESC_ERROR_SIGNING = "Ocurri\u00F3 un error en la operaci\u00F3n de firma"; 
        public const String DESC_ERROR_INVALID_CIPHER_KEY = "La clave de cifrado proporcionada no es válida"; 
        public const String DESC_ERROR_CIPHERING = "Error durante el proceso de cifrado de los datos"; 
        public const String DESC_ERROR_NO_CERT_SELECTED = "El usuario no seleccion\u00F3 ning\u00FAn certificado "; 
        public const String DESC_ERROR_COMMUNICATING_WITH_WEB = "No se ha podido enviar la firma generada a la web de origen"; 
        public const String DESC_ERROR_CONFIGURATION_FILE_PROBLEM = "Error de configuraci\u00F3n de la aplicaci\u00F3n"; 
        public const String DESC_ERROR_MISSING_SYNTAX_VERSION = "No se ha indicado la versi\u00F3n de la sintaxis de la operaci\u00F3n";
                
    }
}

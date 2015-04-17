//
//  CADESConstants.m
//  SignSample02
//
//

#import "CADESConstants.h"

@implementation CADESConstants

NSString *const OPERATION_SIGN = @"sign";
NSString *const OPERATION_COSIGN = @"cosign";
NSString *const OPERATION_COUNTERSIGN = @"countersign";
NSString *const OPERATION_PRESIGN = @"pre";
NSString *const OPERATION_POSTSIGN = @"post";
NSString *const OPERATION_PUT = @"put";
NSString *const OPERATION_GET = @"get";
NSString *const PARAMETER_NAME_OPERATION = @"op";
NSString *const PARAMETER_NAME_COPERATION = @"cop";
NSString *const PARAMETER_NAME_METRO = @"metro";

NSString *const HTTP_CGI = @"?";
NSString *const HTTP_EQUALS = @"=";
NSString *const HTTP_AND = @"&";

NSString *const PARAMETER_NAME_DOCID = @"doc";
NSString *const PARAMETER_NAME_ALGORITHM = @"algo";
NSString *const PARAMETER_NAME_FORMAT = @"format";
NSString *const PARAMETER_NAME_CERT = @"cert";
NSString *const PARAMETER_NAME_EXTRA_PARAM = @"params";
NSString *const PARAMETER_NAME_VERSION = @"v";
NSString *const PARAMETER_NAME_VERSION_1_0 = @"1_0";
NSString *const PARAMETER_NAME_ID = @"id";
NSString *const PARAMETER_NAME_CIPHER_KEY = @"key";
NSString *const PARAMETER_NAME_DAT = @"dat";
NSString *const PARAMETER_NAME_STSERVLET = @"stservlet";
NSString *const PARAMETER_NAME_RTSERVLET = @"rtservlet";
NSString *const PARAMETER_NAME_ALGORITHM2 = @"algorithm";
NSString *const PARAMETER_NAME_PROPERTIES = @"properties";
NSString *const PARAMETER_NAME_TRIPHASIC_SERVER_URL = @"serverUrl";
NSString *const PARAMETER_NAME_FILE_ID = @"fileid";
NSString *const PARAMETER_NAME_TARGET = @"target";
NSString *const PARAMETER_NAME_TARGET_TREE = @"tree";
NSString *const PARAMETER_NAME_TARGET_LEAFS = @"leafs";

NSString *const PARAMETER_LOCAL_SIGNATURE = @"local";
NSString *const PARAMETER_LOCAL_CLOUD_NAME = @"cloudname";

//parámetros del properties
NSString *const PROPERTIES_PARAMETER_INCLUDEONLYSIGNINGCERTIFICATE = @"includeOnlySigningCertificate";
NSString *const PROPERTIES_PARAMETER_MODE = @"mode";
NSString *const PROPERTIES_PARAMETER_MODE_IMPLICIT = @"implicit";
NSString *const PROPERTIES_PARAMETER_MODE_EXPLICIT = @"explicit";
NSString *const PROPERTIES_PARAMETER_POLICYIDENTIFIER = @"policyIdentifier";
NSString *const PROPERTIES_PARAMETER_POLICYIDENTIFIERHASH = @"policyIdentifierHash";
NSString *const PROPERTIES_PARAMETER_POLICYIDENTIFIERHASHALGORITHM = @"policyIdentifierHashAlgorithm";
NSString *const PROPERTIES_PARAMETER_POLICYQUALIFIER = @"policyQualifier";
NSString *const PROPERTIES_PARAMETER_PRECALCULATEDHASHALGORITHM = @"precalculatedHashAlgoritm";
NSString *const PROPERTIES_PARAMETER_SIGNINGCERTIFICATEV2 = @"signingCertificateV2";

NSString *const CADES_FORMAT = @"CAdES";
NSString *const CADES_TRI_FORMAT = @"CAdEStri";
NSString *const PADES_TRI_FORMAT = @"PAdEStri";
NSString *const XADES_TRI_FORMAT = @"XAdEStri";
NSString *const PADES_FORMAT = @"PAdES";
NSString *const XADES_FORMAT = @"XAdES";

NSString *const PROPERTY_NAME_PRESIGN = @"PRE";
NSString *const PARAMETER_NAME_PKCS1_SIGN = @"PK1";

/* ERRORES */
NSString *const ERROR_MISSING_OPERATION_NAME = @"ERR-00";
NSString *const ERROR_UNSUPPORTED_OPERATION_NAME = @"ERR-01";
NSString *const ERROR_MISSING_DATA = @"ERR-02";
NSString *const ERROR_MISSING_DATA_ID = @"ERR-05";
NSString *const ERROR_NOT_SUPPORTED_FORMAT = @"ERR-10";
NSString *const ERROR_SIGNING = @"ERR-14";
NSString *const ERROR_NOT_SUPPORTED_ALGORITHM = @"ERR-15";
NSString *const ERROR_NOT_CERTIFICATE = @"ERR-17";
NSString *const ERROR_NOT_TARGET = @"ERR-18";


NSString *const ERROR_SEPARATOR = @":=";

NSString *const DESC_ERROR_SIGNING = @"Ocurri\u00F3 un error en la operaci\u00F3n de firma";
NSString *const DESC_ERROR_MISSING_DATA_ID = @"No se ha proporcionado un identificador para los datos";
NSString *const DESC_ERROR_NOT_SUPPORTED_FORMAT = @"Se ha configurado un formato de firma no soportado";
NSString *const DESC_ERROR_MISSING_DATA = @"No se han proporcionado los datos de la operaci\u00F3n";
NSString *const DESC_ERROR_MISSING_OPERATION_NAME = @"No se ha indicado el c\u00F3digo de la operaci\u00F3n";
NSString *const DESC_ERROR_UNSUPPORTED_OPERATION_NAME = @"C\u00F3digo de operaci\u00F3n no soportado";
NSString *const DESC_ERROR_NOT_SUPPORTED_ALGORITHM = @"El Algoritmo no es váLido o no está soportado";

NSString *const DESC_ERROR_NOT_CERTIFICATE = @"No hay certificados en la aplicación.";

NSString *const DESC_ERROR_NOT_TARGET = @"El objetivo indicado para la contrafirma no está soportado";


NSString *const PROPERTY_NAME_PRESIGN_PREFIX = @"PRE.";
NSString *const PROPERTY_NAME_SIGN_COUNT = @"SIGN.COUNT";
NSString *const PROPERTY_NAME_SESSION_DATA_PREFIX = @"session";
NSString *const PROPERTY_NAME_PKCS1_SIGN_PREFIX = @"PK1.";
NSString *const PROPERTY_NAME_NEED_DATA =@"NEED_DATA";
NSString *const PROPERTY_NAME_NEED_PRE =@"NEED_PRE";
@end

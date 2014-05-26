//
//  SignSample02Test.m
//  SignSample02Test
//
//

#import "SignSample02Tests.h"
#include <CoreFoundation/CoreFoundation.h>
#include <resolv.h>
#import "NSData+Base64.h"
#include <openssl/x509.h>
#include "CADESOID.h"
#include "EncapsulatedSignedData.h"
#include "CADESSigner.h"
#include "CADESSignUtils.h"
#include "CertificateUtils.h"

static int write_out(const void *buffer, size_t size, void *app_key);

NSString *base64UrlSafeCertificateData;
SecKeyRef privateKeyPkcs12 = NULL;


@implementation SignSample02Tests

- (void)setUp
{
    [super setUp];
    
    // Set-up code here.
}

- (void)tearDown
{
    // Tear-down code here.
    
    [super tearDown];
}


- (void)testExample
{
    NSString *pin = @"12341234";
    OSStatus status = [self openPkcs12Store:pin];

    NSData *sCertificate = NULL;
    //sCertificate = [self decodeBase64:sCert];
    sCertificate = [CADESSignUtils base64DecodeString:base64UrlSafeCertificateData];
    
    SecCertificateRef myCertificate = SecCertificateCreateWithData(kCFAllocatorDefault, (CFDataRef)(sCertificate));
    CFStringRef  certSummary = SecCertificateCopySubjectSummary(myCertificate);
    NSLog(@"%@", certSummary);
    const unsigned char *certificateDataBytes = (const unsigned char *)[sCertificate bytes];
    X509 *certificateX509 = d2i_X509(NULL, &certificateDataBytes, [sCertificate length]);

    
    /****************************************************/
    /****     PREPARAMOS LOS DATOS PARA LA FIRMA     ****/
    /****************************************************/
    
    int signingCertificateV2 = 1; //es un int porque en c no existen los tipos boolean. el "0" representa v1 y el "1" representa el "v2"
    NSString *contentData= @"hola mundo";
    NSString *signAlgoInUse   = @"SHA256withRSA";
    NSString *contentDescription = @"binary";
    
    //NSString *policyOID = @"2.16.724.1.3.1.1.2.1.8";
    NSString *policyOID = NULL;
    NSString *policyHash = @"7SxX3erFuH31TvAw9LZ70N7p1vA=";
    //NSString *policyUri = @"http://administracionelectronica.gob.es/es/ctt/politicafirma/politica_firma_AGE_v1_8.pdf";
    NSString *policyUri = NULL;
    
    char *hashAlgorithm = [CADESSignUtils getAlgorithmOID:signAlgoInUse];
    char *signAlgorithm = RSA_OID; //De momento sólo admitimos RSA
    
    /*** EL SIGNING TIME LO CALCULAMOS FUERA. ESTO ES PORQUE AL GENERAR PRIMERO LOS ATRIBUTOS DEL FIRMANTE  PARA FIRMARLOS Y LUEGO VOLVER A GENERARLOS PARA CREAR LA ESTRUCTURA CADES, LAS FECHAS NO COINCIDIRÍAN. ***/
    struct tm *local;
    time_t t;
    t = time(NULL);
    local = gmtime(&t);
    
    /**** CALCULAMOS EL HASH DE LOS DATOS ******/
    NSData *dataHash = [CADESSignUtils hashData:signAlgoInUse
                                           data:[contentData dataUsingEncoding:NSUTF8StringEncoding]];
    NSLog(@"Result: %@",[dataHash base64EncodedString]);
    //NSString *dataHashString = [ self sha1 ];
    /**** CALCULAMOS EL HASH DEL CERTIFICADO ******/
    NSData *certHash = [CADESSignUtils hashData:signAlgoInUse
                                           data:sCertificate ];
    
    
    /**** CALCULAMOS LOS ATRIBUTOS FIRMADOS *****/
    SignedAttributes_t *CADESSignedAttributes;
    getCADESSignedAttributes(&CADESSignedAttributes,
                             certificateX509,
                             [dataHash bytes],
                             [dataHash length],
                             [contentDescription UTF8String],
                             [policyOID UTF8String],
                             [policyHash UTF8String],
                             [policyUri UTF8String],
                             [certHash bytes],
                             [certHash length],
                             hashAlgorithm,
                             signingCertificateV2,
                             local);
    
    
    /**** CODIFICAMOS LOS ATRIBUTOS FIRMADOS EN UN ARRAY DE BYTES ***/
    /**** ESTA SERIA LA MEJOR FORMA. ASI NOS EVITARIAMOS DE CREAR EL
     FICHERO TEMPORAL A PRIORI. LO SUYO SERÍA SABER EL TAMAÑO DE LOS ATRIBUTOS FIRMADOS PARA PODER UTILIZAR EL METODO "der_encode_to_buffer()" PARA OBTENER LOS ATRIBUTOS CODIFICADOS Y PODER FIRMARLOS.
     COMO NO PODEMOS OBENER DICHO TAMAÑO, CREAMOS UN FICHERO TEMPORAL CON LOS DATOS CODIFICADOSY LO VOLVEMOS A LEER. */
    /*
     char buff[sizeof(*CADESSignedAttributes)];
     NSData *proba = [[NSData alloc] init];
     der_encode_to_buffer(&asn_DEF_SignedAttributes,
     CADESSignedAttributes,
     [proba bytes],
     320);
     */
    //xer_fprint(stdout, &asn_DEF_SignedAttributes, CADESSignedAttributes);
    
    /* CREAMOS EL FICHERO TEMPORAL DONDE SE ALOJARAN LOS DATOS DEL FIRMANTE CODIFICADOS. */
    /* Miramos cuál es la ruta temporal*/
    NSURL *documentDir = [[[NSFileManager defaultManager] URLsForDirectory:NSDocumentDirectory inDomains:NSUserDomainMask] objectAtIndex:0];
    NSURL *tmpDir = [[documentDir URLByDeletingLastPathComponent] URLByAppendingPathComponent:@"tmp" isDirectory:YES];
    
    NSString *directory = [tmpDir path];
    directory= [directory stringByAppendingString:@"/firmantes.csig"];
    NSLog(@"Fichero temporal creado en: %@",directory);
    char *nombre2 = (char*)[directory UTF8String];
    /* Codificamos los atributos firmados y los guardamos en un fichero */
    FILE *fichero2;
    fichero2 = fopen( nombre2, "wb" );
    der_encode(&asn_DEF_SignedAttributes, CADESSignedAttributes, write_out, fichero2);
    fclose(fichero2);
    /* leemos el fichero con los atributos firmados */
    NSData *signAttrib = [[NSFileManager defaultManager] contentsAtPath:directory];
    
    /*** FIRMAMOS LOS ATRIBUTOS FIRMADOS ****/
    NSData *dataSigned = [CADESSignUtils signPkcs1:signAlgoInUse
                                        privateKey:&privateKeyPkcs12
                                              data:signAttrib];
    
    /****************************************************/
    /**** METEMOS EL SIGNED DATA EN UN CONTENT TYPE *****/
    /****************************************************/
    
    EncapsulatedSignedData_t *encapsulatedSignedData;
    encapsulatedSignedData = calloc(1, sizeof(*encapsulatedSignedData));
    
    ContentType_t *eContentSignedData;
    eContentSignedData = calloc(1,sizeof(*eContentSignedData));
    *eContentSignedData = makeOID(SIGNED_DATA_OID);
    encapsulatedSignedData->oid = *eContentSignedData;
    
    //creamos el objeto signedData
    SignedData_t *signedData;
    signedData = calloc(1, sizeof(*signedData));
    
    char *policyHashAlg = NULL;
    
    /*** GENERAMOS LA ESTRUCTURA CADES ****/
    getSignedDataStructure(&signedData,
                           certificateX509,
                           [contentData UTF8String],
                           [sCertificate bytes],
                           [sCertificate length],
                           [dataSigned bytes],
                           [dataSigned length],
                           [dataHash bytes],
                           [dataHash length],
                           [contentDescription UTF8String],
                           [policyOID UTF8String],
                           [policyHash UTF8String],
                           [policyHashAlg UTF8String],
                           [policyUri UTF8String],
                           [certHash bytes],
                           [certHash length],
                           hashAlgorithm,
                           signingCertificateV2,
                           signAlgorithm,
                           local);
    
    encapsulatedSignedData->content = signedData;
    
    
    
    /* GUARDAMOS LOS DATOS EN FICHERO */
    asn_enc_rval_t ec;
    
    char nombre[42]="/Users/tomas/David/pruebas/datos2.dat";
    FILE *fichero;
    
    fichero = fopen( nombre, "wb" );
    
    printf("---------------------\n");
    //xer_fprint(stdout, &asn_DEF_EncapsulatedSignedData, encapsulatedSignedData);
    ec = der_encode(&asn_DEF_EncapsulatedSignedData, encapsulatedSignedData, write_out, fichero);
        
    if( !fclose(fichero) )
        printf( "\nFichero cerrado\n" );
    else
    {
        printf( "\nError: fichero NO CERRADO\n" );
    }
    
    if (ec.encoded == -1){
        fprintf(stderr, "No se ha podido hacer el encode (at %s)\n",
                ec.failed_type ? ec.failed_type->name : "Desconocido");
        exit(65);
    }
    else{
        fprintf(stderr, "Archivo creado correctamente");
        
    }    
       
}


static int
write_out(const void *buffer, size_t size, void *app_key){
    FILE *out_fp = app_key;
    size_t wrote;
    wrote = fwrite(buffer, 1, size, out_fp);
    
    return (wrote == size) ? 0 : -1;
}

-(OSStatus) openPkcs12Store:(NSString*)pin {
    
    // Cargamos el PKCS#12 desde como un recurso
    NSString *thePath = @"/Users/tomas/David/XCode/trunk/version madrid/SignSample02/SignSample02/ANF_PF_Activo.p12";
    
    NSData *PKCS12Data = [[NSData alloc] initWithContentsOfFile:thePath];
    CFDataRef inPKCS12Data = (CFDataRef) CFBridgingRetain(PKCS12Data);
    
    if (inPKCS12Data == NULL) {
        return 255;
    }
    
    OSStatus status = noErr;
	SecIdentityRef myIdentity;
	SecTrustRef myTrust;
    
	status = [CADESSignUtils extractIdentityAndTrust:inPKCS12Data :pin :&myIdentity :&myTrust];
	
	if (status != 0) {
        return status;
	}
    
    SecCertificateRef myReturnedCertificate = NULL;
    status = SecIdentityCopyCertificate (myIdentity, &myReturnedCertificate);
    
    if (status != 0){
        return status;
    }
    
    base64UrlSafeCertificateData = [CADESSignUtils encodeBase64:(NSData*) CFBridgingRelease(SecCertificateCopyData(myReturnedCertificate))];
    //NSLog(@"Certificado en Base64: %@", base64UrlSafeCertificateData);
    
    status = SecIdentityCopyPrivateKey(myIdentity, &privateKeyPkcs12);
    
    if (status != 0){
        return status;
    }
        
    
    return status;
    
}

@end

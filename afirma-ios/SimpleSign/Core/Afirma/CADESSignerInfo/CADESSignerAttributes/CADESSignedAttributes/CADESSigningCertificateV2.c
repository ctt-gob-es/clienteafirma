//
//  CADESSigningCertificateV2.c
//  SignSample02
//
//

#include <stdio.h>
#include "Attribute.h"
#include "CADESOID.h"
#include "SigningCertificateV2.h"
#include <openssl/x509.h>
#import "CADESSignerIdentifier.h"
#include "CADESSigningCertificatePolicies.h"
#include "NULL.h"

void getCADESSigningCertificateV2( Attribute_t **at_ext,
                                  X509 *certificateX509,
                                  char *certHash,
                                  int lengthCertHash,
                                  char *policyOID,
                                  char *policyUri,
                                  char *hashAlgorithm){
    
    /** SIGNING CERTIFICATE V2 **/
    //AtributeValue
    int ret;
    Attribute_t *atContentType;
    atContentType = calloc(1, sizeof(*atContentType));
    atContentType -> type = makeOID(SIGNINGCERTIFICATEV2);
    
    //definimos el valor del atributo
    AttributeValue_t *ContentTypeValue;
    ContentTypeValue = calloc(1,sizeof( *ContentTypeValue));
    
    SigningCertificateV2_t *signingCertificate;
    signingCertificate = calloc(1, sizeof(*signingCertificate));
    
    ESSCertIDv2_t *essCertID;
    essCertID = calloc(1, sizeof(*essCertID));
    
    /* Hash Algorithm */
    AlgorithmIdentifier_t *algorithmIdentifier;
    algorithmIdentifier = calloc(1,sizeof( *algorithmIdentifier));
    
    algorithmIdentifier -> algorithm = makeOID(hashAlgorithm);
    NULL_t *null;
    null = calloc(1, sizeof(*null));
    algorithmIdentifier -> parameters = ANY_new_fromType(&asn_DEF_NULL, null);
    
    essCertID -> hashAlgorithm = algorithmIdentifier;
    
    /* Hash Del certificado */
    OCTET_STRING_t *hashString;
    hashString = calloc(1,sizeof( *hashString));
    //OCTET_STRING_fromString(hashString,certHash);
    OCTET_STRING_fromBuf(hashString, certHash, lengthCertHash);
    
    essCertID -> certHash = *hashString;
    
    /* Issuer and serial */
    IssuerSerial_t *issuerSerial;
    issuerSerial = calloc(1,sizeof( *issuerSerial));
    
    //CREAMOS EL ISSUER
    Name_t *atIssuer;
    atIssuer = calloc(1, sizeof(*atIssuer));
    atIssuer = getCertificateIssuerName(certificateX509);
   
    //DAL
    GeneralNames_t *atGname;
    atGname = calloc(1, sizeof(*atGname));
    
    CustomGeneralName_t *atCuGname;
    atCuGname = calloc(1, sizeof(*atCuGname));
    
    ret = ASN_SET_ADD(&atCuGname ->list, atIssuer);
    
    ret = ASN_SEQUENCE_ADD(&atGname-> list, atCuGname);
    
    xer_fprint(stdout, &asn_DEF_GeneralNames, atGname);
    
    //CREAMOS EL SERIAL NUMBER
    CertificateSerialNumber_t *atSerialNumber;
    atSerialNumber = calloc(1,sizeof(*atSerialNumber));
    atSerialNumber = getCertificateSerialNumber(certificateX509);
    
    //DAL
    //issuerSerial -> issuer = *atIssuer;
    issuerSerial -> issuer = *atGname;
    issuerSerial -> serial = atSerialNumber;
    
    xer_fprint(stdout, &asn_DEF_IssuerSerial, issuerSerial);
    
    essCertID -> issuerSerial = issuerSerial;
    
    struct certs *certsList;
    certsList = calloc(1,sizeof(*certsList));
    ret = ASN_SEQUENCE_ADD(&certsList ->list, essCertID);
    
    signingCertificate ->certs = *certsList;
    
    if(policyOID !=NULL && policyUri!=NULL){
        getCADESSigningCertificatePolicies(&signingCertificate ->policies, policyOID, policyUri);
    }
    
    //introducimos el valor del atributo
    ContentTypeValue = ANY_new_fromType(&asn_DEF_SigningCertificateV2, signingCertificate);
    ret = ASN_SET_ADD(&atContentType-> values, ContentTypeValue);
    
    
    *at_ext = atContentType;
    
    /** FIN SIGNING CERTIFICATE V2 **/
}
//
//  SignedAttributes.c
//  CadesASN1Simplificado
//
//

#include <stdio.h>
#include "SignedAttributes.h"
#include <openssl/x509.h>
#include "CADESOID.h"
#include "UTCTime.h"
#include "ContentHints.h"
#include "asn_SEQUENCE_OF.h"
#include "SignaturePolicyId.h"
#include "NULL.h"
#include "IA5String.h"
#include "SigningCertificate.h"
#include "SigningCertificateV2.h"
#include "SignerIdentifier.h"
#include "CertificateUtil.h"

void getCADESSigningTime(Attribute_t **at_ext, struct tm *local){
    
    /** SINGING TIME **/
    //AtributeValue
    int ret;
    Attribute_t *atSigningTime;
    atSigningTime = calloc(1, sizeof(*atSigningTime));
    
    atSigningTime -> type = makeOID(SIGNING_TIME_OID);
    AttributeValue_t *atSigningTimeValue;
    atSigningTimeValue = calloc(1,sizeof( *atSigningTimeValue));
    
    UTCTime_t *time;
    time = calloc (1, sizeof(*time));
    asn_time2UT(time, local, 1);
    
    atSigningTimeValue = ANY_new_fromType(&asn_DEF_UTCTime, time);
    ret = ASN_SET_ADD(&atSigningTime-> values, atSigningTimeValue);
    
    *at_ext = atSigningTime;
    
    /** FIN SIGNING TIME **/
    
}

void getCADESContentType(Attribute_t **at_ext){
    /** CONTENT TYPE **/
    //AtributeValue
    int ret;
    Attribute_t *atContentType;
    atContentType = calloc(1, sizeof(*atContentType));
    atContentType -> type = makeOID(CONTENT_TYPE_OID);
    
    AttributeValue_t *ContentTypeValue;
    ContentTypeValue = calloc(1,sizeof( *ContentTypeValue));
    
    AttributeType_t *ContentTypeValueDataOID;
    ContentTypeValueDataOID = calloc(1,sizeof( *ContentTypeValueDataOID));
    *ContentTypeValueDataOID = makeOID(DATA_OID);
    
    ContentTypeValue = ANY_new_fromType(&asn_DEF_AttributeType, ContentTypeValueDataOID);
    ret = ASN_SET_ADD(&atContentType-> values, ContentTypeValue);
    
    *at_ext = atContentType;
    
    /** FIN CONTENT TYPE **/
    
}

void getCADESMessageDigest(Attribute_t **at_ext,
                           char *messageDigest,
                           int lengthMessageDigest){
    
    
    /** MESSAGEDIGEST **/
    //AtributeValue
    int ret;
    Attribute_t *atMessageDigest;
    atMessageDigest = calloc(1, sizeof(*atMessageDigest));
    atMessageDigest -> type = makeOID(MESSAGE_DIGEST_OID);
    
    AttributeValue_t *atMessageDigestValue;
    atMessageDigestValue = calloc(1,sizeof( *atMessageDigestValue));
    
    OCTET_STRING_t *attributeValue;
    attributeValue = calloc(1,sizeof( *attributeValue));
    
    //OCTET_STRING_fromString(attributeValue,messageDigest);
    OCTET_STRING_fromBuf(attributeValue, messageDigest, lengthMessageDigest);
    
    atMessageDigestValue = ANY_new_fromType(&asn_DEF_OCTET_STRING, attributeValue);
    
    ret = ASN_SET_ADD(&atMessageDigest-> values, atMessageDigestValue);
    
    *at_ext = atMessageDigest;
    
    /** FIN MESSAGEDIGEST **/
    
}

void getCADESContentHint(Attribute_t **at_ext,
                         char *contentDescription){
    
    /** CONTENT HINT **/
    //AtributeValue
    int ret;
    Attribute_t *atContentHint;
    atContentHint = calloc(1, sizeof(*atContentHint));
    atContentHint -> type = makeOID(CONTENT_HINT_OID);
    
    AttributeValue_t *atContentHintValue;
    atContentHintValue = calloc(1,sizeof( *atContentHintValue));
    
    ContentHints_t *contentHints;
    contentHints = calloc(1, sizeof(*contentHints));
    
    //contentDescription
    OCTET_STRING_t *dataType;
    dataType = calloc(1,sizeof( *dataType));
    OCTET_STRING_fromString(dataType,contentDescription);
    contentHints -> contentDescription = dataType;
    
    contentHints -> contentType = makeOID(DATA_OID);
    
    atContentHintValue = ANY_new_fromType(&asn_DEF_ContentHints, contentHints);
    ret = ASN_SEQUENCE_ADD(&atContentHint-> values,atContentHintValue);
    
    *at_ext = atContentHint;
    
    /** FIN CONTENT HINT **/
    
}

void getCADESSigPolicyId( Attribute_t **at_ext,
                         char *policyOID,
                         char *policyHash,
                         char *policyHashAlg,
                         char *policyUri){
    
    /** POLICY ID **/
    //AtributeValue
    int ret;
    Attribute_t *atContentType;
    atContentType = calloc(1, sizeof(*atContentType));
    atContentType -> type = makeOID(SIG_POLICY_ID_OID);
    
    //definimos el valor del atributo
    AttributeValue_t *ContentTypeValue;
    ContentTypeValue = calloc(1,sizeof( *ContentTypeValue));
    
    //el valor que ocupara el atributo será de tipo SignaturePolicyId
    SignaturePolicyId_t *signaturePolicyId;
    signaturePolicyId = calloc(1,sizeof( *signaturePolicyId));
    
    
    signaturePolicyId -> sigPolicyId = makeOID(policyOID);//tendremos que poner el oid que corresponda
    
    SigPolicyHash_t *otherHashAlgAndValue;
    otherHashAlgAndValue = calloc(1,sizeof( *otherHashAlgAndValue));
    
    AlgorithmIdentifier_t *algorithmIdentifier;
    algorithmIdentifier = calloc(1,sizeof( *algorithmIdentifier));
    algorithmIdentifier -> algorithm = makeOID(policyHashAlg);//tendremos que poner el oid que corresponda
    
    NULL_t *null;
    null = calloc(1, sizeof(*null));
    algorithmIdentifier -> parameters = ANY_new_fromType(&asn_DEF_NULL, null);
    
    otherHashAlgAndValue -> hashAlgorithm = *algorithmIdentifier;
    
    OCTET_STRING_t *otherHashValue;
    otherHashValue = calloc(1,sizeof( *otherHashValue));
    OCTET_STRING_fromString(otherHashValue,policyHash);
    
    otherHashAlgAndValue -> hashValue = *otherHashValue;
    
    //xer_fprint(stdout, &asn_DEF_OtherHashAlgAndValue, otherHashAlgAndValue);
    
    signaturePolicyId -> sigPolicyHash = *otherHashAlgAndValue;
    
    SigPolicyQualifierInfo_t *sigPolicyQualifierInfo;
    sigPolicyQualifierInfo = calloc(1,sizeof( *sigPolicyQualifierInfo));
    
    SigPolicyQualifierId_t *SigPolicyQualifierId;
    SigPolicyQualifierId = calloc(1,sizeof( *SigPolicyQualifierId));
    *SigPolicyQualifierId = makeOID(SPURI_OID);
    
    sigPolicyQualifierInfo -> sigPolicyQualifierId = *SigPolicyQualifierId;
    
    //IA5String -> URL de la politica
    OCTET_STRING_t *octetString;
    octetString = calloc(1,sizeof( *octetString));
    OCTET_STRING_fromString(octetString,policyUri);
    
    IA5String_t *policyURI;
    policyURI = octetString;
    
    ANY_t *sigQualifier;
    sigQualifier = calloc(1,sizeof( *sigQualifier));
    sigQualifier = ANY_new_fromType(&asn_DEF_IA5String, policyURI);
    sigPolicyQualifierInfo -> sigQualifier = *sigQualifier;
    
    struct sigPolicyQualifiers *listSigPolicyQualifiers;
    listSigPolicyQualifiers = calloc(1,sizeof( *listSigPolicyQualifiers));
    ret = ASN_SEQUENCE_ADD(&listSigPolicyQualifiers ->list, sigPolicyQualifierInfo);
    
    signaturePolicyId -> sigPolicyQualifiers = listSigPolicyQualifiers;
    
    //introducimos el valor del atributo
    ContentTypeValue = ANY_new_fromType(&asn_DEF_SignaturePolicyId, signaturePolicyId);
    ret = ASN_SET_ADD(&atContentType-> values, ContentTypeValue);
    
    *at_ext = atContentType;
    
    /** FIN POLICY ID **/
}

void getCADESSigningCertificatePolicies( struct policies **at_ext,
                                        char *policyOID,
                                        char *policyUri){
    
    int ret;
    
    struct PolicyInformation *policyInformation;
    policyInformation = calloc(1,sizeof(*policyInformation));
    
    policyInformation -> policyIdentifier = makeOID(policyOID);
    
    PolicyQualifierInfo_t *policyQualifierInfo;
    policyQualifierInfo = calloc(1,sizeof(*policyQualifierInfo));
    
    policyQualifierInfo -> policyQualifierId = makeOID(CPS_OID);
    
    //IA5String -> URL de la politica
    OCTET_STRING_t *octetString;
    octetString = calloc(1,sizeof( *octetString));
    OCTET_STRING_fromString(octetString,policyUri);
    
    IA5String_t *policyURI;
    policyURI = octetString;
    
    ANY_t *sigQualifier;
    sigQualifier = calloc(1,sizeof( *sigQualifier));
    sigQualifier = ANY_new_fromType(&asn_DEF_IA5String, policyURI);
    
    policyQualifierInfo -> qualifier = *sigQualifier;
    
    struct policyQualifiers  *policyQualifierInfoList;
    policyQualifierInfoList = calloc(1,sizeof( *policyQualifierInfoList));
    ret = ASN_SEQUENCE_ADD(&policyQualifierInfoList -> list, policyQualifierInfo);
    
    policyInformation -> policyQualifiers = policyQualifierInfoList;
    
    struct policies *policiesList;
    policiesList = calloc(1,sizeof( *policiesList));
    ret = ASN_SEQUENCE_ADD(&policiesList -> list, policyInformation);
    
    *at_ext = policiesList;
}

void getCADESSigningCertificateV1( Attribute_t **at_ext,
                                  X509 *certificateX509,
                                  char *certHash,
                                  int lengthCertHash,
                                  char *policyOID,
                                  char *policyUri){
    
    /** SIGNING CERTIFICATE V1 **/
    //AtributeValue
    int ret;
    Attribute_t *atContentType;
    atContentType = calloc(1, sizeof(*atContentType));
    atContentType -> type = makeOID(SIGNINGCERTIFICATEV1);
    
    //definimos el valor del atributo
    AttributeValue_t *ContentTypeValue;
    ContentTypeValue = calloc(1,sizeof( *ContentTypeValue));
    
    SigningCertificate_t *signingCertificate;
    signingCertificate = calloc(1, sizeof(*signingCertificate));
    
    ESSCertID_t *essCertID;
    essCertID = calloc(1, sizeof(*essCertID));
    
    OCTET_STRING_t *hashString;
    hashString = calloc(1,sizeof( *hashString));
    //OCTET_STRING_fromString(hashString,certHash);
    OCTET_STRING_fromBuf(hashString, certHash, lengthCertHash);
    
    essCertID -> certHash = *hashString;
    
    IssuerSerial_t *issuerSerial;
    issuerSerial = calloc(1,sizeof( *issuerSerial));
    
    //CREAMOS EL ISSUER
    Name_t *atIssuer;
    atIssuer = calloc(1, sizeof(*atIssuer));
    atIssuer = getCertificateIssuerName(certificateX509);
    
    GeneralNames_t *atGname;
    atGname = calloc(1, sizeof(*atGname));
    
    CustomGeneralName_t *atCuGname;
    atCuGname = calloc(1, sizeof(*atCuGname));
    
    ret = ASN_SET_ADD(&atCuGname ->list, atIssuer);
    
    ret = ASN_SEQUENCE_ADD(&atGname-> list, atCuGname);
    
    issuerSerial -> issuer = *atGname;
    
    //CREAMOS EL SERIAL NUMBER
    CertificateSerialNumber_t *atSerialNumber;
    atSerialNumber = calloc(1,sizeof(*atSerialNumber));
    atSerialNumber = getCertificateSerialNumber(certificateX509);
    
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
    ContentTypeValue = ANY_new_fromType(&asn_DEF_SigningCertificate, signingCertificate);
    ret = ASN_SET_ADD(&atContentType-> values, ContentTypeValue);
    
    
    *at_ext = atContentType;
    
    /** FIN SIGNING CERTIFICATE V1 **/
}

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

void getCADESSignedAttributes( struct SignedAttributes_t **at_ext,
                              X509 *certificateX509,
                              char *messageDigest,
                              int  lengthMessageDigest,
                              char *contentDescription,
                              char *policyOID,
                              char *policyHash,
                              char *policyHashAlg,
                              char *policyUri,
                              char *certHash,
                              int  lengthCertHash,
                              char *hashAlgorithm,
                              int signingCertificateV2,
                              struct tm *local){
    
    int ret;
    
    SignedAttributes_t *CADESSignedAttributes;
    CADESSignedAttributes = calloc(1, sizeof(*CADESSignedAttributes));
    
    
    /**SIGNING TIME **/    
    Attribute_t *atSigningTime;
    getCADESSigningTime(&atSigningTime, local);
    ret= ASN_SET_ADD(CADESSignedAttributes, atSigningTime);
    
    
    /**CONTENT TYPE **/
    Attribute_t *atContentType;
    getCADESContentType(&atContentType);
    ret= ASN_SET_ADD(CADESSignedAttributes, atContentType);
    
    /**MESSAGEDIGEST **/
    Attribute_t *atMessageDigest;
    getCADESMessageDigest(&atMessageDigest,messageDigest, lengthMessageDigest);
    ret= ASN_SET_ADD(CADESSignedAttributes, atMessageDigest);
    
    /**CONTENTHINT **/
    Attribute_t *atContentHint;
    getCADESContentHint(&atContentHint,
                        contentDescription);
    ret= ASN_SET_ADD(CADESSignedAttributes, atContentHint);
    
    /**SIGNATURE POLICY ID **/
    if(policyOID !=NULL && policyUri!=NULL){
        Attribute_t *atSigPolicyId;
        getCADESSigPolicyId(&atSigPolicyId, policyOID, policyHash, policyHashAlg, policyUri);
        ret= ASN_SET_ADD(CADESSignedAttributes, atSigPolicyId);
    
    }
    
    if(signingCertificateV2 == 0){
        /**SIGNING CERTIFICATE V1 **/
        Attribute_t *atSigningCertificateV1;
        getCADESSigningCertificateV1(&atSigningCertificateV1, certificateX509, certHash,lengthCertHash, policyOID, policyUri);
        ret = ASN_SET_ADD(CADESSignedAttributes, atSigningCertificateV1);
    }
    else{
        /**SIGNING CERTIFICATE V2 **/
        Attribute_t *atSigningCertificateV2;
        getCADESSigningCertificateV2(&atSigningCertificateV2, certificateX509, certHash, lengthCertHash, policyOID, policyUri, hashAlgorithm);
        ret = ASN_SET_ADD(CADESSignedAttributes, atSigningCertificateV2);
    }
    
    //COPIAMOS EL RESULTADO
    *at_ext = CADESSignedAttributes;
    
}
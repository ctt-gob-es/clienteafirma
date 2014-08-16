//
//  CADESSigner.c
//  SignSample02
//
//

#include <stdio.h>
#include "SignedData.h"
#include "NULL.h"
#include "CADESOID.h"
#include <openssl/x509.h>
#include "CADESSignerInfo.h"

void getSignedDataStructure(SignedData_t **sig_dat,
                            X509 *certificateX509,
                            char *contentData,
                            const char *certBuffer,
                            int certLenght,
                            char *dataSigned,
                            int lengthdataSigned,
                            char *messageDigest,
                            int  lengthMessageDigest,
                            char *contentDescription,
                            char *policyOID,
                            char *policyHash,
                            char *policyHashAlg,
                            char *policyUri,
                            char *certHash,                            
                            int lengthCertHash,
                            char *hashAlgorithm,
                            int signingCertificateV2,
                            char *signAlgorithm,
                            struct tm *local){
    
    int rec;
    //creamos el objeto signedData
    SignedData_t *signedData;
    signedData = calloc(1, sizeof(*signedData));
    
    /*****VERSION SIGNEDDATA*****/
    //creamos el objeto CMSVersion
    CMSVersion_t *version;
    version = calloc(1, sizeof(*version));
    version = CMSVersion_v1;
    signedData -> version = version;
    
    /*****DIGEST ALGORITHMS*****/
    DigestAlgorithmIdentifiers_t *digestAlgorithms;
    digestAlgorithms = calloc(1, sizeof(*digestAlgorithms));
    DigestAlgorithmIdentifier_t *digestAlgorithm;
    digestAlgorithm = calloc(1, sizeof(*digestAlgorithm));
    digestAlgorithm -> algorithm = makeOID(hashAlgorithm);
    NULL_t *null;
    null = calloc(1, sizeof(*null));
    digestAlgorithm -> parameters = ANY_new_fromType(&asn_DEF_NULL, null);
    
    rec = ASN_SET_ADD(&digestAlgorithms ->list, digestAlgorithm);
    
    signedData -> digestAlgorithms = *digestAlgorithms;
    
    
    /*****ENCAPCONTENTINFO*****/
    EncapsulatedContentInfo_t *encapsulatedContentInfo;
    encapsulatedContentInfo = calloc(1, sizeof(*encapsulatedContentInfo));
    
    ContentType_t *eContentType;
    eContentType = calloc(1,sizeof(*eContentType));
    *eContentType = makeOID(DATA_OID);
    
    encapsulatedContentInfo->eContentType = *eContentType;
    
    //NSString *contentData= @"datos";
    //NSString *contentData= NULL;
    
    if (contentData != NULL){
        OCTET_STRING_t *osContentData;
        osContentData = calloc(1,sizeof(*osContentData));
        OCTET_STRING_fromString(osContentData,contentData);
        encapsulatedContentInfo->eContent = osContentData;
    }
    
    signedData -> encapContentInfo = *encapsulatedContentInfo;
    
    /*****CERTIFICATES (OPTIONAL)*****/
    CertificateSet_t *certificateSet;
    certificateSet = calloc(1, sizeof(*certificateSet));
    certificateSet = ANY_new_fromBuf(certBuffer, certLenght);
    signedData -> certificates = certificateSet;
    
    
    /*****SIGNERINFO******/
    SignerInfos_t *CADESSignerInfos;
    getCADESSignerInfos(&CADESSignerInfos,
                        certificateX509,
                        dataSigned,
                        lengthdataSigned,
                        messageDigest,
                        lengthMessageDigest,
                        contentDescription,
                        policyOID,
                        policyHash,
                        policyHashAlg,
                        policyUri,
                        certHash,
                        lengthCertHash,
                        hashAlgorithm,
                        signingCertificateV2,
                        signAlgorithm,
                        local);
    signedData-> signerInfos = *CADESSignerInfos;
    
    *sig_dat = signedData;    
    
}
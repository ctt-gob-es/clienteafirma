//
//  CADESSignerInfo.c
//  CadesASN1Simplificado
//
//

#include <stdio.h>
#include "SignerInfos.h"
#include "SignerInfo.h"
#include "DigestAlgorithmIdentifier.h"
#include "DigestAlgorithmIdentifiers.h"
#include <openssl/x509.h>
#include "CADESOID.h"
#include "NULL.h"
#include "CertificateUtil.h"
#include "CADESSignedAttributes.h"

void getCADESSigerIdentifier(SignerIdentifier_t **at_ext, X509 *certificateX509)
{
    
    //ASN_DEBUG("Creamos el Signer Identifier");
    SignerIdentifier_t *atSignerIdentifier;
    atSignerIdentifier = calloc(1, sizeof(*atSignerIdentifier));
    
    IssuerAndSerialNumber_t *atIssuerAndSerialNumber;
    atIssuerAndSerialNumber = calloc(1, sizeof(*atIssuerAndSerialNumber));
    
    //CREAMOS EL ISSUER
    //ASN_DEBUG("Creamos el issuer");
    Name_t *atIssuer;
    atIssuer = calloc(1, sizeof(*atIssuer));
    atIssuer = getCertificateIssuerName(certificateX509);
    //xer_fprint(stdout, &asn_DEF_Name, atIssuer);
    
    //CREAMOS EL SERIAL NUMBER
    //ASN_DEBUG("Creamos el serial number");
    CertificateSerialNumber_t *atSerialNumber;
    atSerialNumber = calloc(1,sizeof(*atSerialNumber));
    atSerialNumber = getCertificateSerialNumber(certificateX509);
    
    //ASIGNAMOS EL ISSUER Y EL SERIAL
    atIssuerAndSerialNumber->issuer = *atIssuer;
    atIssuerAndSerialNumber->serialNumber = atSerialNumber;
    atSignerIdentifier = atIssuerAndSerialNumber;
    
    //ASN_DEBUG("Asignamos el Signer Identifier");
    *at_ext = atSignerIdentifier;
    
}

void getCADESSignerInfos( struct SignerInfos **at_ext,
                         X509 *certificateX509,
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
    
    int ec;
    SignerInfos_t *CADESSignerInfos;
    CADESSignerInfos = calloc(1,sizeof(*CADESSignerInfos));
    
    /*****SIGNERINFO******/
    SignerInfo_t *CADESSignerInfo;
    CADESSignerInfo = calloc(1,sizeof(*CADESSignerInfo));
    
    /*****VERSION*******/
    CMSVersion_t *CADESSignerInfoVersion;
    CADESSignerInfoVersion = calloc(1,sizeof(*CADESSignerInfoVersion));
    CADESSignerInfoVersion = CMSVersion_v1;
    CADESSignerInfo-> version = CADESSignerInfoVersion;
    
    
    /****SIGNERIDENTIFIER********/    
    SignerIdentifier_t *CADESSignerIdentifier;
    CADESSignerIdentifier = calloc(1,sizeof(*CADESSignerIdentifier));
    getCADESSigerIdentifier(&CADESSignerIdentifier, certificateX509);
    //xer_fprint(stdout, &asn_DEF_SignerIdentifier, CADESSignerIdentifier);
    CADESSignerInfo-> sid = *CADESSignerIdentifier;
    
    /****DIGESTALGORITHM ******/    
    DigestAlgorithmIdentifier_t *digestAlgorithm;
    digestAlgorithm = calloc(1, sizeof(*digestAlgorithm));
    digestAlgorithm -> algorithm = makeOID(hashAlgorithm);
    NULL_t *null;
    null = calloc(1, sizeof(*null));
    digestAlgorithm -> parameters = ANY_new_fromType(&asn_DEF_NULL, null);

    CADESSignerInfo -> digestAlgorithm = *digestAlgorithm;
    
    /****** SIGNEDATTRIBUTES ******/
    SignedAttributes_t *CADESSignedAttributes;
    getCADESSignedAttributes(&CADESSignedAttributes,
                             certificateX509,
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
                             local);
    
    
    CADESSignerInfo-> signedAttrs = CADESSignedAttributes;
    
    /***** SIGNATUREALGORITHM ******/
    SignatureAlgorithmIdentifier_t *signatureAlgorithmIdentifier;
    signatureAlgorithmIdentifier = calloc(1, sizeof(*signatureAlgorithmIdentifier));    
    signatureAlgorithmIdentifier -> algorithm = makeOID(signAlgorithm);
    signatureAlgorithmIdentifier -> parameters = ANY_new_fromType(&asn_DEF_NULL, null);
    
    CADESSignerInfo -> signatureAlgorithm = *signatureAlgorithmIdentifier;
    
    
    /***** SIGNATUREVALUE ******/
    SignatureValue_t *signatureValue;
    signatureValue = calloc(1, sizeof(*signatureValue));
    
    /************************************/
    /***** INVOCAMOS A FIRMA DIGITAL ****/
    /************************************/    
    
    //OCTET_STRING_fromString(&CADESSignerInfo ->signature, dataSigned);
    OCTET_STRING_fromBuf(&CADESSignerInfo ->signature, dataSigned, lengthdataSigned);
    
    ec= ASN_SET_ADD(CADESSignerInfos, CADESSignerInfo); 
    
    //xer_fprint(stdout, &asn_DEF_SignerInfos, CADESSignerInfos);
    
    *at_ext = CADESSignerInfos;
}
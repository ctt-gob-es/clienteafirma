//
//  SignedAttributes.c
//  CadesASN1Simplificado
//
//

#include <stdio.h>
#include "SignedAttributes.h"
#include <openssl/x509.h>

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
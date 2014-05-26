//
//  CADESSigPolicyI.c
//  SignSample02
//
//

#include <stdio.h>
#include "Attribute.h"
#include "CADESOID.h"
#include "SignaturePolicyId.h"
#include "NULL.h"
#include "IA5String.h"

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
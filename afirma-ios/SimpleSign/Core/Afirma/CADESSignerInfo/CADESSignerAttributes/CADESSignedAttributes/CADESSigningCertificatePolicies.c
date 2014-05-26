//
//  CADESSigningCertificatePolicies.c
//  SignSample02
//
//

#include <stdio.h>
#include "CADESOID.h"
#include "SigningCertificate.h"
#include "IA5String.h"

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
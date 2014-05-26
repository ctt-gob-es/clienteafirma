//
//  CADESMessageDigest.c
//  SignSample02
//
//

#include <stdio.h>
#include "Attribute.h"
#include "CADESOID.h"

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
//
//  ContentTypeAttribute.c
//  CadesASN1Simplificado
//
//

#include <stdio.h>
#include "Attribute.h"
#include "CADESOID.h"

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
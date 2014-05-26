//
//  CADESContentHint.c
//  SignSample02
//
//

#include <stdio.h>
#include "Attribute.h"
#include "ContentHints.h"
#include "asn_SEQUENCE_OF.h"
#include "CADESOID.h"

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
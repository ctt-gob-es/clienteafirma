//
//  SigningTime.c
//  CadesASN1Simplificado
//
//

#include <stdio.h>
#include "UTCTime.h"
#include "Attribute.h"
#include "time.h"
#include "CADESOID.h"

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
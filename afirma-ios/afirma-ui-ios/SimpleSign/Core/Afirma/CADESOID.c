//
//  CADESOID.c
//  SignSample02
//
//

#include <stdio.h>
#include "OBJECT_IDENTIFIER.h"


OBJECT_IDENTIFIER_t makeOID(const char *oidValue)
{
	long tempArcs[1];
	long tempArcs2[50];
	long *realArcs = 0x00;

	int realLength = OBJECT_IDENTIFIER_parse_arcs(oidValue, -1, tempArcs, 1, 0x00);
    
    
	if (realLength > 1) {
		//tempArcs2 = realLength;
        OBJECT_IDENTIFIER_parse_arcs(oidValue, -1, tempArcs2, realLength, 0x00);
		realArcs = tempArcs2;
        
	} else {
		realArcs = &tempArcs[0]; // NO DEBERÍA ENTRAR NUNCA POR AQUI
	}
   
	OBJECT_IDENTIFIER_t oid;
	oid.buf  = 0x00;
	oid.size = 0;
	OBJECT_IDENTIFIER_set_arcs(&oid, realArcs, sizeof(unsigned long), realLength);
    
	//if (0x00 != tempArcs2)
	//	tempArcs2 = 0x00;
    
	return oid;
}
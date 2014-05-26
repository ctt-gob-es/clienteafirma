//
//  CADESOID.h
//  SignSample02
//
//
#import "OBJECT_IDENTIFIER.h"

#if !defined(__CADESOID_INCLUDED__)
#define __CADESOID_INCLUDED__

#define base_oid                "1.2.840.113549.1"

#define DATA_OID                base_oid ".7.1"
#define SIGNED_DATA_OID         base_oid ".7.2"
#define CONTENT_TYPE_OID        base_oid ".9.3"
#define MESSAGE_DIGEST_OID      base_oid ".9.4"
#define SIGNING_TIME_OID        base_oid ".9.5"
#define CONTENT_HINT_OID        base_oid ".9.16.2.4"
#define SIGNINGCERTIFICATEV1    base_oid ".9.16.2.12"
#define SIG_POLICY_ID_OID       base_oid ".9.16.2.15"
#define SIGNINGCERTIFICATEV2    base_oid ".9.16.2.47"
#define SPURI_OID               base_oid ".9.16.5.1"

#define SHA1_OID                "1.3.14.3.2.26"
#define SHA256_OID                "2.16.840.1.101.3.4.2.1"
#define SHA384_OID                "2.16.840.1.101.3.4.2.2"
#define SHA512_OID                "2.16.840.1.101.3.4.2.3"

#define RSA_OID                 base_oid ".1.1"


#define CPS_OID                 "1.3.6.1.5.5.7.2.1"

OBJECT_IDENTIFIER_t makeOID(const char *oidValue);

#endif

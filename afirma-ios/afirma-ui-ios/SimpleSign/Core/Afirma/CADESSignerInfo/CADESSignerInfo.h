//
//  CADESSignerInfo.h
//  CadesASN1Simplificado
//
//
#include <openssl/x509.h>

#ifndef CadesASN1Simplificado_CADESSignerInfo_h
#define CadesASN1Simplificado_CADESSignerInfo_h

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
                         struct tm *local);

#endif

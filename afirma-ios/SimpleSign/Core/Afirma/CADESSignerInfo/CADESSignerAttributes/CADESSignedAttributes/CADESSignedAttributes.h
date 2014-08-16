//
//  CADESSignedAttributes.h
//  CadesASN1Simplificado
//
//

#ifndef CadesASN1Simplificado_SignedAttributes_h
#define CadesASN1Simplificado_SignedAttributes_h

void getCADESSignedAttributes( struct SignedAttributes **at_ext,
                              X509 *certificateX509,
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
                              struct tm *local);

#endif

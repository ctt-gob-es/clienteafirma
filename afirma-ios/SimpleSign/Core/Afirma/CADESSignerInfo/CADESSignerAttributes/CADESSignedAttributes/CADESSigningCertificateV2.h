//
//  CADESSigningCertificateV2.h
//  SignSample02
//
//

#ifndef SignSample02_CADESSigningCertificateV2_h
#define SignSample02_CADESSigningCertificateV2_h

void getCADESSigningCertificateV2( Attribute_t **at_ext,
                                  X509 *certificateX509,
                                  char *certHash,
                                  int lengthCertHash,
                                  char *policyOID,
                                  char *policyUri,
                                  char *hashAlgorithm);

#endif

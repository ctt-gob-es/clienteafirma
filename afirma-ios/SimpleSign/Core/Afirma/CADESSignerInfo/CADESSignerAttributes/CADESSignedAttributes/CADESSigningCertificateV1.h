//
//  CADESSigningCertificateV1.h
//  SignSample02
//
//

#ifndef SignSample02_CADESSigningCertificateV1_h
#define SignSample02_CADESSigningCertificateV1_h

void getCADESSigningCertificateV1( Attribute_t **at_ext,
                                  X509 *certificateX509,
                                  char *certHash,
                                  int lengthCertHash,
                                  char *policyOID,
                                  char *policyUri);

#endif

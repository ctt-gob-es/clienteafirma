//
//  CADESSignerIdentifier.h
//  SignSample02
//
//

#ifndef SignSample02_CADESSignerIdentifier_h
#define SignSample02_CADESSignerIdentifier_h
#include "SignerIdentifier.h"

void getCADESSigerIdentifier(SignerIdentifier_t **at_ext, X509 *certificateX509);
Name_t* getCertificateIssuerName(X509 *certificateX509);
CertificateSerialNumber_t* getCertificateSerialNumber(X509 *certificateX509);

#endif

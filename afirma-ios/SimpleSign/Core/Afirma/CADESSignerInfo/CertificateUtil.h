//
//  CertificateUtil.h
//  SignSample02
//
//  Created by Tomas Garcia-Meras on 24/8/14.
//  Copyright (c) 2014 Atos. All rights reserved.
//

#ifndef SignSample02_CertificateUtil_h
#define SignSample02_CertificateUtil_h

Name_t* getCertificateIssuerName(X509 *certificateX509);
CertificateSerialNumber_t* getCertificateSerialNumber(X509 *certificateX509);

#endif

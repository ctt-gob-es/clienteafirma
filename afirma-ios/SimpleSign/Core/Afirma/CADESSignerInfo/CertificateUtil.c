//
//  CertificateUtil.c
//  SignSample02
//
//  Created by Tomas Garcia-Meras on 24/8/14.
//  Copyright (c) 2014 Atos. All rights reserved.
//

#include <stdio.h>
#include <openssl/x509.h>
#include "CertificateSerialNumber.h"
#include "Name.h"
#include "CertificateUtil.h"

Name_t* getCertificateIssuerName(X509 *certificateX509)
{
    //ASN_DEBUG("Obteniendo el issuer del certificado");
    //CREAMOS EL ISSUER
    Name_t *atIssuer;
    atIssuer = calloc(1, sizeof(*atIssuer));
    
    if (certificateX509 != NULL) {
        X509_NAME *issuerX509Name = X509_get_issuer_name(certificateX509);
        BUF_MEM mem = *issuerX509Name->bytes;
        ber_decode(0, &asn_DEF_Name, (void **)&atIssuer, mem.data, mem.length);
    }
    //ASN_DEBUG("Obtenido el issuer del certificado");
    return atIssuer;
}

CertificateSerialNumber_t* getCertificateSerialNumber(X509 *certificateX509)
{
    //CREAMOS EL SERIALNUMBER
    CertificateSerialNumber_t *certSerialNumber;
    certSerialNumber = calloc(1, sizeof(*certSerialNumber));
    
    if (certificateX509 != NULL) {
        certSerialNumber = (long)ASN1_INTEGER_get(X509_get_serialNumber(certificateX509));
    }
    return certSerialNumber;
}

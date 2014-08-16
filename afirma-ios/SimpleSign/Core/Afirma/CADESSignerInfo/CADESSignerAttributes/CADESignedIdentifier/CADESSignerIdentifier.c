//
//  CADESSignerIdentifier.c
//  SignSample02
//
//

#include <stdio.h>
#include "SignerIdentifier.h"
#include <openssl/x509.h>
#include "asn_internal.h"

Name_t* getCertificateIssuerName(X509 *certificateX509);
CertificateSerialNumber_t* getCertificateSerialNumber(X509 *certificateX509);

void getCADESSigerIdentifier(SignerIdentifier_t **at_ext, X509 *certificateX509){
    
    //ASN_DEBUG("Creamos el Signer Identifier");
    SignerIdentifier_t *atSignerIdentifier;
    atSignerIdentifier = calloc(1, sizeof(*atSignerIdentifier));
            
    IssuerAndSerialNumber_t *atIssuerAndSerialNumber;
    atIssuerAndSerialNumber = calloc(1, sizeof(*atIssuerAndSerialNumber));
    
    //CREAMOS EL ISSUER
    //ASN_DEBUG("Creamos el issuer");
    Name_t *atIssuer;
    atIssuer = calloc(1, sizeof(*atIssuer));
    atIssuer = getCertificateIssuerName(certificateX509);
    //xer_fprint(stdout, &asn_DEF_Name, atIssuer);
    
    //CREAMOS EL SERIAL NUMBER
    //ASN_DEBUG("Creamos el serial number");
    CertificateSerialNumber_t *atSerialNumber;
    atSerialNumber = calloc(1,sizeof(*atSerialNumber));
    atSerialNumber = getCertificateSerialNumber(certificateX509);

    //ASIGNAMOS EL ISSUER Y EL SERIAL
    atIssuerAndSerialNumber->issuer = *atIssuer;
    atIssuerAndSerialNumber->serialNumber = atSerialNumber;    
    atSignerIdentifier = atIssuerAndSerialNumber;
    
    //ASN_DEBUG("Asignamos el Signer Identifier");
    *at_ext = atSignerIdentifier;
    
}

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
    //ASN_DEBUG("Obteniendo el serial number del certificado");
    //CREAMOS EL SERIALNUMBER
    CertificateSerialNumber_t *certSerialNumber;
    certSerialNumber = calloc(1, sizeof(*certSerialNumber));
    
    if (certificateX509 != NULL) {
        certSerialNumber = (long)ASN1_INTEGER_get(X509_get_serialNumber(certificateX509));
    }
    //ASN_DEBUG("Obtenido el el serial del certificado");
    return certSerialNumber;
}
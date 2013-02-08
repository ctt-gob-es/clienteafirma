package org.bouncycastle.asn1.cryptopro;

import org.bouncycastle.asn1.ASN1ObjectIdentifier;

public interface CryptoProObjectIdentifiers
{
    // GOST Algorithms OBJECT IDENTIFIERS :
    // { iso(1) member-body(2) ru(643) rans(2) cryptopro(2)}
    static final String                 GOST_id              = "1.2.643.2.2";

    static final ASN1ObjectIdentifier    gostR3411          = new ASN1ObjectIdentifier(GOST_id+".9");
    
    static final ASN1ObjectIdentifier    gostR28147_cbc     = new ASN1ObjectIdentifier(GOST_id+".21");

    static final ASN1ObjectIdentifier    gostR3410_94       = new ASN1ObjectIdentifier(GOST_id+".20");
    static final ASN1ObjectIdentifier    gostR3410_2001     = new ASN1ObjectIdentifier(GOST_id+".19");
    static final ASN1ObjectIdentifier    gostR3411_94_with_gostR3410_94   = new ASN1ObjectIdentifier(GOST_id+".4");
    static final ASN1ObjectIdentifier    gostR3411_94_with_gostR3410_2001 = new ASN1ObjectIdentifier(GOST_id+".3");

    // { iso(1) member-body(2) ru(643) rans(2) cryptopro(2) hashes(30) }
    static final ASN1ObjectIdentifier    gostR3411_94_CryptoProParamSet = new ASN1ObjectIdentifier(GOST_id+".30.1");

    // { iso(1) member-body(2) ru(643) rans(2) cryptopro(2) signs(32) }
    static final ASN1ObjectIdentifier    gostR3410_94_CryptoPro_A     = new ASN1ObjectIdentifier(GOST_id+".32.2");
    static final ASN1ObjectIdentifier    gostR3410_94_CryptoPro_B     = new ASN1ObjectIdentifier(GOST_id+".32.3");
    static final ASN1ObjectIdentifier    gostR3410_94_CryptoPro_C     = new ASN1ObjectIdentifier(GOST_id+".32.4");
    static final ASN1ObjectIdentifier    gostR3410_94_CryptoPro_D     = new ASN1ObjectIdentifier(GOST_id+".32.5");

    // { iso(1) member-body(2) ru(643) rans(2) cryptopro(2) exchanges(33) }
    static final ASN1ObjectIdentifier    gostR3410_94_CryptoPro_XchA  = new ASN1ObjectIdentifier(GOST_id+".33.1");
    static final ASN1ObjectIdentifier    gostR3410_94_CryptoPro_XchB  = new ASN1ObjectIdentifier(GOST_id+".33.2");
    static final ASN1ObjectIdentifier    gostR3410_94_CryptoPro_XchC  = new ASN1ObjectIdentifier(GOST_id+".33.3");

    //{ iso(1) member-body(2)ru(643) rans(2) cryptopro(2) ecc-signs(35) }
    static final ASN1ObjectIdentifier    gostR3410_2001_CryptoPro_A = new ASN1ObjectIdentifier(GOST_id+".35.1");
    static final ASN1ObjectIdentifier    gostR3410_2001_CryptoPro_B = new ASN1ObjectIdentifier(GOST_id+".35.2");
    static final ASN1ObjectIdentifier    gostR3410_2001_CryptoPro_C = new ASN1ObjectIdentifier(GOST_id+".35.3");

    // { iso(1) member-body(2) ru(643) rans(2) cryptopro(2) ecc-exchanges(36) }
    static final ASN1ObjectIdentifier    gostR3410_2001_CryptoPro_XchA  = new ASN1ObjectIdentifier(GOST_id+".36.0");
    static final ASN1ObjectIdentifier    gostR3410_2001_CryptoPro_XchB  = new ASN1ObjectIdentifier(GOST_id+".36.1");
    
    static final ASN1ObjectIdentifier    gost_ElSgDH3410_default    = new ASN1ObjectIdentifier(GOST_id+".36.0");
    static final ASN1ObjectIdentifier    gost_ElSgDH3410_1          = new ASN1ObjectIdentifier(GOST_id+".36.1");
}

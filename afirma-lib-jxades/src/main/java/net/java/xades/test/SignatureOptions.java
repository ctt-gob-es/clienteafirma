package net.java.xades.test;

import java.security.KeyStore;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;

public class SignatureOptions
{
    private KeyStore keystore;
    private X509Certificate certificate;
    private PrivateKey privateKey;

    public KeyStore getKeystore()
    {
        return keystore;
    }

    public void setKeystore(KeyStore keystore)
    {
        this.keystore = keystore;
    }

    public X509Certificate getCertificate()
    {
        return certificate;
    }

    public void setCertificate(X509Certificate certificate)
    {
        this.certificate = certificate;
    }

    public PrivateKey getPrivateKey()
    {
        return privateKey;
    }

    public void setPrivateKey(PrivateKey privateKey)
    {
        this.privateKey = privateKey;
    }
}

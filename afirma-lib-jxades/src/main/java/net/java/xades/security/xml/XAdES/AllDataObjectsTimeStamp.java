package net.java.xades.security.xml.XAdES;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.SignatureException;

import org.w3c.dom.Document;

public interface AllDataObjectsTimeStamp 
{
    public byte[] generateEncapsulatedTimeStamp(Document parent, String tsaURL) throws NoSuchAlgorithmException, SignatureException, IOException;
}

package net.java.xades.security.xml.XAdES;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.SignatureException;

import javax.xml.crypto.dsig.XMLSignature;

import net.java.xades.security.timestamp.TimeStampFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class SignatureTimeStampImpl implements SignatureTimeStamp
{
    private byte[] data;

    public SignatureTimeStampImpl(byte[] data)
    {
        this.data = data;
    }

    private Node getSignatureValue(Document base)
    {
        NodeList nl = base.getElementsByTagNameNS(XMLSignature.XMLNS, "SignatureValue");

        if (nl.getLength() > 0)
        {
            return nl.item(0);
        }
        else
        {
            return null;
        }
    }

    public byte[] generateEncapsulatedTimeStamp(Document parent, String tsaURL)
            throws NoSuchAlgorithmException, SignatureException, IOException
    {
        Node signatureValue = getSignatureValue(parent);

        return TimeStampFactory.getTimeStamp(tsaURL, this.data, true);
    }
}

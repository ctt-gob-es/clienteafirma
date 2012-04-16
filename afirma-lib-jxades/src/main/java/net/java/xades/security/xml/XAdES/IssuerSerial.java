package net.java.xades.security.xml.XAdES;

import java.math.BigInteger;
import java.security.cert.X509Certificate;

import javax.security.auth.x500.X500Principal;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/*
 <IssuerSerial>
 <X509IssuerName></X509IssuerName>
 <X509SerialNumber><X509SerialNumber>
 </IssuerSerial>
 */

/**
 * 
 * @author miro
 */
public class IssuerSerial extends XAdESStructure
{
    public IssuerSerial(XAdESStructure parent, X509Certificate cert, String xadesPrefix,
            String xadesNamespace, String xmlSignaturePrefix)
    {
        this(parent, cert.getIssuerX500Principal(), cert.getSerialNumber(), xadesPrefix,
                xadesNamespace, xmlSignaturePrefix);
    }

    public IssuerSerial(XAdESStructure parent, X500Principal issuer, BigInteger serialNumber,
            String xadesPrefix, String xadesNamespace, String xmlSignaturePrefix)
    {
        super(parent, "IssuerSerial", xadesPrefix, xadesNamespace, xmlSignaturePrefix);

        Element thisElement = getElement();

        Element element = createElement("X509IssuerName");
        thisElement.appendChild(element);
        element.setTextContent(issuer.getName());

        element = createElement("X509SerialNumber");
        thisElement.appendChild(element);
        element.setTextContent(serialNumber.toString());
    }

    public IssuerSerial(Node node, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public String getIssuerName()
    {
        return getChildElementTextContent("X509IssuerName");
    }

    public String getSerialNumber()
    {
        return getChildElementTextContent("X509SerialNumber");
    }
}

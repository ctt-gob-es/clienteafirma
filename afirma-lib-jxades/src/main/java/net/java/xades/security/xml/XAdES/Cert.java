package net.java.xades.security.xml.XAdES;

import java.security.GeneralSecurityException;
import java.security.cert.X509Certificate;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/*
 <Cert>
 <CertDigest>
 <DigestMethod Algorithm="" />
 <DigestValue></DigestValue>
 </CertDigest>
 <IssuerSerial>
 <X509IssuerName></X509IssuerName>
 <X509SerialNumber><X509SerialNumber>
 </IssuerSerial>
 </Cert>
 */

/**
 * 
 * @author miro
 */
public class Cert extends XAdESStructure
{
    private CertDigest certDigest;
    private IssuerSerial issuerSerial;

    public Cert(XAdESStructure parent, X509Certificate cert, String xadesPrefix,
            String xadesNamespace, String xmlSignaturePrefix) throws GeneralSecurityException
    {
        super(parent, "Cert", xadesPrefix, xadesNamespace, xmlSignaturePrefix);

        Element thisElement = getElement();

        certDigest = new CertDigest(this, cert, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
        issuerSerial = new IssuerSerial(this, cert, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public Cert(Node node, String xadesPrefix, String xadesNamespace, String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public CertDigest getCertDigest()
    {
        if (certDigest == null)
        {
            Element element = getChildElementNS("CertDigest");
            if (element != null)
                certDigest = new CertDigest(element, xadesPrefix, xadesNamespace,
                        xmlSignaturePrefix);
        }

        return certDigest;
    }

    public IssuerSerial getIssuerSerial()
    {
        if (issuerSerial == null)
        {
            Element element = getChildElementNS("IssuerSerial");
            if (element != null)
                issuerSerial = new IssuerSerial(element, xadesPrefix, xadesNamespace,
                        xmlSignaturePrefix);
        }

        return issuerSerial;
    }
}

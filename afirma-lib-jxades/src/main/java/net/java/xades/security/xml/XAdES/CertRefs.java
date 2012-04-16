package net.java.xades.security.xml.XAdES;

import java.security.GeneralSecurityException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/*
 <CertRefs Id="">
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
 </CertRefs>
 */

/**
 * 
 * @author miro
 */
public class CertRefs extends XAdESStructure
{
    private List<Cert> certs;

    public CertRefs(XAdESStructure parent, Collection<X509Certificate> certificates,
            String signatureIdPrefix, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix) throws GeneralSecurityException
    {
        super(parent, "CertRefs", xadesPrefix, xadesNamespace, xmlSignaturePrefix);

        if (certificates == null || certificates.isEmpty())
            throw new IllegalArgumentException(
                    "The certificates collection can not be NULL or empty.");

        Element thisElement = getElement();
        if (signatureIdPrefix != null)
        {
            setAttribute("Id", signatureIdPrefix + "-CertRefs");
        }

        certs = new ArrayList<Cert>(certificates.size());

        for (X509Certificate certificate : certificates)
        {
            Cert cert = new Cert(this, certificate, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
            certs.add(cert);
        }
    }

    public CertRefs(Node node, String xadesPrefix, String xadesNamespace, String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public List<Cert> getCerts()
    {
        if (certs == null)
        {
            List<Element> elements = getChildElementsNS("Cert");
            if (elements != null && elements.size() > 0)
            {
                certs = new ArrayList<Cert>(elements.size());
                for (Element element : elements)
                {
                    certs.add(new Cert(element, xadesPrefix, xadesNamespace, xmlSignaturePrefix));
                }
            }
            else
            {
                certs = Collections.<Cert> emptyList();
            }
        }

        return certs;
    }
}

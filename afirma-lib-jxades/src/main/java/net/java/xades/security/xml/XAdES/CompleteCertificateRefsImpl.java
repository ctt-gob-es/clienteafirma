package net.java.xades.security.xml.XAdES;

import java.security.GeneralSecurityException;
import java.security.cert.X509Certificate;
import java.util.Collection;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * 
 * @author miro
 */
public class CompleteCertificateRefsImpl extends XAdESStructure implements CompleteCertificateRefs
{
    private CertRefs certRefs;

    public CompleteCertificateRefsImpl(XAdESStructure parent,
            Collection<X509Certificate> caCertificates, String signatureIdPrefix,
            String xadesPrefix, String xadesNamespace, String xmlSignaturePrefix)
            throws GeneralSecurityException
    {
        super(parent, "CompleteCertificateRefs", xadesPrefix, xadesNamespace, xmlSignaturePrefix);

        if (caCertificates == null || caCertificates.isEmpty())
            throw new IllegalArgumentException(
                    "The CA Certificates collection can not be NULL or empty.");

        certRefs = new CertRefs(this, caCertificates, signatureIdPrefix, xadesPrefix,
                xadesNamespace, xmlSignaturePrefix);
    }

    public CompleteCertificateRefsImpl(Node node, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public CertRefs getCertRefs()
    {
        if (certRefs == null)
        {
            Element element = getChildElementNS("CertRefs");
            if (element != null)
            {
                certRefs = new CertRefs(element, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
            }
        }

        return certRefs;
    }
}

package net.java.xades.security.xml.XAdES;

import java.security.GeneralSecurityException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/*
 */

/**
 * 
 * @author miro
 */
public class UnsignedSignatureProperties extends XAdESStructure
{
    private CompleteCertificateRefs completeCertificateRefs;
    private CompleteRevocationRefs completeRevocationRefs;

    public UnsignedSignatureProperties(UnsignedProperties up, String xadesPrefix,
            String xadesNamespace, String xmlSignaturePrefix)
    {
        super(up, XAdES.Element.UNSIGNED_SIGNATURE_PROPERTIES.getElementName(), xadesPrefix,
                xadesNamespace, xmlSignaturePrefix);
    }

    public UnsignedSignatureProperties(Node node, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public CompleteCertificateRefs getCompleteCertificateRefs()
    {
        if (completeCertificateRefs == null)
        {
            Element element = getChildElementNS("CompleteCertificateRefs");
            if (element != null)
                completeCertificateRefs = new CompleteCertificateRefsImpl(element, xadesPrefix,
                        xadesNamespace, xmlSignaturePrefix);
        }

        return completeCertificateRefs;
    }

    public void setCompleteCertificateRefs(Collection<X509Certificate> caCertificates,
            String signatureIdPrefix) throws GeneralSecurityException
    {
        completeCertificateRefs = getCompleteCertificateRefs();
        if (completeCertificateRefs != null)
            throw new UnsupportedOperationException(
                    "The collection of CA Certificates already exists.");

        completeCertificateRefs = new CompleteCertificateRefsImpl(this, caCertificates,
                signatureIdPrefix, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public CompleteRevocationRefs getCompleteRevocationRefs()
    {
        if (completeRevocationRefs == null)
        {
            Element element = getChildElementNS("CompleteRevocationRefs");
            if (element != null)
                completeRevocationRefs = new CompleteRevocationRefsImpl(element, xadesPrefix,
                        xadesNamespace, xmlSignaturePrefix);
        }

        return completeRevocationRefs;
    }

    // public void setCompleteRevocationRefs(CertValidationInfo certValidationInfo,
    // String signatureIdPrefix)
    // throws GeneralSecurityException
    // {
    // completeRevocationRefs = getCompleteRevocationRefs();
    // if(completeRevocationRefs != null)
    // throw new UnsupportedOperationException("The collection of CA Certificates already exists.");
    //
    // completeRevocationRefs = new CompleteRevocationRefsImpl(this,
    // certValidationInfo,
    // signatureIdPrefix);
    // }

    public void setSignatureTimeStamp(ArrayList<SignatureTimeStamp> signatureTimeStamp,
            String tsaURL)
    {
        for (SignatureTimeStamp sts : signatureTimeStamp)
        {
            new SignatureTimeStampDetails(this, sts, xadesPrefix, xadesNamespace,
                    xmlSignaturePrefix, tsaURL);
        }
    }
}

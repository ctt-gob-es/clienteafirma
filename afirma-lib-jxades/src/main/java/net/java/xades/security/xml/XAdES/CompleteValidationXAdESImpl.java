package net.java.xades.security.xml.XAdES;

import java.security.cert.X509Certificate;
import java.util.Collection;

import org.w3c.dom.Element;

/*
 <ds:Signature ID?>
 ...
 <ds:Object>
 <QualifyingProperties>
 ...
 <UnsignedProperties>
 <UnsignedSignatureProperties>
 (CompleteCertificateRefs)
 (CompleteRevocationRefs)
 (AttributeCertificateRefs)?
 (AttributeRevocationRefs)?
 </UnsignedSignatureProperties>
 </UnsignedProperties>
 </QualifyingProperties>
 </ds:Object>
 </ds:Signature>-
 */

/**
 * 
 * @author miro
 */
public class CompleteValidationXAdESImpl extends TimestampXAdESImpl implements XAdES_C
{

    /*
     * public CompleteValidationXAdESImpl(Element baseElement, boolean useExplicitPolicy) {
     * super(baseElement, useExplicitPolicy); }
     */

    public CompleteValidationXAdESImpl(Element baseElement, boolean readOnlyMode,
            String xadesPrefix, String xadesNamespace, String xmlSignaturePrefix,
            String digestMethod)
    {
        super(baseElement, readOnlyMode, xadesPrefix, xadesNamespace, xmlSignaturePrefix,
                digestMethod);
    }

    public CompleteCertificateRefs getCompleteCertificateRefs()
    {
        return (CompleteCertificateRefs) data.get(XAdES.Element.COMPLETE_CERTIFICATE_REFS);
    }

    public void setCompleteCertificateRefs(Collection<X509Certificate> caCertificates)
    {
        if (readOnlyMode)
        {
            throw new UnsupportedOperationException("Set Method is not allowed. Read-only mode.");
        }

        data.put(XAdES.Element.COMPLETE_CERTIFICATE_REFS, caCertificates);
    }

    public CompleteRevocationRefs getCompleteRevocationRefs()
    {
        return (CompleteRevocationRefs) data.get(XAdES.Element.COMPLETE_REVOCATION_REFS);
    }

    // public void setCompleteRevocationRefs(CertValidationInfo certValidationInfo)
    // {
    // if (readOnlyMode)
    // {
    // throw new UnsupportedOperationException("Set Method is not allowed. Read-only mode.");
    // }
    //
    // data.put(XAdES.Element.COMPLETE_REVOCATION_REFS, certValidationInfo);
    // }
}

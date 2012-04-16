package net.java.xades.security.xml.XAdES;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/*
 <CRLRef>
 <DigestAlgAndValue>
 <DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />
 <DigestValue>...</DigestValue>
 </DigestAlgAndValue>
 <CRLIdentifier URI="#Signature_1_EncapsulatedCRLValue_1">
 <Issuer>...</Issuer>
 <IssueTime>...</IssueTime>
 <Number>...</Number>
 </CRLIdentifier>
 <ValidationResult />
 </CRLRef>
 */

/**
 * 
 * @author miro
 */
public class CRLRef extends XAdESStructure
{
    private DigestAlgAndValue digestAlgAndValue;
    private CRLIdentifier crlIdentifier;
    private ValidationResult validationResult;

    // public CRLRef(XAdESStructure parent, XAdESRevocationStatus revocationStatus)
    // throws GeneralSecurityException
    // {
    // super(parent, "CRLRef");
    //
    // Element thisElement = getElement();
    //
    // X509CRL crl = revocationStatus.getCheckedCRL();
    // X509CRLEntry crlEntry = revocationStatus.getCRLEntry();
    //
    // DigestAlgAndValue digestAlgAndValue;
    // digestAlgAndValue = new DigestAlgAndValue(this, crl);
    //
    // CRLIdentifier crlIdentifier = new CRLIdentifier(this, revocationStatus);
    //
    // ValidationResult validationResult;
    // validationResult = new ValidationResult(this, revocationStatus);
    // }

    public CRLRef(Node node, String xadesPrefix, String xadesNamespace, String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public CRLIdentifier getCRLIdentifier()
    {
        if (crlIdentifier == null)
        {
            Element element = getChildElementNS("CRLIdentifier");
            if (element != null)
                crlIdentifier = new CRLIdentifier(element, xadesPrefix, xadesNamespace,
                        xmlSignaturePrefix);
        }

        return crlIdentifier;
    }

    public DigestAlgAndValue getDigestAlgAndValue()
    {
        if (digestAlgAndValue == null)
        {
            Element element = getChildElementNS("DigestAlgAndValue");
            if (element != null)
                digestAlgAndValue = new DigestAlgAndValue(element, xadesPrefix, xadesNamespace,
                        xmlSignaturePrefix);
        }

        return digestAlgAndValue;
    }

    public ValidationResult getValidationResult()
    {
        if (validationResult == null)
        {
            Element element = getChildElementNS("ValidationResult");
            if (element != null)
                validationResult = new ValidationResult(element, xadesPrefix, xadesNamespace,
                        xmlSignaturePrefix);
        }

        return validationResult;
    }

}

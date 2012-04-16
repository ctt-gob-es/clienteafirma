package net.java.xades.security.xml.XAdES;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/*
 <OCSPRef>
 <OCSPIdentifier URI= >
 <ResponderID>
 <ByName>String of X500Principal Name</ByName>
 or
 <ByKey>base64Binary of PublicKey DER value</ByKey>
 </ResponderID>
 <ProducedAt />
 </OCSPIdentifier>
 <DigestAlgAndValue>
 <DigestMethod Algorithm= />
 <DigestValue />
 </DigestAlgAndValue>
 <ValidationResult />
 </OCSPRef>
 */

/**
 * 
 * @author miro
 */
public class OCSPRef extends XAdESStructure
{
    private OCSPIdentifier ocspIdentifier;
    private DigestAlgAndValue digestAlgAndValue;
    private ValidationResult validationResult;

    // public OCSPRef(XAdESStructure parent, XAdESRevocationStatus revocationStatus)
    // throws GeneralSecurityException
    // {
    // super(parent, "OCSPRef");
    //
    // Element thisElement = getElement();
    //
    // OCSPIdentifier ocspIdentifier;
    // OCSPResponse ocspResponse = revocationStatus.getOCSPResponse();
    // URI ocspResponderURI = revocationStatus.getOCSPResponderURI();
    // ocspIdentifier = new OCSPIdentifier(this, ocspResponse, ocspResponderURI);
    //
    // DigestAlgAndValue digestAlgAndValue;
    // digestAlgAndValue = new DigestAlgAndValue(this, ocspResponse);
    //
    // ValidationResult validationResult;
    // validationResult = new ValidationResult(this, revocationStatus);
    // }

    public OCSPRef(Node node, String xadesPrefix, String xadesNamespace, String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public OCSPIdentifier getOCSPIdentifier()
    {
        if (ocspIdentifier == null)
        {
            Element element = getChildElementNS("OCSPIdentifier");
            if (element != null)
                ocspIdentifier = new OCSPIdentifier(element, xadesPrefix, xadesNamespace,
                        xmlSignaturePrefix);
        }

        return ocspIdentifier;
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

package net.java.xades.security.xml.XAdES;

import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/*
 <CRLRefs>
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
 </CRLRefs>
 */

/**
 * 
 * @author miro
 */
public class CRLRefs extends XAdESStructure
{
    private List<CRLRef> crlRefs;

    // public CRLRefs(XAdESStructure parent,
    // CertValidationInfo certValidationInfo)
    // throws GeneralSecurityException
    // {
    // super(parent, "CRLRefs");
    //
    // if(certValidationInfo == null)
    // throw new IllegalArgumentException("The CertValidationInfo can not be NULL.");
    //
    // Element thisElement = getElement();
    //
    // XAdESCertPathValidatorResult validatorResult;
    // validatorResult = certValidationInfo.getCertPathValidatorResult();
    // if(validatorResult != null)
    // {
    // for(XAdESRevocationStatus revocationStatus : validatorResult.getXAdESRevocationStatuses())
    // {
    // if(revocationStatus.getCheckedCRL() != null)
    // {
    // CRLRef crlRef = new CRLRef(this, revocationStatus);
    // }
    // }
    // }
    // }

    public CRLRefs(Node node, String xadesPrefix, String xadesNamespace, String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public List<CRLRef> getCRLRefs()
    {
        if (crlRefs == null)
        {
            List<Element> elements = getChildElementsNS("CRLRef");
            if (elements != null && elements.size() > 0)
            {
                crlRefs = new ArrayList<CRLRef>(elements.size());
                for (Element element : elements)
                {
                    crlRefs
                            .add(new CRLRef(element, xadesPrefix, xadesNamespace,
                                    xmlSignaturePrefix));
                }
            }
            else
            {
                crlRefs = Collections.<CRLRef> emptyList();
            }
        }

        return crlRefs;
    }
}

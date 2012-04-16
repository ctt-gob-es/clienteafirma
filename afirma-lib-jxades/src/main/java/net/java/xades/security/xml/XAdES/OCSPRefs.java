package net.java.xades.security.xml.XAdES;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/*
 <OCSPRefs>
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
 </OCSPRefs>
 */

/**
 * 
 * @author miro
 */
public class OCSPRefs extends XAdESStructure
{
    private List<OCSPRef> ocspRefs;

    // public OCSPRefs(XAdESStructure parent,
    // CertValidationInfo certValidationInfo)
    // throws GeneralSecurityException
    // {
    // super(parent, "OCSPRefs");
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
    // if(revocationStatus.getOCSPResponse() != null)
    // {
    // OCSPRef ocspRef = new OCSPRef(this, revocationStatus);
    // }
    // }
    // }
    // }

    public OCSPRefs(Node node, String xadesPrefix, String xadesNamespace, String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public List<OCSPRef> getOCSPRefs()
    {
        if (ocspRefs == null)
        {
            List<Element> elements = getChildElementsNS("OCSPRef");
            if (elements != null && elements.size() > 0)
            {
                ocspRefs = new ArrayList<OCSPRef>(elements.size());
                for (Element element : elements)
                {
                    ocspRefs.add(new OCSPRef(element, xadesPrefix, xadesNamespace,
                            xmlSignaturePrefix));
                }
            }
            else
            {
                ocspRefs = Collections.<OCSPRef> emptyList();
            }
        }

        return ocspRefs;
    }
}

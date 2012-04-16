package net.java.xades.security.xml.XAdES;

import java.util.Collections;
import java.util.List;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * 7.4.2 The CompleteRevocationRefs element
 * As it was stated in the previous clause, the addition, to an
 * electronic signature, of the full set of references to the
 * revocation data that have been used in the validation of the
 * signer and CAs certificates, provide means to retrieve the
 * actual revocation data archived elsewhere in case of dispute
 * and, in this way, to illustrate that the verifier has taken
 * due diligence of the available revocation information.
 *
 * Currently two major types of revocation data are managed in
 * most of the systems, namely CRLs and responses of on-line
 * certificate status servers, obtained through protocols
 * designed for these purposes, like OCSP protocol.
 * This clause defines the CompleteRevocationRefs element that
 * will carry the full set of revocation information used for
 * the verification of the electronic signature.
 *
 * This is an unsigned property that qualifies the signature.
 *
 * An XML electronic signature aligned with the present document
 * MAY contain at most one CompleteRevocationRefs element.
 *
 * Below follows the Schema definition for this element:

 <CompleteRevocationRefs>
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
 <ValidationResult />
 </CompleteRevocationRefs>

 <xsd:element name="CompleteRevocationRefs" type="CompleteRevocationRefsType"/>
 <xsd:complexType name="CompleteRevocationRefsType">
 <xsd:sequence>
 <xsd:element name="CRLRefs" type="CRLRefsType" minOccurs="0"/>
 <xsd:element name="OCSPRefs" type="OCSPRefsType" minOccurs="0"/>
 <xsd:element name="OtherRefs" type="OtherCertStatusRefsType"
 minOccurs="0"/>
 </xsd:sequence>
 <xsd:attribute name="Id" type="xsd:ID" use="optional"/>
 </xsd:complexType>

 <xsd:complexType name="CRLRefsType">
 <xsd:sequence>
 <xsd:element name="CRLRef" type="CRLRefType" maxOccurs="unbounded"/>
 </xsd:sequence>
 </xsd:complexType>

 <xsd:complexType name="CRLRefType">
 <xsd:sequence>
 <xsd:element name="DigestAlgAndValue" type="DigestAlgAndValueType"/>
 <xsd:element name="CRLIdentifier" type="CRLIdentifierType"
 minOccurs="0"/>
 </xsd:sequence>
 </xsd:complexType>

 <xsd:complexType name="CRLIdentifierType">
 <xsd:sequence>
 <xsd:element name="Issuer" type="xsd:string"/>
 <xsd:element name="IssueTime" type="xsd:dateTime" />
 <xsd:element name="Number" type="xsd:integer" minOccurs="0"/>
 </xsd:sequence>
 <xsd:attribute name="URI" type="xsd:anyURI" use="optional"/>
 </xsd:complexType>

 <xsd:complexType name="OCSPRefsType">
 <xsd:sequence>
 <xsd:element name="OCSPRef" type="OCSPRefType" maxOccurs="unbounded"/>
 </xsd:sequence>
 </xsd:complexType>

 <xsd:complexType name="OCSPRefType">
 <xsd:sequence>
 <xsd:element name="OCSPIdentifier" type="OCSPIdentifierType"/>
 <xsd:element name="DigestAlgAndValue" type="DigestAlgAndValueType"
 minOccurs="0"/>
 </xsd:sequence>
 </xsd:complexType>

 <xsd:complexType name="ResponderIDType">
 <xsd:choice>
 <xsd:element name="ByName" type="xsd:string"/>
 <xsd:element name="ByKey" type="xsd:base64Binary"/>
 </xsd:choice>
 </xsd:complexType>

 <xsd:complexType name="OCSPIdentifierType">
 <xsd:sequence>
 <xsd:element name="ResponderID" type="ResponderIDType"/>
 <xsd:element name="ProducedAt" type="xsd:dateTime"/>
 </xsd:sequence>
 <xsd:attribute name="URI" type="xsd:anyURI" use="optional"/>
 </xsd:complexType>

 <xsd:complexType name="OtherCertStatusRefsType">
 <xsd:sequence>
 <xsd:element name="OtherRef" type="AnyType" maxOccurs="unbounded"/>
 </xsd:sequence>
 </xsd:complexType>

 * The CompleteRevocationRefs element can contain:
 * - sequences of references to CRLs (CRLRefs element);
 * - sequences of references to OCSPResponse data as defined in
 *   RFC 2560 [9] (OCSPRefs element);
 * - other references to alternative forms of revocation data
 *   (OtherRefs element).
 *
 * Each element in a CRLRefs sequence (CrlRef element) references
 * one CRL. Each reference contains:
 * - the digest of the entire DER encoded CRL (DigestAlgAndValue
 *   element);
 * - a set of data (CRLIdentifier element) including the issuer
 *   (Issuer element), the time when the CRL was issued
 *   (IssueTime element) and optionally the number of the CRL
 *   (Number element).
 * CRLIdentifier element contents MUST follow the rules
 * established by XMLDSIG [3] in its clause 4.4.4 for strings
 * representing Distinguished Names. In addition, this element
 * can be dropped if the CRL could be inferred from other
 * information. Its URI attribute could serve to indicate where
 * the identified CRL is archived.
 *
 * Each element in an OCSPRefs sequence (OcspRef element)
 * references one OCSP response. Each reference contains:
 * - a set of data (OCSPIdentifier element) that includes
 *   an identifier of the responder and an indication of the
 *   time when the response was generated. The responder may be
 *   identified by its name, using the Byname element within
 *   ResponderID. It may also be identified by the digest of the
 *   server"s public key computed as mandated in RFC 2560 [9],
 *   using the ByKey element. In this case the content of the
 *   ByKey element will be the DER value of the byKey field
 *   defined in RFC 2560, base64-encoded. The contents of ByName
 *   element MUST follow the rules established by XMLDSIG [3] in
 *   its clause 4.4.4 for strings representing Distinguished
 *   Names. The generation time indication appears in the
 *   ProducedAt element and corresponds to the "ProducedAt"
 *   field of the referenced response. The optional URI
 *   attribute could serve to indicate where the OCSP
 *   response identified is archived;
 * - the digest computed on the DER encoded OCSPResponse defined
 *   in RFC 2560 [9], appearing within DigestAlgAndValue element,
 *   since it MAY be needed to differentiate between two OCSP
 *   responses by the same server with their "ProducedAt" fields
 *   within the same second.
 *
 * Alternative forms of validation data can be included in this
 * property making use of the OtherRefs element, a sequence
 * whose items (OtherRef elements) can contain any kind of
 * information.
 *
 * Should XML time-stamp tokens based in XMLDSIG be standardized
 * and spread, this type could also serve to contain references
 * to the full set of CRL or OCSP responses that have been used
 * to verify the certification chain for any TSUs providing such
 * time-stamp tokens. In this case, an element of this type
 * could be added as an unsigned property to the XML time-stamp
 * token using the incorporation mechanisms defined in the
 * present specification.
 *
 **/

/**
 * 
 * @author miro
 */
public class CompleteRevocationRefsImpl extends XAdESStructure implements CompleteRevocationRefs
{
    private List<OCSPRef> ocspRefs;
    private List<CRLRef> crlRefs;
    private ValidationResult validationResult;

    // public CompleteRevocationRefsImpl(XAdESStructure parent,
    // CertValidationInfo certValidationInfo,
    // String signatureIdPrefix)
    // throws GeneralSecurityException
    // {
    // super(parent, "CompleteRevocationRefs");
    //
    // if(certValidationInfo == null)
    // throw new IllegalArgumentException("The CertValidationInfo can not be NULL.");
    //
    // boolean hasOCSPValidations = false;
    // boolean hasCRLValidations = false;
    // XAdESCertPathValidatorResult validatorResult;
    // validatorResult = certValidationInfo.getCertPathValidatorResult();
    // if(validatorResult != null)
    // {
    // for(XAdESRevocationStatus revocationStatus : validatorResult.getXAdESRevocationStatuses())
    // {
    // if(!hasOCSPValidations && revocationStatus.getOCSPResponse() != null)
    // {
    // hasOCSPValidations = true;
    // }
    // else if(!hasCRLValidations && revocationStatus.getCheckedCRL() != null)
    // {
    // hasCRLValidations = true;
    // }
    //
    // if(hasOCSPValidations && hasCRLValidations)
    // break;
    // }
    //
    // if(hasOCSPValidations)
    // {
    // OCSPRefs ocspRefs = new OCSPRefs(this, certValidationInfo);
    // }
    //
    // if(hasCRLValidations)
    // {
    // CRLRefs crlRefs = new CRLRefs(this, certValidationInfo);
    // }
    // }
    //
    // ValidationResult validationResult = new ValidationResult(this, certValidationInfo);
    // }

    public CompleteRevocationRefsImpl(Node node, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public List<OCSPRef> getOCSPRefs()
    {
        if (ocspRefs == null)
        {
            Element element = getChildElementNS("OCSPRefs");
            if (element != null)
            {
                OCSPRefs refs = new OCSPRefs(element, xadesPrefix, xadesNamespace,
                        xmlSignaturePrefix);
                ocspRefs = refs.getOCSPRefs();
            }
            else
                ocspRefs = Collections.<OCSPRef> emptyList();
        }

        return ocspRefs;
    }

    public List<CRLRef> getCRLRefs()
    {
        if (crlRefs == null)
        {
            Element element = getChildElementNS("CRLRefs");
            if (element != null)
            {
                CRLRefs refs = new CRLRefs(element, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
                crlRefs = refs.getCRLRefs();
            }
            else
                crlRefs = Collections.<CRLRef> emptyList();
        }

        return crlRefs;
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

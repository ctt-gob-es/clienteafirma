package net.java.xades.security.xml.XAdES;

import javax.xml.crypto.MarshalException;
import javax.xml.crypto.dsig.DigestMethod;

import net.java.xades.util.ObjectId;
import net.java.xades.util.OccursRequirement;

/**
 * ETSI TS 101 903 V1.3.2 (2006-03)
 *
 * Xml Advanced Electronic Signature (XAdES)
 *
 * 4.4 Electronic signature forms
 * The current clause specifies four forms of XML advanced
 * electronic signatures, namely the Basic Electronic
 * Signature (XAdES-BES), the Explicit Policy based
 * Electronic Signature (XAdES-EPES), and the Electronic
 * Signature with Validation Data (XAdES-T and XAdES-C).
 * Conformance to the present document mandates the
 * signer to create one of these formats.
 * The informative annex B defines extended forms of XAdES.
 * Conformance to the present document does not mandate
 * the signer to create any of the forms defined in annex B.
 *
 * Similar/related projects/standards:
 * - RFC3126 (http://www.ietf.org/rfc/rfc3126.txt,
 *            http://www.faqs.org/rfcs/rfc3126.html,
 *            http://rfc.net/rfc3126.html
 * - http://www.oasis-open.org/
 **/

/**
 * 
 * @author miro
 */
public enum XAdES
{
    /**
     * 4.4 Electronic signature forms The current clause specifies four forms of XML advanced
     * electronic signatures, namely the Basic Electronic Signature (XAdES-BES), the Explicit Policy
     * based Electronic Signature (XAdES-EPES), and the Electronic Signature with Validation Data
     * (XAdES-T and XAdES-C). Conformance to the present document mandates the signer to create one
     * of these formats. The informative annex B defines extended forms of XAdES. Conformance to the
     * present document does not mandate the signer to create any of the forms defined in annex B.
     **/
    BES("XAdES-BES", "4.4.1", "Basic Electronic Signature"), EPES("XAdES-EPES", "4.4.2",
            "Explicit Policy Electronic Signatures"), T("XAdES-T", "4.4.3.1",
            "Electronic Signature with Time"), C("XAdES-C", "4.4.3.2",
            "Electronic Signature with Complete Validation Data References"),

    /**
     * 4.5 Validation process The Validation Process validates an electronic signature, the output
     * status of the validation process can be: - invalid; - incomplete validation; - valid. An
     * Invalid response indicates that either the signature format is incorrect or that the digital
     * signature value fails verification (e.g. the integrity check on the digital signature value
     * fails or any of the certificates on which the digital signature verification depends is known
     * to be invalid or revoked). An Incomplete Validation response indicates that the format and
     * digital signature verifications have not failed but there is insufficient information to
     * determine if the electronic signature is valid. For example; all the required certificates
     * are not available or the grace period is not completed. In the case of Incomplete Validation,
     * the electronic signature may be checked again at some later time when additional validation
     * information becomes available. Also, in the case of incomplete validation, additional
     * information may be made available to the application or user, thus allowing the application
     * or user to decide what to do with partially correct electronic signatures. A Valid response
     * indicates that the signature has passed verification and it complies with the signature
     * validation policy. Informative annex G gives details on technical rules that verifiers should
     * follow for verifying XAdES signatures.
     * 
     * 4.6 Arbitration In case of arbitration, a XAdES-C form provides reliable evidence for a valid
     * electronic signature, provided that: - the arbitrator knows where to retrieve the signer's
     * certificate (if not already present), all the required certificates and CRLs, ACRLs or OCSP
     * responses referenced in the XAdES-C; - when time-stamping in the XAdES-T is being used, the
     * certificate from the TSU that has issued the time-stamp token in the XAdES-T format is still
     * within its validity period; - when time-stamping in the XAdES-T is being used, the
     * certificate from the TSU that has issued the time-stamp token in the XAdES-T format is not
     * revoked at the time of arbitration; - when time-marking in the XAdES-T is being used, a
     * reliable audit trail from the Time-Marking Authority is available for examination regarding
     * the time; - none of the private keys corresponding to the certificates used to verify the
     * signature chain have ever been compromised; - the cryptography used at the time the XAdES-C
     * was built has not been broken at the time the arbitration is performed. If the signature
     * policy can be explicitly or implicitly identified then an arbitrator is able to determine the
     * rules required to validate the electronic signature.
     * 
     * Annex B (informative): Extended electronic signature forms The XAdES forms specified in
     * clause 4.4 can be extended by addition of certain unsigned properties that are defined in the
     * present document. These properties are applicable for very long term verification, and for
     * preventing some disaster situations which have been identified in the normative part of the
     * present document. The clauses below give an overview of the various forms of extended
     * signature formats in the present document.
     **/
    X("XAdES-X", "B.1", "Extended Signatures with Time Forms"), X_L("XAdES-X-L", "B.2",
            "Extended Long Electronic Signatures with Time"), A("XAdES-A", "B.3",
            "Archival Electronic Signatures");

    private XAdES(String nickname, String contentsId, String title)
    {
        this.nickname = nickname;
        this.contentsId = contentsId;
        this.title = title;
    }

    private String nickname;
    private String contentsId;
    private String title;

    public String getNickname()
    {
        return nickname;
    }

    public String getContentsId()
    {
        return contentsId;
    }

    public String getTitle()
    {
        return title;
    }

    public enum Element implements XadesElement
    {
        OBJECT(null, "Object"), QUALIFYING_PROPERTIES(OBJECT, "QualifyingProperties"), SIGNED_PROPERTIES(
                QUALIFYING_PROPERTIES, "SignedProperties"), SIGNED_SIGNATURE_PROPERTIES(
                SIGNED_PROPERTIES, "SignedSignatureProperties"), SIGNING_TIME(XAdES.BES,
                SIGNED_SIGNATURE_PROPERTIES, "SigningTime", OccursRequirement.ZERO_OR_ONE), SIGNING_CERTIFICATE(
                XAdES.BES, SIGNED_SIGNATURE_PROPERTIES, "SigningCertificate",
                OccursRequirement.ZERO_OR_ONE), SIGNATURE_PRODUCTION_PLACE(XAdES.BES,
                SIGNED_SIGNATURE_PROPERTIES, "SignatureProductionPlace",
                OccursRequirement.ZERO_OR_ONE), SIGNER_ROLE(XAdES.BES, SIGNED_SIGNATURE_PROPERTIES,
                "SignerRole", OccursRequirement.ZERO_OR_ONE), CLAIMED_ROLES(XAdES.BES, SIGNER_ROLE,
                "ClaimedRoles", OccursRequirement.ZERO_OR_MORE), CERTIFIED_ROLES(XAdES.BES,
                SIGNER_ROLE, "CertifiedRoles", OccursRequirement.ZERO_OR_MORE), SIGNATURE_POLICY_IDENTIFIER(
                XAdES.EPES, SIGNED_SIGNATURE_PROPERTIES, "SignaturePolicyIdentifier",
                OccursRequirement.EXACTLY_ONE), SIGNER(XAdES.BES, SIGNED_SIGNATURE_PROPERTIES,
                "Signer", OccursRequirement.ZERO_OR_ONE), SIGNER_DETAILS(XAdES.BES,
                SIGNED_SIGNATURE_PROPERTIES, "SignerDetails", OccursRequirement.ZERO_OR_ONE), SIGNED_DATA_OBJECT_PROPERTIES(
                SIGNED_PROPERTIES, "SignedDataObjectProperties"), DATA_OBJECT_FORMATS(XAdES.BES,
                SIGNED_DATA_OBJECT_PROPERTIES, "DataObjectFormat", OccursRequirement.ZERO_OR_MORE), COMMITMENT_TYPE_INDICATIONS(
                XAdES.BES, SIGNED_DATA_OBJECT_PROPERTIES, "CommitmentTypeIndication",
                OccursRequirement.ZERO_OR_MORE), ALL_DATA_OBJECTS_TIMESTAMPS(XAdES.BES,
                SIGNED_DATA_OBJECT_PROPERTIES, "AllDataObjectsTimeStamp",
                OccursRequirement.ZERO_OR_MORE), INDIVIDUAL_DATA_OBJECTS_TIMESTAMPS(XAdES.BES,
                SIGNED_DATA_OBJECT_PROPERTIES, "IndividualDataObjectsTimeStamp",
                OccursRequirement.ZERO_OR_MORE), UNSIGNED_PROPERTIES(QUALIFYING_PROPERTIES,
                "UnsignedProperties"), UNSIGNED_SIGNATURE_PROPERTIES(UNSIGNED_PROPERTIES,
                "UnsignedSignatureProperties"), COUNTER_SIGNATURES(XAdES.BES,
                UNSIGNED_SIGNATURE_PROPERTIES, "CounterSignature", OccursRequirement.ZERO_OR_MORE), SIGNATURE_TIME_STAMP(
                XAdES.T, UNSIGNED_SIGNATURE_PROPERTIES, "SignatureTimeStamp",
                OccursRequirement.ONE_OR_MORE), COMPLETE_CERTIFICATE_REFS(XAdES.C,
                UNSIGNED_SIGNATURE_PROPERTIES, "CompleteCertificateRefs",
                OccursRequirement.EXACTLY_ONE), COMPLETE_REVOCATION_REFS(XAdES.C,
                UNSIGNED_SIGNATURE_PROPERTIES, "CompleteRevocationRefs",
                OccursRequirement.EXACTLY_ONE), ATTRIBUTE_CERTIFICATE_REFS(XAdES.C,
                UNSIGNED_SIGNATURE_PROPERTIES, "AttributeCertificateRefs",
                OccursRequirement.ZERO_OR_ONE), ATTRIBUTE_REVOCATION_REFS(XAdES.C,
                UNSIGNED_SIGNATURE_PROPERTIES, "CompleteCertificateRefs",
                OccursRequirement.ZERO_OR_ONE), QUALIFYING_PROPERTIES_REFERENCE(OBJECT,
                "QualifyingPropertiesReference");

        private Element(XadesElement parent, String elementName)
        {
            this(null, parent, elementName, OccursRequirement.EXACTLY_ONE);
        }

        private Element(XAdES xades, XadesElement parent, String elementName,
                OccursRequirement occursRequirement)
        {
            this.xades = xades;
            this.parent = parent;
            this.elementName = elementName;
            this.occursRequirement = OccursRequirement.EXACTLY_ONE;
        }

        public XAdES getXAdES()
        {
            return xades;
        }

        public ObjectId getObjectId()
        {
            if (objectId == null)
            {
                int components[];
                if (parent != null)
                {
                    int[] parentComps = parent.getObjectId().getComponents();
                    int size = parentComps.length;
                    components = new int[size + 1];
                    System.arraycopy(parentComps, 0, components, 0, size);
                    components[size] = ordinal() + 1;
                }
                else
                {
                    components = new int[] { 0, ordinal() + 1 };
                }
                objectId = new ObjectId(components);
            }

            return objectId;
        }

        public String getElementName()
        {
            return elementName;
        }

        public OccursRequirement getOccursRequirement()
        {
            return occursRequirement;
        }

        public XadesElement getParent()
        {
            return parent;
        }

        public String toString()
        {
            return "[" + getObjectId() + "] " + getElementName();
        }

        private XAdES xades;
        private ObjectId objectId;
        private String elementName;
        private OccursRequirement occursRequirement;
        private XadesElement parent;

    }

    public static final XadesElementsEnumeration XAdES_ELEMENTS = new XadesElementsEnumeration(
            Element.values());
    public static final XadesElementsEnumeration XAdES_BES_ELEMENTS = new XadesElementsEnumeration(
            Element.values(), BES);
    public static final XadesElementsEnumeration XAdES_EPES_ELEMENTS = new XadesElementsEnumeration(
            Element.values(), EPES);
    public static final XadesElementsEnumeration XAdES_T_ELEMENTS = new XadesElementsEnumeration(
            Element.values(), T);
    public static final XadesElementsEnumeration XAdES_C_ELEMENTS = new XadesElementsEnumeration(
            Element.values(), C);
    public static final XadesElementsEnumeration XAdES_X_ELEMENTS = new XadesElementsEnumeration(
            Element.values(), X);
    public static final XadesElementsEnumeration XAdES_X_L_ELEMENTS = new XadesElementsEnumeration(
            Element.values(), X_L);
    public static final XadesElementsEnumeration XAdES_A_ELEMENTS = new XadesElementsEnumeration(
            Element.values(), A);

    public static XAdES_BES newInstance(XAdES xades, org.w3c.dom.Element baseElement)
    {
        return newInstance(xades, XMLAdvancedSignature.XADES_v132, "xades", "dsign",
                DigestMethod.SHA1, baseElement);
    }

    public static XAdES_BES newInstance(XAdES xades, String xadesNamespace, String xadesPrefix,
            String xmlSignaturePrefix, String digestMethod, org.w3c.dom.Element baseElement)
    {
        if (BES.equals(xades))
        {
            return new BasicXAdESImpl(baseElement, false, xadesPrefix, xadesNamespace,
                    xmlSignaturePrefix, digestMethod);
        }
        else if (EPES.equals(xades))
        {
            return new ExplicitPolicyXAdESImpl(baseElement, false, xadesPrefix, xadesNamespace,
                    xmlSignaturePrefix, digestMethod);
        }
        else if (T.equals(xades))
        {
            return new TimestampXAdESImpl(baseElement, false, xadesPrefix, xadesNamespace,
                    xmlSignaturePrefix, digestMethod);
        }
        else if (C.equals(xades))
        {
            return new CompleteValidationXAdESImpl(baseElement, false, xadesPrefix, xadesNamespace,
                    xmlSignaturePrefix, digestMethod);
        }
        else if (X.equals(xades))
        {
            return new ExtendedXAdESImpl(baseElement, false, xadesPrefix, xadesNamespace,
                    xmlSignaturePrefix, digestMethod);
        }
        else if (X_L.equals(xades))
        {
            return new ExtendedLongXAdESImpl(baseElement, false, xadesPrefix, xadesNamespace,
                    xmlSignaturePrefix, digestMethod);
        }
        else if (A.equals(xades))
        {
            return new ArchivalXAdESImpl(baseElement, false, xadesPrefix, xadesNamespace,
                    xmlSignaturePrefix, digestMethod);
        }

        throw new IllegalArgumentException("Unknown XAdES type: " + xades);
    }

    public static XAdES_C getInstance(org.w3c.dom.Element baseElement, String xadesPrefix,
            String xadesNamespace, String xmlSignaturePrefix, String digestMethod)
            throws MarshalException
    {
        ArchivalXAdESImpl xades = new ArchivalXAdESImpl(baseElement, true, xadesPrefix,
                xadesNamespace, xmlSignaturePrefix, digestMethod);
        xades.unmarshal();
        return xades;
    }
}

package net.java.xades.security.xml.XAdES;

import java.security.cert.X509Certificate;
import java.util.Date;
import java.util.List;
import org.w3c.dom.Element;

/**
 * 4.4.1 Basic electronic signature (XAdES-BES)
 * A Basic Electronic Signature (XAdES-BES) in accordance with
 * the present document will build on a XMLDSIG by incorporating
 * qualifying properties defined in the present specification.
 * They will be added to XMLDSIG within one ds:Object acting as
 * the bag for the whole set of qualifying properties, or by
 * using the mechanism defined in clause 6.3.2 that allows
 * further distribution of the properties.
 * Some properties defined for building up this form will be
 * covered by the signer's signature (signed qualifying
 * information grouped within one new element, SignedProperties,
 * see clause 6.2.1). Other properties will be not covered by
 * the signer's signature (unsigned qualifying information
 * grouped within one new element, UnsignedProperties, see
 * clause 6.2.2).
 * In a XAdES-BES the signature value SHALL be computed in the
 * usual way of XMLDSIG over the data object(s) to be signed
 * and on the whole set of signed properties when present
 * (SignedProperties element).
 * For this form it is mandatory to protect the signing
 * certificate with the signature, in one of the two following
 * ways:
 * - Either incorporating the SigningCertificate signed property;
 *   or
 * - Not incorporating the SigningCertificate but incorporating
 *   the signing certificate within the ds:KeyInfo element and
 *   signing it.
 * A XAdES-BES signature MUST, in consequence, contain at least
 * one of the following elements with the specified contents:
 * - The SigningCertificate signed property. This property
 *   MUST contain the reference and the digest value of the
 *   signing certificate. It MAY contain references and digests
 *   values of other certificates (that MAY form a chain up to
 *   the point of trust). In the case of ambiguities identifying
 *   the actual signer's certificate the applications SHOULD
 *   include the SigningCertificate property.
 * - The ds:KeyInfo element. If SigningCertificate is present
 *   in the signature, no restrictions apply to this element.
 *   If SigningCertificate element is not present in the
 *   signature, then the following restrictions apply:
 *   - the ds:KeyInfo element MUST include a ds:X509Data
 *     containing the signing certificate;
 *   - the ds:KeyInfo element also MAY contain other
 *     certificates forming a chain that MAY reach the point of
 *     trust;
 *   - the ds:SignedInfo element MUST contain a ds:Reference
 *     element referencing ds:KeyInfo, so that the latter is
 *     included in the signature value computation. In this way,
 *     the signing certificate is secured by the signature.
 * By incorporating one of these elements, XAdES-BES prevents
 * the simple substitution of the signer's certificate (see
 * clause 7.2.2).
 * A XAdES-BES signature MAY also contain the following
 * properties:
 * - the SigningTime signed property;
 * - the DataObjectFormat signed property;
 * - the CommitmentTypeIndication signed property;
 * - the SignerRole signed property;
 * - the SignatureProductionPlace signed property;
 * - one or more IndividualDataObjectsTimeStamp or
 *   AllDataObjectTimeStamp signed properties;
 * - one or more CounterSignature unsigned properties.
 * Below follows the structure of the XAdES-BES built by direct
 * incorporation of the qualifying information in the
 * corresponding new XML elements to the XMLDSIG (see clause
 * 6.3.1 for further details). In the example "?" denotes zero
 * or one occurrence; "+" denotes one or more occurrences; and
 * "*" denotes zero or more occurrences.
 * The XML schema definition in clause 5 defines the prefix
 * "ds" for all the XML elements already defined in XMLDSIG,
 * and states that the default namespace is the one defined
 * for the present document. In consequence, in the example
 * of this clause, the elements already defined in XMLDSIG
 * appear with the prefix "ds", whereas the new XML elements
 * defined in the present document appear without prefix.
 **/

/*
<?xml version="1.0"?>
    <schema xmlns:xsd="http://www.w3.org/2001/XMLSchema"
            xmlns="http://uri.etsi.org/01903/v1.3.2#"
            targetNamespace="http://uri.etsi.org/01903/v1.3.2#"
            xmlns:ds="http://www.w3.org/2000/09/xmldsig#"
            elementFormDefault="qualified">
        <xsd:import namespace="http://www.w3.org/2000/09/xmldsig#"
                    schemaLocation="http://www.w3.org/TR/2002/REC-xmldsig-core-20020212/xmldsig-core-schema.xsd"/>

        <ds:Signature ID?>
            ...
            <ds:Object>
                <QualifyingProperties>
                    <SignedProperties>
                        <SignedSignatureProperties>
                            (SigningTime)?
                            (SigningCertificate)?
                            (SignatureProductionPlace)?
                            (SignerRole)?
                        </SignedSignatureProperties>
                        <SignedDataObjectProperties>
                            (DataObjectFormat)*
                            (CommitmentTypeIndication)*
                            (AllDataObjectsTimeStamp)*
                            (IndividualDataObjectsTimeStamp)*
                        </SignedDataObjectProperties>
                    </SignedProperties>
                    <UnsignedProperties>
                        <UnsignedSignatureProperties>
                            (CounterSignature)*
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
public interface XAdES_BES
{
    public Element getBaseElement();

    public Date getSigningTime();
    public void setSigningTime(Date signingTime);

    public X509Certificate getSigningCertificate();
    public void setSigningCertificate(X509Certificate certificate);

    public SignatureProductionPlace getSignatureProductionPlace();
    public void setSignatureProductionPlace(SignatureProductionPlace productionPlace);

    public SignerRole getSignerRole();
    public void setSignerRole(SignerRole signerRole);

    public Signer getSigner();
    public void setSigner(Signer signer);

    public List<DataObjectFormat> getDataObjectFormats();
    public void setDataObjectFormats(List<DataObjectFormat> dataObjectFormats);

    public List<CommitmentTypeIndication> getCommitmentTypeIndications();
    public void setCommitmentTypeIndications(List<CommitmentTypeIndication> commitmentTypeIndications);

    public List<AllDataObjectsTimeStamp> getAllDataObjectsTimeStamps();
    public void setAllDataObjectsTimeStamps(List<AllDataObjectsTimeStamp> allDataObjectsTimeStamps);

    public List<XAdESTimeStamp> getIndividualDataObjectsTimeStamps();
    public void setIndividualDataObjectsTimeStamps(List<IndividualDataObjectsTimeStamp> individualDataObjectsTimeStamps);

    public List<CounterSignature> getCounterSignatures();
    public void setCounterSignatures(List<CounterSignature> counterSignatures);   
    
    public String getXadesPrefix();
    public String getXadesNamespace();
    public String getXmlSignaturePrefix();
    public String getDigestMethod();
}

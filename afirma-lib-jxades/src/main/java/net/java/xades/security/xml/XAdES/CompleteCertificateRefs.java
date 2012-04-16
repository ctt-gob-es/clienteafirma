package net.java.xades.security.xml.XAdES;

/**
 * 7.4.1 The CompleteCertificateRefs element
 * This clause defines the XML element able to carry the
 * aforementioned references to the CA certificates: the
 * CompleteCertificateRefs element.
 * This is an unsigned property that qualifies the signature.
 * An XML electronic signature aligned with the present
 * document MAY contain at most one CompleteCertificateRefs
 * element.
 * Below follows the schema definition for this element:

    <CompleteCertificateRefs>
        <CertRefs Id="">
            <Cert>
                <CertDigest>
                    <DigestMethod Algorithm="" />
                    <DigestValue></DigestValue>
                </CertDigest>
                <IssuerSerial>
                    <X509IssuerName></X509IssuerName>
                    <X509SerialNumber><X509SerialNumber>
                </IssuerSerial>
            </Cert>
        </CertRefs>
    </CompleteCertificateRefs> 

        <xsd:element name="CompleteCertificateRefs" type="CompleteCertificateRefsType" />

        <xsd:complexType name="CompleteCertificateRefsType">
            <xsd:sequence>
                <xsd:element name="CertRefs" type="CertIDListType" />
            </xsd:sequence>
            <xsd:attribute name="Id" type="xsd:ID" use="optional"/>
        </xsd:complexType>

        <xsd:complexType name="CertIDListType">
            <xsd:sequence>
                <xsd:element name="Cert" type="CertIDType" maxOccurs="unbounded"/>
            </xsd:sequence>
        </xsd:complexType>

        <xsd:complexType name="CertIDType">
            <xsd:sequence>
                <xsd:element name="CertDigest" type="DigestAlgAndValueType"/>
                <xsd:element name="IssuerSerial" type="ds:X509IssuerSerialType"/>
            </xsd:sequence>
            <xsd:attribute name="URI" type="xsd:anyURI" use="optional"/>
        </xsd:complexType>

        <xsd:complexType name="DigestAlgAndValueType">
            <xsd:sequence>
                <xsd:element ref="ds:DigestMethod"/>
                <xsd:element ref="ds:DigestValue"/>
            </xsd:sequence>
        </xsd:complexType>

 * The CertRefs element contains a sequence of Cert elements
 * already defined in clause 7.2.2, incorporating the digest of
 * each certificate and the issuer and serial number identifier.
 * Should XML time-stamp tokens based in XMLDSIG be
 * standardized and spread, this type could also serve to
 * contain references to the certification chain for any TSUs
 * providing such time-stamp tokens. In this case, an element
 * of this type could be added as an unsigned property to the
 * XML time-stamp token using the incorporation mechanisms
 * defined in the present specification.
 *
 **/

/**
 *
 * @author miro
 */
public interface CompleteCertificateRefs
{
    public CertRefs getCertRefs();
}

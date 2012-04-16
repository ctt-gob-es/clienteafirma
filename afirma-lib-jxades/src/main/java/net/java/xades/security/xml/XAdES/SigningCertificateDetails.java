package net.java.xades.security.xml.XAdES;

import java.security.GeneralSecurityException;

import javax.xml.crypto.dsig.XMLSignature;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * 
 * @author miro
 * 
 *         Sample usage of signing Certificate:
 * 
 *         <xades:SigningCertificate> <xades:Cert> <xades:CertDigest> <ds:DigestMethod
 *         Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />
 *         <ds:DigestValue>rFQEEAdlZJieHIdInK8bYoB6aMs=</ds:DigestValue> </xades:CertDigest>
 *         <xades:IssuerSerial> <ds:X509IssuerName>CN=UJI Test CA,OU=UJI Test
 *         CA,O=UJI,C=ES</ds:X509IssuerName> <ds:X509SerialNumber>523398</ds:X509SerialNumber>
 *         </xades:IssuerSerial> </xades:Cert> </xades:SigningCertificate>
 * 
 */

public class SigningCertificateDetails extends XAdESStructure
{
    public SigningCertificateDetails(SignedSignatureProperties ssp,
            SigningCertificate signingCertificate, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix) throws GeneralSecurityException
    {
        super(ssp, "SigningCertificate", xadesPrefix, xadesNamespace, xmlSignaturePrefix);

        // TODO: Unimplemented URI parameter
        Element cert = createElement("Cert");

        Element certDigest = createElement("CertDigest");

        Element digestMethod = createElementNS(XMLSignature.XMLNS, xmlSignaturePrefix,
                "DigestMethod");
        digestMethod.setPrefix(xmlSignaturePrefix);
        digestMethod.setAttributeNS(xmlSignaturePrefix, "Algorithm", signingCertificate.getDigestMethodAlgorithm());

        Element digestValue = createElementNS(XMLSignature.XMLNS, xmlSignaturePrefix, "DigestValue");
        digestValue.setPrefix(xmlSignaturePrefix);
        digestValue.setTextContent(signingCertificate.getDigestValue());

        certDigest.appendChild(digestMethod);
        certDigest.appendChild(digestValue);

        Element issuerSerial = createElement("IssuerSerial");

        Element x509IssuerName = createElementNS(XMLSignature.XMLNS, xmlSignaturePrefix,
                "X509IssuerName");
        x509IssuerName.setPrefix(xmlSignaturePrefix);
        x509IssuerName.setTextContent(signingCertificate.getIssuerName());

        Element x509SerialNumber = createElementNS(XMLSignature.XMLNS, xmlSignaturePrefix,
                "X509SerialNumber");
        x509SerialNumber.setPrefix(xmlSignaturePrefix);
        x509SerialNumber.setTextContent(signingCertificate.getX509SerialNumber() + "");

        issuerSerial.appendChild(x509IssuerName);
        issuerSerial.appendChild(x509SerialNumber);

        cert.appendChild(certDigest);
        cert.appendChild(issuerSerial);

        getNode().appendChild(cert);
    }

    public SigningCertificateDetails(Node node, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }
}

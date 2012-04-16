package net.java.xades.security.xml.XAdES;

import java.security.cert.X509Certificate;
import java.util.Collection;

/**
 *
4.4.3.2 Electronic signature with complete validation data references (XAdES-C)
XML Advanced Electronic Signature with Complete validation data references (XAdES-C) in accordance with the
present document adds to the XAdES-T the CompleteCertificateRefs and CompleteRevocationRefs
unsigned properties as defined by the present document. If attribute certificates appear in the signature, then XAdES-C
also incorporates the AttributeCertificateRefs and the AttributeRevocationRefs elements.
CompleteCertificateRefs element contains a sequence of references to the full set of CA certificates that have
been used to validate the electronic signature up to (but not including) the signing certificate.
CompleteRevocationRefs element contains a full set of references to the revocation data that have been used in
the validation of the signer and CA certificates.
AttributeCertificateRefs and AttributeRevocationRefs elements contain references to the full set of
Attribute Authorities certificates and references to the full set of revocation data that have been used in the validation of
the attribute certificates present in the signature, respectively.
Storing the references allows the values of the certification path and revocation data to be stored elsewhere, reducing
the size of a stored electronic signature format.
Below follows the structure for XAdES-C built by direct incorporation of properties on a XAdES-T containing the
SignatureTimeStamp signed property. A XAdES-C form based on time-marks MAY exist without such element.

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

XMLDISG
|
<ds:Signature ID?>- - - - - - - - +- - - - - - +-+-+
<ds:SignedInfo> | | | |
<ds:CanonicalizationMethod/> | | | |
<ds:SignatureMethod/> | | | |
(<ds:Reference URI? > | | | |
(<ds:Transforms>)? | | | |
<ds:DigestMethod/> | | | |
<ds:DigestValue/> | | | |
</ds:Reference>)+ | | | |
</ds:SignedInfo> | | | |
<ds:SignatureValue/> | | | |
(<ds:KeyInfo>)? - - - - - - - - + | | |
| | |
<ds:Object> | | |
| | |
<QualifyingProperties> | | |
| | |
<SignedProperties> | | |
| | |
<SignedSignatureProperties> | | |
(SigningTime)? | | |
(SigningCertificate)? | | |
(SignaturePolicyIdentifier)? | | |
(SignatureProductionPlace)? | | |
(SignerRole)? | | |
</SignedSignatureProperties> | | |
| | |
<SignedDataObjectProperties> | | |
(DataObjectFormat)* | | |
(CommitmentTypeIndication)* | | |
(AllDataObjectsTimeStamp)* | | |
(IndividualDataObjectsTimeStamp)* | | |
</SignedDataObjectProperties> | | |
| | |
</SignedProperties> | | |
| | |
<UnsignedProperties> | | |
| | |
<UnsignedSignatureProperties> | | |
(CounterSignature)*- - - - - - - - - + | |
(SignatureTimeStamp)+- - - - - - - - - + |
(CompleteCertificateRefs) |
(CompleteRevocationRefs) |
(AttributeCertificateRefs)? |
(AttributeRevocationRefs)? |
</UnsignedSignatureProperties>- - - - +-+ |
| | |
</UnsignedProperties> | | |
| | |
</QualifyingProperties> | | |
| | |
</ds:Object> | | |
| | |
</ds:Signature>- - - - - - - - - - - - - - - -+-+-+
| | |
XAdES-BES(-EPES)| |
| |
XAdES-T |
|
XAdES-C
NOTE 1: As a minimum, the signer will provide the XAdES-BES or when indicating that the signature conforms to
an explicit signing policy the XAdES-EPES.
NOTE 2: To reduce the risk of repudiating signature creation, the trusted time indication needs to be as close as
possible to the time the signature was created. The signer or a TSP could provide the XAdES-T. If the
signer did not provide it, the verifier SHOULD create the XAdES-T on first receipt of an electronic
signature, because the XAdES-T provides independent evidence of the existence of the signature prior to
the trusted time indication.
NOTE 3: The XAdES-T trusted time indications MUST be created before a certificate has been revoked or expired.
NOTE 4: The signer or the TSP MAY provide the XAdES-C to minimize this risk and when the signer does not
provide the XAdES-C, the verifier SHOULD create the XAdES-C when the required components of
revocation and validation data become available. This MAY require a grace period.
NOTE 5: A grace period permits certificate revocation information to propagate through the revocation processes.
This period could extend from the time an authorized entity requests certificate revocation, to when
relying parties may be expected to have accessed the revocation information (for example, by contractual
requirements placed on relying parties). In order to make sure that the certificate was not revoked at the
time the signature was time-marked or time-stamped, verifiers SHOULD wait until the end of the grace
period. An illustration of a grace period is provided figure 1.
 *
 **/

/**
 *
 * @author miro
 */
public interface XAdES_C
    extends XAdES_T
{
    public CompleteCertificateRefs getCompleteCertificateRefs();
    public void setCompleteCertificateRefs(Collection<X509Certificate> caCertificates);
    
//    public CompleteRevocationRefs getCompleteRevocationRefs();
//    public void setCompleteRevocationRefs(CertValidationInfo certValidationInfo);
}

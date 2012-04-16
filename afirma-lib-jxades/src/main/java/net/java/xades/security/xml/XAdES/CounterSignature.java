package net.java.xades.security.xml.XAdES;

/**
 *
7.2.4 Countersignatures
Annex C, in its clause C.1, includes a description and discussion on multiple signatures and countersignatures. This
clause defines two standard mechanisms for managing countersignatures. Details are given in clauses below.
7.2.4.1 Countersignature identifier in Type attribute of ds:Reference
The present document defines the following URI value:
• http://uri.etsi.org/01903#CountersignedSignature
A XAdES signature containing a ds:Reference element whose Type attribute has this value will indicate that it is,
in fact, a countersignature of the signature referenced by this element. The ds:Reference element MUST be built so
that the countersignature actually signs the ds:SignatureValue element of the countersigned signature. All the
XMLDSIG rules apply in the processing of the aforementioned ds:Reference element. The only purpose of this
definition is to serve as an easy identification of a signature as actually being a countersignature.
7.2.4.2 Enveloped countersignatures: the CounterSignature element
The CounterSignature is an unsigned property that qualifies the signature. A XAdES signature MAY have more
than one CounterSignature properties. As indicated by its name, it contains one countersignature of the qualified
signature.
The content of this property is a XMLDSIG or XAdES signature whose ds:SignedInfo MUST contain one
ds:Reference element referencing the ds:SignatureValue element of the embedding and countersigned
XAdES signature. The content of the ds:DigestValue in the aforementioned ds:Reference element of the
countersignature MUST be the base64 encoded digest of the complete (and canonicalized) ds:SignatureValue
element (i.e. including the starting and closing tags) of the embedding and countersigned XAdES signature.
Applications MUST build this ds:Reference accordingly, using any of the mechanisms specified by XMLDSIG for
achieving this objective. By doing this the countersignature actually signs the ds:SignatureValue element of the
embedding XAdES signature.
Applications MAY add other ds:Reference elements referencing the ds:SignatureValue elements of
previously existent CounterSignature elements. This allows for building arbitrarily long chains of explicit
countersignatures.
A countersignature MAY itself be qualified by a CounterSignature property, which will have a ds:Reference
element referencing the ds:SignatureValue of the first countersignature, built as described above. This is an
alternative way of constructing arbitrarily long series of countersignatures, each one signing the
ds:SignatureValue element of the one where it is directly embedded.
If the countersignature is a XAdES signature, its production MUST follow the rules dictated by the current
specification. As for its verification they MUST be verified as any regular XAdES signature. Below follows the schema
definition for this element.
<xsd:element name="CounterSignature" type="CounterSignatureType" />
<xsd:complexType name="CounterSignatureType">
<xsd:sequence>
<xsd:element ref="ds:Signature"/>
</xsd:sequence>
</xsd:complexType>
 *
 **/

/**
 *
 * @author miro
 */
public interface CounterSignature
{
}

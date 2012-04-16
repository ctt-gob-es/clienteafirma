package net.java.xades.security.xml.XAdES;

import java.util.ArrayList;

/**
 * 7.2.8 The SignerRole element
While the name of the signer is important, the position of the signer within a company or an organization can be even
more important. Some contracts may only be valid if signed by a user in a particular role, e.g. a Sales Director. In many
cases who the sales Director really is, is not that important but being sure that the signer is empowered by his company
to be the Sales Director is fundamental.
The present document defines two different ways for providing this feature:
- using a claimed role name;
- using an attribute certificate containing a certified role.
The signer MAY state his own role without any certificate to corroborate this claim, in which case the claimed role can
be added to the signature as a signed qualifying property.
Unlike public key certificates that bind an identifier to a public key, Attribute Certificates bind the identifier of a
certificate to some attributes of its owner, like a role. The Attribute Authority will be most of the time under the control
of an organization or a company that is best placed to know which attributes are relevant for which individual. The
Attribute Authority MAY use or point to public key certificates issued by any CA, provided that the appropriate trust
may be placed in that CA. Attribute Certificates MAY have various periods of validity. That period may be quite short,
e.g. one day. While this requires that a new Attribute Certificate is obtained every day, valid for that day, this can be
advantageous since revocation of such certificates may not be needed. When signing, the signer will have to specify
which Attribute Certificate it selects.
This is a signed property that qualifies the signer.
An XML electronic signature aligned with the present document MAY contain at most one SignerRole element.
Below follows the Schema definition for this element.
<xsd:element name="SignerRole" type="SignerRoleType"/>
<xsd:complexType name="SignerRoleType">
<xsd:sequence>
<xsd:element name="ClaimedRoles" type="ClaimedRolesListType"
minOccurs="0"/>
<xsd:element name="CertifiedRoles" type="CertifiedRolesListType"
minOccurs="0"/>
</xsd:sequence>
</xsd:complexType>
<xsd:complexType name="ClaimedRolesListType">
<xsd:sequence>
<xsd:element name="ClaimedRole" type="AnyType"
maxOccurs="unbounded"/>
</xsd:sequence>
</xsd:complexType>
<xsd:complexType name="CertifiedRolesListType">
<xsd:sequence>
<xsd:element name="CertifiedRole" type="EncapsulatedPKIDataType"
maxOccurs="unbounded"/>
</xsd:sequence>
</xsd:complexType>
This property contains a sequence of roles that the signer can play (element SignerRole). At least one of the two
elements ClaimedRoles or CertifiedRoles MUST be present.
The ClaimedRoles element contains a sequence of roles claimed by the signer but not certified. Additional contents
types MAY be defined on a domain application basis and be part of this element. The namespaces given to the
corresponding XML schemas will allow their unambiguous identification in the case these roles use XML.
The CertifiedRoles element contains one or more wrapped DER-encoded attribute certificates for the signer.
 *
 **/

/**
 *
 * @author miro
 */
public interface SignerRole
{
	public ArrayList<String> getCertifiedRole();
	public void setCertifiedRole(ArrayList<String> certifiedRole);
	public ArrayList<String> getClaimedRole();
	public void setClaimedRole(ArrayList<String> claimedRole);
	public void addClaimedRole(String role);
	public void addCertifiedRole(String role);
}

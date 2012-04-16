package net.java.xades.security.xml.XAdES;

/*
<!-- Start SignerRole -->
    <xsd:element name="SignerRole" type="SignerRoleType"/>
    <xsd:complexType name="SignerRoleType">
        <xsd:sequence>
            <xsd:element name="ClaimedRoles" type="ClaimedRolesListType"
                ETSI
                ETSI TS 101 903 V1.3.2 (2006-03) 68
                minOccurs="0"/>
            <xsd:element name="CertifiedRoles" type="CertifiedRolesListType"
                minOccurs="0"/>
        </xsd:sequence>
    </xsd:complexType>
    <xsd:complexType name="ClaimedRolesListType">
        <xsd:sequence>
            <xsd:element name="ClaimedRole" type="AnyType" maxOccurs="unbounded"/>
        </xsd:sequence>
    </xsd:complexType>
    <xsd:complexType name="CertifiedRolesListType">
        <xsd:sequence>
            <xsd:element name="CertifiedRole" type="EncapsulatedPKIDataType"
                maxOccurs="unbounded"/>
        </xsd:sequence>
    </xsd:complexType>
<!-- End SignerRole -->
*/


/**
 *
 * @author miro
 */
public interface RoleType
{
}

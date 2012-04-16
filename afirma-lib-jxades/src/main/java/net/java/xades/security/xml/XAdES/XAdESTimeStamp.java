package net.java.xades.security.xml.XAdES;

/**
 *
7.1.4.3 The XAdESTimeStampType data type
This concrete derived type is provided for containing time-stamp tokens computed on data objects of XAdES
signatures. Applications claiming alignment with the present document MUST implement it because all the properties
listed in clause 7.1.4.1 are elements of this type.
Below follows the schema definition for the data type.
<xsd:element name="XAdESTimeStamp" type="XAdESTimeStampType"/>
<xsd:complexType name="XAdESTimeStampType">
<xsd:complexContent>
<xsd:restriction base="GenericTimeStampType">
<xsd:sequence>
<xsd:element ref="Include" minOccurs="0"
maxOccurs="unbounded"/>
<xsd:element ref="ds:CanonicalizationMethod" minOccurs="0"/>
<xsd:choice maxOccurs="unbounded">
<xsd:element name="EncapsulatedTimeStamp"
type="EncapsulatedPKIDataType"/>
<xsd:element name="XMLTimeStamp" type="AnyType"/>
</xsd:choice>
</xsd:sequence>
<xsd:attribute name="Id" type="xsd:ID" use="optional"/>
</xsd:restriction>
</xsd:complexContent>
</xsd:complexType>
This type provides two mechanisms for identifying data objects that are covered by the time-stamp token present in the
container, and for specifying how to use them for computing the digest value that is sent to the TSA:
• Explicit. This mechanism uses the Include element for referencing specific data objects and for indicating
their contribution to the input of the digest computation.
• Implicit. For certain time-stamp container properties under certain circumstances, applications do not require
any additional indication for knowing that certain data objects are covered by the time-stamp tokens and how
they contribute to the input of the digest computation. The present document specifies, in the clauses defining
such properties (clauses 7.2.9, 7.2.10, 7.3, 7.5 and 7.7), how applications MUST act in these cases without
explicit indications.
Clause 7.1.4.3.1 shows the principles that govern the explicit indication mechanism.
7.1.4.3.1 Include mechanism
Include elements explicitly identify data objects that are time-stamped. Their order of appearance indicates how the
data objects contribute in the generation of the input to the digest computation. The following time-stamp token
container properties use this mechanism:
• IndividualDataObjectsTimeStamp. In this case each Include element contains an URI to one of
the ds:Reference elements in the XAdES signature.
• SigAndRefsTimeStamp, RefsOnlyTimeStamp and ArchiveTimeStamp only and only if these
elements and some of the unsigned properties covered by their time-stamp tokens do not have the same parent
(i.e. some of them are in different QualifyingProperties elements and the XAdES signature contains
QualifyingPropertiesReference elements - see clause 6.3).
The URI attribute in Include element identifies one time-stamped data object. Its value MUST follow the rules
indicated below:
• It MUST have an empty not-fragment part and a bare-name XPointer fragment when the Include and the
time-stamped data object are in the same document.
• It MUST have a not empty not-fragment part and a bare-name XPointer fragment when the Include and the
time-stamped data object are not in the same document.
• When not empty, its not-fragment part MUST be equal to:
- The not-fragment part of the Target attribute of the QualifyingProperties enclosing the
Include element if the time-stamped data object is enveloped by the XAdES signature, or
- The not-fragment part of the URI attribute of the QualifyingPropertiesReference element
referencing the QualifyingProperties element enveloping the time-stamped data object if this
QualifyingProperties element is not enveloped by the XAdES signature.
Applications aligned with the present specification MUST parse the retrieved resource, and then process the bare-name
XPointer as explained below to get a XPath node-set suitable for use by Canonical XML. For processing the bare-name
XPointer applications MUST use as XPointer evaluation context the root node of the XML document that contains the
element referenced by the not-fragment part of URI. Applications MUST derive an XPath node-set from the resultant
location-set as indicated below:
1) Replace the element node E retrieved by the bare-name XPointer with E plus all descendants of E (text,
comments, PIs, elements) and all namespace and attribute nodes of E and its descendant elements.
2) Delete all the comment nodes.
In time-stamps that cover ds:Reference elements, the attribute referencedData MAY be present. If present
with value set to "true", the time-stamp is computed on the result of processing the corresponding ds:Reference
element according to the XMLDSIG processing model. If the attribute is not present or is present with value "false",
the time-stamp is computed on the ds:Reference element itself. When appearing in a time-stamp container
property, each Include element MUST be processed in order as detailed below:
1) Retrieve the data object referenced in the URI attribute following the referencing mechanism indicated above.
2) If the retrieved data is a ds:Reference element and the referencedData attribute is set to the value
"true", take the result of processing the retrieved ds:Reference element according to the reference
processing model of XMLDSIG; otherwise take the ds:Reference element itself.
3) If the resulting data is an XML node set, canonicalize it. If ds:Canonicalization is present, the
algorithm indicated by this element is used. If not, the standard canonicalization method specified by
XMLDSIG is used.
4) Concatenate the resulting octets to those resulting from previous processing as indicated in the corresponding
time-stamp container property.
 *
 **/


/**
 *
 * @author miro
 */
public class XAdESTimeStamp
{
    
    /** Creates a new instance of XAdESTimeStamp */
    public XAdESTimeStamp()
    {
    }
    
}

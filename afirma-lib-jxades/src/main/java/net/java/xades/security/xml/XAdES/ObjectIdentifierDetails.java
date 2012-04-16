package net.java.xades.security.xml.XAdES;

import org.w3c.dom.Element;

/*
 * <p:ObjectIdentifier>
 *   <p:Identifier Qualifier="OIDAsURI">http://tempuri.org</p:Identifier>
 *   <p:Description>p:Description</p:Description>
 *   <p:DocumentationReferences>
 *     <p:DocumentationReference>http://tempuri.org</p:DocumentationReference>
 *   </p:DocumentationReferences>
 * </p:ObjectIdentifier>
 *
 */

public class ObjectIdentifierDetails extends XAdESStructure
{
    public ObjectIdentifierDetails(final DataObjectFormatDetails dataObjectFormatDetails,
            final ObjectIdentifier objectIdentifier, final String xadesPrefix, final String xadesNamespace,
            final String xmlSignaturePrefix)
    {
        super(dataObjectFormatDetails, "ObjectIdentifier", xadesPrefix, xadesNamespace,
                xmlSignaturePrefix);

        final Element identifier = createElement("Identifier");
        identifier.setTextContent(objectIdentifier.getIdentifier());
        identifier.setAttributeNS(null, "Qualifier", objectIdentifier.getQualifier());
        getNode().appendChild(identifier);

        final Element description = createElement("Description");
        description.setTextContent(objectIdentifier.getDescription());
        getNode().appendChild(description);

        if (objectIdentifier.getDocumentationReferences().size() > 0)
        {
            final Element documentationReferences = createElement("DocumentationReferences");

            for (final String reference : objectIdentifier.getDocumentationReferences())
            {
                final Element documentationReference = createElement("DocumentationReference");
                documentationReference.setTextContent(reference);
                documentationReferences.appendChild(documentationReference);
            }

            getNode().appendChild(documentationReferences);
        }
    }
}

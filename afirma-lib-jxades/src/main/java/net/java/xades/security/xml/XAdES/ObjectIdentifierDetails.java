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
    public ObjectIdentifierDetails(DataObjectFormatDetails dataObjectFormatDetails,
            ObjectIdentifier objectIdentifier, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix)
    {
        super(dataObjectFormatDetails, "ObjectIdentifier", xadesPrefix, xadesNamespace,
                xmlSignaturePrefix);

        Element identifier = createElement("Identifier");
        identifier.setTextContent(objectIdentifier.getIdentifier());
        identifier.setAttribute("Qualifier", objectIdentifier.getQualifier());
        getNode().appendChild(identifier);

        Element description = createElement("Description");
        description.setTextContent(objectIdentifier.getDescription());
        getNode().appendChild(description);

        if (objectIdentifier.getDocumentationReferences().size() > 0)
        {
            Element documentationReferences = createElement("DocumentationReferences");

            for (String reference : objectIdentifier.getDocumentationReferences())
            {
                Element documentationReference = createElement("DocumentationReference");
                documentationReference.setTextContent(reference);
                documentationReferences.appendChild(documentationReference);
            }

            getNode().appendChild(documentationReferences);
        }
    }
}

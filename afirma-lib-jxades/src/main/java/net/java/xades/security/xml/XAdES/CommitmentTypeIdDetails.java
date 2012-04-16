package net.java.xades.security.xml.XAdES;

import org.w3c.dom.Element;

/*
 * 
 * <p:CommitmentTypeId>
 *   <p:Identifier Qualifier="OIDAsURI">http://tempuri.org</p:Identifier>
 *   <p:Description>p:Description</p:Description>
 *   <p:DocumentationReferences>
 *     <p:DocumentationReference>http://tempuri.org</p:DocumentationReference>
 *   </p:DocumentationReferences>
 *  </p:CommitmentTypeId>
 * 
 */

public class CommitmentTypeIdDetails extends XAdESStructure
{
    public CommitmentTypeIdDetails(CommitmentTypeIndicationDetails commitmentTypeIndicationDetails,
            CommitmentTypeId commitmentTypeId, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix)
    {
        super(commitmentTypeIndicationDetails, "CommitmentTypeId", xadesPrefix, xadesNamespace,
                xmlSignaturePrefix);

        Element identifier = createElement("Identifier");
        identifier.setTextContent(commitmentTypeId.getIdentifier());
        identifier.setAttribute("Qualifier", commitmentTypeId.getQualifier());
        getNode().appendChild(identifier);

        Element description = createElement("Description");
        description.setTextContent(commitmentTypeId.getDescription());
        getNode().appendChild(description);

        if (commitmentTypeId.getDocumentationReferences().size() > 0)
        {
            Element documentationReferences = createElement("DocumentationReferences");

            for (String reference : commitmentTypeId.getDocumentationReferences())
            {
                Element documentationReference = createElement("DocumentationReference");
                documentationReference.setTextContent(reference);
                documentationReferences.appendChild(documentationReference);
            }

            getNode().appendChild(documentationReferences);
        }
    }
}
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
    public CommitmentTypeIdDetails(final CommitmentTypeIndicationDetails commitmentTypeIndicationDetails,
            final CommitmentTypeId commitmentTypeId, final String xadesPrefix, final String xadesNamespace,
            final String xmlSignaturePrefix)
    {
        super(commitmentTypeIndicationDetails, "CommitmentTypeId", xadesPrefix, xadesNamespace,
                xmlSignaturePrefix);

        final Element identifier = createElement("Identifier");
        identifier.setTextContent(commitmentTypeId.getIdentifier());
        identifier.setAttributeNS(xadesNamespace, "Qualifier", commitmentTypeId.getQualifier());
        getNode().appendChild(identifier);

        final Element description = createElement("Description");
        description.setTextContent(commitmentTypeId.getDescription());
        getNode().appendChild(description);

        if (commitmentTypeId.getDocumentationReferences().size() > 0)
        {
            final Element documentationReferences = createElement("DocumentationReferences");

            for (final String reference : commitmentTypeId.getDocumentationReferences())
            {
                final Element documentationReference = createElement("DocumentationReference");
                documentationReference.setTextContent(reference);
                documentationReferences.appendChild(documentationReference);
            }

            getNode().appendChild(documentationReferences);
        }
    }
}
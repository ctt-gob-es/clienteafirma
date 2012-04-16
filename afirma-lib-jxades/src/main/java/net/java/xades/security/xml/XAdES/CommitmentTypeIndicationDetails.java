package net.java.xades.security.xml.XAdES;

import org.w3c.dom.Element;

/*
 * <p:CommitmentTypeIndication>
 *   <p:CommitmentTypeId>
 *      <p:Identifier Qualifier="OIDAsURI">http://tempuri.org</p:Identifier>
 *      <p:Description>p:Description</p:Description>
 *      <p:DocumentationReferences>
 *        <p:DocumentationReference>http://tempuri.org</p:DocumentationReference>
 *      </p:DocumentationReferences>
 *   </p:CommitmentTypeId>
 *   <p:ObjectReference>http://tempuri.org</p:ObjectReference>
 *   <p:CommitmentTypeQualifiers>
 *     <p:CommitmentTypeQualifier>ANYTYPE</p:CommitmentTypeQualifier>
 *   </p:CommitmentTypeQualifiers>
 * </p:CommitmentTypeIndication>
 * 
 */

public class CommitmentTypeIndicationDetails extends XAdESStructure
{
    public CommitmentTypeIndicationDetails(SignedDataObjectProperties signedDataObjectProperties,
            CommitmentTypeIndication commitmentTypeIndication, String xadesPrefix,
            String xadesNamespace, String xmlSignaturePrefix)
    {
        super(signedDataObjectProperties, "CommitmentTypeIndication", xadesPrefix, xadesNamespace,
                xmlSignaturePrefix);

        CommitmentTypeId commitmentTypeId = commitmentTypeIndication.getCommitmentTypeId();

        if (commitmentTypeId != null)
        {
            new CommitmentTypeIdDetails(this, commitmentTypeId, xadesPrefix, xadesNamespace,
                    xmlSignaturePrefix);
        }

        Element objectReference = createElement("ObjectReference");
        objectReference.setTextContent(commitmentTypeIndication.getObjectReference());

        Element commitmentTypeQualifiers = createElement("CommitmentTypeQualifiers");

        for (String qualifier : commitmentTypeIndication.getCommitmentTypeQualifiers())
        {
            Element commitmentTypeQualifier = createElement("CommitmentTypeQualifier");
            commitmentTypeQualifier.setTextContent(qualifier);
            commitmentTypeQualifiers.appendChild(commitmentTypeQualifier);
        }

        getNode().appendChild(objectReference);
        getNode().appendChild(commitmentTypeQualifiers);
    }
}

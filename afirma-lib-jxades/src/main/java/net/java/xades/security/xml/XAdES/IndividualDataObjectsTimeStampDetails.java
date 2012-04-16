package net.java.xades.security.xml.XAdES;

import net.java.xades.util.Base64;

import org.w3c.dom.Element;

public class IndividualDataObjectsTimeStampDetails extends XAdESStructure
{
    public IndividualDataObjectsTimeStampDetails(
            SignedDataObjectProperties signedDataObjectProperties,
            IndividualDataObjectsTimeStamp individualDataObjectsTimeStampDetails,
            String xadesPrefix, String xadesNamespace, String xmlSignaturePrefix, String tsaURL)
    {
        super(signedDataObjectProperties, "IndividualDataObjectsTimeStamp", xadesPrefix,
                xadesNamespace, xmlSignaturePrefix);

        try
        {
            String tsBase64Data = Base64.encodeBytes(individualDataObjectsTimeStampDetails
                    .generateEncapsulatedTimeStamp(getDocument(), tsaURL));

            Element tsNode = createElement("EncapsulatedTimeStamp");
            tsNode.appendChild(getDocument().createTextNode(tsBase64Data));

            getNode().appendChild(tsNode);
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }
}

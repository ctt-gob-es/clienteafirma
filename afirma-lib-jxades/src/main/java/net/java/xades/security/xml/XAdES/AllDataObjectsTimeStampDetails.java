package net.java.xades.security.xml.XAdES;

import net.java.xades.util.Base64;

import org.w3c.dom.Element;

public class AllDataObjectsTimeStampDetails extends XAdESStructure
{
    public AllDataObjectsTimeStampDetails(SignedDataObjectProperties signedDataObjectProperties,
            AllDataObjectsTimeStamp allDataObjectsTimeStamp, String xadesPrefix,
            String xadesNamespace, String xmlSignaturePrefix, String tsaURL)
    {
        super(signedDataObjectProperties, "AllDataObjectsTimeStamp", xadesPrefix, xadesNamespace,
                xmlSignaturePrefix);

        try
        {
            String tsBase64Data = Base64.encodeBytes(allDataObjectsTimeStamp
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

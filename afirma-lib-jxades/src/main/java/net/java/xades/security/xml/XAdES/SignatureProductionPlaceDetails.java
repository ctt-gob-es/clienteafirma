package net.java.xades.security.xml.XAdES;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * 
 * Sample usage of signing SignatureProductionPlace:
 * 
 * <xades:SignatureProductionPlace> <xades:City>p:City</p:City>
 * <xades:StateOrProvince>p:StateOrProvince</p:StateOrProvince>
 * <xades:PostalCode>p:PostalCode</p:PostalCode> <xades:CountryName>p:CountryName</p:CountryName>
 * </xades:SignatureProductionPlace>
 * 
 */

public class SignatureProductionPlaceDetails extends XAdESStructure
{
    public SignatureProductionPlaceDetails(SignedSignatureProperties ssp,
            SignatureProductionPlace signatureProductionPlace, String xadesPrefix,
            String xadesNamespace, String xmlSignaturePrefix)
    {
        super(ssp, "SignatureProductionPlace", xadesPrefix, xadesNamespace, xmlSignaturePrefix);

        if (signatureProductionPlace.getCity() != null)
        {
            Element city = createElement("City");
            city.setTextContent(signatureProductionPlace.getCity());
            getNode().appendChild(city);
        }

        if (signatureProductionPlace.getStateOrProvince() != null)
        {
            Element stateOrProvince = createElement("StateOrProvince");
            stateOrProvince.setTextContent(signatureProductionPlace.getStateOrProvince());
            getNode().appendChild(stateOrProvince);
        }

        if (signatureProductionPlace.getPostalCode() != null)
        {
            Element postalCode = createElement("PostalCode");
            postalCode.setTextContent(signatureProductionPlace.getPostalCode());
            getNode().appendChild(postalCode);
        }

        if (signatureProductionPlace.getCountryName() != null)
        {
            Element countryName = createElement("CountryName");
            countryName.setTextContent(signatureProductionPlace.getCountryName());
            getNode().appendChild(countryName);
        }
    }

    public SignatureProductionPlaceDetails(Node node, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }
}
package net.java.xades.security.xml.XAdES;

import java.io.IOException;
import java.text.ParseException;
import java.util.Date;

import javax.security.auth.x500.X500Principal;

import net.java.xades.util.Base64;
import net.java.xades.util.SystemUtils;
import net.java.xades.util.XMLUtils;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/*
 <OCSPIdentifier URI="OCSPResponse1.der">
 <ResponderID>
 <ByName>String of X500Principal Name</ByName>
 or
 <ByKey>base64Binary of PublicKey DER value</ByKey>
 </ResponderID>
 <ProducedAt>2003−11−06T13:28:04+01:00</ProducedAt>
 </OCSPIdentifier>
 */

/**
 * 
 * @author miro
 */
public class OCSPIdentifier extends XAdESStructure
{
    private X500Principal responderName;
    private byte[] responderKey;
    private Date producedAt;

    // public OCSPIdentifier(XAdESStructure parent,
    // OCSPResponse ocspResponse,
    // URI ocspResponderURI)
    // throws GeneralSecurityException
    // {
    // super(parent, "OCSPIdentifier");
    //
    // Element thisElement = getElement();
    // if(ocspResponderURI != null)
    // setAttribute("URI", ocspResponderURI.toString());
    //
    // Element responderElement = createElement("ResponderID");
    // thisElement.appendChild(responderElement);
    //
    // BasicOCSPResponse basicOCSPResponse = ocspResponse.getBasicOCSPResponse();
    // byte[] key = basicOCSPResponse.getResponderKey();
    // if(key != null && key.length > 0)
    // {
    // Element element = createElement("ByKey");
    //            
    // String keyValue = Base64.encode(key);
    // element.setTextContent(keyValue);
    // responderElement.appendChild(element);
    // }
    // else
    // {
    // Element element = createElement("ByName");
    // element.setTextContent(basicOCSPResponse.getResponderName());
    // responderElement.appendChild(element);
    // }
    //
    // Element element = createElement("ProducedAt");
    // thisElement.appendChild(element);
    // element.setTextContent(SystemUtils.formatDate(basicOCSPResponse.getProducedAt()));
    // }

    public OCSPIdentifier(Node node, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public X500Principal getResponderName()
    {
        if (responderName == null)
        {
            Element element = getChildElementNS("ResponderID");
            if (element != null)
            {
                element = XMLUtils.getChildElementByTagName(element, "ByName");
                if (element != null)
                {
                    String value = element.getTextContent();
                    if (value != null && (value = value.trim()).length() > 0)
                        responderName = new X500Principal(value);
                }
            }
        }

        return responderName;
    }

    public byte[] getResponderKey() throws IOException
    {
        if (responderKey == null)
        {
            Element element = getChildElementNS("ResponderID");
            if (element != null)
            {
                element = XMLUtils.getChildElementByTagName(element, "ByKey");
                if (element != null)
                {
                    String value = element.getTextContent();
                    if (value != null && (value = value.trim()).length() > 0)
                        responderKey = Base64.decode(value);
                }
            }
        }

        return responderKey;
    }

    public Date getProducedAt() throws ParseException
    {
        if (producedAt == null)
        {
            String value = getChildElementTextContent("ProducedAt");
            if (value != null)
                producedAt = SystemUtils.parseDate(value);
        }

        return producedAt;
    }
}

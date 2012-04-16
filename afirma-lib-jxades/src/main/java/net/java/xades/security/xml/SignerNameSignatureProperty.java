package net.java.xades.security.xml;

import javax.xml.crypto.dom.DOMStructure;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 *
 * @author miro
 */
public class SignerNameSignatureProperty
    extends DOMStructure
{
    public SignerNameSignatureProperty(final Node node)
    {
        super(node.getOwnerDocument().createElement("SignerName"));
    }

    protected Element getElement()
    {
        return (Element)getNode();
    }

    public void setUserId(final String userId)
    {
        getElement().setAttributeNS(null, "UserId", userId);
    }

    public String getUserId()
    {
        return getElement().getAttribute("UserId");
    }

    public void setUsername(final String username)
    {
        getElement().setAttributeNS(null, "Username", username);
    }

    public String getUsername()
    {
        return getElement().getAttribute("Username");
    }

    public void setPersonName(final String personName)
    {
        getElement().setTextContent(personName);
    }

    public String getPersonName()
    {
        return getElement().getTextContent();
    }
}

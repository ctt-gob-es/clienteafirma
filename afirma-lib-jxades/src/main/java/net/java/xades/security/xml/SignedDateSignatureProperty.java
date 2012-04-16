package net.java.xades.security.xml;

import net.java.xades.util.SystemUtils;

import java.util.Date;
import javax.xml.crypto.dom.DOMStructure;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 *
 * @author miro
 */
public class SignedDateSignatureProperty
    extends DOMStructure
{
    public SignedDateSignatureProperty(Node node)
    {
        this(node.getOwnerDocument(), new Date());
    }

    public SignedDateSignatureProperty(Node node, Date date)
    {
        super(node.getOwnerDocument().createElement("SignedDate"));
        Element signedDate = (Element)getNode();
        signedDate.setTextContent(SystemUtils.formatDate(date));
    }
}

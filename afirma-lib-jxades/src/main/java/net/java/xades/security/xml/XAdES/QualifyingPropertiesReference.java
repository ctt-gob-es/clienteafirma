package net.java.xades.security.xml.XAdES;

import javax.xml.crypto.dom.DOMStructure;
import org.w3c.dom.Node;

/**
 * 
 * @author miro
 */
public class QualifyingPropertiesReference extends DOMStructure
{

    public QualifyingPropertiesReference(Node node, String xadesNamespace)
    {
        super(node.getOwnerDocument().createElementNS(xadesNamespace,
                "QualifyingPropertiesReference"));
    }

}

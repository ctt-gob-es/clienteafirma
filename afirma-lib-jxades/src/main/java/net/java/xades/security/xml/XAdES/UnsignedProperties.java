package net.java.xades.security.xml.XAdES;

import org.w3c.dom.Node;

/*
 */

/**
 * 
 * @author miro
 */
public class UnsignedProperties extends XAdESStructure
{
    private UnsignedSignatureProperties unsignedSignatureProperties;

    public UnsignedProperties(QualifyingProperties qp, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix)
    {
        super(qp, "UnsignedProperties", xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public UnsignedProperties(Node node, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public UnsignedSignatureProperties getUnsignedSignatureProperties()
    {
        if (unsignedSignatureProperties == null)
        {
            unsignedSignatureProperties = new UnsignedSignatureProperties(this, xadesPrefix,
                    xadesNamespace, xmlSignaturePrefix);
        }

        return unsignedSignatureProperties;
    }

}

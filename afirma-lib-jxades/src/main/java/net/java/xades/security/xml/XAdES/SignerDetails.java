package net.java.xades.security.xml.XAdES;

import org.w3c.dom.Node;

/**
 * 
 * @author miro
 */
public class SignerDetails extends XAdESStructure
{
    public static final String USERNAME_ATTRIBUTE = "Username";

    public SignerDetails(SignedSignatureProperties ssp, Signer signer, String xadesPrefix,
            String xadesNamespace, String xmlSignaturePrefix)
    {
        super(ssp, "SignerDetails", xadesPrefix, xadesNamespace, xmlSignaturePrefix);

        String value = signer.getPersonName();
        if (value == null)
            throw new IllegalArgumentException("The Signer personal name can not be NULL.");
        setTextContent(value);

        value = signer.getUserId();
        if (value != null)
            setAttribute(ID_ATTRIBUTE, value);

        value = signer.getUsername();
        if (value != null)
            setAttribute(USERNAME_ATTRIBUTE, value);
    }

    public SignerDetails(Node node, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public Signer getSigner()
    {
        SignerImpl signer = new SignerImpl();
        signer.setPersonName(getTextContent());
        signer.setUserId(getAttribute(ID_ATTRIBUTE));
        signer.setUsername(getAttribute(USERNAME_ATTRIBUTE));

        return signer;
    }
}

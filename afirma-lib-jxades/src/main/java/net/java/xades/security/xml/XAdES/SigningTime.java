package net.java.xades.security.xml.XAdES;

import net.java.xades.util.SystemUtils;

import java.text.ParseException;
import java.util.Date;
import org.w3c.dom.Node;

/**
 * 
 * @author miro
 */
public class SigningTime extends XAdESStructure
{
    public SigningTime(SignedSignatureProperties ssp, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix)
    {
        this(ssp, new Date(), xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public SigningTime(SignedSignatureProperties ssp, Date signingTime, String xadesPrefix,
            String xadesNamespace, String xmlSignaturePrefix)
    {
        super(ssp, "SigningTime", xadesPrefix, xadesNamespace, xmlSignaturePrefix);
        getElement().setTextContent(SystemUtils.formatDate(signingTime));
    }

    public SigningTime(Node node, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public Date getSigningTime() throws ParseException
    {
        String value = getTextContent();
        
        if (value != null)
        {
            return SystemUtils.parseDate(value);
        }
        else
        {
            return null;
        }
    }
}

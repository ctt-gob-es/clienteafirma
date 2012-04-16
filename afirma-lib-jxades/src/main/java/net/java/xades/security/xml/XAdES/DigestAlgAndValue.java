package net.java.xades.security.xml.XAdES;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.MessageDigest;
import java.security.cert.X509CRL;

import javax.xml.crypto.dsig.DigestMethod;

import net.java.xades.util.Base64;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/*
 <DigestAlgAndValue>
 <DigestMethod Algorithm= />
 <DigestValue />
 </DigestAlgAndValue>
 */

/**
 *
 * @author miro
 */
public class DigestAlgAndValue extends XAdESStructure
{
    private static final String ALGORITHM_ATTR = "Algorithm";
    private static final String DIGEST_METHOD_ELEMENT = "DigestMethod";
    private static final String DIGEST_VALUE_ELEMENT = "DigestValue";

    // public DigestAlgAndValue(XAdESStructure parent, OCSPResponse ocspResponse)
    // throws GeneralSecurityException
    // {
    // this(parent, "DigestAlgAndValue", ocspResponse.getResponseData());
    // }

    public DigestAlgAndValue(final XAdESStructure parent, final X509CRL crl, final String xadesPrefix,
            final String xadesNamespace, final String xmlSignaturePrefix) throws GeneralSecurityException
    {
        this(parent, "DigestAlgAndValue", crl.getEncoded(), xadesPrefix, xadesNamespace,
                xmlSignaturePrefix);
    }

    protected DigestAlgAndValue(final XAdESStructure parent, final String elementName, final byte[] data,
            final String xadesPrefix, final String xadesNamespace, final String xmlSignaturePrefix)
            throws GeneralSecurityException
    {
        super(parent, elementName, xadesPrefix, xadesNamespace, xmlSignaturePrefix);

        final Element thisElement = getElement();

        Element element = createElement(DIGEST_METHOD_ELEMENT);
        element.setPrefix(xmlSignaturePrefix);
        thisElement.appendChild(element);
        element.setAttributeNS(null, ALGORITHM_ATTR, DigestMethod.SHA1);

        final MessageDigest md = MessageDigest.getInstance("SHA-1");

        final String digestValue = Base64.encodeBytes(md.digest(data));
        element = createElement(DIGEST_VALUE_ELEMENT);
        element.setPrefix(xmlSignaturePrefix);
        thisElement.appendChild(element);
        element.setTextContent(digestValue);
    }

    protected DigestAlgAndValue(final XAdESStructure parent, final String xadesPrefix, final String xadesNamespace,
            final String xmlSignaturePrefix) throws GeneralSecurityException
    {
        super(parent, "DigestAlgAndValue", xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public DigestAlgAndValue(final Node node, final String xadesPrefix, final String xadesNamespace,
            final String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public String getDigestMethod()
    {
        return getChildElementTextContent(DIGEST_METHOD_ELEMENT);
    }

    public byte[] getDigestValue() throws IOException
    {
        String value = getChildElementTextContent(DIGEST_VALUE_ELEMENT);

        if (value != null && (value = value.trim()).length() > 0)
        {
            return Base64.decode(value);
        }

        return null;
    }
}

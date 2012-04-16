package net.java.xades.security.xml;

import java.security.AccessController;
import java.security.Key;
import java.security.KeyException;
import java.security.PublicKey;
import java.security.Security;
import java.security.cert.X509Certificate;
import java.util.Iterator;
import java.util.List;
import javax.xml.crypto.AlgorithmMethod;
import javax.xml.crypto.KeySelector;
import javax.xml.crypto.KeySelectorException;
import javax.xml.crypto.KeySelectorResult;
import javax.xml.crypto.MarshalException;
import javax.xml.crypto.XMLCryptoContext;
import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.SignatureMethod;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignature.SignatureValue;
import javax.xml.crypto.dsig.XMLSignatureException;
import javax.xml.crypto.dsig.XMLSignatureFactory;
import javax.xml.crypto.dsig.dom.DOMValidateContext;
import javax.xml.crypto.dsig.keyinfo.KeyInfo;
import javax.xml.crypto.dsig.keyinfo.KeyValue;
import javax.xml.crypto.dsig.keyinfo.X509Data;

import net.java.xades.security.xml.InvalidSignature;
import net.java.xades.security.xml.InvalidSignatureReason;
import net.java.xades.security.xml.SignatureStatus;
import net.java.xades.security.xml.ValidateResult;
import net.java.xades.security.xml.X509DataKeySelectorResult;
import net.java.xades.security.xml.XAdES.BasicXAdES;

import org.w3c.dom.Element;

/**
 * 
 * @author miro
 */
public class XMLSignatureElement
{
    private Element signatureElement;
    private XMLSignatureFactory xmlSignatureFactory;
    private KeySelector keySelector;

    static
    {
        AccessController.doPrivileged(new java.security.PrivilegedAction<Void>()
        {
            public Void run()
            {
                if (System.getProperty("java.version").startsWith("1.5"))
                {
                    try
                    {
                        Security.addProvider(new org.jcp.xml.dsig.internal.dom.XMLDSigRI());
                    }
                    catch (Throwable e)
                    {
                        e.printStackTrace();
                    }
                }
                return null;
            }
        });
    }

    /** Creates a new instance of XMLSignatureElement */
    public XMLSignatureElement(Element signatureElement)
    {
        if (signatureElement == null)
        {
            throw new IllegalArgumentException("Signature Element can not be NULL.");
        }
        this.signatureElement = signatureElement;
    }

    protected XMLSignatureFactory getXMLSignatureFactory()
    {
        if (xmlSignatureFactory == null)
        {
            xmlSignatureFactory = XMLSignatureFactory.getInstance("DOM");
        }
        return xmlSignatureFactory;
    }

    protected KeySelector getKeySelector()
    {
        if (keySelector == null)
        {
            keySelector = new KeyValueKeySelector();
        }
        return keySelector;
    }

    public XMLSignature getXMLSignature() throws MarshalException
    {
        DOMValidateContext valContext = new DOMValidateContext(getKeySelector(), signatureElement);
        XMLSignatureFactory fac = getXMLSignatureFactory();
        return fac.unmarshalXMLSignature(valContext);
    }

    public KeyInfo getKeyInfo() throws MarshalException
    {
        XMLSignature xmlSignature = getXMLSignature();
        if (xmlSignature != null)
        {
            return xmlSignature.getKeyInfo();
        }

        return null;
    }

    public X509Data getX509Data() throws MarshalException
    {
        KeyInfo keyInfo = getKeyInfo();
        if (keyInfo != null)
        {
            for (Object o1 : keyInfo.getContent())
            {
                if (o1 instanceof X509Data)
                {
                    return (X509Data) o1;
                }
            }
        }

        return null;
    }

    public X509Certificate getX509Certificate() throws MarshalException
    {
        X509Data x509Data = getX509Data();
        if (x509Data != null)
        {
            for (Object o1 : x509Data.getContent())
            {
                if (o1 instanceof X509Certificate)
                {
                    return (X509Certificate) o1;
                }
            }
        }

        return null;
    }

    public SignatureStatus validate()
    {
        DOMValidateContext valContext = new DOMValidateContext(getKeySelector(), signatureElement);
        XMLSignatureFactory fac = getXMLSignatureFactory();
        String signatureId = signatureElement.getAttribute("Id");
        XMLSignature signature = null;
        try
        {
            signature = fac.unmarshalXMLSignature(valContext);
        }
        catch (NullPointerException ex)
        {
            return new SignatureStatus(signatureId, ex);
        }
        catch (ClassCastException ex)
        {
            return new SignatureStatus(signatureId, ex);
        }
        catch (MarshalException ex)
        {
            return new SignatureStatus(signatureId, ex);
        }

        String signId = signature.getId();
        if (signatureId == null || (signId != null && !signId.equals(signatureId)))
            signatureId = signId;

        boolean status = false;
        try
        {
            status = signature.validate(valContext);
        }
        catch (ClassCastException ex)
        {
            InvalidSignatureReason reason = new InvalidSignatureReason(
                    InvalidSignature.NOT_COMPATIBLE_VALIDATE_CONTEXT, ex);
            return new SignatureStatus(signatureId, ValidateResult.INVALID, reason);
        }
        catch (NullPointerException ex)
        {
            InvalidSignatureReason reason = new InvalidSignatureReason("XMLSignature", ex);
            return new SignatureStatus(signatureId, ValidateResult.INVALID, reason);
        }
        catch (XMLSignatureException ex)
        {
            InvalidSignatureReason reason = new InvalidSignatureReason("XMLSignature", ex);
            return new SignatureStatus(signatureId, ValidateResult.INVALID, reason);
        }

        SignatureStatus validateResult;

        if (!status)
        {
            validateResult = new SignatureStatus(signatureId, ValidateResult.INVALID);
            SignatureValue sv = signature.getSignatureValue();
            try
            {
                if (!sv.validate(valContext))
                {
                    InvalidSignatureReason reason = new InvalidSignatureReason(sv);
                    validateResult.addInvalidSignatureReason(reason);
                }
            }
            catch (NullPointerException ex)
            {
                InvalidSignatureReason reason = new InvalidSignatureReason("SignatureValue", ex);
                validateResult.addInvalidSignatureReason(reason);
            }
            catch (XMLSignatureException ex)
            {
                InvalidSignatureReason reason = new InvalidSignatureReason("SignatureValue", ex);
                validateResult.addInvalidSignatureReason(reason);
            }

            Iterator iter = signature.getSignedInfo().getReferences().iterator();
            for (int i = 0; iter.hasNext(); i++)
            {
                Reference ref = (Reference) iter.next();
                try
                {
                    if (!ref.validate(valContext))
                    {
                        InvalidSignatureReason reason = new InvalidSignatureReason(ref);
                        validateResult.addInvalidSignatureReason(reason);
                    }
                }
                catch (NullPointerException ex)
                {
                    InvalidSignatureReason reason = new InvalidSignatureReason("Reference", ex);
                    validateResult.addInvalidSignatureReason(reason);
                }
                catch (XMLSignatureException ex)
                {
                    InvalidSignatureReason reason = new InvalidSignatureReason("Reference", ex);
                    validateResult.addInvalidSignatureReason(reason);
                }
            }
        }
        else
        {
            validateResult = new SignatureStatus(signatureId, ValidateResult.VALID);
        }

        return validateResult;
    }

    private static class KeyValueKeySelector extends KeySelector
    {
        public KeySelectorResult select(KeyInfo keyInfo, KeySelector.Purpose purpose,
                AlgorithmMethod method, XMLCryptoContext context) throws KeySelectorException
        {
            if (keyInfo == null)
            {
                throw new KeySelectorException("Null KeyInfo object!");
            }
            SignatureMethod sm = (SignatureMethod) method;
            List list = keyInfo.getContent();

            for (int i = 0; i < list.size(); i++)
            {
                XMLStructure xmlStructure = (XMLStructure) list.get(i);
                if (xmlStructure instanceof KeyValue)
                {
                    PublicKey pk = null;
                    try
                    {
                        pk = ((KeyValue) xmlStructure).getPublicKey();
                    }
                    catch (KeyException ke)
                    {
                        throw new KeySelectorException(ke);
                    }
                    // make sure algorithm is compatible with method
                    if (algEquals(sm.getAlgorithm(), pk.getAlgorithm()))
                    {
                        return new SimpleKeySelectorResult(pk);
                    }
                }
                else if (xmlStructure instanceof X509Data)
                {
                    X509Certificate cert = null;
                    List dataList = ((X509Data) xmlStructure).getContent();
                    for (Object dataObject : dataList)
                    {
                        if (dataObject instanceof X509Certificate)
                            cert = (X509Certificate) dataObject;
                    }
                    if (cert != null)
                    {
                        PublicKey pk = cert.getPublicKey();

                        if (algEquals(sm.getAlgorithm(), pk.getAlgorithm()))
                        {
                            return new X509DataKeySelectorResult((X509Data) xmlStructure);
                        }
                    }
                }
            }
            throw new KeySelectorException("No KeyValue element found!");
        }

        static boolean algEquals(String algURI, String algName)
        {
            if (algName.equalsIgnoreCase("DSA") && algURI.toUpperCase().contains("DSA"))
            {
                return true;
            }
            else
            {
                if (algName.equalsIgnoreCase("RSA") && algURI.toUpperCase().contains("RSA"))
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
        }
    }

    private static class SimpleKeySelectorResult implements KeySelectorResult
    {
        private PublicKey pk;

        SimpleKeySelectorResult(PublicKey pk)
        {
            this.pk = pk;
        }

        public Key getKey()
        {
            return pk;
        }
    }

}

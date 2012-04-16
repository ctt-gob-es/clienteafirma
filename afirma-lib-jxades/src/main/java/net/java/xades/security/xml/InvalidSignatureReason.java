package net.java.xades.security.xml;

import javax.xml.crypto.MarshalException;
import javax.xml.crypto.dsig.Reference;
import javax.xml.crypto.dsig.XMLSignature;
import javax.xml.crypto.dsig.XMLSignatureException;

import net.java.xades.security.*;
import net.java.xades.util.ComparableBean;
import net.java.xades.util.SystemUtils;
import net.java.xades.util.UniversalIndexKey;

/**
 *
 * @author miro
 */
public class InvalidSignatureReason
    implements ComparableBean
{
    private InvalidSignature invalidSignature;
    private String reason;
    private Comparable key;

    public InvalidSignatureReason()
    {
    }

    public InvalidSignatureReason(MarshalException ex)
    {
        invalidSignature = InvalidSignature.WRONG_XML_SIGNATURE;
        reason = "Wrong XML signature: " + SystemUtils.getCauseMessages(ex);
    }

    public InvalidSignatureReason(InvalidSignature invalidSignature, ClassCastException ex)
    {
        this.invalidSignature = invalidSignature;
        if(InvalidSignature.NOT_COMPATIBLE_VALIDATE_CONTEXT.equals(invalidSignature))
            reason = "Not compatible validate context: " + SystemUtils.getCauseMessages(ex);
        else
            reason = "Inappropriate XML structure: " + SystemUtils.getCauseMessages(ex);
    }

    public InvalidSignatureReason(String source, NullPointerException ex)
    {
        invalidSignature = InvalidSignature.NULL_VALIDATE_CONTEXT;
        reason = "NULL " + source + " validate context: " + SystemUtils.getCauseMessages(ex);
    }

    public InvalidSignatureReason(String source, XMLSignatureException ex)
    {
        invalidSignature = InvalidSignature.UNEXPECTED_EXCEPTION;
        reason = "Unexpected exception occurs in " + source + " while validating the signature: " + SystemUtils.getCauseMessages(ex);
    }

    public InvalidSignatureReason(XMLSignature.SignatureValue signatureValue)
    {
        invalidSignature = InvalidSignature.BAD_SIGNATURE_VALUE;
        StringBuilder sb = new StringBuilder();
        sb.append("Bad signature value");
        String id = signatureValue.getId();
        if(id != null && (id = id.trim()).length() > 0)
            sb.append(" with Id '").append(id).append("'");
        reason = sb.toString();
    }

    public InvalidSignatureReason(Reference reference)
    {
        invalidSignature = InvalidSignature.BAD_REFERENCE;
        StringBuilder sb = new StringBuilder();
        sb.append("Bad reference");
        String id = reference.getId();
        if(id != null && (id = id.trim()).length() > 0)
            sb.append(" with Id '").append(id).append("'");
        String uri = reference.getURI();
        if(uri != null && (uri = uri.trim()).length() > 0)
        {
            if(id != null && id.length() > 0)
                sb.append(" and URI = '");
            else
                sb.append(" with URI = '");
            sb.append(uri).append("'");
        }
        reason = sb.toString();
    }

    public InvalidSignature getInvalidSignature()
    {
        return invalidSignature;
    }

    public String getReason()
    {
        return reason;
    }

    public Comparable getIndexKey()
    {
        if(key == null)
        {
            key = new UniversalIndexKey(invalidSignature.getDescription(), reason);
        }
        return key;
    }
}

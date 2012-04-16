package net.java.xades.security.xml;

/**
 *
 * @author miro
 */
public enum InvalidSignature
{
    // MarshalException, XMLSignatureFactory.unmarshalXMLSignature
    WRONG_XML_SIGNATURE("Wrong XML Signature"),

    // ClassCastException, XMLSignatureFactory.unmarshalXMLSignature
    INAPPROPRIATE_XML_CONTEXT("Inappropriate XML Context"),

    // ClassCastException, XMLSignature.validate
    NOT_COMPATIBLE_VALIDATE_CONTEXT("Not compatible Validate Context"),

    // NullPointerException
    NULL_VALIDATE_CONTEXT("Null/Empty Validate Context"),

    // SignatureValue.validate
    BAD_SIGNATURE_VALUE("Bad Signature Value"),

    // SignedInfo.Reference.validate
    BAD_REFERENCE("Bad Reference"),

    // XMLSignatureException
    UNEXPECTED_EXCEPTION("Unexpected Exception");

    private InvalidSignature(String description)
    {
        this.description = description;
    }

    public String getDescription()
    {
        return description;
    }

    private String description;
}

package net.java.xades.security.xml;

/**
 *
 * @author miro
 */
public enum XmlWrappedKeyInfo
{
    PUBLIC_KEY("PublicKey"),
    CERTIFICATE("Certificate");

    private XmlWrappedKeyInfo(String wrappedKeyInfoName)
    {
        this.wrappedKeyInfoName = wrappedKeyInfoName;
    }

    public String getWrappedKeyInfoName()
    {
        return wrappedKeyInfoName;
    }

    public String toString()
    {
        return getWrappedKeyInfoName();
    }

    private String wrappedKeyInfoName;
}

package net.java.xades.security.xml.XAdES;

import java.security.GeneralSecurityException;
import java.security.cert.X509Certificate;

import org.w3c.dom.Node;

/*
 <CertDigest>
 <DigestMethod Algorithm="" />
 <DigestValue></DigestValue>
 </CertDigest>
 */

/**
 * 
 * @author miro
 */
public class CertDigest extends DigestAlgAndValue
{
    public CertDigest(XAdESStructure parent, X509Certificate cert, String xadesPrefix,
            String xadesNamespace, String xmlSignaturePrefix) throws GeneralSecurityException
    {
        super(parent, "CertDigest", cert.getEncoded(), xadesPrefix, xadesNamespace,
                xmlSignaturePrefix);
    }

    public CertDigest(Node node, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }
}

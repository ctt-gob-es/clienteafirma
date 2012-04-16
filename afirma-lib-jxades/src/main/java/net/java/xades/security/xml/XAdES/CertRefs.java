package net.java.xades.security.xml.XAdES;

import java.security.GeneralSecurityException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/*
 <CertRefs Id="">
 <Cert>
 <CertDigest>
 <DigestMethod Algorithm="" />
 <DigestValue></DigestValue>
 </CertDigest>
 <IssuerSerial>
 <X509IssuerName></X509IssuerName>
 <X509SerialNumber><X509SerialNumber>
 </IssuerSerial>
 </Cert>
 </CertRefs>
 */

/**
 *
 * @author miro
 */
public class CertRefs extends XAdESStructure
{
    private List<Cert> certs;

    public CertRefs(final XAdESStructure parent, final Collection<X509Certificate> certificates,
            final String signatureIdPrefix, final String xadesPrefix, final String xadesNamespace,
            final String xmlSignaturePrefix) throws GeneralSecurityException
    {
        super(parent, "CertRefs", xadesPrefix, xadesNamespace, xmlSignaturePrefix);

        if (certificates == null || certificates.isEmpty()) {
			throw new IllegalArgumentException(
                    "The certificates collection can not be NULL or empty.");
		}

        getElement();
        if (signatureIdPrefix != null)
        {
            setAttributeNS(null, "Id", signatureIdPrefix + "-CertRefs");
        }

        this.certs = new ArrayList<Cert>(certificates.size());

        for (final X509Certificate certificate : certificates)
        {
            final Cert cert = new Cert(this, certificate, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
            this.certs.add(cert);
        }
    }

    public CertRefs(final Node node, final String xadesPrefix, final String xadesNamespace, final String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public List<Cert> getCerts()
    {
        if (this.certs == null)
        {
            final List<Element> elements = getChildElementsNS("Cert");
            if (elements != null && elements.size() > 0)
            {
                this.certs = new ArrayList<Cert>(elements.size());
                for (final Element element : elements)
                {
                    this.certs.add(new Cert(element, this.xadesPrefix, this.xadesNamespace, this.xmlSignaturePrefix));
                }
            }
            else
            {
                this.certs = Collections.<Cert> emptyList();
            }
        }

        return this.certs;
    }
}

package net.java.xades.security.xml.XAdES;

import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.text.ParseException;
import java.util.Date;

import javax.security.auth.x500.X500Principal;

import net.java.xades.util.SystemUtils;

import org.w3c.dom.Node;

/*
 <CRLIdentifier URI="#Signature_1_EncapsulatedCRLValue_1">
 <Issuer>...</Issuer>
 <IssueTime>...</IssueTime>
 <Number>...</Number>
 </CRLIdentifier>
 */

/**
 *
 * @author miro
 */
public class CRLIdentifier extends XAdESStructure
{
    private URI crlURI;
    private X500Principal issuer;
    private Date issueTime;
    private BigInteger crlNumber;

    // public CRLIdentifier(XAdESStructure parent,
    // XAdESRevocationStatus revocationStatus)
    // throws GeneralSecurityException
    // {
    // super(parent, "CRLIdentifier");
    //
    // Element thisElement = getElement();
    //
    // URI crlURI = revocationStatus.getCrlURI();
    // if(crlURI != null)
    // setAttributeNS(null, "URI", crlURI.toString());
    //
    // X509CRL crl = revocationStatus.getCheckedCRL();
    //
    // Element element = createElement("Issuer");
    // thisElement.appendChild(element);
    // element.setTextContent(crl.getIssuerX500Principal().getName());
    //
    // element = createElement("IssueTime");
    // thisElement.appendChild(element);
    // element.setTextContent(SystemUtils.formatDate(crl.getThisUpdate()));
    //
    // X509CRLImpl crlImpl = X509CRLImpl.toImpl(crl);
    // try
    // {
    // BigInteger crlNumber = crlImpl.getCRLNumber();
    // if(crlNumber != null)
    // {
    // element = createElement("Number");
    // thisElement.appendChild(element);
    // element.setTextContent(crlNumber.toString());
    // }
    // }
    // catch(IOException ex)
    // {
    // ex.printStackTrace();
    // }
    // }

    public CRLIdentifier(final Node node, final String xadesPrefix, final String xadesNamespace,
            final String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public X500Principal getIssuer()
    {
        if (this.issuer == null)
        {
            final String value = getChildElementTextContent("Issuer");
            if (value != null) {
				this.issuer = new X500Principal(value);
			}
        }

        return this.issuer;
    }

    public Date getIssueTime() throws ParseException
    {
        if (this.issueTime == null)
        {
            final String value = getChildElementTextContent("IssueTime");
            if (value != null) {
				this.issueTime = SystemUtils.parseDate(value);
			}
        }

        return this.issueTime;
    }

    public BigInteger getCRLNumber()
    {
        if (this.crlNumber == null)
        {
            final String value = getChildElementTextContent("Number");
            if (value != null) {
				this.crlNumber = new BigInteger(value);
			}
        }

        return this.crlNumber;
    }

    public URI getCrlURI() throws URISyntaxException
    {
        if (this.crlURI == null)
        {
            final String value = getAttribute("URI");
            if (value != null) {
				this.crlURI = new URI(value);
			}
        }

        return this.crlURI;
    }
}

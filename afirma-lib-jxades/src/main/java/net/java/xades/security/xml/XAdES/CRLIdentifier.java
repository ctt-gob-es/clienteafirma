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
    // setAttribute("URI", crlURI.toString());
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

    public CRLIdentifier(Node node, String xadesPrefix, String xadesNamespace,
            String xmlSignaturePrefix)
    {
        super(node, xadesPrefix, xadesNamespace, xmlSignaturePrefix);
    }

    public X500Principal getIssuer()
    {
        if (issuer == null)
        {
            String value = getChildElementTextContent("Issuer");
            if (value != null)
                issuer = new X500Principal(value);
        }

        return issuer;
    }

    public Date getIssueTime() throws ParseException
    {
        if (issueTime == null)
        {
            String value = getChildElementTextContent("IssueTime");
            if (value != null)
                issueTime = SystemUtils.parseDate(value);
        }

        return issueTime;
    }

    public BigInteger getCRLNumber()
    {
        if (crlNumber == null)
        {
            String value = getChildElementTextContent("Number");
            if (value != null)
                crlNumber = new BigInteger(value);
        }

        return crlNumber;
    }

    public URI getCrlURI() throws URISyntaxException
    {
        if (crlURI == null)
        {
            String value = getAttribute("URI");
            if (value != null)
                crlURI = new URI(value);
        }

        return crlURI;
    }
}

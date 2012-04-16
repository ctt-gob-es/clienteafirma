package net.java.xades.security;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.security.GeneralSecurityException;
import java.security.MessageDigest;
import java.security.PrivateKey;
import java.security.Provider;
import java.security.cert.CertPath;
import java.security.cert.CertPathValidatorException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateExpiredException;
import java.security.cert.CertificateFactory;
import java.security.cert.CertificateNotYetValidException;
import java.security.cert.X509Certificate;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import javax.security.auth.x500.X500Principal;

import net.java.xades.util.Base64;
import net.java.xades.util.ComparableBean;
import net.java.xades.util.SystemUtils;
import net.java.xades.util.UniversalIndexKey;
import sun.security.x509.AccessDescription;
import sun.security.x509.AuthorityInfoAccessExtension;
import sun.security.x509.AuthorityKeyIdentifierExtension;
import sun.security.x509.CRLDistributionPointsExtension;
import sun.security.x509.DistributionPoint;
import sun.security.x509.GeneralName;
import sun.security.x509.GeneralNameInterface;
import sun.security.x509.GeneralNames;
import sun.security.x509.IssuerAlternativeNameExtension;
import sun.security.x509.KeyIdentifier;
import sun.security.x509.SerialNumber;
import sun.security.x509.SubjectKeyIdentifierExtension;
import sun.security.x509.URIName;
import sun.security.x509.X500Name;
import sun.security.x509.X509CertImpl;

/**
 * <p>Title: </p>
 *
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2006</p>
 *
 * <p>Company: </p>
 *
 * @author not attributable
 * @version 1.0
 */
public class CertificateHelper
    implements ComparableBean
{
/*
	 openssl req -config /usr/lib/ssl/openssl.cnf -new -x509 -keyout test-cert-1.p12 -out test-cert-1.p12 -days 3650
*/

    private static final String EMAIL_ADDRESS_NAME = "EMAILADDRESS";

    X509Certificate certificate = null;
    X509CertImpl certificateImpl;
    X509Certificate certificateChain[] = null;
    X509Certificate trustedCACertificates[] = null;
    CertPath certPath = null;
    CertificateFactory certFactory = null;
    String thumbprintAlgorithm = "SHA1";
    X500Name subjectName = null;
    X500Name issuerName = null;
    String eMail = null;
    Provider provider;
    PrivateKey privateKey;

    byte certEncoded[] = null;
    String alias = null;

    private Comparable<UniversalIndexKey> indexKey;

//	{
//		keyStore = KeyStore.getInstance("PKCS12");
//		stream = new FileInputStream(fileName);
//	}
//	else
//		keyStore = KeyStore.getInstance("PKCS11");
//	keyStore.load(stream, passwordChars);

    public CertificateHelper()
    {
    }

    public CertificateHelper(X509Certificate certificate)
    {
        this.certificate = certificate;
    }

    public Comparable<UniversalIndexKey> getIndexKey()
    {
        if(indexKey == null)
        {
            try
            {
                indexKey = new UniversalIndexKey(getIssuerCommonName(), getSerialNumberAsString());
            }
            catch(Exception ex)
            {
                ex.printStackTrace();
            }
        }
        
        return indexKey;
    }

    public boolean equals(Object other)
    {
        if(other != null && (other instanceof CertificateHelper))
            return getIndexKey().equals(((CertificateHelper)other).getIndexKey());
        return false;
    }

    public X509Certificate getCertificate()
    {
        return certificate;
    }

    public X509CertImpl getCertificateImpl()
        throws CertificateException
    {
        if(certificateImpl == null)
        {
            certificateImpl = X509CertImpl.toImpl(getCertificate());
        }
        return certificateImpl;
    }

    public Provider getProvider()
    {
        return provider;
    }

	public void setProvider(Provider provider) 
	{
		this.provider = provider;
	}
    
    public PrivateKey getPrivateKey()
    {
    	return this.privateKey;
    }

	public void setPrivateKey(PrivateKey privateKey) 
	{
		this.privateKey = privateKey;
	}

    public void verifyCertificate()
        throws CertificateExpiredException,
        CertificateNotYetValidException
    {
        verifyCertificate(null);
    }

    public void verifyCertificate(Date date)
        throws CertificateExpiredException,
        CertificateNotYetValidException
    {
        X509Certificate cert = getCertificate();
        if(date == null)
            cert.checkValidity();
        else
            cert.checkValidity(date);
    }

    public CertificateFactory getCertificateFactory()
        throws CertificateException
    {
        if(certFactory == null)
        {
            certFactory = CertificateFactory.getInstance("X.509");
        }

        return certFactory;
    }

    public URI getOCSPServerURI()
        throws GeneralSecurityException, IOException
    {
        AuthorityInfoAccessExtension aia = getAuthorityInfoAccessExtension();
        if(aia != null)
        {
            List<AccessDescription> descriptions;
            descriptions = (List<AccessDescription>) aia.get(AuthorityInfoAccessExtension.DESCRIPTIONS);

            for(AccessDescription description : descriptions)
            {
                if(description.getAccessMethod().equals((Object)AccessDescription.Ad_OCSP_Id))
                {
                    GeneralName generalName = description.getAccessLocation();
                    if(generalName.getType() == GeneralNameInterface.NAME_URI)
                    {
                        try
                        {
                            URIName uri = (URIName)generalName.getName();
                            return (new URI(uri.getName()));

                        }
                        catch(URISyntaxException ex)
                        {
                            throw new CertPathValidatorException(ex);
                        }
                    }
                }
            }
        }

        return null;
    }

    public URI getCRLDistributionPointsURI()
        throws IOException,
        GeneralSecurityException,
        URISyntaxException
    {
        CRLDistributionPointsExtension crlDistributionPoints;
        crlDistributionPoints = getCRLDistributionPointsExtension();
        if(crlDistributionPoints != null)
        {
            Object obj = crlDistributionPoints.get(CRLDistributionPointsExtension.POINTS);
            if(obj != null && obj instanceof List)
            {
                List list = (List)obj;
                DistributionPoint dPoints[] = (DistributionPoint[])list.toArray(new DistributionPoint[list.size()]);
                for(int i = 0; i < dPoints.length; i++)
                {
                    DistributionPoint dPoint = dPoints[i];
                    GeneralNames fullName = dPoint.getFullName();
                    if(fullName != null && !fullName.isEmpty())
                    {
                        Iterator<GeneralName> iter = fullName.iterator();
                        while(iter.hasNext())
                        {
                            GeneralNameInterface nameObject = iter.next().getName();
                            if(nameObject != null && nameObject instanceof URIName)
                            {
                                return ((URIName)nameObject).getURI();
                            }
                        }
                    }
                }
            }
        }

        return null;
    }

    public KeyIdentifier getSubjectKeyIdentifier()
        throws CertificateException,
        IOException
    {
        SubjectKeyIdentifierExtension subjectKeyIdExt;
        subjectKeyIdExt = getSubjectKeyIdentifierExtension();
        if(subjectKeyIdExt != null)
        {
            return (KeyIdentifier)subjectKeyIdExt.get(SubjectKeyIdentifierExtension.KEY_ID);
        }

        return null;
    }

    public String getSubjectKeyIdentifierAsString()
        throws CertificateException,
        IOException
    {
        KeyIdentifier keyId = getSubjectKeyIdentifier();
        if(keyId != null)
        {
            return SystemUtils.toHexString(keyId.getIdentifier());
        }

        return null;
    }

    public byte[] getThumbprint()
            throws GeneralSecurityException
    {
            X509Certificate cert = getCertificate();
            MessageDigest md = MessageDigest.getInstance(thumbprintAlgorithm);
            return md.digest(cert.getEncoded());
    }

    private X500Name getSubjectName()
    {
        if(subjectName == null)
        {
            X500Principal subjectPrincipal = getCertificate().getSubjectX500Principal();
            subjectName = X500Name.asX500Name(subjectPrincipal);
        }

        return subjectName;
    }

    private X500Name getIssuerName()
    {
            if(issuerName == null)
            {
                    X500Principal issuerPrincipal = getCertificate().getIssuerX500Principal();
                    issuerName = X500Name.asX500Name(issuerPrincipal);
            }

            return issuerName;
    }

    public Date getNotAfter()
    {
        return getCertificate().getNotAfter();
    }

    public Date getNotBefore()
    {
        return getCertificate().getNotBefore();
    }

    public String getEMail()
    {
        if(eMail == null)
        {
            String name = getSubjectName().getName();
            String subNames[] = name.split(",");
            int length = subNames.length;
            for(int i = 0; i < length; i++)
            {
                String subName = subNames[i];
                int index;
                if((index = subName.toUpperCase().indexOf(EMAIL_ADDRESS_NAME)) >= 0)
                {
                    subName = subName.substring(index);
                    int equalPos = subName.indexOf("=");
                    eMail = subName.substring(equalPos + 1).trim();
                    StringTokenizer st = new StringTokenizer(eMail, ", +;");
                    if(st.hasMoreTokens())
                        eMail = st.nextToken();
                    break;
                }
            }

            if(eMail == null)
            {
                try
                {
                    Collection<List<?>> alternativeNames;
                    alternativeNames = getCertificate().getSubjectAlternativeNames();
                    if(alternativeNames != null)
                    {
                        boolean foundEmail = false;
                        Iterator<List<?>> iter = alternativeNames.iterator();
                        while(iter.hasNext())
                        {
                            List names = iter.next();
                            for(int i = 0; i < names.size(); i += 2)
                            {
                                Object idObj = names.get(i);
                                // rfc822Name                      [1]     IA5String,
                                if(idObj instanceof Integer && ((Integer)idObj).intValue() == 1)
                                {
                                    eMail = names.get(i + 1).toString();
                                    break;
                                }
                            }

                            if(eMail != null)
                                break;
                        }
                    }
                }
                catch(GeneralSecurityException ex)
                {
                    ex.printStackTrace();
                }
            }
        }

        return eMail;
    }

    public AuthorityKeyIdentifierExtension getAuthorityKeyIdentifierExtension()
        throws CertificateException
    {
        return getCertificateImpl().getAuthorityKeyIdentifierExtension();
    }

    public KeyIdentifier getAuthorityKeyIdentifier()
        throws CertificateException,
        IOException
    {
        AuthorityKeyIdentifierExtension authKeyIdExt = getAuthorityKeyIdentifierExtension();
        if(authKeyIdExt != null)
        {
            return (KeyIdentifier)authKeyIdExt.get(AuthorityKeyIdentifierExtension.KEY_ID);
        }
        return null;
    }

    public String getAuthorityKeyIdentifierAsString()
        throws CertificateException,
        IOException
    {
        KeyIdentifier keyId = getAuthorityKeyIdentifier();
        if(keyId != null)
        {
            return SystemUtils.toHexString(keyId.getIdentifier());
        }

        return null;
    }

    public X500Name getAuthorityName()
        throws CertificateException,
        IOException
    {
        AuthorityKeyIdentifierExtension authKeyIdExt = getAuthorityKeyIdentifierExtension();
        if(authKeyIdExt != null)
        {
            GeneralNames gnames = (GeneralNames)authKeyIdExt.get(AuthorityKeyIdentifierExtension.AUTH_NAME);
            if(gnames != null)
            {
                if(gnames.size() > 0)
                {
                    GeneralNameInterface gn = gnames.get(0).getName();
                    if(gn != null && gn instanceof X500Name)
                        return (X500Name)gn;
                }
            }
        }
        return null;
    }

    public SerialNumber getAuthoritySerialNumber()
        throws CertificateException,
        IOException
    {
        AuthorityKeyIdentifierExtension authKeyIdExt = getAuthorityKeyIdentifierExtension();
        if(authKeyIdExt != null)
        {
            return (SerialNumber)authKeyIdExt.get(AuthorityKeyIdentifierExtension.SERIAL_NUMBER);
        }
        return null;
    }

    public String getAuthoritySerialNumberAsString()
        throws CertificateException,
        IOException
    {
        SerialNumber sn = getAuthoritySerialNumber();
        if(sn != null)
        {
            return sn.getNumber().toString();
        }
        return null;
    }

    public AuthorityInfoAccessExtension getAuthorityInfoAccessExtension()
        throws CertificateException
    {
        return getCertificateImpl().getAuthorityInfoAccessExtension();
    }

    public CRLDistributionPointsExtension getCRLDistributionPointsExtension()
        throws CertificateException
    {
        return getCertificateImpl().getCRLDistributionPointsExtension();
    }

    public SubjectKeyIdentifierExtension getSubjectKeyIdentifierExtension()
        throws CertificateException
    {
        return getCertificateImpl().getSubjectKeyIdentifierExtension();
    }

    public IssuerAlternativeNameExtension getIssuerAlternativeNameExtension()
        throws CertificateException
    {
        return getCertificateImpl().getIssuerAlternativeNameExtension();
    }

    public String getIssuerAlternativeName()
        throws CertificateException,
        IOException
    {
        IssuerAlternativeNameExtension ext = getIssuerAlternativeNameExtension();
        if(ext != null)
        {
            GeneralNames gnames = (GeneralNames)ext.get(IssuerAlternativeNameExtension.ISSUER_NAME);
            if(gnames != null)
            {
                if(gnames.size() > 0)
                {
                    GeneralNameInterface gn = gnames.get(0).getName();
                    if(gn != null && gn instanceof URIName)
                    {
                        URI uri = ((URIName)gn).getURI();
                        if(uri != null)
                            return uri.toString();
                    }
                }
            }
        }
        return null;
    }

    public String getSubjectCommonName()
        throws IOException
    {
        return getSubjectName().getCommonName();
    }

    public String getSubjectOrganizationalUnit()
            throws IOException
    {
            return getSubjectName().getOrganizationalUnit();
    }

    public String getSubjectOrganization()
            throws IOException
    {
            return getSubjectName().getOrganization();
    }

    public String getSubjectLocality()
            throws IOException
    {
            return getSubjectName().getLocality();
    }

    public String getSubjectCountry()
            throws IOException
    {
            return getSubjectName().getCountry();
    }

    public String getSerialNumberAsString()
    {
        return getCertificate().getSerialNumber().toString();
    }

    public String getIssuerCommonName()
        throws IOException
    {
        return getIssuerName().getCommonName();
    }

    public String getIssuerOrganizationalUnit()
            throws IOException
    {
            return getIssuerName().getOrganizationalUnit();
    }

    public String getIssuerOrganization()
            throws IOException
    {
            return getIssuerName().getOrganization();
    }

    public String getIssuerLocality()
        throws IOException
    {
        return getIssuerName().getLocality();
    }

    public String getIssuerCountry()
        throws IOException
    {
        return getIssuerName().getCountry();
    }

    public String toString()
    {
        try
        {
            return getSubjectCommonName();
        }
        catch(Exception ex)
        {
            return "ERROR retrieving Subject CN";
        }
    }

    public static void exportToPKCS7(X509Certificate certificate, OutputStream outStream)
        throws CertificateEncodingException,
        IOException
    {
        exportToPKCS7(certificate, outStream, true);
    }

    public static void exportToPKCS7(X509Certificate certificate, OutputStream outStream, boolean binary)
        throws CertificateEncodingException,
        IOException
    {
        byte buffer[] = certificate.getEncoded();
        if(binary)
            outStream.write(buffer);
        else
        {
            Writer wr = new OutputStreamWriter(outStream, Charset.forName("UTF-8"));
            wr.write("-----BEGIN CERTIFICATE-----\n");
            wr.write(Base64.encodeBytes(buffer));
            wr.write("\n-----END CERTIFICATE-----\n");
            wr.flush();
        }
        outStream.flush();
        outStream.close();
    }

    public static X509Certificate getPKCS7Certificate(byte byteArray[])
        throws CertificateException,
        IOException
    {
        ByteArrayInputStream inStream = new ByteArrayInputStream(byteArray);
        return getPKCS7Certificate(inStream);
    }

    public static X509Certificate getPKCS7Certificate(InputStream inStream)
        throws CertificateException,
        IOException
    {
        X509Certificate cert = null;
        CertificateFactory certFactory = CertificateFactory.getInstance("X.509");
        cert = (X509Certificate)certFactory.generateCertificate(inStream);
        inStream.close();

        return cert;
    }
}

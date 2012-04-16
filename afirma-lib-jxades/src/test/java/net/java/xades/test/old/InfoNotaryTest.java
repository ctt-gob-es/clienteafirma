package net.java.xades.test.old;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLConnection;
import java.security.GeneralSecurityException;
import java.security.Security;
import java.security.cert.CertPath;
import java.security.cert.CertPathValidator;
import java.security.cert.CertPathValidatorException;
import java.security.cert.CertStore;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.CollectionCertStoreParameters;
import java.security.cert.PKIXCertPathValidatorResult;
import java.security.cert.PKIXParameters;
import java.security.cert.TrustAnchor;
import java.security.cert.X509CRL;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.TreeSet;
import javax.security.auth.x500.X500Principal;

import sun.security.x509.X509CertImpl;
import sun.security.x509.AuthorityInfoAccessExtension;
import sun.security.x509.AccessDescription;
import sun.security.x509.GeneralName;
import sun.security.x509.GeneralNameInterface;
import sun.security.x509.URIName;

/**
 *
 * @author Miroslav Nachev
 * 
 */
public class InfoNotaryTest
{
    private PKIXParameters parameters;
    
    public InfoNotaryTest(File trustRootCACertificatesFolder)
        throws Exception
    {
        HashSet<TrustAnchor> trustAnchors = getTrustRootCACertificates(trustRootCACertificatesFolder);
        parameters = new PKIXParameters(trustAnchors);
    }

    // Validate Off-Line using CRL File
    public PKIXCertPathValidatorResult validateOffLine(X509Certificate cert,
                                                       X509CRL crl)
        throws CertPathValidatorException,
            GeneralSecurityException
    {
        CertPath certPath = convertToCertPath(cert);

        Collection<X509CRL> crls = Collections.<X509CRL>singletonList(crl);
        CollectionCertStoreParameters certStoreParams = new CollectionCertStoreParameters(crls);
        CertStore crlStore = CertStore.getInstance("Collection", certStoreParams);
        parameters.addCertStore(crlStore);

        Security.setProperty("ocsp.enable", "false");
        System.setProperty("com.sun.security.enableCRLDP", "false");

        return validateCertificate(certPath);
    }

    // Validate On-Line using OCSP Request and CRLDP as backup
    public PKIXCertPathValidatorResult validateOnLine(X509Certificate cert,
                                                      X509Certificate ocspCertificate)
        throws CertPathValidatorException,
            GeneralSecurityException, IOException
    {
        CertPath certPath = convertToCertPath(cert);

        Security.setProperty("ocsp.enable", "true");

        // disable backup 
        System.setProperty("com.sun.security.enableCRLDP", "false");

        URI uri = getOCSPServerURI(cert);
        Security.setProperty("ocsp.responderURL", uri.toString());

        X500Principal principal = ocspCertificate.getSubjectX500Principal();
        Security.setProperty("ocsp.responderCertSubjectName", principal.getName("RFC1779"));

        principal = ocspCertificate.getIssuerX500Principal();
        Security.setProperty("ocsp.responderCertIssuerName", principal.getName("RFC1779"));

        BigInteger serialNumber = ocspCertificate.getSerialNumber();
        Security.setProperty("ocsp.responderCertSerialNumber", serialNumber.toString());

        return validateCertificate(certPath);
    }

    private PKIXCertPathValidatorResult validateCertificate(CertPath certPath)
        throws CertPathValidatorException,
            GeneralSecurityException
    {
        CertPathValidator cpv = CertPathValidator.getInstance("PKIX");
        return (PKIXCertPathValidatorResult)cpv.validate(certPath, parameters);
    }


    public static void main(String[] args)
    {
        PKIXCertPathValidatorResult validateResult = null;
        InfoNotaryTest test = null;
        X509Certificate cert = null;
        X509Certificate ocspCert = null;
        X509CRL crl = null;

        // Info Notary test
        try
        {
            test = new InfoNotaryTest(new File("src/TrustRootCACertificates"));

            cert = getCertificate("src/InfoNotary-Certificate.cer");
            crl = getCRL("src/qsign-company-ca.crl");
        }
        catch(Exception ex)
        {
            ex.printStackTrace();
        }

        // Info Notary Off-Line Validation test
        try
        {
            if(true)
            {
                validateResult = test.validateOffLine(cert, crl);
                System.out.println("i-Notary Off-Line VALIDATION SUCCESSFUL. The Trust Anchor is: " + validateResult.getTrustAnchor().getTrustedCert().getSubjectX500Principal());
            }
        }
        catch(CertPathValidatorException ex)
        {
            System.out.println("i-Notary Off-Line VALIDATION FAILD: " + ex.getMessage());
        }
        catch(Exception ex)
        {
            ex.printStackTrace();
        }

        // Info Notary On-Line Validation test
        try
        {
            ocspCert = getCertificate("src/TrustRootCACertificates/infonotary.com/INotaryOCSPResponder.cer");

            validateResult = test.validateOnLine(cert, ocspCert);
            System.out.println("i-Notary On-Line VALIDATION SUCCESSFUL. The Trust Anchor is: " + validateResult.getTrustAnchor().getTrustedCert().getSubjectX500Principal());
        }
        catch(CertPathValidatorException ex)
        {
            System.out.println("i-Notary On-Line VALIDATION FAILD: " + ex.getMessage());
        }
        catch(Exception ex)
        {
            ex.printStackTrace();
        }

        if(false)
            return;

        // B-Trust Off-Line Validation test
        try
        {
            cert = getCertificate("src/B-Trust-Certificate.cer");
            crl = getCRL("src/b-trust_ca3_oper.crl");

            validateResult = test.validateOffLine(cert, crl);
            System.out.println("B-Trust Off-Line VALIDATION SUCCESSFUL. The Trust Anchor is: " + validateResult.getTrustAnchor().getTrustedCert().getSubjectX500Principal());
        }
        catch(CertPathValidatorException ex)
        {
            System.out.println("B-Trust Off-Line VALIDATION FAILD: " + ex.getMessage());
        }
        catch(Exception ex)
        {
            ex.printStackTrace();
        }

        // B-Trust On-Line Validation test
        try
        {
            ocspCert = getCertificate("src/TrustRootCACertificates/b-trust.org/ocsp.b-trust.org_DER.cer");

            validateResult = test.validateOnLine(cert, ocspCert);
            System.out.println("B-Trust On-Line VALIDATION SUCCESSFUL. The Trust Anchor is: " + validateResult.getTrustAnchor().getTrustedCert().getSubjectX500Principal());
        }
        catch(CertPathValidatorException ex)
        {
            System.out.println("B-Trust On-Line VALIDATION FAILD: " + ex.getMessage());
        }
        catch(Exception ex)
        {
            ex.printStackTrace();
        }
    }











    /******************************************************************
     * Helper classes and methods. Not ipmortant for the test target. *
     ******************************************************************/

    private static HashSet<TrustAnchor> getTrustRootCACertificates(File folder)
        throws CertificateException,
        IOException
    {
        List<X509Certificate> certificates = getCertificates(folder);
        HashSet<TrustAnchor> trustRootCACerts = new HashSet<TrustAnchor>(certificates.size());
        for(X509Certificate cert : certificates)
        {
            trustRootCACerts.add(new TrustAnchor(cert, null));
        }

        return trustRootCACerts;
    }

    private static List<X509Certificate> getCertificates(File folder)
        throws CertificateException,
        IOException
    {
        CertificateFactory cf = getCertificateFactory();

        ArrayList<X509Certificate> certificates = new ArrayList<X509Certificate>();
        FileFilter ff = new CertificateFileFilter();
        File[] certFiles = folder.listFiles(ff);
        for(File certFile : certFiles)
        {
            if(certFile.isDirectory())
            {
                List<X509Certificate> certs = getCertificates(certFile);
                if(certs.size() > 0)
                    certificates.addAll(certs);
            }
            else
            {
                FileInputStream inStream = new FileInputStream(certFile);
                X509Certificate cert = (X509Certificate)cf.generateCertificate(inStream);
                certificates.add(cert);
            }
        }

        return certificates;
    }

    private static CertificateFactory certificateFactory;

    private static CertificateFactory getCertificateFactory()
        throws CertificateException
    {
        if(certificateFactory == null)
        {
            certificateFactory = CertificateFactory.getInstance("X509");
        }

        return certificateFactory;
    }

    private static final TreeSet<String> certFileExt = new TreeSet<String>();
    static
    {
        certFileExt.add(".cer");
        certFileExt.add(".crt");
    }

    private static class CertificateFileFilter
        implements java.io.FileFilter
    {

        public boolean accept(File pathName)
        {
            if(pathName.isDirectory())
                return true;

            if(!pathName.isFile())
                return false;

            String fileName = pathName.getName();

            String extStr = getFileExtension(fileName);
            if(extStr == null)
                return false;

            return certFileExt.contains(extStr.toLowerCase());
        }
    }

    private static String getFileExtension(String fileName)
    {
        if(fileName != null)
        {
            int dotIndex;
            if((dotIndex = fileName.lastIndexOf(".")) >= 0)
            {
                return fileName.substring(dotIndex).trim().toLowerCase();
            }
        }

        return null;
    }

    private static final CertPath convertToCertPath(X509Certificate certificate)
        throws CertificateException
    {
        return convertToCertPath(Collections.singletonList(certificate));
    }

    private static final CertPath convertToCertPath(List<X509Certificate> certificates)
        throws CertificateException
    {
        return getCertificateFactory().generateCertPath(certificates);
    }

    public static X509Certificate getCertificate(String fileName)
        throws IOException,
        CertificateException
    {
        return getCertificate(new File(fileName));
    }

    public static X509Certificate getCertificate(File certificateFile)
        throws IOException,
        CertificateException
    {
        return getCertificate(certificateFile.toURI());
    }

    public static X509Certificate getCertificate(URI uri)
        throws IOException,
        CertificateException
    {
        URL url = uri.toURL();
        URLConnection connection = url.openConnection();
        InputStream inStream = connection.getInputStream();
        return getCertificate(inStream);
    }

    public static X509Certificate getCertificate(InputStream inStream)
        throws CertificateException
    {
        return (X509Certificate)getCertificateFactory().generateCertificate(inStream);
    }

    public static X509CRL getCRL(String crlFileName)
        throws GeneralSecurityException,
        IOException
    {
        return getCRL(new File(crlFileName));
    }

    public static X509CRL getCRL(File crlFile)
        throws GeneralSecurityException,
        IOException
    {
        return getCRL(crlFile.toURI());
    }

    public static X509CRL getCRL(URI crlURI)
        throws GeneralSecurityException,
        IOException
    {
        CertificateFactory factory = getCertificateFactory();
        InputStream in = null;
        try
        {
            URL url = crlURI.toURL();
            URLConnection connection = url.openConnection();
            in = connection.getInputStream();
            return (X509CRL)factory.generateCRL(in);
        }
	finally
        {
            if(in != null)
            {
                try
                {
                    in.close();
                }
                catch(IOException ex) {}
            }
        }
    }

    public static URI getOCSPServerURI(X509Certificate cert)
        throws GeneralSecurityException, IOException
    {
        X509CertImpl certImpl = X509CertImpl.toImpl(cert);
        AuthorityInfoAccessExtension aia = certImpl.getAuthorityInfoAccessExtension(); 
        if(aia != null)
        {
            List<AccessDescription> descriptions;
            descriptions = (List<AccessDescription>) aia.get(AuthorityInfoAccessExtension.DESCRIPTIONS);

            for(AccessDescription description : descriptions)
            {
                if(description.getAccessMethod().equals(AccessDescription.Ad_OCSP_Id))
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
}


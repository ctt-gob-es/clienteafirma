package net.java.xades.test.old;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.net.URI;
import java.security.Security;
import java.security.cert.CertPath;
import java.security.cert.CertPathValidator;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.cert.CertificateFactory;
import java.security.cert.PKIXCertPathValidatorResult;
import java.security.cert.PKIXParameters;
import java.security.cert.TrustAnchor;
import java.security.cert.X509Certificate;
import java.util.Collections;
import java.util.HashSet;

import net.java.xades.security.CertificateHelper;
import net.java.xades.util.DefaultFileExtension;

/**
 *
 * @author miro
 */
public class ValidateCertificate {

    /** Creates a new instance of ValidateCertificate */
    public ValidateCertificate()
    {
    }

//    public static HashSet<TrustAnchor> getTrustRootCACertificates()
//        throws IOException,
//        GeneralSecurityException
//    {
//        return getTrustRootCACertificates(new File("TrustRootCACertificates/b-trust.org"));
//    }

//    public static HashSet<TrustAnchor> getTrustRootCACertificates(File folder)
//        throws IOException,
//        GeneralSecurityException
//    {
//        HashSet<TrustAnchor> trustRootCACerts = new HashSet<TrustAnchor>();
//        FileFilter ff = FileFilters.getIOFileFilter(DefaultFileExtension.ALL_CERTIFICATES);
//        File[] certFiles = folder.listFiles(ff);
//
//        certFiles = new File[] {new File("TrustRootCACertificates/b-trust.org/ocsp.b-trust.org_DER.cer")};
//
//        for(File certFile : certFiles)
//        {
//            if(certFile.isDirectory())
//            {
//                HashSet<TrustAnchor> trustAnchors = getTrustRootCACertificates(certFile);
//                if(trustAnchors.size() > 0)
//                    trustRootCACerts.addAll(trustAnchors);
//            }
//            else
//            {
//                FileInputStream inStream = new FileInputStream(certFile);
//                X509Certificate rootCACert = CertificateHelper.getPKCS7Certificate(inStream);
//                trustRootCACerts.add(new TrustAnchor(rootCACert, null));
//            }
//        }
//
//        return trustRootCACerts;
//    }


    // -Dsecurity.debug=all -Djava.security.debug=all -Djava.security.auth.debug=all
//    public static void main(String[] args)
//    {
//        try
//        {
//            FileInputStream inStream = new FileInputStream("22083@B-Trust_Operational_CA_-_Universal_Electronic_Signature.cer");
//            X509Certificate certificate = CertificateHelper.getPKCS7Certificate(inStream);
//            CertificateHelper certHelper = new CertificateHelper(certificate);
//
//            //certHelper.verifyCertificate();
//            //certHelper.verifyCertificateChain();
//            //certHelper.verifyCertificationChain();
//
//            System.out.println("certHelper.getEMail(): " + certHelper.getEMail());
//            URI crlServerURI = certHelper.getCRLDistributionPointsURI();
//            System.out.println("certHelper.getCRLDistributionPointsURI(): " + crlServerURI);
//            URI ocspServerURI = certHelper.getOCSPServerURI();
//            //ocspServerURI = null;
//            System.out.println("getOCSPServerURL(): " + ocspServerURI);
//
//	    CertificateFactory cf = CertificateFactory.getInstance("X509");
//	    CertPath cp = cf.generateCertPath(Collections.singletonList(certificate));
//
//	    // init PKIX parameters
//            PKIXParameters params = null;
//	    params = new PKIXParameters(getTrustRootCACertificates());
//
//	    // activate OCSP
//	    Security.setProperty("ocsp.enable", "true");
//	    if(ocspServerURI != null)
//            {
//		Security.setProperty("ocsp.responderURL", ocspServerURI.toASCIIString());
//	    }
//
//	    // perform validation
//	    CertPathValidator cpv = CertPathValidator.getInstance("PKIX");
//	    PKIXCertPathValidatorResult cpv_result;
//            cpv_result = (PKIXCertPathValidatorResult)cpv.validate(cp, params);
//	    X509Certificate trustedCert;
//            trustedCert = (X509Certificate)cpv_result.getTrustAnchor().getTrustedCert();
//
//	    if(trustedCert == null)
//            {
//		System.out.println("Trsuted Cert = NULL");
//	    }
//            else
//            {
//		System.out.println("Trusted CA DN = " + trustedCert.getSubjectDN());
//	    }
//        }
//        catch(Exception ex)
//        {
//            ex.printStackTrace();
//        }
//    }

}

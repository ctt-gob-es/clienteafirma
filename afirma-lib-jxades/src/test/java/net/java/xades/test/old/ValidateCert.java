package net.java.xades.test.old;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.security.cert.X509Certificate;
import java.security.cert.PKIXParameters;
import javax.security.auth.x500.X500Principal;
import java.math.BigInteger;
import java.security.GeneralSecurityException;
import java.security.Security;
import java.security.cert.CertPath;
import java.security.cert.CertPathValidator;
import java.security.cert.CertPathValidatorException;
import java.security.cert.CertificateFactory;
import java.security.cert.CertificateParsingException;
import java.security.cert.PKIXCertPathValidatorResult;
import java.security.cert.TrustAnchor;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Check the revocation status of a public key certificate using OCSP.
 */

// -Dsecurity.debug=all -Djava.security.debug=all -Djava.security.auth.debug=all
public class ValidateCert
{
    private static final String[] CERT_PATH =
        {
        "TrustRootCACertificates/b-trust.org/RootCA3cert_DER.cer",
        "TrustRootCACertificates/b-trust.org/OperCA3cert_DER.cer",
        //"TrustRootCACertificates/b-trust.org/ocsp.b-trust.org_DER.cer",
    };

    //private static final String OCSP_SERVER_ROOT_CA = "RootCA.pem";
    private static final String OCSP_SERVER_ROOT_CA =
        "TrustRootCACertificates/b-trust.org/ocsp.b-trust.org_DER.cer";

    public static Set<TrustAnchor> getTrustRootCACertificates()
        throws IOException,
        GeneralSecurityException
    {
        if(false)
            return Collections.<TrustAnchor>emptySet();

        return getTrustRootCACertificates(new File("src/TrustRootCACertificates"));
    }

    public static Set<TrustAnchor> getTrustRootCACertificates(File folder)
        throws IOException,
        GeneralSecurityException
    {
        HashSet<TrustAnchor> trustRootCACerts = new HashSet<TrustAnchor>();
        File[] certFiles = folder.listFiles(new CertificateFileFilter());

        //certFiles = new File[] {new File("TrustRootCACertificates/b-trust.org/ocsp.b-trust.org_DER.cer")};
        /*certFiles = new File[CERT_PATH.length];
                 for(int i = 0; i < CERT_PATH.length; i++)
                 {
            certFiles[i] = new File(CERT_PATH[i]);
                 }*/

        for(File certFile : certFiles)
        {
            if(certFile.isDirectory())
            {
                Set<TrustAnchor> trustAnchors = getTrustRootCACertificates(certFile);
                if(trustAnchors.size() > 0)
                    trustRootCACerts.addAll(trustAnchors);
            }
            else
            {
                FileInputStream inStream = new FileInputStream(certFile);
                X509Certificate rootCACert = getCertFromFile(inStream);
                trustRootCACerts.add(new TrustAnchor(rootCACert, null));

                if("ocsp.b-trust.org_DER.cer".equals(certFile.getName()))
                {
                    System.out.println("OCSP Root CA Cert: " + certFile);

                    X500Principal principal = rootCACert.getSubjectX500Principal();
                    Security.setProperty("ocsp.responderCertSubjectName", principal.getName("RFC1779"));
                    System.out.println("ocsp.responderCertSubjectName: " + principal.getName("RFC1779"));

                    principal = rootCACert.getIssuerX500Principal();
                    Security.setProperty("ocsp.responderCertIssuerName", principal.getName("RFC1779"));
                    System.out.println("ocsp.responderCertIssuerName: " + principal.getName("RFC1779"));

                    BigInteger serialNumber = rootCACert.getSerialNumber();
                    Security.setProperty("ocsp.responderCertSerialNumber", serialNumber.toString());
                    System.out.println("ocsp.responderCertSerialNumber: " + serialNumber.toString());

                    showCertificateInfo(rootCACert);
                }
                else
                    if("OperCA3cert_DER.cer".equals(certFile.getName()))
                    {
                        System.out.println("\nOperCA3cert_DER.cer");
                        showCertificateInfo(rootCACert);
                    }
                    else
                        if("RootCA3cert_DER.cer".equals(certFile.getName()))
                        {
                            System.out.println("\nRootCA3cert_DER.cer");
                            showCertificateInfo(rootCACert);
                        }
            }
        }

        return trustRootCACerts;
    }

    public static void showCertificateInfo(X509Certificate cert)
        throws CertificateParsingException
    {
        if(true)
            return;
        //System.out.println("\n\t *** Certificate Info *** BEGIN\n" + cert + "\n\t *** Certificate Info *** END\n");

        boolean[] keyUsage = cert.getKeyUsage();
        System.out.println("Key Usage: ");
        for(int i = 0; i < keyUsage.length; i++)
        {
            System.out.println("\t (" + i + ") = " + keyUsage[i]);
        }

        List<String> extKeyUsage = cert.getExtendedKeyUsage();
        System.out.println("Extended Key Usage: " + extKeyUsage);
        if(extKeyUsage != null)
        {
            for(String keyPurpose : extKeyUsage)
            {
                System.out.println("\t keyPurpose: " + keyPurpose);
            }
        }
    }

    private static class CertificateFileFilter
        implements FileFilter
    {
        public boolean accept(File pathname)
        {
            if(pathname != null)
            {
                if(pathname.isDirectory())
                    return true;
                String filename = pathname.getName().toLowerCase();
                if(filename.endsWith(".cer") || filename.endsWith(".crt"))
                    return true;
            }

            return false;
        }
    }

    /**
     * Checks the revocation status of a public key certificate using OCSP.
     *
     * Usage:  java ValidateCert <cert-file> [<OCSP-server>]
     *     <cert-file> is the filename of the certificate to be checked.
     *            The certificate must be in PEM format.
     *     <OCSP-server> is the URL of the OCSP server to use.
     *            If not supplied then the certificate must identify an OCSP
     *            server by means of its AuthorityInfoAccess extension.
     *            If supplied then it overrides any URL which may be present
     *            in the certificate's AuthorityInfoAccess extension.
     *
     * Example:  java \
     *             ValidateCert \
     *             mycert.pem \
     *             http://ocsp.openvalidation.org:80
     */
    public static void main(String[] args)
    {
        try
        {
            args = new String[]
                   {
                   "22083@B-Trust_Operational_CA_-_Universal_Electronic_Signature.cer",
                   /*"http://ocsp.b-trust.org"*/};

            if(args.length == 0 || args.length > 2)
            {
                System.out.println(
                    "Usage: java ValidateCert <cert-file> [<OCSP-server>]");
                System.exit( -1);
            }

            // load the cert to be checked
            X509Certificate toBeCheckedCert = getCertFromFile(args[0]);
            //System.out.println("toBeCheckedCert.getIssuerX500Principal(): " + toBeCheckedCert.getIssuerX500Principal());
            showCertificateInfo(toBeCheckedCert);

            Set<TrustAnchor> trustAnchors = getTrustRootCACertificates();
            X500Principal toBeCheckedCertIssuer = toBeCheckedCert.getIssuerX500Principal();
            //System.out.println("toBeCheckedCertIssuer: " + toBeCheckedCertIssuer);
            for(TrustAnchor trustAnchor : trustAnchors)
            {
                X500Principal trustCertSubject = trustAnchor.getTrustedCert().getSubjectX500Principal();
                //System.out.println("trustCertSubject: " + trustCertSubject);
                //System.out.println("toBeCheckedCertIssuer.equals(trustCertSubject): " + toBeCheckedCertIssuer.equals(trustCertSubject));

                X500Principal trustCertIssuer = trustAnchor.getTrustedCert().getIssuerX500Principal();
                //System.out.println("trustCertIssuer: " + trustCertIssuer);
                //System.out.println("toBeCheckedCertIssuer.equals(trustCertIssuer): " + toBeCheckedCertIssuer.equals(trustCertIssuer));
            }

            // AuthorityKeyIdentifierExtension


            // handle location of OCSP server
            URI ocspServer = null;
            if(args.length == 2)
            {
                ocspServer = new URI(args[1]);
                System.out.println("Using the OCSP server at: " + args[1]);
                System.out.println("to check the revocation status of: "/* +
                                   toBeCheckedCert*/);
                System.out.println();
            }
            else
            {
                System.out.println("Using the OCSP server specified in the " +
                                   "cert to check the revocation status of: "/* +
                                   toBeCheckedCert*/);
                System.out.println();
            }

            // init cert path
            CertificateFactory cf = CertificateFactory.getInstance("X509");
            CertPath cp = cf.generateCertPath(
                Collections.singletonList(toBeCheckedCert));

            // load the root CA for the OCSP server
            //X509Certificate rootCACert = getCertFromFile(OCSP_SERVER_ROOT_CA);

            // init PKIX parameters
            PKIXParameters params = null;
            //TrustAnchor trustAnchor = new TrustAnchor(rootCACert, null);
            //Set set = Collections.singleton(trustAnchor);
            //params = new PKIXParameters(set);
            params = new PKIXParameters(trustAnchors);
            System.out.println("params.isRevocationEnabled(): " + params.isRevocationEnabled());

            // activate OCSP
            Security.setProperty("ocsp.enable", "true");
            //Security.setProperty("ocsp.enable", "false");
            if(ocspServer != null)
            {
                Security.setProperty("ocsp.responderURL", args[1]);
            }

            // perform validation
            CertPathValidator cpv = CertPathValidator.getInstance("PKIX");

            PKIXCertPathValidatorResult cpv_result;
            cpv_result = (PKIXCertPathValidatorResult)cpv.validate(cp, params);

            X509Certificate trustedCert;
            trustedCert = (X509Certificate)cpv_result.getTrustAnchor().getTrustedCert();

            if(trustedCert == null)
            {
                System.out.println("Trsuted Cert = NULL");
            }
            else
            {
                System.out.println("\n\nTrusted CA Certificate is:");
                System.out.println("\t Issuer: " + trustedCert.getIssuerX500Principal());
                System.out.println("\t Serial Number: " + trustedCert.getSerialNumber() + "\n");
            }

        }
        catch(CertPathValidatorException e)
        {
            System.out.flush();
            e.printStackTrace();
            System.exit(1);
        }
        catch(Exception e)
        {
            System.out.flush();
            e.printStackTrace();
            System.exit( -1);
        }
        System.out.println("CERTIFICATE VALIDATION SUCCEEDED");
        System.exit(0);
    }

    /*
     * Read a certificate from the specified filepath.
     */
    public static X509Certificate getCertFromFile(String path)
    {
        X509Certificate cert = null;
        try
        {
            return getCertFromFile(new File(path));
        }
        catch(Exception e)
        {
            System.out.println("Can't construct X509 Certificate. " + e.getMessage());
        }
        return cert;
    }

    public static X509Certificate getCertFromFile(File certFile)
    {
        X509Certificate cert = null;
        try
        {
            if(!certFile.canRead())
                throw new IOException(" File " + certFile.toString() + " is unreadable");

            return getCertFromFile(new FileInputStream(certFile));
        }
        catch(Exception e)
        {
            System.out.println("Can't construct X509 Certificate. " + e.getMessage());
        }
        return cert;
    }

    public static X509Certificate getCertFromFile(InputStream inStream)
    {
        X509Certificate cert = null;
        try
        {
            CertificateFactory cf = CertificateFactory.getInstance("X509");
            cert = (X509Certificate)cf.generateCertificate(inStream);
        }
        catch(Exception e)
        {
            System.out.println("Can't construct X509 Certificate. " + e.getMessage());
        }
        return cert;
    }
}

package es.gob.afirma.test.pades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.MessageDigest;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.Assert;

import org.bouncycastle.util.encoders.Base64;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.pkcs7.tsp.CMSTimestamper;

public final class TestPAdESETSI {
    
    private static final String CERT_PATH = "RequestedKeyCert.p12"; //$NON-NLS-1$
    private static final String CERT_PASS = "1111"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "certificado pruebas plugtests"; //$NON-NLS-1$
    private static final String POL_PATH = "TARGET-SIGPOL-ETSI4.der";
    
    private static final String[] TEST_FILES = {
        "aaa.pdf",
        "aaasvd.pdf",
        "aaaxml.pdf",
        "SeedValuePKCS1.pdf"
    };
    
    private static final Properties[] PADES_MODES;
    
    static {
        final Properties p1 = new Properties();
        p1.setProperty("policyIdentifier", "1.2.3.4.5.2"); //$NON-NLS-1$ //$NON-NLS-2$
        try {
            p1.setProperty(
               "policyIdentifierHash", 
               new String(Base64.encode(MessageDigest.getInstance("SHA1").digest(AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(POL_PATH)))))
           );
        }
        catch(final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("no se ha podido calcular la huella digital de la politica");
        }
        p1.setProperty("policyIdentifierHashAlgorithm", "SHA1"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("signReason", "test"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("signatureProductionCity", "Madrid"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("signerContact", "tomas.garciameras@atos.net"); //$NON-NLS-1$ //$NON-NLS-2$
        
        final Properties p2 = new Properties();
        
        final Properties p3 = new Properties();
        p3.put("tsaURL", CMSTimestamper.CATCERT_TSP); //$NON-NLS-1$
        p3.put("tsaRequireCert", CMSTimestamper.CATCERT_REQUIRECERT); //$NON-NLS-1$
        p3.put("tsaPolicy", CMSTimestamper.CATCERT_POLICY); //$NON-NLS-1$
        
        final Properties p4 = new Properties();
        p4.put("tsaURL", CMSTimestamper.CATCERT_TSP); //$NON-NLS-1$
        p4.put("tsaRequireCert", CMSTimestamper.CATCERT_REQUIRECERT); //$NON-NLS-1$
        p4.put("tsaPolicy", CMSTimestamper.CATCERT_POLICY); //$NON-NLS-1$
        p4.setProperty("policyIdentifier", "1.2.3.4.5.2"); //$NON-NLS-1$ //$NON-NLS-2$
        try {
            p4.setProperty(
               "policyIdentifierHash", 
               new String(Base64.encode(MessageDigest.getInstance("SHA1").digest(AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(POL_PATH)))))
           );
        }
        catch(final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("no se ha podido calcular la huella digital de la politica");
        }
        p4.setProperty("policyIdentifierHashAlgorithm", "SHA1"); //$NON-NLS-1$ //$NON-NLS-2$

        PADES_MODES = new Properties[] {
                p1, p2 //, p3, p4
        };
    }
    
    /** Algoritmos de firma a probar. */
    private final static String[] ALGOS = new String[] {
        AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA
    };
    
    @Test
    public void testSignature() throws Exception {
     
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        
        final PrivateKeyEntry pke;
        final X509Certificate cert;

        KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
        cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);
        
        AOSigner signer = new AOPDFSigner();
        
        String prueba;
        
        for (final Properties extraParams : PADES_MODES) {
            for (final String algo : ALGOS) {
                for (final String file : TEST_FILES) {
                    
                    final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(file)); 
                    Assert.assertTrue("No se ha reconocido como un PDF", signer.isValidDataFile(testPdf)); //$NON-NLS-1$
                    
                    prueba = "Firma PAdES en modo '" +  //$NON-NLS-1$
                    extraParams.getProperty("mode") +  //$NON-NLS-1$
                    "' con el algoritmo ': " + //$NON-NLS-1$
                    algo +
                    "'m el fichero '" +  //$NON-NLS-1$
                    file +
                    "' y las propiedades: " + //$NON-NLS-1$
                    extraParams;
                    System.out.println(prueba);
                    
                    byte[] result = signer.sign(testPdf, algo, pke, extraParams);
                    
                    Assert.assertNotNull(prueba, result);
                    Assert.assertTrue(signer.isSign(result));
                    Assert.assertEquals(result, signer.getData(result));
                    Assert.assertEquals(AOSignConstants.SIGN_FORMAT_PDF, signer.getSignInfo(result).getFormat());
                    
                    final File saveFile = File.createTempFile(file.replace(".pdf", "") + "_" + ((extraParams.getProperty("policyIdentifier") != null) ? "POL_" : "") + ((extraParams.getProperty("tsaURL") != null) ? "TSP_" : "") + algo + "_", ".pdf"); //$NON-NLS-1$
                    final OutputStream os = new FileOutputStream(saveFile);
                    os.write(result);
                    os.flush();
                    os.close();
                    System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$
                    
                    
                    
                    
                }
            }
        }
    }

}

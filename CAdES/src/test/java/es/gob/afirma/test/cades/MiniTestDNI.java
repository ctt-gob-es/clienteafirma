package es.gob.afirma.test.cades;

import java.io.File;
import java.io.FileInputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;

import org.junit.Test;

import es.gob.afirma.signers.cades.GenCAdESEPESSignedData;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;

/**
 * Pruebas espec&iacute;ficas de CAdES para DNIe.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 *
 */
public class MiniTestDNI {
    
    private static final String DNIE_DRIVER_PATH = "c:\\windows\\system32\\UsrPkcs11.dll"; //$NON-NLS-1$
    //private static final String DNIE_DRIVER_PATH = "c:\\windows\\sysWOW64\\UsrPkcs11.dll";
    
    private static final char[] DNI_PIN = "pin0".toCharArray();  //$NON-NLS-1$
    
    private static final String DNI_SIGN_ALIAS = "CertFirmaDigital"; //$NON-NLS-1$
    
    private static final String TEXTO_FIRMAR = "Tom\u00F3"; //$NON-NLS-1$
    
    /**
     * Mini-prueba CAdES espec’fica para DNIe.
     * @throws Exception en caso de cualquier tipo de problema
     */
    @Test
    public void testCAdESDNIe() throws Exception {
        
        KeyStore ks = KeyStore.getInstance("PKCS11"); //$NON-NLS-1$
        ks.load(new FileInputStream(new File(DNIE_DRIVER_PATH)), DNI_PIN);
        
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(DNI_SIGN_ALIAS, new KeyStore.PasswordProtection(DNI_PIN));
        
        P7ContentSignerParameters p7ContentSignerParameters = new P7ContentSignerParameters(TEXTO_FIRMAR.getBytes("UTF-8"), "SHA1withRSA", (X509Certificate[]) pke.getCertificateChain());  //$NON-NLS-1$ //$NON-NLS-2$
        
        String policy = "http://www.boe.es/boe/dias/2011/07/30/pdfs/BOE-A-2011-13171.pdf"; //$NON-NLS-1$
        String qualifier = "2.16.724.1.3.1.1.2.1.8"; //$NON-NLS-1$
        
        GenCAdESEPESSignedData  genCAdESEPESSignedData = new GenCAdESEPESSignedData();                                         

        boolean omitContent = false; 
        boolean signingCertificateV2 = true; 
        byte[] messageDigest = null; // Se calcula internamente el digest de los datos a firmar. 
        
        byte[] firma = genCAdESEPESSignedData.generateSignedData(p7ContentSignerParameters, omitContent, policy, qualifier, signingCertificateV2, pke, messageDigest);  

    }

}

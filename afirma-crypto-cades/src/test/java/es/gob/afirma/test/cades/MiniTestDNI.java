/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.test.cades;

import java.io.ByteArrayInputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.Provider;
import java.security.Security;
import java.security.cert.X509Certificate;
import java.util.Properties;

import junit.framework.Assert;

import org.junit.Test;

import sun.security.pkcs11.SunPKCS11;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signers.cades.GenCAdESEPESSignedData;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;

/**
 * Pruebas espec&iacute;ficas de CAdES para DNIe.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 *
 */
public class MiniTestDNI {
    
    private static final String DNIE_DRIVER_PATH = "name=testdni\r\nlibrary=c:/windows/system32/UsrPkcs11.dll\r\nshowInfo=false"; //$NON-NLS-1$
    
    private static final char[] DNI_PIN = "pin".toCharArray();  //$NON-NLS-1$
    
    private static final String DNI_SIGN_ALIAS = "CertFirmaDigital"; //$NON-NLS-1$
    
    private static final String TEXTO_FIRMAR = "Tom\u00F3"; //$NON-NLS-1$
    
    /**
     * Mini-prueba CAdES espec’fica para DNIe.
     * @throws Exception en caso de cualquier tipo de problema
     */
    @Test
    public void testCAdESDNIe() throws Exception {
        
        Provider p = new SunPKCS11(new ByteArrayInputStream(DNIE_DRIVER_PATH.getBytes()));
        Security.addProvider(p);
        KeyStore ks = KeyStore.getInstance("PKCS11", p); //$NON-NLS-1$
        ks.load(null, DNI_PIN);
        
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(DNI_SIGN_ALIAS, new KeyStore.PasswordProtection(DNI_PIN));
        
        P7ContentSignerParameters p7ContentSignerParameters = new P7ContentSignerParameters(TEXTO_FIRMAR.getBytes("UTF-8"), "SHA1withRSA", (X509Certificate[]) pke.getCertificateChain());  //$NON-NLS-1$ //$NON-NLS-2$
        
//        String policy = "http://www.boe.es/boe/dias/2011/07/30/pdfs/BOE-A-2011-13171.pdf"; //$NON-NLS-1$
//        String qualifier = "2.16.724.1.3.1.1.2.1.8"; //$NON-NLS-1$
        
        GenCAdESEPESSignedData  genCAdESEPESSignedData = new GenCAdESEPESSignedData();                                         

        boolean omitContent = false; 
        boolean signingCertificateV2 = true; 
        byte[] messageDigest = null; // Se calcula internamente el digest de los datos a firmar. 
        
        byte[] firma = genCAdESEPESSignedData.generateSignedData(p7ContentSignerParameters, omitContent, new AdESPolicy(new Properties()), signingCertificateV2, pke, messageDigest);
        
        Assert.assertNotNull(firma);

        //System.out.println(AOUtil.hexify(firma, true));
        
//        java.io.FileOutputStream fos = new java.io.FileOutputStream("C:/pruebas/salida/MiniTestCadesNuevo"+ (omitContent ? "Expl" : "Impl") + ".csig");
//        fos.write(firma);
//        try { fos.close(); } catch (Exception e) { }
    }

}

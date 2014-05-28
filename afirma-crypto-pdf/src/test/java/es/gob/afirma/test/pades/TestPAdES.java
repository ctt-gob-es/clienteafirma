/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.test.pades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.pades.AOPDFSigner;

/**
 * Pruebas del m&oacute;dulo PAdES de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 *
 */
public class TestPAdES {

	private static final String CATCERT_POLICY = "0.4.0.2023.1.1"; //$NON-NLS-1$
	private static final String CATCERT_TSP = "http://psis.catcert.net/psis/catcert/tsp"; //$NON-NLS-1$
	private static final Boolean CATCERT_REQUIRECERT = Boolean.TRUE;

    private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    private static final Properties[] PADES_MODES;

    private static final String[] TEST_FILES = { "TEST_PDF.pdf", "TEST_PDF_Signed.pdf" }; //$NON-NLS-1$ //$NON-NLS-2$

    private static final String TEST_FILE_PWD = "TEST_PDF_Password.pdf"; //$NON-NLS-1$
    private static final String TEST_FILE_PWD_MOD = "TEST_PDF_Password_Modification.pdf"; //$NON-NLS-1$
    private static final String TEST_FILE_CTF = "TEST_PDF_Certified.pdf"; //$NON-NLS-1$

    static {
        final Properties p1 = new Properties();
        p1.setProperty("format", AOSignConstants.SIGN_FORMAT_PDF); //$NON-NLS-1$
        p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        p1.setProperty("signReason", "test"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("signatureProductionCity", "madrid"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("signerContact", "sink@usa.net"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyQualifier", "http://administracionelectronica.gob.es/es/ctt/politicafirma/politica_firma_AGE_v1_8.pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyIdentifier", "2.16.724.1.3.1.1.2.1.8"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyIdentifierHash", "8lVVNGDCPen6VELRD1Ja8HARFk=="); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyIdentifierHashAlgorithm", "SHA-1"); //$NON-NLS-1$ //$NON-NLS-2$

        final Properties p2 = new Properties();
        p2.setProperty("format", AOSignConstants.SIGN_FORMAT_PDF); //$NON-NLS-1$
        p2.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$

        PADES_MODES = new Properties[] {
                p1, p2
        };
    }

    /** Algoritmos de firma a probar. */
    private final static String[] ALGOS = new String[] {
            AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA,
    };

    /** Prueba de identificaci&oacute;n de un PDF sin ninguna firma.
     * @throws Exception */
    @SuppressWarnings("static-method")
	@Test
    public void testIsSign() throws Exception {
    	Assert.assertFalse(
			new AOPDFSigner().isSign(
				AOUtil.getDataFromInputStream(
					ClassLoader.getSystemResourceAsStream(TEST_FILES[0])
				)
			)
		);
    	Assert.assertTrue(
			new AOPDFSigner().isSign(
				AOUtil.getDataFromInputStream(
					ClassLoader.getSystemResourceAsStream(TEST_FILES[1])
				)
			)
		);
    }

    /** Prueba de PDF con sello de tiempo contra la TSA de CATCert.
     * @throws Exception */
    @SuppressWarnings("static-method")
	@Test
    public void testTimestampedSignature() throws Exception {

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOPDFSigner();

        final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILES[0]));

        final String prueba = "Firma PAdES de PDF con sello de tiempo en SHA512withRSA"; //$NON-NLS-1$

        System.out.println(prueba);

        final Properties extraParams = new Properties();
        //********* TSA CATCERT ********************************************************************
        //******************************************************************************************
        extraParams.put("tsaURL", CATCERT_TSP); //$NON-NLS-1$
        extraParams.put("tsaPolicy", CATCERT_POLICY); //$NON-NLS-1$
        extraParams.put("tsaRequireCert", CATCERT_REQUIRECERT); //$NON-NLS-1$
        extraParams.put("tsaHashAlgorithm", "SHA1"); //$NON-NLS-1$ //$NON-NLS-2$
        //******************************************************************************************
        //********* FIN TSA CATCERT ****************************************************************

//        //********** TSA AFIRMA ********************************************************************
//        //******************************************************************************************
//        extraParams.put("tsaURL", "https://10.253.252.184:10318/tsamap/TspHttpServer"); //$NON-NLS-1$ //$NON-NLS-2$
//        //extraParams.put("tsaURL", "socket://10.253.252.184:318/tsamap/TspHttpServer"/*"http://des-tsafirma.redsara.es:318/tsamap/TspHttpServer"*/); //$NON-NLS-1$ //$NON-NLS-2$
//        extraParams.put("tsaPolicy", "1.3.4.6.1.3.4.6"); //$NON-NLS-1$ //$NON-NLS-2$
//        extraParams.put("tsaRequireCert", "true"); //$NON-NLS-1$ //$NON-NLS-2$
//        extraParams.put("tsaHashAlgorithm", "SHA1"); //$NON-NLS-1$ //$NON-NLS-2$
//        extraParams.put("tsaHashAlgorithm", "SHA1"); //$NON-NLS-1$ //$NON-NLS-2$
//        extraParams.put("tsaExtensionOid", "1.3.4.6.1.3.4.6");  //$NON-NLS-1$//$NON-NLS-2$
//        extraParams.put("tsaExtensionValueBase64", "NOMBRE_APP_AFIRMA_EN_BASE64"); //$NON-NLS-1$ //$NON-NLS-2$
//        extraParams.put("tsaUsr", "USUARIO"); //$NON-NLS-1$ //$NON-NLS-2$
//        extraParams.put("tsaPwd", "CONTRASENA"); //$NON-NLS-1$ //$NON-NLS-2$
//        //******************************************************************************************
//        //********** FIN TSA AFIRMA ****************************************************************

        final byte[] result = signer.sign(
    		testPdf,
    		"SHA512withRSA", //$NON-NLS-1$
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		extraParams
		);

        final File saveFile = File.createTempFile("TSA-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        final OutputStream os = new FileOutputStream(saveFile);
        os.write(result);
        os.flush();
        os.close();
        System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

        Assert.assertNotNull(prueba, result);
        Assert.assertTrue(signer.isSign(result));

    }


    /**
     * Prueba la firma de un PDF protegido con contrase&ntilde;a.
     * @throws Exception en cualquier error
     */
    @SuppressWarnings("static-method")
	@Test
    public void testPasswordSignature() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOPDFSigner();

        final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE_PWD));

        Assert.assertTrue("No se ha reconocido como un PDF", signer.isValidDataFile(testPdf)); //$NON-NLS-1$

        final String prueba = "Firma PAdES de PDF con contrasena en SHA512withRSA"; //$NON-NLS-1$

        System.out.println(prueba);

        final Properties extraParams = new Properties();
        extraParams.put("headLess", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("ownerPassword", "password"); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] result = signer.sign(
    		testPdf,
    		"SHA512withRSA",  //$NON-NLS-1$
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		extraParams
		);

        Assert.assertNotNull(prueba, result);

    }

    /**
     * Prueba la firma de un PDF protegido con contrase&ntilde;a contra modificaci&oacute;n.
     * @throws Exception en cualquier error
     */
    @SuppressWarnings("static-method")
	@Test
    public void testModificationPasswordSignature() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOPDFSigner();

        final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE_PWD_MOD));

        Assert.assertTrue("No se ha reconocido como un PDF", signer.isValidDataFile(testPdf)); //$NON-NLS-1$

        final String prueba = "Firma PAdES de PDF con contrasena en SHA512withRSA"; //$NON-NLS-1$

        System.out.println(prueba);

        final Properties extraParams = new Properties();
        extraParams.put("headLess", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("userPassword", "1111"); //$NON-NLS-1$ //$NON-NLS-2$

        final byte[] result = signer.sign(
    		testPdf,
    		"SHA512withRSA",  //$NON-NLS-1$
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		extraParams
		);

        Assert.assertNotNull(prueba, result);

        final File out = File.createTempFile("TEST-PWD", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        final FileOutputStream fos = new FileOutputStream(out);
        fos.write(result);
        fos.flush();
        fos.close();
        System.out.println("Temporal para comprobacion manual: " + out.getAbsolutePath()); //$NON-NLS-1$

    }


    /**
     * Prueba de firma convencional.
     * @throws Exception en cualquier error
     */
    @SuppressWarnings("static-method")
	@Test
    public void testSignature() throws Exception {

        Assert.assertEquals("file.signed.pdf", AOPDFSigner.getSignedName("file.pdf")); //$NON-NLS-1$ //$NON-NLS-2$

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;
        final X509Certificate cert;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
        cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

        final AOSigner signer = new AOPDFSigner();

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
                    "' y el fichero '" +  //$NON-NLS-1$
                    file +
                    "'"; //$NON-NLS-1$

                    System.out.println(prueba);

                    final byte[] result = signer.sign(
                		testPdf,
                		algo,
                		pke.getPrivateKey(),
                		pke.getCertificateChain(),
                		extraParams
            		);

                    Assert.assertNotNull(prueba, result);
                    Assert.assertTrue(signer.isSign(result));

                    AOTreeModel tree = signer.getSignersStructure(result, false);
                    Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$
                    Assert.assertEquals("ANF Usuario Activo", ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject()); //$NON-NLS-1$

                    tree = signer.getSignersStructure(result, true);
                    Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$
                    final AOSimpleSignInfo simpleSignInfo = (AOSimpleSignInfo) ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject();

                    //Assert.assertNotNull(simpleSignInfo.getSigningTime());
                    Assert.assertEquals(cert, simpleSignInfo.getCerts()[0]);

                    Assert.assertEquals(result, signer.getData(result));

                    Assert.assertEquals(AOSignConstants.SIGN_FORMAT_PDF, signer.getSignInfo(result).getFormat());

                    final File saveFile = File.createTempFile(algo, ".pdf"); //$NON-NLS-1$
                    final OutputStream os = new FileOutputStream(saveFile);
                    os.write(result);
                    os.flush();
                    os.close();
                    System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

                }


            }
        }
    }


    /**
     * Prueba la firma de un PDF certificado.
     * @throws Exception en cualquier error
     */
    @SuppressWarnings("static-method")
	@Test
    public void testCertifiedSignature() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOPDFSigner();

        final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE_CTF));

        Assert.assertTrue("No se ha reconocido como un PDF", signer.isValidDataFile(testPdf)); //$NON-NLS-1$

        String prueba = "Firma PAdES de PDF certificado en SHA512withRSA indicando allowSigningCertifiedPdfs=true"; //$NON-NLS-1$

        System.out.println(prueba);

        Properties extraParams = new Properties();
        extraParams.put("allowSigningCertifiedPdfs", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        byte[] result = signer.sign(
    		testPdf,
    		"SHA512withRSA",  //$NON-NLS-1$
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		extraParams
		);

        Assert.assertNotNull(prueba, result);
        Assert.assertTrue(signer.isSign(result));

        prueba = "Firma PAdES de PDF certificado en SHA512withRSA indicando unicamente headLess=true"; //$NON-NLS-1$
        System.out.println(prueba);

        extraParams = new Properties();
        extraParams.put("headLess", "true"); //$NON-NLS-1$ //$NON-NLS-2$

        boolean failed = false;
        try {
            result = signer.sign(
        		testPdf,
        		"SHA512withRSA",  //$NON-NLS-1$
        		pke.getPrivateKey(),
        		pke.getCertificateChain(),
        		extraParams
    		);
        }
        catch(final Exception e) {
            failed = true;
        }
        Assert.assertTrue("Deberia haber fallado", failed); //$NON-NLS-1$

        prueba = "Firma PAdES de PDF certificado en SHA512withRSA indicando unicamente allowSigningCertifiedPdfs=false"; //$NON-NLS-1$
        System.out.println(prueba);

        extraParams = new Properties();
        extraParams.put("allowSigningCertifiedPdfs", "false"); //$NON-NLS-1$ //$NON-NLS-2$

        failed = false;
        try {
            result = signer.sign(
        		testPdf,
        		"SHA512withRSA", //$NON-NLS-1$
        		pke.getPrivateKey(),
        		pke.getCertificateChain(),
        		extraParams
    		);
        }
        catch(final Exception e) {
            failed = true;
        }
        Assert.assertTrue("Deberia haber fallado", failed); //$NON-NLS-1$

    }

    /**
     * Prueba la firma de un PDF certificado.
     * @throws Exception en cualquier error
     */
    @SuppressWarnings("static-method")
	@Test
    public void testCertificatedSignature() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOPDFSigner();

        final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILES[0]));

        Assert.assertTrue("No se ha reconocido como un PDF", signer.isValidDataFile(testPdf)); //$NON-NLS-1$

        final String prueba = "Firma certificada PAdES de documento PDF indicando la propiedad certificationLevel"; //$NON-NLS-1$

        final String[] certificationLevels = new String[] {
        	"Firma de autor. No se permite ningun cambio posterior en el documento", //$NON-NLS-1$
        	"Firma de autor certificada para formularios. Se permite unicamente el relleno posterior de los campos del formulario", //$NON-NLS-1$
        	"Firma certificada. Se permite unicamente el relleno posterior de los campos del formulario o el anadido de firmas de aprobacion" //$NON-NLS-1$
        };

        System.out.println(prueba);

        final Properties extraParams = new Properties();

        for (int i = 1; i <= certificationLevels.length; i++) {

        	extraParams.put("certificationLevel", Integer.toString(i)); //$NON-NLS-1$

        	System.out.println(certificationLevels[i-1]);

        	final byte[] result = signer.sign(
        			testPdf,
        			"SHA512withRSA",  //$NON-NLS-1$
        			pke.getPrivateKey(),
        			pke.getCertificateChain(),
        			extraParams
        			);

        	final File tempFile = File.createTempFile("afirmaPDF", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$

        	final FileOutputStream fos = new FileOutputStream(tempFile);
        	fos.write(result);
        	fos.close();

//        	Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
//        			"Fichero temporal para la comprobacion manual del resultado: " + //$NON-NLS-1$
//        			tempFile.getAbsolutePath());
        	System.out.println("Fichero temporal para la comprobacion manual del resultado: " + //$NON-NLS-1$
        			tempFile.getAbsolutePath());
        }
    }
}

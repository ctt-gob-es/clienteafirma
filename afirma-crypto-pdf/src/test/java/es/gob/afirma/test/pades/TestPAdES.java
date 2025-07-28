/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.test.pades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.GregorianCalendar;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.pades.PdfTimestamper;
import es.gob.afirma.signers.pades.common.PdfExtraParams;

/** Pruebas del m&oacute;dulo PAdES de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestPAdES {

	private static final String CATCERT_POLICY = "0.4.0.2023.1.1"; //$NON-NLS-1$
	private static final String CATCERT_TSP = "http://psis.catcert.net/psis/catcert/tsp"; //$NON-NLS-1$
	private static final Boolean CATCERT_REQUIRECERT = Boolean.TRUE;

    private static final Properties[] PADES_MODES;

    private static final String[] TEST_FILES = { "TEST_PDF.pdf", "TEST_PDF_Signed.pdf", "pades_basic.pdf", "firma_CM.pdf" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    //private static final String TEST_FILE_CTF = "TEST_PDF_Certified.pdf"; //$NON-NLS-1$
    private static final String TEST_FILE_CTF2 = "PDF_certificado_tipo_1.pdf"; //$NON-NLS-1$

    private static final String TEST_FILE_PDFA1B = "PDF-A1B.pdf"; //$NON-NLS-1$

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
        p1.setProperty("allowCosigningUnregisteredSignatures", "true"); //$NON-NLS-1$ //$NON-NLS-2$

        final Properties p2 = new Properties();
        p2.setProperty("format", AOSignConstants.SIGN_FORMAT_PDF); //$NON-NLS-1$
        p2.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$
        p2.setProperty("allowCosigningUnregisteredSignatures", "true"); //$NON-NLS-1$ //$NON-NLS-2$

        PADES_MODES = new Properties[] {
                p1, p2
        };
    }

    /** Algoritmos de firma a probar. */
    private final static String[] ALGOS = new String[] {
            AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA
    };

    /** Main para pruebas sin JUNit.
     * @param args No se usa.
     * @throws Exception EN cualquier error. */
    public static void main(final String args[]) throws Exception {
    	new TestPAdES().testSignature();
    }

    /** Prueba de identificaci&oacute;n de un PDF sin ninguna firma.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testIsSign() throws Exception {
    	try (
    		final InputStream is0 = ClassLoader.getSystemResourceAsStream(TEST_FILES[0]);
			final InputStream is1 = ClassLoader.getSystemResourceAsStream(TEST_FILES[1]);
			final InputStream is2 = ClassLoader.getSystemResourceAsStream(TEST_FILES[2]);
			final InputStream is3 = ClassLoader.getSystemResourceAsStream(TEST_FILES[3])
		) {
	    	Assert.assertFalse("El fichero " + TEST_FILES[0] + " se identifica como firma y no lo es", //$NON-NLS-1$ //$NON-NLS-2$
				new AOPDFSigner().isSign(
					AOUtil.getDataFromInputStream(
						is0
					)
				)
			);
	    	Assert.assertTrue("El fichero " + TEST_FILES[1] + " no se identifica como firma", //$NON-NLS-1$ //$NON-NLS-2$
				new AOPDFSigner().isSign(
					AOUtil.getDataFromInputStream(
						is1
					)
				)
			);
	    	Assert.assertTrue("El fichero " + TEST_FILES[2] + " no se identifica como firma", //$NON-NLS-1$ //$NON-NLS-2$
				new AOPDFSigner().isSign(
					AOUtil.getDataFromInputStream(
						is2
					)
				)
			);
	    	System.setProperty("allowCosigningUnregisteredSignatures", "true"); //$NON-NLS-1$ //$NON-NLS-2$
	    	Assert.assertTrue("El fichero " + TEST_FILES[3] + " no se identifica como firma", //$NON-NLS-1$ //$NON-NLS-2$
    			new AOPDFSigner().isSign(
    				AOUtil.getDataFromInputStream(
    					is3
    				)
    			)
    		);
    	}
    }

    /** Prueba de PDF con sello de tiempo contra la TSA de CATCert.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
	@Ignore
    public void testTimestampedSignatureAndDocument() throws Exception {

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        try (
    		final InputStream is = ClassLoader.getSystemResourceAsStream(TestContants.CERT_PATH)
		) {
        	ks.load(is, TestContants.CERT_PASS.toCharArray());
        }
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(TestContants.CERT_ALIAS, new KeyStore.PasswordProtection(TestContants.CERT_PASS.toCharArray()));

        final AOSigner signer = new AOPDFSigner();

        final byte[] testPdf;
        try (
    		final InputStream is0 = ClassLoader.getSystemResourceAsStream(TEST_FILES[0])
		) {
        	testPdf = AOUtil.getDataFromInputStream(is0);
        }

        final String prueba = "Firma PAdES de PDF con sello de tiempo sobre la firma y el documento"; //$NON-NLS-1$

        System.out.println(prueba);

        final Properties extraParams = new Properties();
        //********* TSA CATCERT ********************************************************************
        //******************************************************************************************
        extraParams.put("tsaURL", CATCERT_TSP); //$NON-NLS-1$
        extraParams.put("tsaPolicy", CATCERT_POLICY); //$NON-NLS-1$
        extraParams.put("tsaRequireCert", CATCERT_REQUIRECERT); //$NON-NLS-1$
        extraParams.put("tsaHashAlgorithm", "SHA-512"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("tsType", PdfTimestamper.TS_LEVEL_SIGN_DOC); //$NON-NLS-1$
        //******************************************************************************************
        //********* FIN TSA CATCERT ****************************************************************

        // Certificacion
        extraParams.put("certificationLevel", "1"); //$NON-NLS-1$ //$NON-NLS-2$

        // Politica
        extraParams.put("policyIdentifier", "urn:oid:2.16.724.1.3.1.1.2.1.9"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("policyQualifier", "https://sede.060.gob.es/politica_de_firma_anexo_1.pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("policyIdentifierHashAlgorithm", "http://www.w3.org/2000/09/xmldsig#sha1"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("policyIdentifierHash", "G7roucf600+f03r/o0bAOQ6WAs0="); //$NON-NLS-1$ //$NON-NLS-2$

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
        try (
    		final OutputStream os = new FileOutputStream(saveFile);
		) {
	        os.write(result);
	        os.flush();
        }

        System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

        Assert.assertNotNull(prueba, result);
        Assert.assertTrue(signer.isSign(result));

    }

    /** Prueba de PDF con sello de tiempo contra la TSA de CATCert.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
	@Ignore
    public void testTimestampedDocument() throws Exception {

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        try (
    		final InputStream is = ClassLoader.getSystemResourceAsStream(TestContants.CERT_PATH)
		) {
        	ks.load(is, TestContants.CERT_PASS.toCharArray());
        }
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(TestContants.CERT_ALIAS, new KeyStore.PasswordProtection(TestContants.CERT_PASS.toCharArray()));

        final AOSigner signer = new AOPDFSigner();

        final byte[] testPdf;
        try (
    		final InputStream is = ClassLoader.getSystemResourceAsStream(TEST_FILES[0])
		) {
        	testPdf = AOUtil.getDataFromInputStream(is);
        }

        final String prueba = "Firma PAdES de PDF con sello de tiempo sobre el documento"; //$NON-NLS-1$

        System.out.println(prueba);

        final Properties extraParams = new Properties();
        //********* TSA CATCERT ********************************************************************
        //******************************************************************************************
        extraParams.put("tsaURL", CATCERT_TSP); //$NON-NLS-1$
        extraParams.put("tsaPolicy", CATCERT_POLICY); //$NON-NLS-1$
        extraParams.put("tsaRequireCert", CATCERT_REQUIRECERT); //$NON-NLS-1$
        extraParams.put("tsaHashAlgorithm", "SHA-512"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("tsType", PdfTimestamper.TS_LEVEL_DOC); //$NON-NLS-1$
        //******************************************************************************************
        //********* FIN TSA CATCERT ****************************************************************

        final byte[] result = signer.sign(
    		testPdf,
    		"SHA512withRSA", //$NON-NLS-1$
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		extraParams
		);

        final File saveFile = File.createTempFile("TSA-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final OutputStream os = new FileOutputStream(saveFile);
		) {
	        os.write(result);
	        os.flush();
        }

        System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

        Assert.assertNotNull(prueba, result);
        Assert.assertTrue(signer.isSign(result));

    }

    /** Prueba de PDF con sello de tiempo contra la TSA de CATCert.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
	@Ignore
    public void testTimestampedDocumentWithoutSignature() throws Exception {

        final byte[] testPdf;
        try (
    		final InputStream is = ClassLoader.getSystemResourceAsStream(TEST_FILES[0])
		) {
        	testPdf = AOUtil.getDataFromInputStream(is);
        }

        final String prueba = "Firma PAdES de PDF con sello de tiempo sobre el documento y sin firmas adicionales"; //$NON-NLS-1$

        System.out.println(prueba);

        final Properties extraParams = new Properties();
        //********* TSA CATCERT ********************************************************************
        //******************************************************************************************
        extraParams.put("tsaURL", CATCERT_TSP); //$NON-NLS-1$
        extraParams.put("tsaPolicy", CATCERT_POLICY); //$NON-NLS-1$
        extraParams.put("tsaRequireCert", CATCERT_REQUIRECERT); //$NON-NLS-1$
        extraParams.put("tsaHashAlgorithm", "SHA-512"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("tsType", PdfTimestamper.TS_LEVEL_DOC); //$NON-NLS-1$
        //******************************************************************************************
        //********* FIN TSA CATCERT ****************************************************************

        final byte[] result = PdfTimestamper.timestampPdf(testPdf, extraParams, new GregorianCalendar());

        final File saveFile = File.createTempFile("TSA-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final OutputStream os = new FileOutputStream(saveFile);
		) {
	        os.write(result);
	        os.flush();
        }

        System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

        Assert.assertNotNull(prueba, result);

    }

    /** Prueba de PDF con sello de tiempo contra la TSA de CATCert.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
	@Ignore
    public void testTimestampedSignature() throws Exception {

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        try (
    		final InputStream is = ClassLoader.getSystemResourceAsStream(TestContants.CERT_PATH)
		) {
        	ks.load(is, TestContants.CERT_PASS.toCharArray());
        }
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(TestContants.CERT_ALIAS, new KeyStore.PasswordProtection(TestContants.CERT_PASS.toCharArray()));

        final AOSigner signer = new AOPDFSigner();

        final byte[] testPdf;
        try (
    		final InputStream is0 = ClassLoader.getSystemResourceAsStream(TEST_FILES[0])
		) {
        	testPdf = AOUtil.getDataFromInputStream(is0);
        }

        final String prueba = "Firma PAdES de PDF con sello de tiempo sobre la firma"; //$NON-NLS-1$

        System.out.println(prueba);

        final Properties extraParams = new Properties();
        //********* TSA CATCERT ********************************************************************
        //******************************************************************************************
        extraParams.put("tsaURL", CATCERT_TSP); //$NON-NLS-1$
        extraParams.put("tsaPolicy", CATCERT_POLICY); //$NON-NLS-1$
        extraParams.put("tsaRequireCert", CATCERT_REQUIRECERT); //$NON-NLS-1$
        extraParams.put("tsaHashAlgorithm", "SHA-512"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("tsType", PdfTimestamper.TS_LEVEL_SIGN); //$NON-NLS-1$
        //******************************************************************************************
        //********* FIN TSA CATCERT ****************************************************************

        final byte[] result = signer.sign(
    		testPdf,
    		"SHA512withRSA", //$NON-NLS-1$
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		extraParams
		);

        final File saveFile = File.createTempFile("TSA-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final OutputStream os = new FileOutputStream(saveFile);
		) {
	        os.write(result);
	        os.flush();
        }
        System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

        Assert.assertNotNull(prueba, result);
        Assert.assertTrue(signer.isSign(result));
    }

    /** Prueba de firma convencional.
     * @throws Exception en cualquier error */
    @SuppressWarnings("static-method")
	@Test
    public void testSignature() throws Exception {

        Assert.assertEquals("file.signed.pdf", AOPDFSigner.getSignedName("file.pdf")); //$NON-NLS-1$ //$NON-NLS-2$

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        try (
    		final InputStream is = ClassLoader.getSystemResourceAsStream(TestContants.CERT_PATH)
		) {
        	ks.load(is, TestContants.CERT_PASS.toCharArray());
        }
        pke = (PrivateKeyEntry) ks.getEntry(TestContants.CERT_ALIAS, new KeyStore.PasswordProtection(TestContants.CERT_PASS.toCharArray()));

        final AOPDFSigner signer = new AOPDFSigner();

        String prueba;

        for (final Properties extraParams : PADES_MODES) {
            for (final String algo : ALGOS) {
                for (final String file : TEST_FILES) {

                    final byte[] testPdf;
                    try (
                		final InputStream is = ClassLoader.getSystemResourceAsStream(file)
            		) {
                    	testPdf = AOUtil.getDataFromInputStream(is);
                    }

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

                    tree = signer.getSignersStructure(result, true);
                    Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$
                    final AOSimpleSignInfo simpleSignInfo = (AOSimpleSignInfo) ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject();
                    simpleSignInfo.getCerts();

                    Assert.assertEquals(result, signer.getData(result));

                    Assert.assertEquals(AOSignConstants.SIGN_FORMAT_PDF, signer.getSignInfo(result).getFormat());

                    final File saveFile = File.createTempFile(algo, ".pdf"); //$NON-NLS-1$
                    try (
                		final OutputStream os = new FileOutputStream(saveFile);
            		) {
            	        os.write(result);
            	        os.flush();
                    }
                    System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

                }


            }
        }
    }

    /** Prueba la firma de un PDF certificado.
     * @throws Exception en cualquier error */
    @SuppressWarnings("static-method")
	@Test
    public void testCertifiedSignature() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        try (
    		final InputStream is = ClassLoader.getSystemResourceAsStream(TestContants.CERT_PATH)
		) {
        	ks.load(is, TestContants.CERT_PASS.toCharArray());
        }
        pke = (PrivateKeyEntry) ks.getEntry(TestContants.CERT_ALIAS, new KeyStore.PasswordProtection(TestContants.CERT_PASS.toCharArray()));

        final AOSigner signer = new AOPDFSigner();
        final byte[] testPdf;
        try (
    		final InputStream isCtf = ClassLoader.getSystemResourceAsStream(TEST_FILE_CTF2)
		) {
        	testPdf = AOUtil.getDataFromInputStream(isCtf);
        }
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

        prueba = "Firma PAdES de PDF certificado en SHA512withRSA indicando unicamente headless=true"; //$NON-NLS-1$
        System.out.println(prueba);

        extraParams = new Properties();
        extraParams.put("headless", "true"); //$NON-NLS-1$ //$NON-NLS-2$

        boolean failed = false;
        try {
            result = signer.sign(
        		testPdf,
        		"SHA512withRSA",  //$NON-NLS-1$
        		pke.getPrivateKey(),
        		pke.getCertificateChain(),
        		extraParams
    		);
            final File file = File.createTempFile("PDF-FALLIDO_", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
            try (
        		final OutputStream fos = new FileOutputStream(file);
    		) {
	            fos.write(result);
	            fos.flush();
            }
            System.out.println("PDF Fallido: " + file.getAbsolutePath()); //$NON-NLS-1$
        }
        catch(final Exception e) {
	    	Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
				"Fallo esperado: " + e //$NON-NLS-1$
			);
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
        	Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
				"Fallo esperado: " + e //$NON-NLS-1$
			);
            failed = true;
        }
        Assert.assertTrue("Deberia haber fallado", failed); //$NON-NLS-1$

    }

    /** Prueba la firma de un PDF certificado.
     * @throws Exception en cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testCertificatedSignature() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        try (
    		final InputStream is = ClassLoader.getSystemResourceAsStream(TestContants.CERT_PATH)
		) {
        	ks.load(is, TestContants.CERT_PASS.toCharArray());
        }
        pke = (PrivateKeyEntry) ks.getEntry(TestContants.CERT_ALIAS, new KeyStore.PasswordProtection(TestContants.CERT_PASS.toCharArray()));

        final AOSigner signer = new AOPDFSigner();

        final byte[] testPdf;
        try (
    		final InputStream is0 = ClassLoader.getSystemResourceAsStream(TEST_FILES[0])
		) {
        	testPdf = AOUtil.getDataFromInputStream(is0);
        }

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

        	try (
    			final FileOutputStream fos = new FileOutputStream(tempFile);
			) {
        		fos.write(result);
        	}

//        	Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
//        			"Fichero temporal para la comprobacion manual del resultado: " + //$NON-NLS-1$
//        			tempFile.getAbsolutePath());
        	System.out.println("Fichero temporal para la comprobacion manual del resultado: " + //$NON-NLS-1$
        			tempFile.getAbsolutePath());
        }
    }

    /** Prueba de la verificaci&oacute;n de la versi&oacute;n de iText.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testIdentyFormat() throws Exception {

        final byte[] testPdf;
        try (
    		final InputStream isBasic = ClassLoader.getSystemResourceAsStream("pades_basic.pdf") //$NON-NLS-1$
		) {
        	testPdf = AOUtil.getDataFromInputStream(isBasic);
        }
    	final AOSigner signer = AOSignerFactory.getSigner(testPdf);
    	Assert.assertNotNull("No se ha identificado correctamente el formato PAdES", signer); //$NON-NLS-1$

    	Assert.assertTrue("El formato no se ha identificado correctamente", signer instanceof AOPDFSigner); //$NON-NLS-1$
    }

    /** Prueba de la verificaci&oacute;n de la versi&oacute;n de iText.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testReservedSignatureSize() throws Exception {

    	final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        try (
    		final InputStream is = ClassLoader.getSystemResourceAsStream(TestContants.CERT_PATH)
		) {
        	ks.load(is, TestContants.CERT_PASS.toCharArray());
        }
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(TestContants.CERT_ALIAS, new KeyStore.PasswordProtection(TestContants.CERT_PASS.toCharArray()));

        final byte[] testPdf;
        try (
    		final InputStream isPdf = ClassLoader.getSystemResourceAsStream(TEST_FILES[0])
		) {
        	testPdf = AOUtil.getDataFromInputStream(isPdf);
        }
    	final AOSigner signer = new AOPDFSigner();

    	final byte[] defaultSignature = signer.sign(testPdf, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, pke.getPrivateKey(), pke.getCertificateChain(), null);

    	final Properties reservedSpaceConfig = new Properties();
    	reservedSpaceConfig.setProperty("signReservedSize", "40000"); //$NON-NLS-1$ //$NON-NLS-2$

    	final byte[] signatureWithReservedSpace = signer.sign(testPdf, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, pke.getPrivateKey(), pke.getCertificateChain(), reservedSpaceConfig);

    	System.out.println("Tamano estandar: " + defaultSignature.length); //$NON-NLS-1$
    	System.out.println("Con tamano reservado: " + signatureWithReservedSpace.length); //$NON-NLS-1$

    	Assert.assertTrue(
    			"El tamano de la firma con espacio reservado deveria ser considerablemente mayor a la por defecto", //$NON-NLS-1$
    			signatureWithReservedSpace.length > defaultSignature.length + 10000);
    }

    /** Prueba de firma visible PDF sobre un documento PDF/A.
     * @throws Exception Cuando ocurre cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testVisibleSignature() throws Exception {

    	final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        try (
    		final InputStream is = ClassLoader.getSystemResourceAsStream(TestContants.CERT_PATH)
		) {
        	ks.load(is, TestContants.CERT_PASS.toCharArray());
        }
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(TestContants.CERT_ALIAS, new KeyStore.PasswordProtection(TestContants.CERT_PASS.toCharArray()));

        final byte[] testPdf;
        try (
    		final InputStream isPdf = ClassLoader.getSystemResourceAsStream(TEST_FILE_PDFA1B)
		) {
        	testPdf = AOUtil.getDataFromInputStream(isPdf);
        }

    	final Properties config = new Properties();
    	config.setProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTX, "100"); //$NON-NLS-1$
    	config.setProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTY, "100"); //$NON-NLS-1$
    	config.setProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTX, "200"); //$NON-NLS-1$
    	config.setProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTY, "200"); //$NON-NLS-1$
    	config.setProperty(PdfExtraParams.SIGNATURE_PAGES, "1"); //$NON-NLS-1$

    	final AOSigner signer = new AOPDFSigner();
    	final byte[] signedPdf = signer.sign(testPdf, AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, pke.getPrivateKey(), pke.getCertificateChain(), config);

    	final File tempFile = File.createTempFile("firmavisible-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
    	try (OutputStream fos = new FileOutputStream(tempFile)) {
    		fos.write(signedPdf);
    	}

    	System.out.println("Fichero temporal para la comprobacion manual del resultado: " + //$NON-NLS-1$
    			tempFile.getAbsolutePath());

    }
}

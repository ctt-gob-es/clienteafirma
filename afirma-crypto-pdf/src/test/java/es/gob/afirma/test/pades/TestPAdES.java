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
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Test;

import com.aowagie.text.pdf.PdfReader;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.tsp.pkcs7.TsaParams;

/** Pruebas del m&oacute;dulo PAdES de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestPAdES {

	private static final String CATCERT_POLICY = "0.4.0.2023.1.1"; //$NON-NLS-1$
	private static final String CATCERT_TSP = "http://psis.catcert.net/psis/catcert/tsp"; //$NON-NLS-1$
	private static final Boolean CATCERT_REQUIRECERT = Boolean.TRUE;

    private static final String CERT_PATH = "PFActivoFirSHA256.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "fisico activo prueba"; //$NON-NLS-1$

    private static final Properties[] PADES_MODES;

    private static final String[] TEST_FILES = { "TEST_PDF.pdf", "TEST_PDF_Signed.pdf", "pades_basic.pdf", "firma_CM.pdf" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    //private static final String TEST_FILE_CTF = "TEST_PDF_Certified.pdf"; //$NON-NLS-1$
    private static final String TEST_FILE_CTF2 = "pruebafirma_certificado.pdf"; //$NON-NLS-1$

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
            AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA,
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
    	Assert.assertFalse("El fichero " + TEST_FILES[0] + " se identifica como firma y no lo es", //$NON-NLS-1$ //$NON-NLS-2$
			new AOPDFSigner().isSign(
				AOUtil.getDataFromInputStream(
					ClassLoader.getSystemResourceAsStream(TEST_FILES[0])
				)
			)
		);
    	Assert.assertTrue("El fichero " + TEST_FILES[1] + " no se identifica como firma", //$NON-NLS-1$ //$NON-NLS-2$
			new AOPDFSigner().isSign(
				AOUtil.getDataFromInputStream(
					ClassLoader.getSystemResourceAsStream(TEST_FILES[1])
				)
			)
		);

    	Assert.assertTrue("El fichero " + TEST_FILES[2] + " no se identifica como firma", //$NON-NLS-1$ //$NON-NLS-2$
			new AOPDFSigner().isSign(
				AOUtil.getDataFromInputStream(
					ClassLoader.getSystemResourceAsStream(TEST_FILES[2])
				)
			)
		);

    	System.setProperty("allowCosigningUnregisteredSignatures", "true"); //$NON-NLS-1$ //$NON-NLS-2$
    	Assert.assertTrue("El fichero " + TEST_FILES[3] + " no se identifica como firma", //$NON-NLS-1$ //$NON-NLS-2$
    			new AOPDFSigner().isSign(
    				AOUtil.getDataFromInputStream(
    					ClassLoader.getSystemResourceAsStream(TEST_FILES[3])
    				)
    			)
    		);
    }

    /** Prueba de PDF con sello de tiempo contra la TSA de CATCert.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testTimestampedSignatureAndDocument() throws Exception {

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOPDFSigner();

        final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILES[0]));

        final String prueba = "Firma PAdES de PDF con sello de tiempo sobre la firma y el documento"; //$NON-NLS-1$

        System.out.println(prueba);

        final Properties extraParams = new Properties();
        //********* TSA CATCERT ********************************************************************
        //******************************************************************************************
        extraParams.put("tsaURL", CATCERT_TSP); //$NON-NLS-1$
        extraParams.put("tsaPolicy", CATCERT_POLICY); //$NON-NLS-1$
        extraParams.put("tsaRequireCert", CATCERT_REQUIRECERT); //$NON-NLS-1$
        extraParams.put("tsaHashAlgorithm", "SHA-512"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("tsType", TsaParams.TS_SIGN_DOC); //$NON-NLS-1$
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
        final OutputStream os = new FileOutputStream(saveFile);
        os.write(result);
        os.flush();
        os.close();
        System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

        Assert.assertNotNull(prueba, result);
        Assert.assertTrue(signer.isSign(result));

    }

    /** Prueba de PDF con sello de tiempo contra la TSA de CATCert.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testTimestampedSignature() throws Exception {

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOPDFSigner();

        final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILES[0]));

        final String prueba = "Firma PAdES de PDF con sello de tiempo sobre la firma"; //$NON-NLS-1$

        System.out.println(prueba);

        final Properties extraParams = new Properties();
        //********* TSA CATCERT ********************************************************************
        //******************************************************************************************
        extraParams.put("tsaURL", CATCERT_TSP); //$NON-NLS-1$
        extraParams.put("tsaPolicy", CATCERT_POLICY); //$NON-NLS-1$
        extraParams.put("tsaRequireCert", CATCERT_REQUIRECERT); //$NON-NLS-1$
        extraParams.put("tsaHashAlgorithm", "SHA-512"); //$NON-NLS-1$ //$NON-NLS-2$
        extraParams.put("tsType", TsaParams.TS_SIGN); //$NON-NLS-1$
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
        final OutputStream os = new FileOutputStream(saveFile);
        os.write(result);
        os.flush();
        os.close();
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
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

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

                    tree = signer.getSignersStructure(result, true);
                    Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$
                    final AOSimpleSignInfo simpleSignInfo = (AOSimpleSignInfo) ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject();
                    simpleSignInfo.getCerts();

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

    /** Prueba la firma de un PDF certificado.
     * @throws Exception en cualquier error */
    @SuppressWarnings("static-method")
	@Test
    public void testCertifiedSignature() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOPDFSigner();
        final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE_CTF2));
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
            final OutputStream fos = new FileOutputStream(file);
            fos.write(result);
            fos.flush();
            fos.close();
            System.out.println("PDF Fallido: " + file.getAbsolutePath()); //$NON-NLS-1$
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

    /** Prueba la firma de un PDF certificado.
     * @throws Exception en cualquier error. */
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

    /** Prueba de la verificaci&oacute;n de la versi&oacute:n de iText. */
    @SuppressWarnings("static-method")
	@Test
    public void testItextVersion() {
    	PdfReader.isAfirmaModifiedItext();
    }

    /** Prueba de la verificaci&oacute;n de la versi&oacute:n de iText.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testIdentyFormat() throws Exception {

    	final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("pades_basic.pdf")); //$NON-NLS-1$
    	final AOSigner signer = AOSignerFactory.getSigner(testPdf);
    	Assert.assertNotNull("No se ha identificado correctamente el formato PAdES", signer); //$NON-NLS-1$

    	Assert.assertTrue("El formato no se ha identificado correctamente", signer instanceof AOPDFSigner); //$NON-NLS-1$
    }
}

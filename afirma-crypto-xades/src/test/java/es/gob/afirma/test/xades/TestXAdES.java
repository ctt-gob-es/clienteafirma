/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.test.xades;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.crypto.dsig.DigestMethod;
import javax.xml.parsers.DocumentBuilderFactory;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.signers.xml.Utils;



/**
 * Pruebas del m&oacute;dulo XAdES de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 *
 */
public final class TestXAdES {

    private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

//    private static final String CERT_PATH2 = "CATCERT GENCAT SAFP PF Identidad y Firma Reconocida de Clase 1 Caducado.pfx"; //$NON-NLS-1$
//    private static final String CERT_PASS2 = "1234"; //$NON-NLS-1$
//    private static final String CERT_ALIAS2 = "{71e526c4-0f27-4f32-8be0-90df52dcbc53}"; //$NON-NLS-1$
//
//    private static final String CERT_PATH3 = "CAMERFIRMA_PF_SW_Clave_usuario_Activo.p12"; //$NON-NLS-1$
//    private static final String CERT_PASS3 = "1111"; //$NON-NLS-1$
//    private static final String CERT_ALIAS3 = "1"; //$NON-NLS-1$

    private static final Properties[] XADES_MODES;

    static {
        final Properties p1 = new Properties();
        p1.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_DETACHED); //$NON-NLS-1$
        p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        p1.setProperty("policyIdentifier", "urn:oid:2.16.724.1.3.1.1.2.1.8"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyIdentifierHash", "V8lVVNGDCPen6VELRD1Ja8HARFk=");  //$NON-NLS-1$//$NON-NLS-2$
        p1.setProperty("policyIdentifierHashAlgorithm", DigestMethod.SHA1);         //$NON-NLS-1$
        p1.setProperty("policyDescription", "Politica de firma electronica para las Administraciones Publicas en Espana"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyQualifier", "http://administracionelectronica.gob.es/es/ctt/politicafirma/politica_firma_AGE_v1_8.pdf"); //$NON-NLS-1$ //$NON-NLS-2$

        final Properties p2 = new Properties();
        p2.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_DETACHED); //$NON-NLS-1$
        p2.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$

        final Properties p3 = new Properties();
        p3.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED); //$NON-NLS-1$
        p3.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$


        final Properties p4 = new Properties();
        p4.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING); //$NON-NLS-1$
        p4.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$

        final Properties p5 = new Properties();
        p5.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING); //$NON-NLS-1$
        p5.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$

        final Properties p6 = new Properties();
        p6.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING); //$NON-NLS-1$
        p6.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        p6.setProperty("encoding", "Base64"); //$NON-NLS-1$ //$NON-NLS-2$


        XADES_MODES = new Properties[] {
                p1, p2, p3, p4, p5, p6
        };
    }

    /** Algoritmos de firma a probar. */
    private final static String[] ALGOS = new String[] {
            AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA
    };

    // IMPORTANTE: Poner extension ".xml" a los ficheros de prueba con contenido XML
    private static final String[] TEST_FILES_DATA = new String[] {
            "ANF_PF_Activo.pfx", //$NON-NLS-1$
            "base64.b64", //$NON-NLS-1$
            "sample-class-attributes.xml", //$NON-NLS-1$
            "sample-facturae.xml", //$NON-NLS-1$
            "sample-embedded-style.xml", //$NON-NLS-1$
            "sample-encoding-UTF-8.xml", //$NON-NLS-1$
            "sample-internal-dtd.xml", //$NON-NLS-1$
            "sample-namespace-encoding-us-ascii.xml" //$NON-NLS-1$
    };

    private static final String[] TEST_FILES_MULTISIGN = new String[] {
            "XAdES-Detached-SHA1withRSA-B64.xml", //$NON-NLS-1$
            "XAdES-Detached-SHA1withRSA-XML.xml", //$NON-NLS-1$
            "XAdES-Enveloped-SHA1withRSA-XML.xml", //$NON-NLS-1$
            "XAdES-Enveloping-SHA1withRSA-B64.xml", //$NON-NLS-1$
            "XAdES-Enveloping-SHA1withRSA-XML.xml" //$NON-NLS-1$
    };

    /** Prueba de firma de nodo indicado expl&iacute;citamente.
     * @throws Exception */
    @SuppressWarnings("static-method")
	@Test
    public void testNodeTbs() throws Exception {

    	Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
    	Logger.getLogger("com.sun.org.apache.xml.internal.security.utils.CachedXPathFuncHereAPI").setLevel(Level.WARNING); //$NON-NLS-1$

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        final PrivateKeyEntry pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

    	final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream("xml_with_ids.xml")); //$NON-NLS-1$

    	final String[] formats = new String[] {
    			AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING,
    			AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED,
    			AOSignConstants.SIGN_FORMAT_XADES_DETACHED
    	};

    	final AOXAdESSigner signer = new AOXAdESSigner();

        final Properties p = new Properties();
        p.setProperty("nodeToSign", "1"); //$NON-NLS-1$ //$NON-NLS-2$
        p.setProperty("mode", "implicit"); //$NON-NLS-1$ //$NON-NLS-2$

    	for (final String format : formats) {
    		p.setProperty("format", format); //$NON-NLS-1$
    		final byte[] signature = signer.sign(data,
				AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				p
			);
    		final File f = File.createTempFile("xades-NODESIGN-" + format + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    		final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
    		fos.write(signature);
    		fos.flush(); fos.close();
    		System.out.println("Firma " + format + " para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$ //$NON-NLS-2$
    	}
    }

    /** Pruebas de cofirma.
     * @throws Exception Cuando ocurre un error */
    @SuppressWarnings("static-method")
	@Test
    public void testCoSign() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOXAdESSigner();

        final Properties p = new Properties();
        p.put("mode", "implicit"); //$NON-NLS-1$ //$NON-NLS-2$
        p.put("ignoreStyleSheets", "false"); //$NON-NLS-1$ //$NON-NLS-2$

        String prueba;

//        es.gob.afirma.platform.ws.TestSignVerifier verifier = null;
//        try {
//          verifier = new es.gob.afirma.platform.ws.TestSignVerifier();
//        }
//        catch (Exception e) {
//          System.out.println("No se ha podido inicializar el validador de firmas, no se validaran como parte de las pruebas: " + e); //$NON-NLS-1$
//        }

        for (final String algo : ALGOS) {
          for(final String filename : TEST_FILES_MULTISIGN) {

            prueba = "Cofirma XAdES con el algoritmo '" + //$NON-NLS-1$
            algo + "' y el fichero '" + filename + "'"; //$NON-NLS-1$ //$NON-NLS-2$

            System.out.println();
            System.out.println(prueba);

            final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(filename));

            final byte[] result = signer.cosign(
        		data,
        		algo,
        		pke.getPrivateKey(),
        		pke.getCertificateChain(),
        		p
    		);

            final File f = File.createTempFile(algo + "-" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
            final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
            fos.write(result);
            fos.flush(); fos.close();
            System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

            // Enviamos a validar a AFirma
//            if (verifier != null) {
//                Assert.assertTrue("Fallo al validar " + filename, verifier.verifyXML(result)); //$NON-NLS-1$
//            }

            Assert.assertTrue("UnsignedProperties invalidas", isValidUnsignedProperties(new ByteArrayInputStream(result),null)); //$NON-NLS-1$

            Assert.assertNotNull(prueba, result);
            Assert.assertTrue("El resultado no se reconoce como firma", signer.isSign(result)); //$NON-NLS-1$
          }
        }

    }

    /** Pruebas de contrafirma.
     * @throws Exception Cuando ocurre un error */
    @SuppressWarnings("static-method")
	@Test
    public void testCounterSign() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        final AOSigner signer = new AOXAdESSigner();

        final Properties p = new Properties();
        p.put("mode", "implicit"); //$NON-NLS-1$ //$NON-NLS-2$
        p.put("ignoreStyleSheets", "false"); //$NON-NLS-1$ //$NON-NLS-2$

        String prueba;

        for (final String algo : ALGOS) {
          for(final String filename : TEST_FILES_MULTISIGN) {

            prueba = "Contrafirma XAdES con el algoritmo '" + //$NON-NLS-1$
            algo + "' y el fichero '" + filename + "'"; //$NON-NLS-1$ //$NON-NLS-2$

            System.out.println();
            System.out.println(prueba);

            final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(filename));

            final byte[] result = signer.countersign(
        		data,
        		algo,
        		CounterSignTarget.LEAFS,
        		null,
        		pke.getPrivateKey(),
        		pke.getCertificateChain(),
        		p
    		);

            final File f = File.createTempFile(algo + "-" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
            final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
            fos.write(result);
            fos.flush(); fos.close();
            System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

            Assert.assertTrue("UnsignedProperties invalidas", isValidUnsignedProperties(new ByteArrayInputStream(result),null)); //$NON-NLS-1$

            Assert.assertNotNull(prueba, result);
            Assert.assertTrue("El resultado no se reconoce como firma", signer.isSign(result)); //$NON-NLS-1$
          }
        }

    }

    /**
     * Prueba con hoja de estilo externa.
     * <b>Necesita GUI</b>
     * @throws Exception Cuando ocurre un error
     */
    @SuppressWarnings("static-method")
	@Test
    @Ignore
    public void testSignExternalStyle() throws Exception {

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;
        final X509Certificate cert;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
        cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

        final AOSigner signer = new AOXAdESSigner();

        final Properties p1 = new Properties();
        p1.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_DETACHED); //$NON-NLS-1$
        p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        p1.setProperty("ignoreStyleSheets", "false"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyIdentifier", "urn:oid:2.16.724.1.3.1.1.2.1.8"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyIdentifierHash", "ESTEESUNHASH=");  //$NON-NLS-1$//$NON-NLS-2$
        p1.setProperty("policyIdentifierHashAlgorithm", DigestMethod.SHA1);         //$NON-NLS-1$
        p1.setProperty("policyDescription", "Politica de firma electronica para las Administraciones Publicas en Espana"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyQualifier", "http://blogs.adobe.com/security/91014620_eusig_wp_ue.pdf"); //$NON-NLS-1$ //$NON-NLS-2$

        String prueba;

        for (final String algo : ALGOS) {

            prueba = "Firma XAdES con hoja de estilo externa con el algoritmo ': " + //$NON-NLS-1$
            algo +
            "'"; //$NON-NLS-1$

            System.out.println();
            System.out.println(prueba);

            final String filename = "external-style.xml"; //$NON-NLS-1$

            final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(filename));

            final byte[] result = signer.sign(
        		data,
        		algo,
        		pke.getPrivateKey(),
        		pke.getCertificateChain(),
        		p1
    		);

            final File f = File.createTempFile(algo + "-" + p1.getProperty("mode") + "-" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
            final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
            fos.write(result);
            fos.flush(); fos.close();
            System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

//                    // Enviamos a validar a AFirma
//                    if (verifier != null) {
//                        Assert.assertTrue("Fallo al validar " + filename, verifier.verifyXML(result)); //$NON-NLS-1$
//                    }

            Assert.assertTrue(isValidUnsignedProperties(new ByteArrayInputStream(result),null));

            Assert.assertNotNull(prueba, result);
            Assert.assertTrue(signer.isSign(result));

            AOTreeModel tree = signer.getSignersStructure(result, false);
            Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$
            Assert.assertEquals("ANF Usuario Activo", ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject()); //$NON-NLS-1$

            tree = signer.getSignersStructure(result, true);
            Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$
            final AOSimpleSignInfo simpleSignInfo = (AOSimpleSignInfo) ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject();

            Assert.assertNotNull(simpleSignInfo.getSigningTime());
            Assert.assertEquals(cert, simpleSignInfo.getCerts()[0]);

        }

    }


    /**
     * Prueba de firma convencional.
     * @throws Exception en cualquier error
     */
    @SuppressWarnings("static-method")
	@Test
    public void testSignature() throws Exception {

//        TestSignVerifier verifier = null;
//        try {
//            verifier = new TestSignVerifier();
//        }
//        catch (Exception e) {
//            System.out.println("No se ha podido inicializar el validador de firmas, no se validaran como parte de las pruebas: " + e); //$NON-NLS-1$
//        }


        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;
        final X509Certificate cert;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
        cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

        final AOSigner signer = new AOXAdESSigner();

        String prueba;

        for (final Properties extraParams : XADES_MODES) {
            for (final String algo : ALGOS) {
                for (final String filename : TEST_FILES_DATA) {

                    // Omitimos la firma de binarios en modo enveloped
                    if ("XAdES Enveloped".equals(extraParams.getProperty("format")) && !filename.toLowerCase().endsWith(".xml")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                        break;
                    }

                    prueba = "Firma XAdES en modo '" +  //$NON-NLS-1$
                    extraParams.getProperty("mode") +  //$NON-NLS-1$
                    ", formato '" + //$NON-NLS-1$
                    extraParams.getProperty("format") +  //$NON-NLS-1$
                    "' con el algoritmo ': " + //$NON-NLS-1$
                    algo +
                    "' y el fichero '" + //$NON-NLS-1$
                    filename + "'"; //$NON-NLS-1$

                    System.out.println();
                    System.out.println(prueba);

                    final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(filename));

                    final byte[] result = signer.sign(
                		data,
                		algo,
                		pke.getPrivateKey(),
                		pke.getCertificateChain(),
                		extraParams
            		);

                    Assert.assertFalse("El XML contiene '&#13;'", new String(result).contains("&#13;")); //$NON-NLS-1$ //$NON-NLS-2$

                    File f = File.createTempFile(algo + "-" + extraParams.getProperty("mode") + "-" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
                    java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
                    fos.write(result);
                    fos.flush(); fos.close();
                    System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

//                    // Enviamos a validar a AFirma
//                    if (verifier != null) {
//                        Assert.assertTrue("Fallo al validar " + filename, verifier.verifyXML(result)); //$NON-NLS-1$
//                    }

                    Assert.assertTrue(isValidUnsignedProperties(new ByteArrayInputStream(result),null));

                    Assert.assertNotNull(prueba, result);
                    Assert.assertTrue(signer.isSign(result));

                    if ("implicit".equals(extraParams.getProperty("mode")) && !filename.toLowerCase().endsWith(".xml") && !Arrays.equals(signer.getData(result), data)) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                        f = File.createTempFile(algo + "-" + extraParams.getProperty("mode") + "-" + filename.replace(".xml", "") + "-", "-" + filename); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
                        fos = new java.io.FileOutputStream(f);
                        fos.write(signer.getData(result));
                        fos.flush(); fos.close();
                        System.out.println("Temporal de los datos extraidos para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
                        Assert.fail("Los datos extraidos no coinciden con los originales: " + filename); //$NON-NLS-1$
                    }

                    AOTreeModel tree = signer.getSignersStructure(result, false);
                    Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$
                    Assert.assertEquals("ANF Usuario Activo", ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject()); //$NON-NLS-1$

                    tree = signer.getSignersStructure(result, true);
                    Assert.assertEquals("Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$
                    final AOSimpleSignInfo simpleSignInfo = (AOSimpleSignInfo) ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject();

                    Assert.assertNotNull(simpleSignInfo.getSigningTime());
                    Assert.assertEquals(cert, simpleSignInfo.getCerts()[0]);

                }
            }
        }

    }

    /**
     * Comprueba que el nodo UnsignedSignatureProperties (en caso de aparecer)
     * de la firma XAdES contiene atributos. Busca el nodo con el namespace
     * indicado.
     * @param sign Firma.
     * @param namespace Espacio de nombres a utilizar.
     * @return {@code false} si se encuentra el nodo UnsignedSignatureProperties
     * vac&iacute;o, {@code true} en caso contrario.
     */
    private static boolean isValidUnsignedProperties(final InputStream sign, final String namespace) {

        final Document document;
        try {
            document = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(sign);
        }
        catch (final Exception e) {
            System.out.println("No es una firma valida"); //$NON-NLS-1$
            return false;
        }

        final String xadesNamespace = namespace != null ? namespace : Utils.guessXAdESNamespaceURL(document.getFirstChild());

        final NodeList upNodes = document.getElementsByTagName(xadesNamespace + ":UnsignedProperties"); //$NON-NLS-1$
        for (int i = 0; i < upNodes.getLength(); i++) {
            final NodeList uspNodes = upNodes.item(i).getChildNodes();
            for (int j = 0; j < uspNodes.getLength(); j++) {
                if (uspNodes.item(i).getNodeName().equals(xadesNamespace + ":UnsignedSignatureProperties")) { //$NON-NLS-1$
                    if (uspNodes.item(i).getChildNodes().getLength() == 0) {
                        return false;
                    }
                    break;
                }
            }
        }

        return true;
    }

    /** Prueba de detecci&oacute;n de formato XAdES.
     * @throws Exception Cuando se produce un error durante la prueba.
     */
    @SuppressWarnings("static-method")
	@Test
    public void TestDetection() throws Exception {
    	final String[] files = new String[] { "firmaIgae.xsig.xml" }; //$NON-NLS-1$
    	final AOSigner signer = new AOXAdESSigner();
    	for (final String f : files) {
    		Assert.assertTrue("La firma " + f + " no se reconoce como XAdES", signer.isSign(AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(f)))); //$NON-NLS-1$ //$NON-NLS-2$
    	}
    }

}

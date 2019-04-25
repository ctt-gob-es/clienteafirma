/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.test.xmldsig;

import java.io.File;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.xmldsig.AOXMLDSigSigner;

/** Pruebas del m&oacute;dulo XMLDSig.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestXMLdSig {

    private static final String CERT_PATH = "PFActivoFirSHA256.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "fisico activo prueba"; //$NON-NLS-1$

    private static final Properties[] XMLDSIG_CONFIGS;

    private static final Properties ENVELOPED_WITHOUT_XPATH_EXTRAPARAMS = new Properties();
    static {
	    ENVELOPED_WITHOUT_XPATH_EXTRAPARAMS.setProperty("format", AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED); //$NON-NLS-1$
	    ENVELOPED_WITHOUT_XPATH_EXTRAPARAMS.setProperty("avoidXpathExtraTransformsOnEnveloped", "true"); //$NON-NLS-1$ //$NON-NLS-2$
	    ENVELOPED_WITHOUT_XPATH_EXTRAPARAMS.setProperty("canonicalizationAlgorithm", "none"); //$NON-NLS-1$ //$NON-NLS-2$
    }

    static {

        final Properties p1 = new Properties();
        p1.setProperty("format", AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED); //$NON-NLS-1$
        p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$

        final Properties p2 = new Properties();
        p2.setProperty("format", AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED); //$NON-NLS-1$
        p2.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$

        final Properties p3 = new Properties();
        p3.setProperty("format", AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED); //$NON-NLS-1$
        p3.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$

        final Properties p4 = new Properties();
        p4.setProperty("format", AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING); //$NON-NLS-1$
        p4.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$

        final Properties p5 = new Properties();
        p5.setProperty("format", AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING); //$NON-NLS-1$
        p5.setProperty("mode", AOSignConstants.SIGN_MODE_EXPLICIT); //$NON-NLS-1$

        final Properties p6 = new Properties();
        p6.setProperty("format", AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING); //$NON-NLS-1$
        p6.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        p6.setProperty("encoding", "Base64"); //$NON-NLS-1$ //$NON-NLS-2$


        XMLDSIG_CONFIGS = new Properties[] {
                p1, p2, p3, p4, p5, p6
        };
    }

    /** Algoritmos de firma a probar. */
    private final static String[] ALGOS = new String[] {
            AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
//            AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
//            AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA
    };

    // IMPORTANTE: Poner extension ".xml" a los ficheros de prueba con contenido XML
    private static final String[] TEST_FILES_DATA = new String[] {
            "PFActivoFirSHA256.pfx", //$NON-NLS-1$
            "base64.b64", //$NON-NLS-1$
            "sample-class-attributes.xml", //$NON-NLS-1$
            //"xmlwithremotestyle.xml" //$NON-NLS-1$
    };

    /** Prueba de firma Enveloped sin transformaci&oacute;n XPath y sin <i>canonicalizaci&oacute;n</i>.
     * @throws Exception En cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testEvelopedWithoutXpath() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;
        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$

        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
        final AOSigner signer = new AOXMLDSigSigner();

        final byte[] data = AOUtil.getDataFromInputStream(
    		ClassLoader.getSystemResourceAsStream("sample-encoding-UTF-8.xml") //$NON-NLS-1$
		);

        final byte[] result = signer.sign(
    		data,
    		"SHA512withRSA", //$NON-NLS-1$
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		ENVELOPED_WITHOUT_XPATH_EXTRAPARAMS
		);

        final File f = File.createTempFile("XmlDSig-EnvelopedWithoutXpath-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$
        try (
    		final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
        ) {
    		fos.write(result);
    		fos.flush();
        }
        System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
	}

    /** Prueba de firma convencional.
     * @throws Exception en cualquier error. */
    @SuppressWarnings("static-method")
	@Test
    public void testSignature() throws Exception {

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;
        final X509Certificate cert;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
        cert = (X509Certificate) ks.getCertificate(CERT_ALIAS);

        final AOSigner signer = new AOXMLDSigSigner();

        String prueba;

        for (final Properties extraParams : XMLDSIG_CONFIGS) {
            for (final String algo : ALGOS) {
                for (final String filename : TEST_FILES_DATA) {

                    // Omitimos la firma de binarios en modo enveloped
                    if ("XMLdSig Enveloped".equalsIgnoreCase(extraParams.getProperty("format")) && !filename.toLowerCase().endsWith(".xml")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                    	continue;
                    }

                    prueba = "Firma XMLdSig en modo '" +  //$NON-NLS-1$
                    extraParams.getProperty("mode") +  //$NON-NLS-1$
                    ", formato '" + //$NON-NLS-1$
                    extraParams.getProperty("format") +  //$NON-NLS-1$
                    "' con el algoritmo ': " + //$NON-NLS-1$
                    algo +
                    "' y el fichero '" + //$NON-NLS-1$
                    filename + "'"; //$NON-NLS-1$

                    System.out.println();
                    System.out.println(prueba);

                    final byte[] data = AOUtil.getDataFromInputStream(TestXMLdSig.class.getResourceAsStream("/" + filename)); //$NON-NLS-1$

                    final byte[] result = signer.sign(
                		data,
                		algo,
                		pke.getPrivateKey(),
                		pke.getCertificateChain(),
                		extraParams
            		);

                    File f = File.createTempFile("Sign-XMLdSig-" + algo + "-" + extraParams.getProperty("mode") + "-" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
                    try (
                		java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
                    ) {
                    	fos.write(result);
                    	fos.flush();
                    }
                    System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

                    Assert.assertNotNull(prueba, result);
                    Assert.assertTrue("El signer XMLdSig no reconoce como una firma los datos generados", signer.isSign(result)); //$NON-NLS-1$

                    if ("implicit".equals(extraParams.getProperty("mode")) && !filename.toLowerCase().endsWith(".xml")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

                    	boolean extractionProblem;
                    	if (filename.toLowerCase().endsWith(".b64")) { //$NON-NLS-1$
                    		extractionProblem = !Arrays.equals(signer.getData(result), Base64.decode(new String(data)));
                    	}
                    	else {
                    		extractionProblem = !Arrays.equals(signer.getData(result), data);
                    	}

                    	if (extractionProblem) {
                    		f = File.createTempFile(algo + "-" + extraParams.getProperty("mode") + "-" + filename.replace(".xml", "") + "-", "-" + filename); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
                    		try (
                				final OutputStream fos = new java.io.FileOutputStream(f);
            				) {
                    			fos.write(signer.getData(result));
                    			fos.flush();
                    		}
                    		System.out.println("Temporal de los datos extraidos para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$
                    		Assert.fail("Los datos extraidos no coinciden con los originales: " + filename); //$NON-NLS-1$
                    	}
                    }

                    AOTreeModel tree = signer.getSignersStructure(result, false);
                    Assert.assertEquals("Los datos del nodo raiz no son los esperados", "Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$ //$NON-NLS-2$
                    Assert.assertEquals("El firmante encontrado en la firma no es el esperado", "FISICO ACTIVO PRUEBA", ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject()); //$NON-NLS-1$ //$NON-NLS-2$

                    tree = signer.getSignersStructure(result, true);
                    Assert.assertEquals("Los datos del nodo raiz no son los esperados", "Datos", ((AOTreeNode) tree.getRoot()).getUserObject()); //$NON-NLS-1$ //$NON-NLS-2$
                    final AOSimpleSignInfo simpleSignInfo = (AOSimpleSignInfo) ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject();

                    Assert.assertEquals("El certificado del firmante encontrado en la firma no es el esperado", cert, simpleSignInfo.getCerts()[0]); //$NON-NLS-1$
                }
            }
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

        final AOSigner signer = new AOXMLDSigSigner();

        String prueba;
        for (final Properties extraParams : XMLDSIG_CONFIGS) {
        	for (final String algo : ALGOS) {
        		for(final String filename : TEST_FILES_DATA) {

        			if ("XMLdSig Enveloped".equalsIgnoreCase(extraParams.getProperty("format")) && !filename.toLowerCase().endsWith(".xml")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        				continue;
        			}

        			prueba = "Firma y cofirma XMLdSig en modo '" +  //$NON-NLS-1$
                    extraParams.getProperty("mode") +  //$NON-NLS-1$
                    ", formato '" + //$NON-NLS-1$
                    extraParams.getProperty("format") +  //$NON-NLS-1$
                    "' con el algoritmo ': " + //$NON-NLS-1$
                    algo +
                    "' y el fichero '" + //$NON-NLS-1$
                    filename + "'"; //$NON-NLS-1$

        			System.out.println();
        			System.out.println(prueba);

        			final byte[] data = AOUtil.getDataFromInputStream(TestXMLdSig.class.getResourceAsStream("/" + filename)); //$NON-NLS-1$

        			final byte[] signature = signer.sign(
    					data,
    					algo,
    					pke.getPrivateKey(),
    					pke.getCertificateChain(),
    					extraParams
					);

        			final byte[] result = signer.cosign(
    					data,
    					signature,
    					algo,
    					pke.getPrivateKey(),
    					pke.getCertificateChain(),
    					extraParams
					);

        			final File f = File.createTempFile("Cosign-XMLdSig-" + algo + "-" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
        			try (
    					final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
        			) {
        				fos.write(result);
        				fos.flush();
        			}
        			System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

        			Assert.assertNotNull(prueba, result);
        			Assert.assertTrue("El resultado no se reconoce como firma", signer.isSign(result)); //$NON-NLS-1$
        		}
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

        final AOSigner signer = new AOXMLDSigSigner();

        String prueba;
        for (final Properties extraParams : XMLDSIG_CONFIGS) {
        	for (final String algo : ALGOS) {
        		for(final String filename : TEST_FILES_DATA) {

        			if ("XMLdSig Enveloped".equalsIgnoreCase(extraParams.getProperty("format")) && !filename.toLowerCase().endsWith(".xml")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        				continue;
        			}

        			prueba = "Firma y contrafirma XMLdSig en modo '" +  //$NON-NLS-1$
                    extraParams.getProperty("mode") +  //$NON-NLS-1$
                    ", formato '" + //$NON-NLS-1$
                    extraParams.getProperty("format") +  //$NON-NLS-1$
                    "' con el algoritmo ': " + //$NON-NLS-1$
                    algo +
                    "' y el fichero '" + //$NON-NLS-1$
                    filename + "'"; //$NON-NLS-1$

        			System.out.println();
        			System.out.println(prueba);

        			final byte[] data = AOUtil.getDataFromInputStream(TestXMLdSig.class.getResourceAsStream("/" + filename)); //$NON-NLS-1$

        			final byte[] signature = signer.sign(
    					data,
    					algo,
    					pke.getPrivateKey(),
    					pke.getCertificateChain(),
    					extraParams
					);

        			final byte[] cosignature = signer.cosign(
    					data,
    					signature,
    					algo,
    					pke.getPrivateKey(),
    					pke.getCertificateChain(),
    					extraParams
					);

        			final byte[] result = signer.countersign(
    					cosignature,
    					algo,
    					CounterSignTarget.LEAFS,
    					null,
    					pke.getPrivateKey(),
    					pke.getCertificateChain(),
    					extraParams
					);

        			final File f = File.createTempFile("Countersign-XMLdSig-" + algo + "-" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
        			try (
    					final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
        			) {
	        			fos.write(result);
	        			fos.flush();
        			}
        			System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

        			Assert.assertNotNull(prueba, result);
        			Assert.assertTrue("El resultado no se reconoce como firma", signer.isSign(result)); //$NON-NLS-1$
        		}
        	}
        }
    }
}

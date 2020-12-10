/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.xades;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Assert;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.signers.xml.Utils;



/**
 * Pruebas del m&oacute;dulo XAdES de Afirma.
 * @author Carlos Gamuci
 *
 */
public final class Test_Ticket_209544_OnlySignningCert {

    private static final String CERT_PATH = "ANF_con cadena_certificacion.jks"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    private static final Properties[] XADES_MODES;

    static {
        final Properties p1 = new Properties();
        p1.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_DETACHED); //$NON-NLS-1$
        p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        p1.setProperty("includeOnlySignningCertificate", "true"); //$NON-NLS-1$ //$NON-NLS-2$

        final Properties p2 = new Properties();
        p2.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING); //$NON-NLS-1$
        p2.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        p2.setProperty("includeOnlySignningCertificate", "true"); //$NON-NLS-1$ //$NON-NLS-2$

        final Properties p3 = new Properties();
        p3.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED); //$NON-NLS-1$
        p3.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        p3.setProperty("includeOnlySignningCertificate", "true"); //$NON-NLS-1$ //$NON-NLS-2$

        XADES_MODES = new Properties[] {
                p1, p2, p3
        };
    }

    /** Algoritmos de firma a probar. */
    private final static String[] ALGOS = new String[] {
            AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA
    };

    // IMPORTANTE: Poner extension ".xml" a los ficheros de prueba con contenido XML
    private static final String[] TEST_FILES_DATA = new String[] {
            "sample-encoding-UTF-8.xml" //$NON-NLS-1$
    };

    /**
     * Prueba de firma convencional.
     * @throws Exception en cualquier error
     */
    @SuppressWarnings("static-method")
	@Test
    public void testSignatureWithIncludeOnlySignningCertProperty() throws Exception {

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;
        final KeyStore ks = KeyStore.getInstance("JKS"); //$NON-NLS-1$

        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
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

                    Assert.assertNotNull("Error al generar la firma: " + prueba, result); //$NON-NLS-1$
                    Assert.assertTrue(prueba + ", no se reconoce como una firma", signer.isSign(result)); //$NON-NLS-1$

                    final File f = File.createTempFile(algo + "-" + extraParams.getProperty("mode") + "-" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
                    try (
                		final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
            		) {
                    	fos.write(result);
                    	fos.flush();
                    }
                    System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

                    final Document doc = Utils.getNewDocumentBuilder().parse(
                    		new ByteArrayInputStream(result));

                    final NodeList nl = doc.getElementsByTagNameNS(es.gob.afirma.signers.xml.XMLConstants.DSIGNNS, "X509Data"); //$NON-NLS-1$
                    Assert.assertTrue("No se han encontrado nodos X509Data en el resultado", nl.getLength() > 0); //$NON-NLS-1$
                    for (int i = 0; i < nl.getLength(); i++) {
                    	int x509CertNodesCount = 0;
                    	final NodeList x509ChildsList = nl.item(i).getChildNodes();

                    	for (int j = 0; j < x509ChildsList.getLength(); j++) {
                    		if (x509ChildsList.item(j).getNodeName().endsWith(":X509Certificate")) { //$NON-NLS-1$
                    			x509CertNodesCount++;
                    		}
                    	}
                    	Assert.assertTrue("Se ha encontrado una cadena de certificacion dentro de una firma", x509CertNodesCount <= 1); //$NON-NLS-1$
                    }
                }
            }
        }
    }

    /** Pruebas de cofirma.
     * @throws Exception Cuando ocurre un error */
    @SuppressWarnings("static-method")
	@Test
    public void testCoSignWithIncludeOnlySignningCertProperty() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;
        final KeyStore ks = KeyStore.getInstance("JKS"); //$NON-NLS-1$

        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
        final AOSigner signer = new AOXAdESSigner();

        String prueba;

        for (final Properties extraParams : XADES_MODES) {
            for (final String algo : ALGOS) {
                for (final String filename : TEST_FILES_DATA) {

                    // Omitimos la firma de binarios en modo enveloped
                    if ("XAdES Enveloped".equals(extraParams.getProperty("format")) && !filename.toLowerCase().endsWith(".xml")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                        break;
                    }

                    prueba = "Cofirma XAdES en modo '" +  //$NON-NLS-1$
                    extraParams.getProperty("mode") +  //$NON-NLS-1$
                    ", formato '" + //$NON-NLS-1$
                    extraParams.getProperty("format") +  //$NON-NLS-1$
                    "' con el algoritmo ': " + //$NON-NLS-1$
                    algo +
                    "' y el fichero '" + //$NON-NLS-1$
                    filename + "', sin incluir la cadena de certificacion"; //$NON-NLS-1$

                    System.out.println();
                    System.out.println(prueba);

                    final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(filename));

                    final byte[] signResult = signer.sign(
                		data,
                		algo,
                		pke.getPrivateKey(),
                		pke.getCertificateChain(),
                		extraParams
            		);

                    Assert.assertNotNull(prueba, signResult);
                    Assert.assertTrue(signer.isSign(signResult));

                    final byte[] cosignResult = signer.cosign(
                		signResult,
                		algo,
                		pke.getPrivateKey(),
                		pke.getCertificateChain(),
                		extraParams
            		);

                    Assert.assertNotNull(prueba, cosignResult);
                    Assert.assertTrue(signer.isSign(cosignResult));

                    final File f = File.createTempFile(algo + "-" + extraParams.getProperty("mode") + "-" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
                    try (
                		final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
            		) {
                    	fos.write(cosignResult);
                    	fos.flush();
                	}

                    System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

                    final Document doc = Utils.getNewDocumentBuilder().parse(
                    		new ByteArrayInputStream(cosignResult));

                    final NodeList nl = doc.getElementsByTagNameNS(es.gob.afirma.signers.xml.XMLConstants.DSIGNNS, "X509Data"); //$NON-NLS-1$
                    Assert.assertTrue("No se han encontrado nodos X509Data en el resultado", nl.getLength() > 0); //$NON-NLS-1$
                    for (int i = 0; i < nl.getLength(); i++) {
                    	int x509CertNodesCount = 0;
                    	final NodeList x509ChildsList = nl.item(i).getChildNodes();

                    	for (int j = 0; j < x509ChildsList.getLength(); j++) {
                    		if (x509ChildsList.item(j).getNodeName().endsWith(":X509Certificate")) { //$NON-NLS-1$
                    			x509CertNodesCount++;
                    		}
                    	}
                    	Assert.assertTrue("Se ha encontrado una cadena de certificacion dentro de una cofirma", x509CertNodesCount <= 1); //$NON-NLS-1$
                    }
                }
            }
        }
    }

    /** Pruebas de contrafirma.
     * @throws Exception Cuando ocurre un error */
    @SuppressWarnings("static-method")
	@Test
    public void testCounterSignWithIncludeOnlySignningCertProperty() throws Exception {
        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;
        final KeyStore ks = KeyStore.getInstance("JKS"); //$NON-NLS-1$

        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));
        final AOSigner signer = new AOXAdESSigner();

        String prueba;

        for (final Properties extraParams : XADES_MODES) {
            for (final String algo : ALGOS) {
                for (final String filename : TEST_FILES_DATA) {

                    // Omitimos la firma de binarios en modo enveloped
                    if ("XAdES Enveloped".equals(extraParams.getProperty("format")) && !filename.toLowerCase().endsWith(".xml")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                        break;
                    }

                    prueba = "Contrafirma XAdES en modo '" +  //$NON-NLS-1$
                    extraParams.getProperty("mode") +  //$NON-NLS-1$
                    ", formato '" + //$NON-NLS-1$
                    extraParams.getProperty("format") +  //$NON-NLS-1$
                    "' con el algoritmo ': " + //$NON-NLS-1$
                    algo +
                    "' y el fichero '" + //$NON-NLS-1$
                    filename + "', sin incluir la cadena de certificacion"; //$NON-NLS-1$

                    System.out.println();
                    System.out.println(prueba);

                    final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(filename));

                    final byte[] signResult = signer.sign(
                		data,
                		algo,
                		pke.getPrivateKey(),
                		pke.getCertificateChain(),
                		extraParams
            		);

                    Assert.assertNotNull(prueba, signResult);
                    Assert.assertTrue(signer.isSign(signResult));

                    final byte[] countersignResult = signer.countersign(
                		signResult,
                		algo,
                		CounterSignTarget.TREE,
                		null,
                		pke.getPrivateKey(),
                		pke.getCertificateChain(),
                		extraParams
            		);

                    Assert.assertNotNull(prueba, countersignResult);
                    Assert.assertTrue(signer.isSign(countersignResult));

                    final File f = File.createTempFile(algo + "-" + extraParams.getProperty("mode") + "-" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
                    try (
                		final java.io.FileOutputStream fos = new java.io.FileOutputStream(f);
            		) {
                    	fos.write(countersignResult);
                    	fos.flush();
                    }
                    System.out.println("Temporal para comprobacion manual: " + f.getAbsolutePath()); //$NON-NLS-1$

                    final Document doc = Utils.getNewDocumentBuilder().parse(
                    		new ByteArrayInputStream(countersignResult));

                    final NodeList nl = doc.getElementsByTagNameNS(es.gob.afirma.signers.xml.XMLConstants.DSIGNNS, "X509Data"); //$NON-NLS-1$
                    Assert.assertTrue("No se han encontrado nodos X509Data en el resultado", nl.getLength() > 0); //$NON-NLS-1$
                    for (int i = 0; i < nl.getLength(); i++) {
                    	int x509CertNodesCount = 0;
                    	final NodeList x509ChildsList = nl.item(i).getChildNodes();

                    	for (int j = 0; j < x509ChildsList.getLength(); j++) {
                    		if (x509ChildsList.item(j).getNodeName().endsWith(":X509Certificate")) { //$NON-NLS-1$
                    			x509CertNodesCount++;
                    		}
                    	}
                    	Assert.assertTrue("Se ha encontrado una cadena de certificacion dentro de una contrafirma", x509CertNodesCount <= 1); //$NON-NLS-1$
                    }
                }
            }
        }
    }
}

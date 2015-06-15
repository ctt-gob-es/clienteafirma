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
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilderFactory;

import org.junit.Assert;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSimpleSignInfo;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.core.util.tree.AOTreeNode;
import es.gob.afirma.signers.xml.Utils;

/**
 * Pruebas del m&oacute;dulo XAdES de Afirma para facturas electr&oacute;nicas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestFacturaE {

    private static final String CERT_PATH = "PFActivoFirSHA256.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "fisico activo prueba"; //$NON-NLS-1$

    private static final Properties[] XADES_MODES;

    static {
        final Properties p1 = new Properties();
//        p1.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED); //$NON-NLS-1$

        XADES_MODES = new Properties[] {
                p1
        };
    }

    /** Algoritmos de firma a probar. */
    private final static String[] ALGOS = new String[] {
            AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
//            AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
//            AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
//            AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA
    };

    // IMPORTANTE: Poner extension ".xml" a los ficheros de prueba con contenido XML
    private static final String[] TEST_FILES_DATA = new String[] {
            "facturae_32v1.xml", //$NON-NLS-1$
    		"factura_sinFirmar.xml" //$NON-NLS-1$
    };

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

        final AOSigner signer = new AOFacturaESigner();

        String prueba;

        for (final Properties extraParams : XADES_MODES) {
            for (final String algo : ALGOS) {
                for (final String filename : TEST_FILES_DATA) {

                    prueba = "Factura electronica con el algoritmo ' " + //$NON-NLS-1$
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

                    final File f = File.createTempFile("Factura_firmada_" + filename.replace(".xml", "") + "-", ".xml"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
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
                    Assert.assertEquals("FISICO ACTIVO PRUEBA", ((AOTreeNode) tree.getRoot()).getChildAt(0).getUserObject()); //$NON-NLS-1$

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
            document = DocumentBuilderFactory.newInstance().
            newDocumentBuilder().parse(sign);
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

//    private byte[] canonicalize(final byte[] in) {
//        return in;
//        org.apache.xml.security.Init.init();
//        try {
//            return Canonicalizer.getInstance(Canonicalizer.ALGO_ID_C14N11_WITH_COMMENTS).canonicalize(in);
//        }
//        catch (final Exception e) {
//            System.err.println(e.toString());
//            return in;
//        }
//    }
}

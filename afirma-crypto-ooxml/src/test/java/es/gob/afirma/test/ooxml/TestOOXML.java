/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.test.ooxml;

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

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.signers.ooxml.AOOOXMLSigner;

/** Pruebas del m&oacute;dulo OOXML de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestOOXML {

    private static final String CERT_PATH3 = "PFActivoFirSHA1.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS3 = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS3 = "fisico activo prueba"; //$NON-NLS-1$

    private static final String[] DATA_PATHS = new String[] { "entrada_w2013.docx", "Entrada.docx" };  //$NON-NLS-1$ //$NON-NLS-2$
    private static byte[][] DATAS = new byte[2][];

    private static final Properties[] OOXML_MODES;

    static {

    	try {
			DATAS[0] = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(DATA_PATHS[0]));
			DATAS[1] = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(DATA_PATHS[1]));
		}
    	catch (final Exception e) {
			System.err.println("No se pudo cargar el documento de pruebas"); //$NON-NLS-1$
			DATAS[0] = "Error0".getBytes(); //$NON-NLS-1$
			DATAS[1] = "Error1".getBytes(); //$NON-NLS-1$
		}

        final Properties p1 = new Properties();
        p1.setProperty("format", AOSignConstants.SIGN_FORMAT_OOXML); //$NON-NLS-1$
        p1.setProperty("signatureReason", "Comentario : Razon de firma"); //$NON-NLS-1$ //$NON-NLS-2$

        OOXML_MODES = new Properties[] {
            p1
        };
    }

    /** Algoritmos de firma a probar. */
    private final static String[] ALGOS = new String[] {
        AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA
    };

    /** Prueba de reconocimiento de formato. */
    @SuppressWarnings("static-method")
	@Test
    public void TestFormatDetection() {
    	for (final byte[] data : DATAS) {
    		Assert.assertTrue(new AOOOXMLSigner().isValidDataFile(data));
    	}
    }

    /** Prueba de firma convencional. */
    @SuppressWarnings("static-method")
	@Test
    public void testGetSignersStructure() {
    	new AOOOXMLSigner().getSignersStructure(DATAS[0], false);
    }

    /** Prueba de firma convencional.
     * @throws Exception en cualquier error */
    @SuppressWarnings("static-method")
	@Test
    public void testSignature() throws Exception {

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH3), CERT_PASS3.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS3, new KeyStore.PasswordProtection(CERT_PASS3.toCharArray()));

        final AOSigner signer = new AOOOXMLSigner();

        String prueba;
        for (final byte[] data : DATAS) {
	        for (final Properties extraParams : OOXML_MODES) {
	            for (final String algo : ALGOS) {

	                prueba = "Firma OOXML con el algoritmo ': " + //$NON-NLS-1$
	                algo +
	                "' y politica '" + //$NON-NLS-1$
	                extraParams.getProperty("policyIdentifier") + //$NON-NLS-1$
	                "'"; //$NON-NLS-1$

	                System.out.println(prueba);

	                final byte[] result = signer.sign(
	            		data,
	            		algo,
	            		pke.getPrivateKey(),
	            		pke.getCertificateChain(),
	            		extraParams
	        		);

	                final File saveFile = File.createTempFile(algo + "-", ".docx"); //$NON-NLS-1$ //$NON-NLS-2$
	                final OutputStream os = new FileOutputStream(saveFile);
	                os.write(result);
	                os.flush();
	                os.close();
	                System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

	                Assert.assertNotNull(prueba, result);
	                Assert.assertTrue(signer.isSign(result));

	            }
	        }

	        signer.sign(
	    		data,
	    		"SHA1withRSA", //$NON-NLS-1$
	    		pke.getPrivateKey(),
	    		pke.getCertificateChain(),
	    		null
			);
        }

    }

    /** Prueba de cofirma.
     * @throws Exception en cualquier error */
    @SuppressWarnings("static-method")
	@Test
    public void testCoSignature() throws Exception {

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke3 = loadKeyEntry(CERT_PATH3, CERT_ALIAS3, CERT_PASS3);

        final AOSigner signer = new AOOOXMLSigner();

        String prueba;

        for (final byte[] data : DATAS) {
	        for (final Properties extraParams : OOXML_MODES) {
	            for (final String algo : ALGOS) {

	                prueba = "Cofirma OOXML con el algoritmo ': " + //$NON-NLS-1$
	                algo +
	                "'"; //$NON-NLS-1$

	                System.out.println(prueba);

	                // Firma simple
	                final byte[] sign1 = sign(signer, data, algo, pke3, extraParams);

	                // Cofirma sin indicar los datos
	                final byte[] sign2 = cosign(signer, sign1, algo, pke3, extraParams);

	                //checkSign(signer, sign2, prueba);

	                final OutputStream fos = new FileOutputStream(File.createTempFile("OOXML_", ".docx")); //$NON-NLS-1$ //$NON-NLS-2$
	                fos.write(sign2);
	                fos.flush();
	                fos.close();

	                // Cofirma indicando los datos
	                final byte[] sign3 = cosign(signer, data, sign2, algo, pke3, extraParams);
	                Assert.assertNotNull(sign3);

	            }
	        }

	        signer.sign(
	    		data,
	    		"SHA1withRSA", //$NON-NLS-1$
	    		pke3.getPrivateKey(),
	    		pke3.getCertificateChain(),
	    		null
			);
        }

    }

    /** Carga la clave privada un certificado de un almac&eacute;n en disco.
     * @param pkcs12File Fichero P12/PFX.
     * @param alias Alias del certificado.
     * @param password Contrase&ntilde;a.
     * @return Clave privada del certificado. */
    private static PrivateKeyEntry loadKeyEntry(final String pkcs12File, final String alias, final String password) throws Exception {
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(pkcs12File), password.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(alias, new KeyStore.PasswordProtection(password.toCharArray()));

        return pke;
    }

    private static byte[] sign(final AOSigner signer, final byte[] data, final String algorithm, final PrivateKeyEntry pke, final Properties params) throws Exception {
        return signer.sign(
    		data,
    		algorithm,
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		params
		);
    }

    /** Cofirma sin necesidad de los datos originales. */
    private static byte[] cosign(final AOSigner signer, final byte[] sign, final String algorithm, final PrivateKeyEntry pke, final Properties params) throws Exception {
        return signer.cosign(
    		sign,
    		algorithm,
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		params
		);
    }

    private static byte[] cosign(final AOSigner signer, final byte[] data, final byte[] sign, final String algorithm, final PrivateKeyEntry pke, final Properties params) throws Exception {
        return signer.cosign(
    		data,
    		sign,
    		algorithm,
    		pke.getPrivateKey(),
    		pke.getCertificateChain(),
    		params
		);
    }
}

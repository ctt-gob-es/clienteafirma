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

import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.crypto.dsig.DigestMethod;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;

/**
 * Pruebas del m&oacute;dulo XAdES de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 *
 */
public final class TestXAdESPolicyJava7 {

    private static final String CERT_PATH = "PFActivoFirSHA256.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "fisico activo prueba"; //$NON-NLS-1$

    private static final Properties XADES_CONFIG;

    static {
    	XADES_CONFIG = new Properties();
    	XADES_CONFIG.setProperty("format", AOSignConstants.SIGN_FORMAT_XADES_DETACHED); //$NON-NLS-1$
    	XADES_CONFIG.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
    	XADES_CONFIG.setProperty("policyIdentifier", "urn:oid:2.16.724.1.3.1.1.2.1.8"); //$NON-NLS-1$ //$NON-NLS-2$
        XADES_CONFIG.setProperty("policyIdentifierHash", "ESTEESUNHASH=");  //$NON-NLS-1$//$NON-NLS-2$
    	XADES_CONFIG.setProperty("policyIdentifierHashAlgorithm", DigestMethod.SHA1);         //$NON-NLS-1$
    	XADES_CONFIG.setProperty("policyDescription", "Politica de firma electronica para las Administraciones Publicas en Espana"); //$NON-NLS-1$ //$NON-NLS-2$
    	XADES_CONFIG.setProperty("policyQualifier", "http://blogs.adobe.com/security/91014620_eusig_wp_ue.pdf"); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /** Algoritmos de firma a probar. */
    private final static String SIGN_ALGO = AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA;

    // IMPORTANTE: Poner extension ".xml" a los ficheros de prueba con contenido XML
    private static final String TEST_FILE_DATA = "TEST_PDF_Certified.pdf"; //$NON-NLS-1$

    /**
     * Prueba de firma convencional.
     * @throws Exception en cualquier error
     */
    @SuppressWarnings("static-method")
	@Test
	public void testSignature() throws Exception {

    	Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
    	final PrivateKeyEntry pke;

    	final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
    	ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
    	pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

    	final byte[] data = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE_DATA));

    	try {
    		new AOXAdESSigner().sign(
				data,
				SIGN_ALGO,
				pke.getPrivateKey(),
				pke.getCertificateChain(),
				XADES_CONFIG
			);
    	}
    	catch (final Exception e) {
    		Assert.fail("Ocurrio un error durante la generacion de la firma: " + e); //$NON-NLS-1$
    	}
    }
}

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
import java.util.Enumeration;
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
public final class TestOOXMLVersions {

    private static final String CERT_PATH3 = "ANCERTCCP_FIRMA.p12"; //$NON-NLS-1$
    private static final String CERT_PASS3 = "1111"; //$NON-NLS-1$
    private static final String CERT_ALIAS3 = "juan ejemplo espa\u00F1ol"; //$NON-NLS-1$

    private static final String[] DATA_PATHS = new String[] {
		"Documento_nuevo_creado_en_Word_2010.docx", //$NON-NLS-1$
		"Documento_nuevo_creado_en_Word_2013.docx", //$NON-NLS-1$
		"Documento_nuevo_creado_en_Word_2016.docx" //$NON-NLS-1$
	};
    private static byte[][] DATAS = new byte[3][];
    private static final Properties P1;

    static {

    	try {
			DATAS[0] = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(DATA_PATHS[0]));
			DATAS[1] = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(DATA_PATHS[1]));
			DATAS[2] = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(DATA_PATHS[2]));
		}
    	catch (final Exception e) {
			System.err.println("No se pudo cargar el documento de pruebas: " + e); //$NON-NLS-1$
			DATAS[0] = "Error0".getBytes(); //$NON-NLS-1$
			DATAS[1] = "Error1".getBytes(); //$NON-NLS-1$
			DATAS[2] = "Error2".getBytes(); //$NON-NLS-1$
		}

        P1 = new Properties();
        P1.setProperty("format", AOSignConstants.SIGN_FORMAT_OOXML); //$NON-NLS-1$
        P1.setProperty("signatureReason", "Comentario: Razon de firma"); //$NON-NLS-1$ //$NON-NLS-2$
	    P1.setProperty("commitmentTypeIndications", "1"); //$NON-NLS-1$ //$NON-NLS-2$
	    P1.setProperty("commitmentTypeIndication0Identifier", "5"); //$NON-NLS-1$ //$NON-NLS-2$
	    P1.setProperty("commitmentTypeIndication0Description", "Aprob\u00F3 este documento"); //$NON-NLS-1$ //$NON-NLS-2$
	    P1.setProperty("commitmentTypeIndication0CommitmentTypeQualifiers", "RAZON-PRUEBA"); //$NON-NLS-1$ //$NON-NLS-2$
	    P1.setProperty("signatureComments", "RAZON-PRUEBA"); //$NON-NLS-1$ //$NON-NLS-2$
	    P1.setProperty("signatureProductionCity", "Ciudad 1"); //$NON-NLS-1$ //$NON-NLS-2$
	    P1.setProperty("signatureProductionProvince", "Provincia 1"); //$NON-NLS-1$ //$NON-NLS-2$
	    P1.setProperty("signatureProductionPostalCode", "CPostal 1"); //$NON-NLS-1$ //$NON-NLS-2$
	    P1.setProperty("signatureProductionCountry", "Pais 1"); //$NON-NLS-1$ //$NON-NLS-2$
	    P1.setProperty("signerClaimedRoles", "Rol"); //$NON-NLS-1$ //$NON-NLS-2$
	    P1.setProperty("signatureAddress1", "Dir 1"); //$NON-NLS-1$ //$NON-NLS-2$
	    P1.setProperty("signatureAddress2", "Dir 2"); //$NON-NLS-1$ //$NON-NLS-2$

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

        final Enumeration<String> a = ks.aliases();
        while (a.hasMoreElements()) {
        	System.out.println(a.nextElement());
        }

        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS3, new KeyStore.PasswordProtection(CERT_PASS3.toCharArray()));

        final AOSigner signer = new AOOOXMLSigner();

        for (int i=0; i<DATAS.length; i++) {

	        System.out.println("Firma OOXML del fichero ': " + DATA_PATHS[i]); //$NON-NLS-1$

	        final byte[] result = signer.sign(
	    		DATAS[i],
	    		AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
	    		pke.getPrivateKey(),
	    		pke.getCertificateChain(),
	    		P1
			);

	        final File saveFile = File.createTempFile(DATA_PATHS[i] + ".signed.", ".docx"); //$NON-NLS-1$ //$NON-NLS-2$
	        try (
        		final OutputStream os = new FileOutputStream(saveFile);
    		) {
	        	os.write(result);
	        	os.flush();
	        }
	        System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

	        Assert.assertTrue(signer.isSign(result));
        }
    }
}

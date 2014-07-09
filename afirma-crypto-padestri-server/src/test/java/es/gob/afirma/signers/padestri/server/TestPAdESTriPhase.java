/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.padestri.server;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.padestri.server.PAdESTriPhaseSignerServerSide.PdfPreSignResult;

/**
 * Pruebas del m&oacute;dulo PAdES de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 *
 */
public class TestPAdESTriPhase {

	private static final String CATCERT_POLICY = "0.4.0.2023.1.1"; //$NON-NLS-1$
	private static final String CATCERT_TSP = "http://psis.catcert.net/psis/catcert/tsp"; //$NON-NLS-1$
	private static final boolean CATCERT_REQUIRECERT = true;

    private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    private static final String[] TEST_FILES = { "TEST_PDF.pdf" }; //$NON-NLS-1$

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

        final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILES[0]));

        final String prueba = "Firma PAdES de PDF con sello de tiempo en SHA512withRSA"; //$NON-NLS-1$

        System.out.println(prueba);

        final Properties extraParams = new Properties();
        //********* TSA CATCERT ********************************************************************
        //******************************************************************************************
        extraParams.put("tsaURL", CATCERT_TSP); //$NON-NLS-1$
        extraParams.put("tsaPolicy", CATCERT_POLICY); //$NON-NLS-1$
        extraParams.put("tsaRequireCert", Boolean.valueOf(CATCERT_REQUIRECERT)); //$NON-NLS-1$
        extraParams.put("tsaHashAlgorithm", "SHA1"); //$NON-NLS-1$ //$NON-NLS-2$
        //******************************************************************************************
        //********* FIN TSA CATCERT ****************************************************************

        final Calendar signingTime = new GregorianCalendar();
        final String algo = AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA;

        final PdfPreSignResult pre = PAdESTriPhaseSignerServerSide.preSign(
    		algo,
    		testPdf,
    		(X509Certificate[]) pke.getCertificateChain(),
    		signingTime,
    		extraParams
		);

        final byte[] result = PAdESTriPhaseSignerServerSide.postSign(
    		algo,
    		testPdf,
    		(X509Certificate[]) pke.getCertificateChain(),
    		extraParams,
    		new AOPkcs1Signer().sign(pre.getPreSign(), algo, pke.getPrivateKey(), pke.getCertificateChain(), extraParams),
    		pre.getPreSign(),
    		pre.getFileID(),
    		signingTime,
    		null,
    		null
		);

        final File saveFile = File.createTempFile("TSA-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
        try (final OutputStream os = new FileOutputStream(saveFile)) {
        	os.write(result);
        	os.flush();
        }
        System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

    }

}

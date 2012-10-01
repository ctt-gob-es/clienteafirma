/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.test.pades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.KeyStore;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.X509Certificate;
import java.util.Calendar;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.Assert;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pades.PAdESTriPhaseSigner;
import es.gob.afirma.signers.pades.PAdESTriPhaseSigner.PdfPreSignResult;
import es.gob.afirma.signers.tsp.pkcs7.CMSTimestamper;

/** Pruebas del m&oacute;dulo PAdES en modo trif&aacute;sico de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestPAdESTriPhase {

    private static final String CERT_PATH = "ANF_PF_Activo.pfx"; //$NON-NLS-1$
    private static final String CERT_PASS = "12341234"; //$NON-NLS-1$
    private static final String CERT_ALIAS = "anf usuario activo"; //$NON-NLS-1$

    private static final String[] TEST_FILES = { "TEST_PDF_Signed.pdf",  "TEST_PDF.pdf" }; //$NON-NLS-1$ //$NON-NLS-2$

    private final static Properties p1;
    static {
        p1 = new Properties();
        p1.setProperty("format", AOSignConstants.SIGN_FORMAT_PDF); //$NON-NLS-1$
        p1.setProperty("mode", AOSignConstants.SIGN_MODE_IMPLICIT); //$NON-NLS-1$
        p1.setProperty("signReason", "test"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("signatureProductionCity", "madrid"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("signerContact", "sink@usa.net"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyQualifier", "http://google.com/"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyIdentifier", "2.16.724.1.3.1.1.2"); //$NON-NLS-1$ //$NON-NLS-2$
        p1.setProperty("policyIdentifierHash", "0"); //$NON-NLS-1$ //$NON-NLS-2$

        p1.put("tsaURL", CMSTimestamper.CATCERT_TSP); //$NON-NLS-1$
        p1.put("tsaPolicy", CMSTimestamper.CATCERT_POLICY); //$NON-NLS-1$
        p1.put("tsaRequireCert", CMSTimestamper.CATCERT_REQUIRECERT); //$NON-NLS-1$
        p1.put("tsaHashAlgorithm", "SHA1"); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /** Algoritmos de firma a probar. */
    private final static String[] ALGOS = new String[] {
            AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA,
            AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA,
    };

    private static final Calendar SIGN_TIME = (Calendar) Calendar.getInstance().clone();

    private static byte[] sign(final byte[] data, final String algorithm, final PrivateKeyEntry keyEntry, final Properties extraParams) throws Exception {

        final X509Certificate[] certChain = (X509Certificate[]) keyEntry.getCertificateChain();

        // Primera fase (servidor)
        final PdfPreSignResult preSignature = PAdESTriPhaseSigner.preSign(
             AOSignConstants.getDigestAlgorithmName(algorithm),
             data,
             certChain,
             SIGN_TIME,
             extraParams
        );

        // Segunda fase (dispositivo)
        final byte[] signature = new AOPkcs1Signer().sign(preSignature.getPreSign(), algorithm, keyEntry, null);

        // Tercera fase
        return PAdESTriPhaseSigner.postSign(
             AOSignConstants.getDigestAlgorithmName(algorithm), // Datos se sesion: Algoritmo de firma (debe ser el mismo que en prefirma)
             data,                      // Documento PDF: A obtener desde repositorio en servidor
             certChain,                 // Datos de sesion: Cadena de certificados (debe ser la misma que en prefirma)
             extraParams,               // Datos de sesion: Opciones adicionales (deben ser las mismas que en prefirma)
             signature,                 // Resultado de la firma PKCS#1 en dispostivo (segunda fase)
             preSignature.getPreSign(), // Datos de sesion: SignedAttributes de CAdES
             preSignature.getFileID(),  // Datos de sesion: FileID del diccionario PDF
             SIGN_TIME					// Datos de sesion: Momento de la firma (debe ser el mismo que en prefirma)
        );

    }

    /**
     * Prueba de firma convencional.
     * @throws Exception en cualquier error
     */
    @SuppressWarnings("static-method")
	@Test
    public void testTriPhaseSignature() throws Exception {

        Logger.getLogger("es.gob.afirma").setLevel(Level.WARNING); //$NON-NLS-1$
        final PrivateKeyEntry pke;

        final KeyStore ks = KeyStore.getInstance("PKCS12"); //$NON-NLS-1$
        ks.load(ClassLoader.getSystemResourceAsStream(CERT_PATH), CERT_PASS.toCharArray());
        pke = (PrivateKeyEntry) ks.getEntry(CERT_ALIAS, new KeyStore.PasswordProtection(CERT_PASS.toCharArray()));

        String prueba;

        for (final String algo : ALGOS) {
            for (final String file : TEST_FILES) {

                final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(file));

                prueba = "Firma trifasica PAdES con el algoritmo ': " + //$NON-NLS-1$
                algo +
                "' y el fichero '" +  //$NON-NLS-1$
                file +
                "'"; //$NON-NLS-1$

                System.out.println(prueba);

                final byte[] result = sign(testPdf, algo, pke, p1);

                Assert.assertNotNull(prueba, result);

                final File saveFile = File.createTempFile(file.replace(".pdf", "") + "_" + algo + "-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
                final OutputStream os = new FileOutputStream(saveFile);
                os.write(result);
                os.flush();
                os.close();
                System.out.println("Temporal para comprobacion manual: " + saveFile.getAbsolutePath()); //$NON-NLS-1$

            }

        }
    }

}

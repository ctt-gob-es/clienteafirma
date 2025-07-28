package es.gob.afirma.test.pades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.GregorianCalendar;
import java.util.Properties;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signers.pades.PdfTimestamper;

/** Pruebas de sellos de tiempo de forma independiente a las firmas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class TestStandaloneTimestamp {

	private static final String CATCERT_POLICY = "0.4.0.2023.1.1"; //$NON-NLS-1$
	private static final String CATCERT_TSP = "http://psis.catcert.net/psis/catcert/tsp"; //$NON-NLS-1$
	private static final Boolean CATCERT_REQUIRECERT = Boolean.TRUE;

    private static final Properties EXTRAPARAMS = new Properties();
    static {
	    EXTRAPARAMS.put("tsaURL", CATCERT_TSP); //$NON-NLS-1$
	    EXTRAPARAMS.put("tsaPolicy", CATCERT_POLICY); //$NON-NLS-1$
	    EXTRAPARAMS.put("tsaRequireCert", CATCERT_REQUIRECERT); //$NON-NLS-1$
	    EXTRAPARAMS.put("tsaHashAlgorithm", "SHA-512"); //$NON-NLS-1$ //$NON-NLS-2$
	    EXTRAPARAMS.put("tsType", PdfTimestamper.TS_LEVEL_SIGN_DOC); //$NON-NLS-1$
    }

    /** Prueba de sello a nivel de documento de un PDF cofirmado.
     * @throws Exception en cualquier error. */
    @SuppressWarnings("static-method")
    @Test
    @Ignore
	public void testTimestampCosignedPdf() throws Exception {
    	final byte[] inPdf;
    	try (
			final InputStream is = TestStandaloneTimestamp.class.getResourceAsStream("/cosigned.pdf") //$NON-NLS-1$
		) {
	    	inPdf = AOUtil.getDataFromInputStream(
				is
			);
    	}
    	final byte[] outPdf = PdfTimestamper.timestampPdf(
			inPdf,
			EXTRAPARAMS,
			new GregorianCalendar()
		);
    	try (
	    	final OutputStream fos = new FileOutputStream(
				File.createTempFile("COSIGNED_TIMESTAMPED_", ".pdf") //$NON-NLS-1$ //$NON-NLS-2$
			);
		) {
    		fos.write(outPdf);
    		fos.flush();
    	}
    }

    /** Prueba de sello a nivel de documento de un PDF normal.
     * @throws Exception en cualquier error. */
    @SuppressWarnings("static-method")
    @Test
    @Ignore
	public void testTimestampPdf() throws Exception {
    	final byte[] inPdf;
    	try (
			final InputStream is = TestStandaloneTimestamp.class.getResourceAsStream("/multipage.pdf") //$NON-NLS-1$
		) {
	    	inPdf = AOUtil.getDataFromInputStream(
				is
			);
    	}
    	final byte[] outPdf = PdfTimestamper.timestampPdf(
			inPdf,
			EXTRAPARAMS,
			new GregorianCalendar()
		);
    	try (
	    	final OutputStream fos = new FileOutputStream(
				File.createTempFile("NORMAL_TIMESTAMPED_", ".pdf") //$NON-NLS-1$ //$NON-NLS-2$
			);
		) {
    		fos.write(outPdf);
    		fos.flush();
    	}
    }

}

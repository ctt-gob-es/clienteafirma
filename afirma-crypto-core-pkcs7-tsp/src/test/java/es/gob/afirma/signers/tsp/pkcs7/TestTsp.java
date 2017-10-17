package es.gob.afirma.signers.tsp.pkcs7;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.net.URI;
import java.security.MessageDigest;

import org.junit.Ignore;
import org.junit.Test;

/** Pruebas de sellos de tiempo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class TestTsp {

	private static final String CATCERT_POLICY = "0.4.0.2023.1.1"; //$NON-NLS-1$
	private static final String CATCERT_TSP_SSL = "https://psis.catcert.net/psis/catcert/tsp"; //$NON-NLS-1$
	private static final boolean CATCERT_REQUIRECERT = true;

	/** Prueba de obtenci&oacute;n directa de <i>token</i> TSP RFC3161 por HTTP.
	 * @throws Exception En cualquier error */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void TestRfc3161TokenHttp() throws Exception {

		final CMSTimestamper cmsTsp = new CMSTimestamper(
			CATCERT_REQUIRECERT,
			CATCERT_POLICY,
			new URI(CATCERT_TSP_SSL),
			null,
			null,
			null,
			null,
			null
		);
		final byte[] tspToken = cmsTsp.getTimeStampToken(
			MessageDigest.getInstance("SHA-256").digest("Hola".getBytes()), //$NON-NLS-1$ //$NON-NLS-2$
			"SHA-256", //$NON-NLS-1$
			null
		);
		try (
			final OutputStream fos = new FileOutputStream(File.createTempFile("TSP_", ".asn1")); //$NON-NLS-1$ //$NON-NLS-2$
		) {
			fos.write(tspToken);
			fos.flush();
		}
		System.out.println(new String(tspToken));
	}

}

package es.gob.afirma.signers.xades;

import java.io.InputStream;

import org.junit.Test;
import static org.junit.Assert.*;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.signers.xml.Utils;

/** Pruebas de validaci&oacute;n b&aacute;sica de firmas XAdES.
 * Ported from afirma-crypto-validation TestSignatureValidation. */
public class TestXAdESValidation {

	private static final String XADES_EPES_FILE = "xades_epes_detached.xsig";

	@Test
	public void testXadesEpesValidation() throws Exception {
		try (final InputStream is = ClassLoader.getSystemResourceAsStream(XADES_EPES_FILE)) {
			final byte[] signature = AOUtil.getDataFromInputStream(is);
			// Basic validation: parse XML and verify it has a Signature element
			assertNotNull("Signature data should not be null", signature);
			assertTrue("Signature data should not be empty", signature.length > 0);
			
			// Verify it's valid XML with Signature element
			final org.w3c.dom.Document doc = Utils.getNewDocumentBuilder().parse(new java.io.ByteArrayInputStream(signature));
			final org.w3c.dom.NodeList nl = doc.getElementsByTagNameNS("http://www.w3.org/2000/09/xmldsig#", "Signature");
			assertNotNull("Signature element should exist", nl);
			assertEquals("Should have exactly one Signature element", 1, nl.getLength());
		}
	}
}

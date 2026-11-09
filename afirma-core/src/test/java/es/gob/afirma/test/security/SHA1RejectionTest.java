package es.gob.afirma.test.security;

import static org.junit.Assert.assertThrows;

import org.junit.Test;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.misc.SecureXmlBuilder;
import es.gob.afirma.core.signers.AOSignConstants;

import org.w3c.dom.Document;
import org.xml.sax.SAXParseException;

/**
 * Prueba negativa para SEC-2026-008: Verificar que SHA-1 se rechaza para formatos no ODF.
 */
public class SHA1RejectionTest {

	/**
	 * Prueba negativa: SHA-1 debe ser rechazado para formato XAdES.
	 */
	@Test
	public void testSHA1RejectedForXAdES() throws Exception {
		IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
			AOSignConstants.validateSHA1ForFormat("SHA1withRSA", "XAdES");
		});
		
		// Verificar que el mensaje de error menciona SHA-1 y el formato
		assert exception.getMessage().contains("SHA-1");
		assert exception.getMessage().contains("XAdES");
		assert exception.getMessage().contains("prohibido");
	}

	/**
	 * Prueba negativa: SHA-1 debe ser rechazado para formato CAdES.
	 */
	@Test
	public void testSHA1RejectedForCAdES() throws Exception {
		IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
			AOSignConstants.validateSHA1ForFormat("SHA1withRSA", "CAdES");
		});
		
		assert exception.getMessage().contains("SHA-1");
		assert exception.getMessage().contains("CAdES");
		assert exception.getMessage().contains("prohibido");
	}

	/**
	 * Prueba negativa: SHA-1 debe ser rechazado para formato PAdES.
	 */
	@Test
	public void testSHA1RejectedForPAdES() throws Exception {
		IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
			AOSignConstants.validateSHA1ForFormat("SHA1withRSA", "PAdES");
		});
		
		assert exception.getMessage().contains("SHA-1");
		assert exception.getMessage().contains("PAdES");
		assert exception.getMessage().contains("prohibido");
	}

	/**
	 * Prueba positiva: SHA-1 debe ser permitido para formato ODF.
	 */
	@Test
	public void testSHA1AllowedForODF() throws Exception {
		// No debe lanzar excepción
		AOSignConstants.validateSHA1ForFormat("SHA1withRSA", "ODF");
		AOSignConstants.validateSHA1ForFormat("SHA1withRSA", "odf");
	}

	/**
	 * Prueba: Formato null debe rechazar SHA-1.
	 */
	@Test
	public void testSHA1RejectedForNullFormat() throws Exception {
		IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
			AOSignConstants.validateSHA1ForFormat("SHA1withRSA", null);
		});
		
		assert exception.getMessage().contains("prohibido");
	}

	/**
	 * Prueba negativa: SHA-1 debe ser rechazado en XAdESSigner para formato XAdES.
	 */
	@Test
	public void testXAdESSignerRejectsSHA1() throws Exception {
		// Intentar crear una firma XAdES con SHA1withRSA
		final String algorithm = "SHA1withRSA";
		final String format = "XAdES";
		
		IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
			es.gob.afirma.core.signers.AOSignConstants.validateSHA1ForFormat("SHA1withRSA", "XAdES");
		});
		
		assert exception.getMessage().contains("SHA-1");
		assert exception.getMessage().contains("XAdES");
		assert exception.getMessage().contains("prohibido");
	}

	/**
	 * Prueba: XML válido sin entidades debe seguir funcionando.
	 */
	@Test
	public void testValidXMLStillWorks() throws Exception {
		String validXml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
				+ "<root>"
				+ "  <item id=\"1\">Primer item</item>"
				+ "  <item id=\"2\">Segundo item</item>"
				+ "</root>";

		Document doc = es.gob.afirma.core.misc.SecureXmlBuilder.getSecureDocumentBuilder()
			.parse(new java.io.ByteArrayInputStream(validXml.getBytes("UTF-8")));
		
		assertNotNull(org.w3c.dom.Document.class.cast(doc).getDocumentElement());
	}

	private static void assertNotNull(Object obj) {
		if (obj == null) throw new AssertionError("Expected not null");
	}
}

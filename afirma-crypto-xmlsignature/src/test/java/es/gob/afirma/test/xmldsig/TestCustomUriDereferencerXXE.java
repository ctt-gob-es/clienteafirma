package es.gob.afirma.test.xmldsig;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

import javax.xml.parsers.DocumentBuilder;

import org.junit.Assert;
import org.junit.Test;

import es.gob.afirma.core.misc.SecureXmlBuilder;
import es.gob.afirma.signers.xml.Utils;

/**
 * Regresi&oacute;n XXE: el parser usado por CustomUriDereferencer / Utils
 * no debe resolver entidades externas (el contenido de /etc/passwd no aparece).
 */
public final class TestCustomUriDereferencerXXE {

	private static final String XXE_PAYLOAD = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
			+ "<!DOCTYPE root ["
			+ "  <!ENTITY xxe SYSTEM \"file:///etc/passwd\">"
			+ "]>"
			+ "<root>&xxe;</root>";

	@Test
	public void testSecureXmlBuilderDoesNotResolveExternalEntity() throws Exception {
		final DocumentBuilder builder = SecureXmlBuilder.getSecureDocumentBuilder();
		final org.w3c.dom.Document doc = builder.parse(new ByteArrayInputStream(XXE_PAYLOAD.getBytes(StandardCharsets.UTF_8)));
		final String text = doc.getDocumentElement().getTextContent();
		Assert.assertFalse("La entidad externa se resolvio (XXE)", text.contains("root:"));
	}

	@Test
	public void testUtilsDocumentBuilderDoesNotResolveExternalEntity() throws Exception {
		final DocumentBuilder builder = Utils.getNewDocumentBuilder();
		final org.w3c.dom.Document doc = builder.parse(new ByteArrayInputStream(XXE_PAYLOAD.getBytes(StandardCharsets.UTF_8)));
		final String text = doc.getDocumentElement().getTextContent();
		Assert.assertFalse("La entidad externa se resolvio (XXE)", text.contains("root:"));
	}
}

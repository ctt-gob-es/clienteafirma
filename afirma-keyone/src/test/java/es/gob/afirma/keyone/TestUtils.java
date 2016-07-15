package es.gob.afirma.keyone;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.junit.Assert;
import org.junit.Test;

public final class TestUtils {

	@Test
	public static void testEnumSignatureFieldNames() {
		try {
			Assert.assertEquals(null, "EmptySignatureField", KeyOneUtil.enumSignatureFieldNames("C:\\Users\\A621916\\Desktop\\Pruebas\\empty_signature_field.pdf").split(",")[0]); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		} catch (final PdfException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Test
	public static void TestAddBlankPage() {
		try {
			KeyOneUtil.addBlankPage("C:\\Users\\A621916\\Desktop\\Pruebas\\defensa\\aaa.pdf"); //$NON-NLS-1$
		} catch (final PdfException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Test
	public static void TestPdfSign() {

		byte[] encoded = null;
		try {
			encoded = Files.readAllBytes(Paths.get("C:\\Users\\A621916\\Desktop\\Pruebas\\defensa\\aparienciaPrueba.xml")); //$NON-NLS-1$
		} catch (final IOException e1) {
			e1.printStackTrace();
		}
		final String xml = new String(encoded);
		try {
			KeyOneUtil.pdfSign(
				"C:\\Users\\A621916\\Desktop\\Pruebas\\empty_signature_field.pdf", //$NON-NLS-1$
				"C:\\Users\\A621916\\Desktop\\Pruebas\\empty_signature_field_signed.pdf", //$NON-NLS-1$
				null,
				null,
				null,
				xml
			);
		} catch (final Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Test
	public static void testVerifySignature() {
		try {
			Assert.assertTrue(KeyOneUtil.verifySignature("C:\\Users\\A621916\\Desktop\\Pruebas\\defensa\\aparienciaPrueba.xml_signed.xsig")); //$NON-NLS-1$
		} catch (final PdfException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}

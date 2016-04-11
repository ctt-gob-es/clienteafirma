package es.gob.afirma.test.pades;

import java.util.List;

import org.junit.Test;

import com.aowagie.text.pdf.AcroFields;
import com.aowagie.text.pdf.PdfReader;

/** Pruebas de tratamiento de campos de firma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestListSignatureFields {

	private static final String TEST_FILE = "/empty_signature_field.pdf"; //$NON-NLS-1$

	/** Pruebas de listado de campos de firma vac&iacute;ios.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testListEmptySignatureFields() throws Exception {
		final PdfReader reader = new PdfReader(
			TestListSignatureFields.class.getResourceAsStream(
				TEST_FILE
			)
		);
		final AcroFields fields = reader.getAcroFields();
		final List<String> emptySignatureFields = fields.getBlankSignatureNames();
		System.out.println(emptySignatureFields);
	}

}

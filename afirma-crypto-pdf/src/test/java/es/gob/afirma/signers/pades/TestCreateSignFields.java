package es.gob.afirma.signers.pades;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

import org.junit.Test;

import com.aowagie.text.Rectangle;
import com.aowagie.text.pdf.PdfAnnotation;
import com.aowagie.text.pdf.PdfFormField;
import com.aowagie.text.pdf.PdfName;
import com.aowagie.text.pdf.PdfReader;
import com.aowagie.text.pdf.PdfStamper;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.test.pades.TestListSignatureFields;

/**
 * Ejemplos de creacion de campos de firma con iText.
 */
public class TestCreateSignFields {

	private static final String TEST_EMPTY_FILE = "/pdf_without_signatures.pdf"; //$NON-NLS-1$

	private static final String SIGNATURE_FIELD = "CampoFirma"; //$NON-NLS-1$

	/**
	 * Crea un campo de firma con presentaci&oacute;n en dos p&aacute;ginas
	 * del documento.
	 * @throws Exception Cuando ocurre cualquier error.
	 */
	@SuppressWarnings("static-method")
	@Test
	public void testCreateSignatureFieldsOnMultiPages() throws Exception {

		System.out.println("Prueba de generacion de campos de firma en multiples paginas"); //$NON-NLS-1$

		byte[] inPdf;
		try (InputStream is = TestListSignatureFields.class.getResourceAsStream(TEST_EMPTY_FILE)) {
			inPdf = AOUtil.getDataFromInputStream(is);
		}

		final PdfReader pdf = new PdfReader(inPdf);

		final File outFile = File.createTempFile("signedPdf-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
		try (OutputStream os = new FileOutputStream(outFile)) {

			final PdfStamper stp = new PdfStamper(pdf, os);

			final PdfFormField sig = PdfFormField.createSignature(stp.getWriter());
			sig.setWidget(new Rectangle(100, 500, 300, 600), PdfName.HIGHLIGHT);
			sig.setFlags(PdfAnnotation.FLAGS_PRINT);
			sig.setFieldName(SIGNATURE_FIELD);
			sig.setPage(1);
			stp.addAnnotation(sig, 1);
			stp.addAnnotation(sig, 2);
			stp.close();
		}

		System.out.println("Documento guardado para comprobacion manual: " + outFile.getAbsolutePath()); //$NON-NLS-1$
	}
}

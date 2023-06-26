package es.gob.afirma.test.simple;

import java.io.File;

import org.junit.Ignore;
import org.junit.Test;

import es.gob.afirma.standalone.SimpleAfirma;

/** Pruebas de firma desde l&iacute;nea de comandos. */
public final class SignFromCommandLineTest {

	private static final String PDF_FILE = "/samples/2.pdf"; //$NON-NLS-1$
	private static final String PDF_MULTIPLE_PAGES_FILE = "/samples/multiple_pages.pdf"; //$NON-NLS-1$
	private static final String PKCS12_FILE = "/ANF_PF_Activo.pfx"; //$NON-NLS-1$
	private static final String ALIAS = "anf usuario activo"; //$NON-NLS-1$
	private static final String PWD = "12341234"; //$NON-NLS-1$

	/** Prueba de generaci&oacute;n de firma visible PDF.
	 * @throws Exception Cuando ocurre cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testSignPadesVisible() throws Exception {

		final String inputFile = new File(SignFromCommandLineTest.class.getResource(PDF_FILE).toURI()).getAbsolutePath();
		final String outFile = new File("C:\\Users\\carlos.gamuci\\Desktop\\pdf_visible.pdf").getAbsolutePath(); //$NON-NLS-1$

		final String p12File = new File(SignFromCommandLineTest.class.getResource(PKCS12_FILE).toURI()).getAbsolutePath();

		SimpleAfirma.main(
				new String[] {
					"sign", //$NON-NLS-1$
					"-i", inputFile,  //$NON-NLS-1$
					"-o", outFile, //$NON-NLS-1$
					"-store", "pkcs12:" + p12File, //$NON-NLS-1$ //$NON-NLS-2$
					"-password", PWD, //$NON-NLS-1$
					"-alias", ALIAS,  //$NON-NLS-1$,
					"-format", "pades", //$NON-NLS-1$ //$NON-NLS-2$
					"-config", "signaturePositionOnPageLowerLeftX=100\\n" + //$NON-NLS-1$ //$NON-NLS-2$
							"signaturePositionOnPageLowerLeftY=100\\n" + //$NON-NLS-1$
							"signaturePositionOnPageUpperRightX=200\\n" + //$NON-NLS-1$
							"signaturePositionOnPageUpperRightY=200\\n" + //$NON-NLS-1$
							"signaturePage=1" //$NON-NLS-1$
				}

			);

	}

	/** Prueba de generaci&oacute;n de firma visible PDF con r&uacute;brica.
	 * @throws Exception Cuando ocurre cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testSignPadesVisibleWithRubric() throws Exception {

		final String inputFile = new File(SignFromCommandLineTest.class.getResource(PDF_FILE).toURI()).getAbsolutePath();
		final String outFile = new File("C:\\Users\\carlos.gamuci\\Desktop\\pdf_visible.pdf").getAbsolutePath(); //$NON-NLS-1$

		final String p12File = new File(SignFromCommandLineTest.class.getResource(PKCS12_FILE).toURI()).getAbsolutePath();

		SimpleAfirma.main(
				new String[] {
					"sign", //$NON-NLS-1$
					"-i", inputFile,  //$NON-NLS-1$
					"-o", outFile, //$NON-NLS-1$
					"-store", "pkcs12:" + p12File, //$NON-NLS-1$ //$NON-NLS-2$
					"-password", PWD, //$NON-NLS-1$
					"-alias", ALIAS,  //$NON-NLS-1$,
					"-format", "pades", //$NON-NLS-1$ //$NON-NLS-2$
					"-config", "signaturePositionOnPageLowerLeftX=100\\n" + //$NON-NLS-1$ //$NON-NLS-2$
							"signaturePositionOnPageLowerLeftY=100\\n" + //$NON-NLS-1$
							"signaturePositionOnPageUpperRightX=200\\n" + //$NON-NLS-1$
							"signaturePositionOnPageUpperRightY=200\\n" + //$NON-NLS-1$
							"signatureRubricImage=C:/Users/carlos.gamuci/Desktop/Entrada/Rubrica.png\\n" + //$NON-NLS-1$
							"signaturePage=1" //$NON-NLS-1$
				}

			);

	}

	/** Prueba de generaci&oacute;n de firma visible PDF para varias p&aacute;ginas.
	 * @throws Exception Cuando ocurre cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	@Ignore
	public void testSignPadesVisibleMultiplePages() throws Exception {

		final String inputFile = new File(SignFromCommandLineTest.class.getResource(PDF_MULTIPLE_PAGES_FILE).toURI()).getAbsolutePath();
		final String outFile = new File("C:\\testFirma\\pdf_visible.pdf").getAbsolutePath(); //$NON-NLS-1$

		final String p12File = new File(SignFromCommandLineTest.class.getResource(PKCS12_FILE).toURI()).getAbsolutePath();

		SimpleAfirma.main(
				new String[] {
					"sign", //$NON-NLS-1$
					"-i", inputFile,  //$NON-NLS-1$
					"-o", outFile, //$NON-NLS-1$
					"-store", "pkcs12:" + p12File, //$NON-NLS-1$ //$NON-NLS-2$
					"-password", PWD, //$NON-NLS-1$
					"-alias", ALIAS,  //$NON-NLS-1$,
					"-format", "pades", //$NON-NLS-1$ //$NON-NLS-2$
					"-config", "signaturePositionOnPageLowerLeftX=100\\n" + //$NON-NLS-1$ //$NON-NLS-2$
							"signaturePositionOnPageLowerLeftY=100\\n" + //$NON-NLS-1$
							"signaturePositionOnPageUpperRightX=200\\n" + //$NON-NLS-1$
							"signaturePositionOnPageUpperRightY=200\\n" + //$NON-NLS-1$
							"signaturePages=2-4,6" //$NON-NLS-1$
				}

			);

	}
}

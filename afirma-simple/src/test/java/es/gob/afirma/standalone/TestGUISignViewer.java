package es.gob.afirma.standalone;

import java.io.File;
import java.net.URISyntaxException;

/** Pruebas del visor gr&aacute;fico de firmas.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class TestGUISignViewer {

	//private static final String TEST_FILE_OOXML = "/samples/2_signed.docx"; //$NON-NLS-1$
	private static final String TEST_FILE_ODF = "/samples/2_signed.odt"; //$NON-NLS-1$

	private static void testFile(final String filePath) throws URISyntaxException {
		final File file = new File(
			TestGUISignViewer.class.getResource(filePath).toURI()
		);
		SimpleAfirma.main(
			new String[] {
				CommandLineCommand.VERIFY.toString().toLowerCase(),
				"-i", //$NON-NLS-1$
				file.getAbsolutePath()
			}
		);
	}

	/** Main para pruebas.
	 * @param args No se usa.
	 * @throws Exception EN cualquier error. */
	public static void main(final String args[]) throws Exception {
		testFile(TEST_FILE_ODF);
	}
}

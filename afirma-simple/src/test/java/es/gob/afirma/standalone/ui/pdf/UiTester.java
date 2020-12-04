package es.gob.afirma.standalone.ui.pdf;

import java.awt.Dimension;
import java.awt.image.BufferedImage;
import java.util.List;
import java.util.Properties;

import javax.swing.JFrame;

import org.junit.Test;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.standalone.ui.pdf.PdfLoader.PdfLoaderListener;
import es.gob.afirma.standalone.ui.pdf.SignPdfUiPanel.SignPdfUiPanelListener;

/** Pruebas del UI de firma PDF visible.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class UiTester {

	//private final static String TEST_FILE = "sec1-v2.pdf"; //$NON-NLS-1$
	private final static String TEST_FILE = "PDF_MULTISIZE.pdf"; //$NON-NLS-1$
	private final static boolean IS_SIGN = false;

	/** Prueba de di&aacute;logo fallido. */
	@SuppressWarnings({ "static-method" })
	@Test
	public void testFailedDialog() {
		SignPdfDialog.getVisibleSignatureExtraParams(IS_SIGN, new byte[] { (byte) 0xff, (byte) 0xff, (byte) 0xff }, null, true, false, false, extraParams -> {
			// Vacio
		});
	}

	/** Prueba de di&aacute;logo.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings({ "static-method" })
	@Test
	public void testDialog() throws Exception {
		final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE));
		SignPdfDialog.getVisibleSignatureExtraParams(
				IS_SIGN,
				testPdf,
				null, true, false,
				false,
				extraParams -> System.out.println(extraParams));
		for(;;) {
			// Bucle infinito
		}
	}

	/** Prueba el panel de vista de p&aacute;ginas del PDF.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings("static-method")
	@Test
	public void testMainPanel() throws Exception {

		final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE));

		final JFrame frame = new JFrame("Prueba"); //$NON-NLS-1$
		frame.setBounds(100, 20, 900, 900);

		PdfLoader.loadPdf(
			IS_SIGN,
			testPdf,
			new PdfLoaderListener() {
				@Override
				public void pdfLoaded(final boolean isSign, final List<BufferedImage> pages, final List<Dimension> pageSizes, final byte[] pdf) {
					System.out.println("Cargado"); //$NON-NLS-1$
					frame.add(
						new SignPdfUiPanel(
							isSign,
							pages,
							pageSizes,
							pdf,
							new SignPdfUiPanelListener() {

								@Override
								public void nextPanel(final Properties p, final BufferedImage im) {
								}

								@Override
								public void positionSelected(final Properties extraParams) {
									System.out.println(extraParams);
									frame.dispose();
									System.exit(0);
								}

								@Override
								public void positionCancelled() {
									System.out.println("Seleccion cancelada"); //$NON-NLS-1$
									frame.dispose();
									System.exit(0);
								}

							}
						)
					);
					frame.pack();
					frame.setVisible(true);

				}
				@Override
				public void pdfLoadedFailed(final Throwable cause) {
					cause.printStackTrace();
				}
			}
		);

		for(;;) {
			// Vacio
		}
	}

}

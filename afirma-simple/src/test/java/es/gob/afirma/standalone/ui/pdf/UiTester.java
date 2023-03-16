package es.gob.afirma.standalone.ui.pdf;

import java.awt.Dimension;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JFrame;

import org.junit.Ignore;
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
	private final static boolean IS_MASSIVE_SIGN = false;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Prueba de di&aacute;logo fallido. */
	@SuppressWarnings({ "static-method" })
	@Test
	public void testFailedDialog() {
		SignPdfDialog.getVisibleSignatureExtraParams(IS_SIGN, IS_MASSIVE_SIGN, new byte[] { (byte) 0xff, (byte) 0xff, (byte) 0xff }, null, true, false, false, extraParams -> {
			// Vacio
		});
	}

	/** Prueba de di&aacute;logo.
	 * @throws Exception En cualquier error. */
	@SuppressWarnings({ "static-method" })
	@Test
	@Ignore // Requiere interfaz grafica
	public void testDialog() throws Exception {
		final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE));
		SignPdfDialog.getVisibleSignatureExtraParams(
				IS_SIGN,
				IS_MASSIVE_SIGN,
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
	@Ignore // Requiere interfaz grafica
	public void testMainPanel() throws Exception {

		final byte[] testPdf = AOUtil.getDataFromInputStream(ClassLoader.getSystemResourceAsStream(TEST_FILE));

		final JFrame frame = new JFrame("Prueba"); //$NON-NLS-1$
		frame.setBounds(100, 20, 900, 900);

		PdfLoader.loadPdf(
			IS_SIGN,
			IS_MASSIVE_SIGN,
			testPdf,
			new PdfLoaderListener() {
				@Override
				public void pdfLoaded(final boolean isSign, final boolean isMassiveSign, final byte[] pdf) throws IOException {
					LOGGER.info("Cargado"); //$NON-NLS-1$

					final List<BufferedImage> pages = Pdf2ImagesConverter.pdf2ImagesUsefulSections(pdf, null, 0);
					final List<Dimension> pageSizes = SignPdfUiUtil.getPageSizes(pdf, null);

					frame.add(
						new SignPdfUiPanel(
							isSign,
							isMassiveSign,
							pages,
							pageSizes,
							pdf,
							null,
							new SignPdfUiPanelListener() {

								@Override
								public void nextPanel(final Properties p, final BufferedImage im) {
									// No hace nada
								}

								@Override
								public void positionSelected(final Properties extraParams) {
									System.out.println(extraParams);
									frame.dispose();
									System.exit(0);
								}

								@Override
								public void positionCancelled() {
									LOGGER.info("Seleccion cancelada"); //$NON-NLS-1$
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
					LOGGER.log(Level.SEVERE, "Error en la carga del PDF", cause); //$NON-NLS-1$
				}
			}
		);

		for(;;) {
			// Vacio
		}
	}

}

package es.gob.afirma.standalone.ui.pdf;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.EventListener;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;

import com.aowagie.text.pdf.PdfReader;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.ui.CommonWaitDialog;

final class PdfLoader {


	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private PdfLoader() {
		// No instanciable
	}

	interface PdfLoaderListener extends EventListener {
		void pdfLoaded(boolean isSing, List<BufferedImage> pages, List<Dimension> pageSizes);
		void pdfLoadedFailed(Throwable cause);
	}

	static void loadPdf(final boolean isSign, final byte[] inPdf, final PdfLoaderListener pll) {
		new Thread(() ->  {
				try {
					pll.pdfLoaded(
						isSign,
						Pdf2ImagesConverter.pdf2Images(inPdf),
						getPageSizes(inPdf)
					);
				}
				catch(final OutOfMemoryError e) {
					pll.pdfLoadedFailed(e);
				}
				catch (final IOException e) {
					pll.pdfLoadedFailed(e);
				}
			}
		).start();
	}

	static void loadPdfWithProgressDialog(final boolean isSign,
										  final Frame parent,
			                              final byte[] inPdf,
			                              final PdfLoaderListener pll) {

		final JPanel panel = new JPanel();
		panel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		final JLabel labelProgress = new JLabel(
			 //$NON-NLS-1$
		);
		panel.add(labelProgress);
		final JProgressBar jpb = new JProgressBar();
		jpb.setIndeterminate(true);
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
            final Method putCLientPropertyMethod;
			try {
				putCLientPropertyMethod = JProgressBar.class.getMethod("putClientProperty", Object.class, Object.class); //$NON-NLS-1$
				putCLientPropertyMethod.invoke(jpb, "JProgressBar.style", "circular"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			catch (final Exception e) {
				LOGGER.warning(
					"No se ha podido establecer el estilo OS X en el dialogo de espera: " + e //$NON-NLS-1$
				);
			}
        }
		labelProgress.setLabelFor(jpb);
		panel.add(jpb);

		final CommonWaitDialog dialog = new CommonWaitDialog(
			parent,
			SignPdfUiMessages.getString("PdfLoader.0"), // Mensaje //$NON-NLS-1$
			SignPdfUiMessages.getString("PdfLoader.1")  // Titulo //$NON-NLS-1$
		);

		new Thread(() -> {
				try {
					final List<BufferedImage> pages = Pdf2ImagesConverter.pdf2Images(inPdf);
					dialog.setVisible(false);
					pll.pdfLoaded(isSign, pages, getPageSizes(inPdf));
				}
				catch (final IOException e) {
					dialog.setVisible(false);
					pll.pdfLoadedFailed(e);
				}
				finally {
					dialog.dispose();
				}
			}
		).start();

		dialog.pack();
		dialog.setVisible(true);
	}

	static List<Dimension> getPageSizes(final byte[] inPdf) throws IOException {
		final PdfReader reader = new PdfReader(inPdf);
		final int numberOfPages = reader.getNumberOfPages();
		final List<Dimension> pageSizes = new ArrayList<>(numberOfPages);
		for(int i=1;i<=numberOfPages;i++) {
			final com.aowagie.text.Rectangle rect = reader.getPageSize(i);
			pageSizes.add(
				new Dimension(
					Math.round(rect.getWidth()),
					Math.round(rect.getHeight())
				)
			);
		}
		return pageSizes;
	}

}

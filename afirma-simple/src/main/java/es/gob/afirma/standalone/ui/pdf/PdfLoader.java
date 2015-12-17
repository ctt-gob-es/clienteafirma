package es.gob.afirma.standalone.ui.pdf;

import java.awt.Dialog.ModalityType;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.ArrayList;
import java.util.EventListener;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.WindowConstants;

import com.lowagie.text.pdf.PdfReader;

final class PdfLoader {

	private static final int PREFERRED_WIDTH = 250;
	private static final int PREFERRED_HEIGHT = 100;

	private PdfLoader() {
		// No instanciable
	}

	interface PdfLoaderListener extends EventListener {
		void pdfLoaded(List<BufferedImage> pages, List<Dimension> pageSizes);
		void pdfLoadedFailed(IOException cause);
	}

	static void loadPdf(final byte[] inPdf, final PdfLoaderListener pll) {
		new Thread(
			new Runnable() {
				@Override
				public void run() {
					try {
						pll.pdfLoaded(
							Pdf2ImagesConverter.pdf2Images(inPdf),
							getPageSizes(inPdf)
						);
					}
					catch (final IOException e) {
						pll.pdfLoadedFailed(e);
					}

				}
			}
		).start();
	}

	static void loadPdfWithProgressDialog(final Frame parent,
			                              final byte[] inPdf,
			                              final PdfLoaderListener pll) {

		final JPanel panel = new JPanel();
		final JLabel labelProgress = new JLabel(
				SignPdfUiMessages.getString("PdfLoader.0") //$NON-NLS-1$
		);
		panel.add(labelProgress);
		final JProgressBar jpb = new JProgressBar();
		jpb.setIndeterminate(true);
		labelProgress.setLabelFor(jpb);
		panel.add(jpb);

		final JDialog dialog = new JDialog(parent, true);
		dialog.setContentPane(panel);
		dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		dialog.setPreferredSize(new Dimension(PREFERRED_WIDTH, PREFERRED_HEIGHT));
		final Point cp = GraphicsEnvironment.getLocalGraphicsEnvironment().getCenterPoint();
		dialog.setLocation(cp.x - PREFERRED_WIDTH/2, cp.y - PREFERRED_HEIGHT/2);
		dialog.setResizable(false);
		dialog.setModalityType(ModalityType.APPLICATION_MODAL);
		dialog.setTitle(
			SignPdfUiMessages.getString("PdfLoader.1") //$NON-NLS-1$
		);
        try {
            dialog.setIconImage(
        		Toolkit.getDefaultToolkit().getImage(
    				dialog.getClass().getResource(
						"/resources/afirma_ico.png" //$NON-NLS-1$
					)
				)
            );
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
        		"No se ha podido cargar el icono del dialogo de espera de carga de PDF: " + e  //$NON-NLS-1$
    		);
        }

		new Thread(
			new Runnable() {
				@Override
				public void run() {
					try {
						final List<BufferedImage> pages = Pdf2ImagesConverter.pdf2Images(inPdf);
						dialog.setVisible(false);
						pll.pdfLoaded(pages, getPageSizes(inPdf));
					}
					catch (final IOException e) {
						dialog.setVisible(false);
						pll.pdfLoadedFailed(e);
					}
					finally {
						dialog.dispose();
					}

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
			final com.lowagie.text.Rectangle rect = reader.getPageSize(i);
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

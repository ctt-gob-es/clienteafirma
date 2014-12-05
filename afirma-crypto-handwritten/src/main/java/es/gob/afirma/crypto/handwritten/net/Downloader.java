package es.gob.afirma.crypto.handwritten.net;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Frame;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.WindowConstants;
import javax.swing.border.Border;

import es.gob.afirma.crypto.handwritten.HandwrittenMessages;

/** Clase de auda para descarga de ficheros. */
public final class Downloader implements DownloadListener {

	/** Valores m&iacute;nimo y m&acute;ximo de la barra de progreso.*/
	private static int MIN_PG = 0;
	private static int MAX_PG = 1000;

	/** Dimensiones para la barra de progreso.*/
	private static int WIDTH_DPB = 300;
	private static int HEIGHT_DPB = 35;

	/** Dimensiones del di&acute;logo de proceso.*/
	private static int WIDTH_DLG = 600;
	private static int HEIGHT_DLG = 200;

	private static int BORDER = 10;

	private static final Border BORDER_PANEL = BorderFactory.createEmptyBorder(BORDER, BORDER, BORDER, BORDER);

	private final JDialog dlg;
	private final DownloadListener dl;

	/** Construye una clase de auda para descarga de ficheros.
	 * @param parent Padre para la modalidad gr&aacute;fica.
	 * @param dlistener Clase a la que notificar el resultado de la descarga. */
	public Downloader(final Container parent, final DownloadListener dlistener) {
		if (dlistener == null) {
			throw new IllegalArgumentException(
				"Es obligatorio indicar una clase a la que notificar el resulatdo de la descarga" //$NON-NLS-1$
			);
		}
		this.dlg = new JDialog(new Frame(), HandwrittenMessages.getString("ProgressUrlHttpManagerImpl.1"), true); //$NON-NLS-1$
		this.dl = dlistener;
	}

	/** Descarga un fichero por HTTP/HTTPS GET.
	 * @param url URL a descargar.
	 * @throws IOException Si hay problemas preparando la descarga, los errores durante la
	 *                     descarga en si se notifican al <i>listener</i>. */
	public void downloadFile(final String url) throws IOException {

		// Mostramos dialogo de espera indeterminado y cambiamos el cursor a reloj de arena
		this.dlg.setCursor(new Cursor(Cursor.WAIT_CURSOR));

		final JProgressBar dpb = new JProgressBar(MIN_PG, MAX_PG);

		dpb.setIndeterminate(true);
		dpb.setBorder(BORDER_PANEL);
		dpb.setPreferredSize(new Dimension(WIDTH_DPB, HEIGHT_DPB));

		final JLabel jlText = new JLabel(HandwrittenMessages.getString("ProgressUrlHttpManagerImpl.2", url)); //$NON-NLS-1$
		jlText.setBorder(BORDER_PANEL);

		this.dlg.add(BorderLayout.CENTER, dpb);
		this.dlg.add(BorderLayout.PAGE_START, jlText);

		this.dlg.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

		this.dlg.setSize(WIDTH_DLG, HEIGHT_DLG);
		this.dlg.setLocationByPlatform(true);

		// Ponemos un icono transparente
		this.dlg.setIconImage(ImageIO.read(ProgressUrlHttpManagerImpl.class.getResource("/icono_transparente.png"))); //$NON-NLS-1$

		this.dlg.pack();

		ProgressUrlHttpManagerImpl.readUrlByGetAsync(url, this);

		this.dlg.setVisible(true);

	}

	@Override
	public void downloadComplete(final byte[] data) {
		closeDialog();
		this.dl.downloadComplete(data);
	}

	@Override
	public void downloadError(final Throwable t) {
		closeDialog();
		this.dl.downloadError(t);
	}

	private void closeDialog() {
		// Quitamos dialogo de espera y ponemos cursor normal
		this.dlg.setVisible(false);
		this.dlg.dispose();
		this.dlg.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	}

}

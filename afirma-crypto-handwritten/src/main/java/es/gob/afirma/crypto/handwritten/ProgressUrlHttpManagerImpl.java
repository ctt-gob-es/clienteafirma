package es.gob.afirma.crypto.handwritten;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Frame;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.logging.Logger;

import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.WindowConstants;
import javax.swing.border.Border;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.UrlHttpManagerImpl;

/** Clase utilizada para descarga de documentos.
 * @author Astrid Idoate **/
public final class ProgressUrlHttpManagerImpl extends UrlHttpManagerImpl {

	private final Frame parent;

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


	ProgressUrlHttpManagerImpl(final Frame p) {
		super();
		this.parent = p;
	}

	private final Border borderPanel = BorderFactory.createEmptyBorder(BORDER, BORDER, BORDER, BORDER);

	/** M&eacute;todo para descargar un documento a trav&eactues de una URL.
	 * @param url URL para la descarga del documento
	 * @return devuelve el documento descargado.
	 * @throws IOException tramiento de excepciones.*/
	@Override
	public byte[] readUrlByGet(final String url) throws IOException {

		final URL uri = new URL(url);

		// Mostramos dialogo de espera indeterminado y cambiamos el cursor a reloj de arena
		final JDialog dlg = new JDialog(this.parent, HandwrittenMessages.getString("ProgressUrlHttpManagerImpl.1"), true); //$NON-NLS-1$
		dlg.setCursor(new Cursor(Cursor.WAIT_CURSOR));

		JProgressBar dpb = new JProgressBar(MIN_PG, MAX_PG);

		dpb.setIndeterminate(true);
		dpb.setBorder(this.borderPanel);
		dpb.setPreferredSize(new Dimension(WIDTH_DPB, HEIGHT_DPB));

		JLabel jlText = new JLabel(HandwrittenMessages.getString("ProgressUrlHttpManagerImpl.2", url)); //$NON-NLS-1$
		jlText.setBorder(this.borderPanel);

		dlg.add(BorderLayout.CENTER, dpb);
		dlg.add(BorderLayout.PAGE_START, jlText);

		dlg.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

		dlg.setSize(WIDTH_DLG, HEIGHT_DLG);
		dlg.setLocationByPlatform(true);

		// Ponemos un icono transparente
		dlg.setIconImage(ImageIO.read(ProgressUrlHttpManagerImpl.class.getResource("/icono_transparente.png"))); //$NON-NLS-1$

		dlg.pack();

		new Thread(
			new Runnable() {
				@Override
				public void run() {
					dlg.setVisible(true);

				}
			}
		).start();

		if (uri.getProtocol().equals("https")) { //$NON-NLS-1$
			try {
				disableSslChecks();
			}
			catch(final Exception e) {
				Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
					"No se ha podido ajustar la confianza SSL, es posible que no se pueda completar la conexion: " + e //$NON-NLS-1$
				);
			}
		}

		final InputStream is = uri.openStream();

		final byte[] data = AOUtil.getDataFromInputStream(is);

		is.close();

		if (uri.getProtocol().equals("https")) { //$NON-NLS-1$
			enableSslChecks();
		}

		// Quitamos dialogo de espera y ponemos cursor normal
		dlg.setVisible(false);
		dlg.dispose();

		dlg.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

		// Recuperamos los datos
		return data;

	}
}

package es.gob.afirma.standalone.ui.pdf;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.ui.pdf.PdfLoader.PdfLoaderListener;
import es.gob.afirma.standalone.ui.pdf.SignPdfUiPanel.SignPdfUiPanelListener;

/** Di&aacute;logo para la obtenci&oacute;n de los datos de firma PDF Visible.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class SignPdfDialog extends JDialog implements PdfLoaderListener, SignPdfUiPanelListener {

	private static final long serialVersionUID = -7987676963743094243L;

	private static final int PREFERRED_WIDTH = 500;
	private static final int PREFERRED_HEIGHT = 615;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final Frame parent;
	private final SignPdfDialogListener listener;

	private SignPdfUiPanel areaPanel;
	SignPdfDialogListener getListener() {
		return this.listener;
	}

	/** Construye un di&aacute;logo para la obtenci&oacute;n de los datos de firma PDF Visible.
	 * @param parentFrame Marco padre para la modalidad.
	 * @param spdl Clase a la que notificar la obtencion de propiedades de la firma visible. */
	private SignPdfDialog(final Frame parentFrame, final SignPdfDialogListener spdl) {
		super(parentFrame);
		this.parent = parentFrame;
		this.listener = spdl;
		createUI();
	}

	private void createUI() {
		setTitle(SignPdfUiMessages.getString("SignPdfDialog.3")); //$NON-NLS-1$
		setIconImage(
			AutoFirmaUtil.getDefaultDialogsIcon()
		);
		getAccessibleContext().setAccessibleDescription(
			SignPdfUiMessages.getString("SignPdfDialog.2") //$NON-NLS-1$
		);
		setModalityType(ModalityType.TOOLKIT_MODAL);
		setLocationRelativeTo(this.parent);

		addWindowListener(new java.awt.event.WindowAdapter() {
		    @Override
		    public void windowClosing(final java.awt.event.WindowEvent windowEvent) {
		    	positionCancelled();
		    }
		});
	}

	/** Obtiene los par&aacute;metros adicionales de una firma visible PDF mediante
	 * un di&aacute;logo gr&aacute;fico.
	 * @param isSign <code>true</code> si el PDF de entrada ya contiene firmas electr&oacute;nicas previas,
	 *               <code>false</code> en caso contrario.
	 * @param pdf PDF al que aplicar la firma visible.
	 * @param parentFrame Marco padre para la modalidad.
	 * @param spdl Clase a la que notificar la obtencion de propiedades de la firma visible. */
	public static void getVisibleSignatureExtraParams(final boolean isSign,
													  final byte[] pdf,
			                                          final Frame parentFrame,
			                                          final SignPdfDialogListener spdl) {
		if (pdf == null || pdf.length < 3) {
			throw new IllegalArgumentException(
				"El PDF a aplicarle la firma visible no puede ser nulo ni vacio" //$NON-NLS-1$
			);
		}
		if (spdl == null) {
			throw new IllegalArgumentException(
				"La clase a la que notificar la obtencion de propiedades no puede ser nula" //$NON-NLS-1$
			);
		}

		final JDialog dialog = new SignPdfDialog(parentFrame, spdl);
		dialog.setPreferredSize(new Dimension(PREFERRED_WIDTH, PREFERRED_HEIGHT));
		final Point cp = GraphicsEnvironment.getLocalGraphicsEnvironment().getCenterPoint();
		dialog.setLocation(cp.x - PREFERRED_WIDTH/2, cp.y - PREFERRED_HEIGHT/2);
		dialog.setResizable(false);

		PdfLoader.loadPdf(
			isSign,
			pdf,
			(PdfLoaderListener) dialog
		);
	}

	@Override
	public void pdfLoaded(final boolean isSign, final List<BufferedImage> pages, final List<Dimension> pageSizes) {
		this.areaPanel = new SignPdfUiPanel(
				isSign,
				pages,
				pageSizes,
				this,
				SignPdfDialog.this
			);
		add(this.areaPanel);
		pack();
		setVisible(true);
	}

	void nextPanel(final Properties p, final BufferedImage im) {
		remove(this.areaPanel);
		final JPanel preview = new SignPdfUiPanelPreview(this, p, im);
		add(preview);
		pack();
	}

	@Override
	public void pdfLoadedFailed(final Throwable cause) {
		LOGGER.severe("Error creando la previsualizacion del PDF: " + cause); //$NON-NLS-1$
		if (cause instanceof OutOfMemoryError) {
			AOUIFactory.showErrorMessage(
					this.parent,
					SignPdfUiMessages.getString("SignPdfDialog.4"), //$NON-NLS-1$
					SignPdfUiMessages.getString("SignPdfDialog.1"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
		}
		else {
			AOUIFactory.showErrorMessage(
				this.parent,
				SignPdfUiMessages.getString("SignPdfDialog.0"), //$NON-NLS-1$
				SignPdfUiMessages.getString("SignPdfDialog.1"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE
			);
		}
		setVisible(false);
		this.listener.propertiesCreated(new Properties());
		dispose();
	}

	@Override
	public void positionSelected(final Properties extraParams) {
		setVisible(false);
		this.listener.propertiesCreated(extraParams);
		dispose();
	}

	@Override
	public void positionCancelled() {
		setVisible(false);
		this.listener.propertiesCreated(new Properties());
		dispose();
	}

	/** Define los requerimientos de las clases a las que se informa de que ya se cuenta
	 * con las propiedades de la firma visible PDF. */
	public static interface SignPdfDialogListener {

		/** Establece los par&aacute;metros adicionales de la firma visible PDF indicados por
		 * el usuario mediante el di&aacute;logo.
		 * @param extraParams Par&aacute;metros adicionales de la firma visible PDF, o un fichero
		 *                    de propiedades vac&iacute;o si al usuario cancel&oacute; la operaci&oacute;n
		 *                    o hubo un error por el que no pudieron recogerse. */
		void propertiesCreated(final Properties extraParams);
	}
}

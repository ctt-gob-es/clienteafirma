/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.pdf;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

import org.apache.pdfbox.pdmodel.encryption.InvalidPasswordException;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.standalone.DesktopUtil;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.pdf.PdfLoader.PdfLoaderListener;
import es.gob.afirma.standalone.ui.pdf.SignPdfUiPanel.SignPdfUiPanelListener;

/**
 * Di&aacute;logo para la obtenci&oacute;n de los datos de firma PDF Visible.
 *
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s.
 */
public final class SignPdfDialog extends JDialog implements PdfLoaderListener, SignPdfUiPanelListener {

	private static final long serialVersionUID = -7987676963743094243L;

	private static final int PREFERRED_WIDTH = 500;
	private static final int PREFERRED_HEIGHT = 800;

	private static final int LOWER_LIMIT_X = 30;
	private static final int LOWER_LIMIT_Y = 45;
	private static final int AREA_LIMIT = 1350;

	private static final String MESSAGE_REFERENCE_NEED_PASSWORD = "pdfpasswordprotected"; //$NON-NLS-1$
	private static final String MESSAGE_REFERENCE_BAD_PASSWORD = "pdfbadpassword"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final Frame parent;
	private final SignPdfDialogListener listener;

	private JScrollPane scrollPanel;
	private JPanel activePanel;

	SignPdfDialogListener getListener() {
		return this.listener;
	}

	private boolean isPdfSign;
	private boolean isMassiveSign;
	private byte[] pdfData;
	private final boolean customAppearance;

	private final boolean signatureVisible;
	private final boolean stampVisible;

	/**
	 * Construye un di&aacute;logo para la obtenci&oacute;n de los datos de firma
	 * PDF Visible.
	 *
	 * @param parentFrame      Marco padre para la modalidad.
	 * @param spdl             Clase a la que notificar la obtencion de propiedades
	 *                         de la firma visible.
	 * @param signatureVisible Indica si se va a insertar una firma.
	 * @param customAppearance Indica si se va a establecer una apariencia distinta a la por defecto.
	 * @param stampVisible     Indica si se va a insertar una marca.
	 */
	private SignPdfDialog(final Frame parentFrame, final SignPdfDialogListener spdl, final boolean signatureVisible,
			final boolean customAppearance, final boolean stampVisible) {
		super(parentFrame);
		this.parent = parentFrame;
		this.listener = spdl;
		this.signatureVisible = signatureVisible;
		this.stampVisible = stampVisible;
		this.customAppearance = customAppearance;
		createUI();
	}

	private void createUI() {
		setTitle(SignPdfUiMessages.getString("SignPdfDialog.3")); //$NON-NLS-1$
		setIconImages(DesktopUtil.getIconImages());
		getAccessibleContext().setAccessibleDescription(SignPdfUiMessages.getString("SignPdfDialog.2") //$NON-NLS-1$
		);
		setModalityType(ModalityType.TOOLKIT_MODAL);
		setLocationRelativeTo(this.parent);

		this.scrollPanel = new JScrollPane();
		this.scrollPanel.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		this.scrollPanel.getHorizontalScrollBar().setUnitIncrement(16);
		this.scrollPanel.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
		this.scrollPanel.getVerticalScrollBar().setUnitIncrement(16);
		add(this.scrollPanel);

		addWindowListener(new java.awt.event.WindowAdapter() {
			@Override
			public void windowClosing(final java.awt.event.WindowEvent windowEvent) {
				positionCancelled();
			}
		});

	}

	/**
	 * Muestra un di&aacute;logo para la selecci&oacute;n de las propiedades de firma visible PDF
	 * que aplicar a la firma.
	 *
	 * @param isSign           {@code true} si el PDF de entrada ya contiene
	 *                         firmas electr&oacute;nicas previas,
	 *                         {@code false} en caso contrario.
	 * @param isMassiveSign    {@code true} si los datos se van a utilizar para una
	 * 						   operaci&oacute;n masiva, {@code false} en caso contrario.
	 * @param pdf              PDF al que aplicar la firma visible.
	 * @param parentFrame      Marco padre para la modalidad.
	 * @param signatureVisible Indica si se va a insertar una firma.
	 * @param customAppearance Indica si se va a establecer una apariencia distinta a la por defecto.
	 * @param stampVisible     Indica si se va a insertar una marca.
	 * @param spdl             Clase a la que notificar la configuraci&oacute;n de firma obtenida.
	 */
	public static void getVisibleSignatureExtraParams(final boolean isSign, final boolean isMassiveSign, final byte[] pdf, final Frame parentFrame,
			final boolean signatureVisible, final boolean customAppearance, final boolean stampVisible,
			final SignPdfDialogListener spdl) {
		if (pdf == null || pdf.length < 3) {
			throw new IllegalArgumentException("El PDF a aplicarle la firma visible no puede ser nulo ni vacio" //$NON-NLS-1$
			);
		}
		if (spdl == null) {
			throw new IllegalArgumentException(
					"La clase a la que notificar la obtencion de propiedades no puede ser nula" //$NON-NLS-1$
			);
		}

		final JDialog dialog = new SignPdfDialog(parentFrame, spdl, signatureVisible, customAppearance, stampVisible);
		dialog.setPreferredSize(getPreferredDimensionToSignatureDialog());
		final Point cp = GraphicsEnvironment.getLocalGraphicsEnvironment().getCenterPoint();
		dialog.setLocation(cp.x - (int) dialog.getPreferredSize().getWidth() / 2,
				cp.y - (int) dialog.getPreferredSize().getHeight() / 2);

		PdfLoader.loadPdf(isSign, isMassiveSign, pdf, (PdfLoaderListener) dialog);
	}

	/**
	 * Obtiene un di&aacute;logo para la selecci&oacute;n de las propiedades de firma visible PDF
	 * que aplicar a la firma.
	 *
	 * @param isSign           <code>true</code> si el PDF de entrada ya contiene
	 *                         firmas electr&oacute;nicas previas,
	 *                         <code>false</code> en caso contrario.
	 * @param isMassiveSign    <code>true</code> si es una operaci&oacute;n de firma masiva,
	 *                         <code>false</code> en caso contrario.
	 * @param pdf              PDF al que aplicar la firma visible.
	 * @param parentFrame      Marco padre para la modalidad.
	 * @param signatureVisible Indica si se va a insertar una firma
	 * @param stampVisible     Indica si se va a insertar una marca
	 * @param customAppearance Indica si se va a establecer una apariencia distinta a la por defecto.
	 * @param spdl             Clase a la que notificar la obtencion de propiedades
	 *                         de la firma visible.
	 * @return El di&aacute;logo generado.
	 */
	public static JDialog getVisibleSignatureDialog(final boolean isSign, final boolean isMassiveSign, final byte[] pdf, final Frame parentFrame,
			final boolean signatureVisible, final boolean customAppearance, final boolean stampVisible,
			final SignPdfDialogListener spdl) {
		if (pdf == null || pdf.length < 3) {
			throw new IllegalArgumentException("El PDF a aplicarle la firma visible no puede ser nulo ni vacio" //$NON-NLS-1$
			);
		}
		if (spdl == null) {
			throw new IllegalArgumentException(
					"La clase a la que notificar la obtencion de propiedades no puede ser nula" //$NON-NLS-1$
			);
		}

		final JDialog dialog = new SignPdfDialog(parentFrame, spdl, signatureVisible, customAppearance, stampVisible);
		dialog.setPreferredSize(getPreferredDimensionToSignatureDialog());
		final Point cp = GraphicsEnvironment.getLocalGraphicsEnvironment().getCenterPoint();
		dialog.setLocation(cp.x - (int) dialog.getPreferredSize().getWidth() / 2,
				cp.y - (int) dialog.getPreferredSize().getHeight() / 2);

		PdfLoader.loadPdf(isSign, isMassiveSign, pdf, (PdfLoaderListener) dialog);
		return dialog;
	}

	private List<BufferedImage> pages;
	private List<Dimension> pageSizes;

	@Override
	public void pdfLoaded(final boolean isSign, final boolean isMassive,
			final byte[] pdf) throws IOException {

		this.isPdfSign = isSign;
		this.isMassiveSign = isMassive;
		this.pdfData = pdf;

		char[] password = null;
		final Properties initialParams = new Properties();
		do {
			try {
				this.pages = Pdf2ImagesConverter.pdf2ImagesUsefulSections(this.pdfData, password, 0);
			}
			catch (final InvalidPasswordException e) {
				// Si no se pudo abrir porque requiere contrasena, la pedimos, teniendo en cuenta si lo
				// hemos hecho antes para usar un mensaje u otro
				final String msgRequest = password == null ? MESSAGE_REFERENCE_NEED_PASSWORD : MESSAGE_REFERENCE_BAD_PASSWORD;
    			try {
    				password = AOUIFactory.getPassword(SimpleAfirmaMessages.getString(msgRequest), this.parent);
    			}
    			catch (final AOCancelledOperationException ce) {
    				throw e;
    			}
			}
			catch (final IOException e) {
				throw e;
			}
		}
		while (this.pages == null);

		this.pageSizes = SignPdfUiUtil.getPageSizes(this.pdfData, password);

		if (password != null) {
			initialParams.setProperty(PdfExtraParams.OWNER_PASSWORD_STRING, new String(password));
		}

		if (this.signatureVisible) {
			setPreferredSize(getPreferredDimensionToSignatureDialog());
			this.activePanel = new SignPdfUiPanel(this.isPdfSign, this.isMassiveSign, this.pages, this.pageSizes, this.pdfData, initialParams, this);
			this.scrollPanel.setViewportView(this.activePanel);
		} else if (this.stampVisible) {
			setPreferredSize(getPreferredDimensionToStampDialog());
			setTitle(SignPdfUiMessages.getString("SignPdfUiStamp.0")); //$NON-NLS-1$
			this.activePanel = new SignPdfUiPanelStamp(this.pages, this.pageSizes, this.pdfData, this,
					initialParams);
			((SignPdfUiPanelStamp) this.activePanel).setDialogParent(this);
			this.scrollPanel.setViewportView(this.activePanel);
		}
		pack();

		setVisible(true);
	}

	private static Dimension getPreferredDimensionToSignatureDialog() {
		final double screenHeight = LookAndFeelManager.getScreenSize().getHeight();
		return new Dimension(PREFERRED_WIDTH, (int) Math.min(PREFERRED_HEIGHT, screenHeight * 0.9));
	}

	private static Dimension getPreferredDimensionToStampDialog() {
		final double screenHeight = LookAndFeelManager.getScreenSize().getHeight();
		return new Dimension(PREFERRED_WIDTH, (int) Math.min(PREFERRED_HEIGHT + 60, screenHeight * 0.9));
	}

	@Override
	public void pdfLoadedFailed(final Throwable cause) {
		LOGGER.severe("Error creando la previsualizacion del PDF: " + cause); //$NON-NLS-1$
		if (cause instanceof OutOfMemoryError) {
			AOUIFactory.showErrorMessage(SignPdfUiMessages.getString("SignPdfDialog.4"), //$NON-NLS-1$
					SignPdfUiMessages.getString("SignPdfDialog.1"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					cause);
		} else {
			AOUIFactory.showErrorMessage(SignPdfUiMessages.getString("SignPdfDialog.0"), //$NON-NLS-1$
					SignPdfUiMessages.getString("SignPdfDialog.1"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE,
					cause);
		}
		setVisible(false);
		this.listener.propertiesCreated(new Properties());
		dispose();
	}

	@Override
	public void nextPanel(final Properties p, final BufferedImage im) {

		// Si estamos en el panel de seleccion de la posicion de firma:
		//   1.- Comprobamos que el area seleccionada sea correcta.
		//       - Si es correcta, continuamos.
		//       - Si no, no hacemos nada y seguimos en el mismo panel.
		//   2.- Comprobamos si hay que configurar la apariencia de la firma.
		//       - Si hay que configurarla, cargamos el panel de configuracion.
		//       - Si no, cerramos el panel.
		if (this.activePanel instanceof SignPdfUiPanel) {
			if (checkSignatureDimensions(p)) {
				if (this.customAppearance) {
					setPreferredSize(getPreferredDimensionToSignatureDialog());
					this.activePanel = new SignPdfUiPanelPreview(this, p, im);
					((SignPdfUiPanelPreview) this.activePanel).setDialogParent(this);

					// Comprobamos la imagen precargada, en caso de que exista, para avisar si
					// contiene transparencias
					if (((SignPdfUiPanelPreview) this.activePanel).checkRubricTransparency()) {
			        	AOUIFactory.showMessageDialog(
			        			this,
								SignPdfUiMessages.getString("SignPdfDialog.9"),  //$NON-NLS-1$
								SignPdfUiMessages.getString("SignPdfDialog.8"),  //$NON-NLS-1$
			                    JOptionPane.WARNING_MESSAGE,
			                    null
			                );
					}

					this.scrollPanel.setViewportView(this.activePanel);
					pack();
					((SignPdfUiPanelPreview) this.activePanel).requestFocusInWindow();
				}
				else {
					positionSelected(p);
				}
			}
		}
		// Si estamos en el panel de configuracion de la apariencia de la firma, pero
		// tambien hay que configurar la posicion de una imagen en el PDF, se muestra
		// la pantalla seleccion de la posicion de la imagen.
		else if (this.activePanel instanceof SignPdfUiPanelPreview && this.stampVisible) {
			setPreferredSize(getPreferredDimensionToStampDialog());
			this.activePanel = new SignPdfUiPanelStamp(this.pages, this.pageSizes, this.pdfData, this, p);
			((SignPdfUiPanelStamp) this.activePanel).setDialogParent(this);
			this.scrollPanel.setViewportView(this.activePanel);
			pack();
			((SignPdfUiPanelStamp) this.activePanel).requestFocusInWindow();
		}
		// En cualquier otro caso, damos por terminada la configuracion.
		else {
			positionSelected(p);
		}
	}

	private boolean checkSignatureDimensions(final Properties extraParams) {
		boolean res = true;
		final int lowerLeftX = Integer.parseInt(extraParams.getProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTX));
		final int lowerLeftY = Integer.parseInt(extraParams.getProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTY));
		final int upperRightX = Integer.parseInt(extraParams.getProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTX));
		final int upperRightY = Integer.parseInt(extraParams.getProperty(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTY));
		final int dimensionX = Math.abs(upperRightX - lowerLeftX);
		final int dimensionY = Math.abs(upperRightY - lowerLeftY);

		final boolean limitSizeCondition = dimensionX < LOWER_LIMIT_X || dimensionY < LOWER_LIMIT_Y;
		final boolean limitAreaCondition = dimensionX * dimensionY < AREA_LIMIT;
		if (limitSizeCondition || limitAreaCondition) {
			final String msg = SignPdfUiMessages.getString("SignPdfDialog.6"); //$NON-NLS-1$
			final String title = SignPdfUiMessages.getString("SignPdfDialog.7"); //$NON-NLS-1$
			transferFocus();
			AOUIFactory.showErrorMessage(this, msg, title, AOUIFactory.ERROR_MESSAGE, null);
			res = false;
		}
		return res;
	}

	@Override
	public void positionSelected(final Properties extraParams) {
		setVisible(false);
		LOGGER.info("Propiedades establecidas mediante GUI: " + extraParams); //$NON-NLS-1$
		this.listener.propertiesCreated(extraParams);
		dispose();
	}

	@Override
	public void positionCancelled() {
		setVisible(false);
		this.listener.propertiesCreated(new Properties());
		dispose();
	}

	@Override
	public Frame getParent() {
		return this.parent;
	}

	/**
	 * Define los requerimientos de las clases a las que se informa de que ya se
	 * cuenta con las propiedades de la firma visible PDF.
	 */
	public interface SignPdfDialogListener {

		/**
		 * Establece los par&aacute;metros adicionales de la firma visible PDF indicados
		 * por el usuario mediante el di&aacute;logo.
		 *
		 * @param extraParams Par&aacute;metros adicionales de la firma visible PDF, o
		 *                    un fichero de propiedades vac&iacute;o si al usuario
		 *                    cancel&oacute; la operaci&oacute;n o hubo un error por el
		 *                    que no pudieron recogerse.
		 */
		void propertiesCreated(final Properties extraParams);
	}
}

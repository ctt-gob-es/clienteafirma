/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.pdf;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import org.apache.pdfbox.pdmodel.encryption.InvalidPasswordException;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.signers.pades.PdfUtil.SignatureField;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.standalone.DesktopUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.pdf.PdfLoader.PdfLoaderListener;
import es.gob.afirma.standalone.ui.pdf.SignPdfDialog.SignPdfDialogListener;
import es.gob.afirma.standalone.ui.pdf.SignPdfUiPanel.SignPdfUiPanelListener;

/** Di&aacute;logo de selecci&oacute;n de campo de firma PDF visible.
 * @author Mariano Mart&iacute;nez. */
public final class PdfEmptySignatureFieldsChooserDialog extends JDialog implements SignPdfUiPanelListener, PdfLoaderListener{

	private static final long serialVersionUID = 4904673758490090781L;

	private static final int PREFERRED_WIDTH = 500;
	private static final int PREFERRED_HEIGHT = 615;

	private static final String MESSAGE_REFERENCE_NEED_PASSWORD = "pdfpasswordprotected"; //$NON-NLS-1$
	private static final String MESSAGE_REFERENCE_BAD_PASSWORD = "pdfbadpassword"; //$NON-NLS-1$

	private final SignPdfDialogListener listener;
	SignPdfDialogListener getListener() {
		return this.listener;
	}

	private final byte[] pdf;
	private final boolean isSign;

	private JPanel activePanel;

	private final Properties extraParams;
	private final boolean stampVisible;

	private final SignatureField field;
	SignatureField getField() {
		return this.field;
	}

	/** Inicia el proceso de creaci&oacute;n de di&aacute;logo de selecci&oacute;n del campo de firma PDF visible.
	 * @param pdf PDF de entrada.
	 * @param isSign Indica si el pdf de entrada esta firmado
	 * @param parent Componente padre para la modalidad.
	 * @param field Campos de firma seleccionado.
	 * @param signPdfDialogListener Clase a la que hay que notificar la selecci&oacute;n de propiedades de firma visible PDF.
	 * @param stampVisible Indica si se va a insertar una marca visible */
	public static void startPdfEmptySignatureFieldsChooserDialog(final byte[] pdf,
																 final boolean isSign,
																 final Frame parent,
																 final SignatureField field,
																 final SignPdfDialogListener signPdfDialogListener,
																 final boolean stampVisible) {

		final PdfEmptySignatureFieldsChooserDialog tsd = new PdfEmptySignatureFieldsChooserDialog(
			pdf,
			isSign,
			parent,
			field,
			signPdfDialogListener,
			stampVisible
		);
		tsd.setSize(PREFERRED_WIDTH, PREFERRED_HEIGHT);
		tsd.setLocationRelativeTo(parent);
	}

	private PdfEmptySignatureFieldsChooserDialog(final byte[] pdf,
												 final boolean isSign,
												 final Frame parentFrame,
												 final SignatureField fld,
												 final SignPdfDialogListener spul,
												 final boolean stampVisible) {
		super(parentFrame);
		this.listener = spul;
		this.pdf = pdf;
		this.isSign = isSign;
		this.field = fld;
		this.stampVisible = stampVisible;
		setTitle(SignPdfUiMessages.getString("SignPdfDialog.3")); //$NON-NLS-1$
		setModalityType(ModalityType.APPLICATION_MODAL);

		this.extraParams = new Properties();
		this.extraParams.put(PdfExtraParams.SIGNATURE_FIELD, getField().getName());
		this.extraParams.put(PdfExtraParams.SIGNATURE_PAGE, Integer.toString(getField().getPage()));
		this.extraParams.put(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTX, Integer.toString(getField().getSignaturePositionOnPageLowerLeftX()));
		this.extraParams.put(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTY, Integer.toString(getField().getSignaturePositionOnPageLowerLeftY()));
		this.extraParams.put(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTX, Integer.toString(getField().getSignaturePositionOnPageUpperRightX()));
		this.extraParams.put(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTY, Integer.toString(getField().getSignaturePositionOnPageUpperRightY()));

		SwingUtilities.invokeLater(
			() -> createUI()
		);
	}

	void createUI() {
		final Container c = getContentPane();
		final GridBagLayout gbl = new GridBagLayout();
		c.setLayout(gbl);
		final GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;

        setIconImages(DesktopUtil.getIconImages());
		getAccessibleContext().setAccessibleDescription(
			SignPdfUiMessages.getString("SignPdfFieldChooser.1") //$NON-NLS-1$
		);

		addWindowListener(
			new java.awt.event.WindowAdapter() {
			    @Override
			    public void windowClosing(final java.awt.event.WindowEvent windowEvent) {
			    	positionCancelled();
			    }
			}
		);

		try {
			final BufferedImage im = getFragmentImage(getField());
			if(im!=null) {
				nextPanel(this.extraParams, im);
			}
			else {
				throw new IOException("Error creando la imagen para previsualizar"); //$NON-NLS-1$
			}
		}
		catch (final Exception e1) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"No ha sido posible cargar la previsualizacion del acrocampo: " + e1//$NON-NLS-1$
			);
			AOUIFactory.showErrorMessage(
                  SignPdfUiMessages.getString("SignPdfFieldChooser.2"), //$NON-NLS-1$
                  SignPdfUiMessages.getString("SignPdfFieldChooser.3"), //$NON-NLS-1$
                  JOptionPane.ERROR_MESSAGE,
                  e1
            );
			setVisible(false);
			getListener().propertiesCreated(new Properties());
			dispose();
		}
	}

	BufferedImage getFragmentImage(final SignatureField sf) throws IOException {

		final int currentPage = sf.getPage() - 1;

		char[] password = null;
		List<BufferedImage> pages = null;
		do {
			try {
				pages = Pdf2ImagesConverter.pdf2ImagesUsefulSections(this.pdf, password, currentPage);
			}
			catch (final InvalidPasswordException e) {
				// Si no se pudo abrir porque requiere contrasena, la pedimos, teniendo en cuenta si lo
				// hemos hecho antes para usar un mensaje u otro
				final String msgRequest = password == null ? MESSAGE_REFERENCE_NEED_PASSWORD : MESSAGE_REFERENCE_BAD_PASSWORD;
    			try {
    				password = AOUIFactory.getPassword(SimpleAfirmaMessages.getString(msgRequest), this);
    			}
    			catch (final AOCancelledOperationException ce) {
    				throw e;
    			}
			}
			catch (final IOException e) {
				throw e;
			}
		}
		while (pages == null);

		final BufferedImage page = pages.get(currentPage);

		final int uxr = sf.getSignaturePositionOnPageUpperRightX();
		final int uyr = sf.getSignaturePositionOnPageUpperRightY();
		final int lxl = sf.getSignaturePositionOnPageLowerLeftX();
		final int lyl = sf.getSignaturePositionOnPageLowerLeftY();

		return page.getSubimage(lxl, uyr, uxr - lxl, uyr - lyl);
	}

	@Override
	public void nextPanel(final Properties p, final BufferedImage im) {
		if(this.activePanel == null) {
			getContentPane().removeAll();
			final GridBagConstraints constraints = new GridBagConstraints();
			constraints.fill = GridBagConstraints.BOTH;
			constraints.weightx = 1.0;
			constraints.weighty = 1.0;
			constraints.insets = new Insets(0, 0, 0, 0);
			this.activePanel = new SignPdfUiPanelPreview(this, p, im);
			((SignPdfUiPanelPreview) this.activePanel).setDialogParent(this);
			getContentPane().add(this.activePanel, constraints);
			setVisible(true);

			((SignPdfUiPanelPreview) this.activePanel).requestFocusInWindow();
		}
		else if(this.activePanel instanceof SignPdfUiPanelPreview && this.stampVisible)
		{
			PdfLoader.loadPdf(
				this.isSign,
				false,
				this.pdf,
				this
			);
		}
		else {
			positionSelected(p);
		}
	}

	@Override
	public void positionSelected(final Properties params) {
		setVisible(false);
		this.listener.propertiesCreated(params);
		dispose();
	}

	@Override
	public void positionCancelled() {
		setVisible(false);
		this.listener.propertiesCreated(new Properties());
		dispose();
	}

	/**
	 * Muestra un di&aacute;logo gr&aacute;fico para la selecci&oacute;n de un campo de firma en un PDF.
	 * @param emptySignatureFields Listado de campos de firma del PDF.
	 * @return Campo de firma seleccionado o {@code null} si debe crearse uno nuevo.
	 * @throws AOCancelledOperationException Se cancela por completo la operaci&oacute;n de firma.
	 */
	public static SignatureField selectField(final List<SignatureField> emptySignatureFields) throws AOCancelledOperationException {

	    final String fieldsMessage = SignPdfUiMessages.getString("SignPdfFieldChooser.7"); //$NON-NLS-1$

		final JComboBox<Object> combo = new JComboBox<>(emptySignatureFields.toArray());
		final JLabel label = new JLabel(fieldsMessage);

		final JPanel panel = new JPanel(new GridBagLayout());
		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 1.0;
		c.gridy = 0;
		panel.add(label, c);
		c.gridy++;
		panel.add(combo, c);
		c.fill = GridBagConstraints.BOTH;
		c.weighty = 1.0;
		panel.add(new JLabel(), c);

		final String[] options = {
			SignPdfUiMessages.getString("SignPdfFieldChooser.4"), //$NON-NLS-1$
			SignPdfUiMessages.getString("SignPdfFieldChooser.6"), //$NON-NLS-1$
			SignPdfUiMessages.getString("SignPdfFieldChooser.5") //$NON-NLS-1$
		};

		final String title = SimpleAfirmaMessages.getString("SignPanelSignTask.1"); //$NON-NLS-1$
		final int selection = JOptionPane.showOptionDialog(
			null,
			panel,
			title,
			JOptionPane.DEFAULT_OPTION,
			JOptionPane.QUESTION_MESSAGE,
			null,
			options,
			options[0]
		);

		// Se ha pulsado en el boton cancelar firma
		if (selection == 2) {
			throw new AOCancelledOperationException();
		}
		// Se ha seleccionado un campo
		if (selection == 0) {
			return (SignatureField) combo.getSelectedItem();
		}
		// Se debe crear un campo nuevo
		return null;
	}

	@Override
	public void pdfLoaded(final boolean signed, final boolean isMassiveSign, final byte[] document) throws IOException {
		getContentPane().remove(this.activePanel);

		char[] password = null;
		List<BufferedImage> pages = null;
		final Properties initialParams = new Properties();
		do {
			try {
				pages = Pdf2ImagesConverter.pdf2ImagesUsefulSections(document, password, 0);
			}
			catch (final InvalidPasswordException e) {
				// Si no se pudo abrir porque requiere contrasena, la pedimos, teniendo en cuenta si lo
				// hemos hecho antes para usar un mensaje u otro
				final String msgRequest = password == null ? MESSAGE_REFERENCE_NEED_PASSWORD : MESSAGE_REFERENCE_BAD_PASSWORD;
    			try {
    				password = AOUIFactory.getPassword(SimpleAfirmaMessages.getString(msgRequest), this);
    			}
    			catch (final AOCancelledOperationException ce) {
    				throw e;
    			}
			}
			catch (final IOException e) {
				throw e;
			}
		}
		while (pages == null);

		final List<Dimension> pageSizes = SignPdfUiUtil.getPageSizes(document, password);

		if (password != null) {
			initialParams.setProperty(PdfExtraParams.OWNER_PASSWORD_STRING, new String(password));
		}

		this.activePanel = new SignPdfUiPanelStamp(
			pages,
			pageSizes,
			document,
			this,
			new Properties()
		);
		getContentPane().add(this.activePanel);
	}

	@Override
	public void pdfLoadedFailed(final Throwable cause) {
		Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"Error creando la previsualizacion del PDF: " + cause); //$NON-NLS-1$
		if (cause instanceof OutOfMemoryError) {
			AOUIFactory.showErrorMessage(
				SignPdfUiMessages.getString("SignPdfDialog.4"), //$NON-NLS-1$
				SignPdfUiMessages.getString("SignPdfDialog.1"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE,
				cause
			);
		}
		else {
			AOUIFactory.showErrorMessage(
				SignPdfUiMessages.getString("SignPdfDialog.0"), //$NON-NLS-1$
				SignPdfUiMessages.getString("SignPdfDialog.1"), //$NON-NLS-1$
				JOptionPane.ERROR_MESSAGE,
				cause
			);
		}
		setVisible(false);
		this.listener.propertiesCreated(new Properties());
		dispose();
	}
}

/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.pdf;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.SwingConstants;
import javax.swing.border.TitledBorder;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.ui.pdf.PageLabel.PageLabelListener;
import es.gob.afirma.standalone.ui.pdf.SignPdfUiPanel.SignPdfUiPanelListener;

final class SignPdfUiPanelStamp extends JPanel implements KeyListener, PageLabelListener, ActionListener {

	private static final long serialVersionUID = -4465164058611491582L;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final int PREFERRED_WIDTH = 466;
	private static final int PREFERRED_HEIGHT = 410;
	static final String IMAGE_EXT[] = {"jpg", "jpeg", "png", "gif"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

	private final Properties extraParams;

	private Properties extraParamsForLocation = null;
	Properties getExtraParamsForLocation() {
		return this.extraParamsForLocation;
	}
	private void setProperties(final Properties p) {
		this.extraParamsForLocation = p;
	}

	private int currentPage = 1;
	int getCurrentPage() {
		return this.currentPage;
	}

	private int currentScale = 100;

	private final SignPdfUiPanelListener listener;
	SignPdfUiPanelListener getListener() {
		return this.listener;
	}

	private JPanel pagePanel;
	private List<BufferedImage> pdfPages;

	private final List<Dimension> pdfPageSizes;
	private JLabel pageLabel;
	private final JButton okButton = new JButton(SignPdfUiMessages.getString("SignPdfUiStamp.8")); //$NON-NLS-1$
	private final JTextField posX = new JTextField(4);
	private final JTextField posY = new JTextField(4);
	private final JLabel indexLabel = new JLabel();

	private final JButton firstPageButton = new JButton("<<"); //$NON-NLS-1$
	private final JButton previousPageButton = new JButton("<"); //$NON-NLS-1$
	private final JButton nextPageButton = new JButton(">"); //$NON-NLS-1$
	private final JButton lastPageButton = new JButton(">>"); //$NON-NLS-1$
	private final JToggleButton allPagesButton = new JToggleButton(SignPdfUiMessages.getString("SignPdfUiStamp.7")); //$NON-NLS-1$

	private final PdfDocument pdfDocument;
	private int pressButton = 0;

	SignPdfUiPanelStamp(final boolean isSign,
			   final List<BufferedImage> pages,
			   final List<Dimension> pageSizes,
			   final byte[] pdf,
		       final SignPdfUiPanelListener spul,
		       final Properties extraParams) {

		if (pages == null || pages.isEmpty()) {
			throw new IllegalArgumentException(
				"La lista de paginas no puede ser nula ni vacia" //$NON-NLS-1$
			);
		}
		if (pageSizes == null || pages.size() != pageSizes.size()) {
			throw new IllegalArgumentException(
				"Las dimensiones de las paginas tienen que corresponderse con la lista de paginas proporcionada" //$NON-NLS-1$
			);
		}
		if (spul == null) {
			throw new IllegalArgumentException(
				"La clase a la que notificar la seleccion no puede ser nula" //$NON-NLS-1$
			);
		}
		this.pdfPages = pages;
		this.pdfPageSizes = pageSizes;
		this.listener = spul;
		this.extraParams = extraParams;

		this.pdfDocument = new PdfDocument();
		this.pdfDocument.setBytesPdf(pdf);

		createUI();
	}

	private JLabel createPageLabel(final BufferedImage page,
				final PageLabelListener pll,
	            final KeyListener kl,
	            final Component parentFrame,
	            final Dimension pdfPageOriginalDimension) {

		int pageWidth = page.getWidth();
		int pageHeight = page.getHeight();

		final Dimension screen = parentFrame.getPreferredSize();

		// Si la previsualizacion de la pagina es mayor que la propia pantalla,
		// hacemos reducciones incrementales del 10%
		while (pageWidth > screen.width || pageHeight > screen.height) {
			pageWidth = (int) Math.round(pageWidth * 0.9);
			pageHeight = (int) Math.round(pageHeight * 0.9);
		}

		// Comprobacion de si las paginas estan rotadas
		final float pdfRatio;
		if (page.getWidth() <= page.getHeight() && pdfPageOriginalDimension.width <= pdfPageOriginalDimension.height) {
			// No rotada
			pdfRatio = (float) pdfPageOriginalDimension.height / pageHeight;
		}
		else {
			// Pagina rotada
			pdfRatio = (float) pdfPageOriginalDimension.width / pageWidth;
		}

		final float aspectRatio = (float) pageWidth / page.getWidth();
		this.currentScale = Math.round(100 * aspectRatio);

		// Recalculamos las dimensiones para asegurarnos de que los sucesivos redondeos del reescalado
		// no han provocado que se altere la proporcion de altura y anchura de la pagina
		pageWidth = Math.round(page.getWidth() * aspectRatio);
		pageHeight = Math.round(page.getHeight() * aspectRatio);

		final JLabel ret = new PageLabel(
			page.getScaledInstance(pageWidth, pageHeight, Image.SCALE_SMOOTH),
			pageWidth,
			pageHeight,
			pll,
			pdfRatio
		);

		ret.addKeyListener(kl);
		ret.setFocusable(false);

		ret.getAccessibleContext().setAccessibleDescription(
			SignPdfUiMessages.getString("SignPdfUiStamp.5") //$NON-NLS-1$
		);

		return ret;
	}

	void createUI() {
		addKeyListener(this);

		getAccessibleContext().setAccessibleDescription(
			SignPdfUiMessages.getString("SignPdfUiStamp.17") //$NON-NLS-1$
		);

		setLayout(new GridBagLayout());

		final GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.BOTH;
		gbc.insets = new Insets(10, 10, 10, 10);
		gbc.weightx = 1.0;
		gbc.weighty = 1.0;
		gbc.gridy = 0;

		final JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new GridBagLayout());
		final TitledBorder tb = BorderFactory.createTitledBorder(SignPdfUiMessages.getString("SignPdfUiStamp.2")); //$NON-NLS-1$
		tb.setTitleFont(getFont().deriveFont(Font.PLAIN));
		mainPanel.setBorder(tb);

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.anchor = GridBagConstraints.PAGE_START;
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.gridy = 0;

		mainPanel.add(createCoordenatesPanel(), c);

		c.gridy++;
		c.ipady = 3;
		c.fill = GridBagConstraints.BOTH;

		mainPanel.add(createPagePanel(), c);

		c.ipady = 3;
		c.gridy++;
		c.anchor = GridBagConstraints.PAGE_END;
		c.fill = GridBagConstraints.HORIZONTAL;
		mainPanel.add(createPaginationPanel(), c);

		add(mainPanel, gbc);

		gbc.gridy++;
		gbc.weighty = 0.0;
		add(createButtonsPanel(), gbc);
		enableButtons();
	}

	/** Crea el panel con la previsualizaci&oacute;n de las p&aacute;ginas del PDF.
	 * @return Panel con la previsualizaci&oacute;n. */
	private JPanel createPagePanel() {

		this.pagePanel = new JPanel();
		this.pagePanel.setLayout(new GridBagLayout());
		this.pagePanel.setPreferredSize(new Dimension(PREFERRED_WIDTH, PREFERRED_HEIGHT));

		// Creamos la etiqueta y establecemos la primera pagina
		this.pageLabel = createPageLabel(
			this.pdfPages.get(0),
			this,
			this,
			this.pagePanel,
			this.pdfPageSizes.get(0)
		);

		this.pagePanel.add(this.pageLabel);

		return this.pagePanel;
	}

	/** Crea el panel para la paginaci&oacute;n del PDF.
	 * @return Panel para la paginaci&oacute;n del PDF. */
	private JPanel createPaginationPanel() {

		final JPanel panel = new JPanel();
		panel.setLayout(new BoxLayout(panel,BoxLayout.PAGE_AXIS));
		panel.setAlignmentY(CENTER_ALIGNMENT);
		
		final JPanel selectPagePanel = new JPanel();
		selectPagePanel.setLayout(new FlowLayout(FlowLayout.CENTER));

		final JPanel selectAllPanel = new JPanel();
		selectAllPanel.setLayout(new FlowLayout(FlowLayout.CENTER));

		this.indexLabel.setText(
			SignPdfUiMessages.getString(
				"SignPdfUiStamp.6", //$NON-NLS-1$
				Integer.toString(getCurrentPage()),
				Integer.toString(this.pdfPages.size()),
				Integer.toString(this.currentScale)
			)
		);
		this.indexLabel.setHorizontalAlignment(SwingConstants.CENTER);
		this.indexLabel.setFocusable(false);
		this.indexLabel.addKeyListener(this);

		this.firstPageButton.addActionListener(this);
		this.firstPageButton.addKeyListener(this);
		this.previousPageButton.addActionListener(this);
		this.previousPageButton.addKeyListener(this);
		this.nextPageButton.addActionListener(this);
		this.nextPageButton.addKeyListener(this);
		this.lastPageButton.addActionListener(this);
		this.lastPageButton.addKeyListener(this);

		selectPagePanel.add(this.firstPageButton);
		selectPagePanel.add(this.previousPageButton);
		selectPagePanel.add(this.indexLabel);
		selectPagePanel.add(this.nextPageButton);
		selectPagePanel.add(this.lastPageButton);

		allPagesButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				if(allPagesButton.isSelected())
				{
					firstPageButton.setEnabled(false);
					previousPageButton.setEnabled(false);
					nextPageButton.setEnabled(false);
					lastPageButton.setEnabled(false);
				}
				else {
					enableButtons();
				}
			}
		});
		allPagesButton.getAccessibleContext().setAccessibleDescription(
				SignPdfUiMessages.getString("SignPdfUiStamp.16") //$NON-NLS-1$
			);

		selectAllPanel.add(this.allPagesButton);

		panel.add(selectPagePanel);
		panel.add(selectAllPanel);

		return panel;
	}

	/** Crea el panel con los elementos que muestran las coordenadas del cursor dentro
	 * del panel de firma.
	 * @return Panel con los componentes para la visualizacion de coordenadas. */
	private JPanel createCoordenatesPanel() {

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.CENTER));

		panel.add(new JLabel(SignPdfUiMessages.getString("SignPdfUiStamp.3"))); //$NON-NLS-1$

		this.posX.setEnabled(false);
		this.posX.setFocusable(false);
		this.posX.addKeyListener(this);
		panel.add(this.posX);

		panel.add(new JLabel(SignPdfUiMessages.getString("SignPdfUiStamp.4"))); //$NON-NLS-1$

		this.posY.setEnabled(false);
		this.posY.setFocusable(false);
		this.posY.addKeyListener(this);
		panel.add(this.posY);

		return panel;
	}

	String getInsertImageBase64(BufferedImage bi) throws IOException{
		try (final ByteArrayOutputStream osImage = new ByteArrayOutputStream()) {
			ImageIO.write(bi, "jpg", osImage); //$NON-NLS-1$
			return Base64.encode(osImage.toByteArray());
		}
        catch (final IOException e1) {
        	Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"No ha sido posible pasar la imagen a JPG: " + e1 //$NON-NLS-1$
			);
        	throw e1;
		}
	}

	/** Crea el panel con los botones de aceptar y cancelar.
	 * @return Panel de botones. */
	private JPanel createButtonsPanel() {

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		this.okButton.setEnabled(false);
		this.okButton.setMnemonic('A');
		this.okButton.getAccessibleContext().setAccessibleDescription(
			SignPdfUiMessages.getString("SignPdfUiStamp.9") //$NON-NLS-1$
		);
		this.okButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					final File[] files;
					try {
						files = AOUIFactory.getLoadFiles(
								SignPdfUiMessages.getString("SignPdfUiPreview.21"), //$NON-NLS-1$,
								null,
								null,
								IMAGE_EXT,
								SignPdfUiMessages.getString("SignPdfUiPreview.22"), //$NON-NLS-1$,
								false,
								false,
								null,
								SignPdfUiPanelStamp.this
						);
					}
					catch(AOCancelledOperationException ex)
					{
						return;
					}

					try {
						BufferedImage stampImage = ImageIO.read(files[0]);
						extraParams.put("image", getInsertImageBase64(stampImage)); //$NON-NLS-1$
					}
					catch (IOException ioe) {
						Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
								"No ha sido posible cargar la imagen: " + ioe //$NON-NLS-1$
						);
						AOUIFactory.showMessageDialog(
							this,
							SignPdfUiMessages.getString("SignPdfUiPreview.24"), //$NON-NLS-1$
							SignPdfUiMessages.getString("SignPdfUiPreview.23"), //$NON-NLS-1$
							JOptionPane.ERROR_MESSAGE
						);
						return;
					}

					if (allPagesButton.isSelected()) {
						extraParams.put("imagePage", "0");
					}
					else {
						extraParams.put("imagePage", Integer.toString(getCurrentPage())); //$NON-NLS-1$
					}

					extraParams.put(
						"imagePositionOnPageLowerLeftX", //$NON-NLS-1$
						getExtraParamsForLocation().getProperty("signaturePositionOnPageLowerLeftX") //$NON-NLS-1$
					);
					extraParams.put(
						"imagePositionOnPageLowerLeftY", //$NON-NLS-1$
						getExtraParamsForLocation().getProperty("signaturePositionOnPageLowerLeftY") //$NON-NLS-1$
					);
					extraParams.put(
						"imagePositionOnPageUpperRightX", //$NON-NLS-1$
						getExtraParamsForLocation().getProperty("signaturePositionOnPageUpperRightX") //$NON-NLS-1$
					);
					extraParams.put(
						"imagePositionOnPageUpperRightY", //$NON-NLS-1$
						getExtraParamsForLocation().getProperty("signaturePositionOnPageUpperRightY") //$NON-NLS-1$
					);
					getListener().nextPanel(extraParams, null);
				}
			}
		);
		this.okButton.addKeyListener(this);
		panel.add(this.okButton);

		final JButton cancelButton = new JButton(SignPdfUiMessages.getString("SignPdfUiStamp.10")); //$NON-NLS-1$
		cancelButton.setMnemonic('C');
		cancelButton.getAccessibleContext().setAccessibleDescription(
			SignPdfUiMessages.getString("SignPdfUiStamp.11") //$NON-NLS-1$
		);
		cancelButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					getListener().positionCancelled();
				}
			}
		);
		cancelButton.addKeyListener(this);

		// En Mac OS X el orden de los botones es distinto
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			panel.add(cancelButton);
			panel.add(this.okButton);
		}
		else {
			panel.add(this.okButton);
			panel.add(cancelButton);
		}

		return panel;
	}

	@Override
	public void selectionAvailable(final Properties p) {
		if (p != null) {
			this.okButton.setEnabled(true);
			this.okButton.requestFocusInWindow();
		}
		else {
			this.okButton.setEnabled(false);
		}

		setProperties(p);
	}

	@Override
	public void setX(final String x) {
		this.posX.setText(x);
	}

	@Override
	public void setY(final String y) {
		this.posY.setText(y);
	}

	@Override public void keyTyped(final KeyEvent e) { /* vacio */ }
	@Override public void keyReleased(final KeyEvent e) { /* vacio */ }

	@Override
	public void keyPressed(final KeyEvent ke) {
		if (ke != null) {
			if (ke.getKeyCode() == KeyEvent.VK_LEFT && getCurrentPage() > 1) {
				this.currentPage--;
				changePage();
			}
			else if (ke.getKeyCode() == KeyEvent.VK_RIGHT && getCurrentPage() < this.pdfPages.size()) {
				this.currentPage++;
				changePage();
			}
			else if (ke.getKeyCode() == KeyEvent.VK_ESCAPE) {
				this.currentPage++;
				getListener().positionCancelled();
			}
		}
	}

	@Override
	public void actionPerformed(final ActionEvent e) {
		if (e != null) {
			if (e.getSource() == this.firstPageButton && getCurrentPage() > 1) {
				this.currentPage = 1;
				changePage();
			}
			else if (e.getSource() == this.previousPageButton && getCurrentPage() > 1) {
				this.currentPage--;
				if (this.pressButton > 0) {
					try {
						preLoadImages(getCurrentPage() + 1, this.currentPage);
					} catch (final IOException ex) {
						LOGGER.log(Level.SEVERE, "Error durante la carga de las miniaturas anteriores: " + ex, ex); //$NON-NLS-1$
						this.currentPage++; // Deshacemos el cambio de pagina
						AOUIFactory.showErrorMessage(
								SignPdfUiPanelStamp.this,
								SignPdfUiMessages.getString("SignPdfUiStamp.15"), //$NON-NLS-1$
								SignPdfUiMessages.getString("SignPdfUiStamp.12"), //$NON-NLS-1$
								JOptionPane.ERROR_MESSAGE
						);
					}
				}
				if(this.pdfPages.get(getCurrentPage() - 1) != null) {
					changePage();
					this.pressButton++;
				}
			}
			else if (e.getSource() == this.nextPageButton && getCurrentPage() < this.pdfPages.size()) {
				this.currentPage++;
				if (this.pressButton > 0) {
					try {
						preLoadImages(getCurrentPage() - 1, this.currentPage);
					} catch (final IOException ex) {
						LOGGER.log(Level.SEVERE, "Error durante la carga de las miniaturas siguientes: " + ex, ex); //$NON-NLS-1$
						this.currentPage--; // Deshacemos el cambio de pagina
						AOUIFactory.showErrorMessage(
								SignPdfUiPanelStamp.this,
								SignPdfUiMessages.getString("SignPdfUiStamp.15"), //$NON-NLS-1$
								SignPdfUiMessages.getString("SignPdfUiStamp.12"), //$NON-NLS-1$
								JOptionPane.ERROR_MESSAGE
							);
					}
				}
				if (this.pdfPages.get(getCurrentPage() - 1) != null) {
					changePage();
					this.pressButton++;
				}
			}
			else if (e.getSource() == this.lastPageButton && getCurrentPage() < this.pdfPages.size()) {
				this.currentPage = this.pdfPages.size();
				changePage();
			}
		}
	}

	private void changePage() {
		enableButtons();
		this.pagePanel.remove(this.pageLabel);
		this.posX.setText(""); //$NON-NLS-1$
		this.posY.setText(""); //$NON-NLS-1$
		this.pageLabel = createPageLabel(
			this.pdfPages.get(getCurrentPage() - 1),
			this,
			this,
			this.pagePanel,
			this.pdfPageSizes.get(getCurrentPage() - 1)
		);

		this.indexLabel.setText(
			SignPdfUiMessages.getString(
				"SignPdfUiStamp.6", //$NON-NLS-1$
				Integer.toString(getCurrentPage()),
				Integer.toString(this.pdfPages.size()),
				Integer.toString(this.currentScale)
			)
		);
		this.pagePanel.add(this.pageLabel);
		this.pagePanel.repaint();
	}

	private void enableButtons() {
		if (this.pdfPages.size() == 1) {
			this.firstPageButton.setEnabled(false);
			this.previousPageButton.setEnabled(false);
			this.nextPageButton.setEnabled(false);
			this.lastPageButton.setEnabled(false);
		}
		else if (getCurrentPage() == 1) {
			this.firstPageButton.setEnabled(false);
			this.previousPageButton.setEnabled(false);
			this.nextPageButton.setEnabled(true);
			this.lastPageButton.setEnabled(true);
		}
		else if (getCurrentPage() == this.pdfPages.size()) {
			this.firstPageButton.setEnabled(true);
			this.previousPageButton.setEnabled(true);
			this.nextPageButton.setEnabled(false);
			this.lastPageButton.setEnabled(false);
		}
		else {
			this.firstPageButton.setEnabled(true);
			this.previousPageButton.setEnabled(true);
			this.nextPageButton.setEnabled(true);
			this.lastPageButton.setEnabled(true);
		}
	}

	private void preLoadImages(final int actualPage, int pageToLoad) throws IOException {

		int necessaryPage;
		// Pulsado boton izquierdo (anterior)
		if (actualPage > pageToLoad) {
			necessaryPage = pageToLoad - 2;
		}
		// Pulsado boton derecho (siguente)
		else {
			necessaryPage = pageToLoad + 1;
		}

		// Verificamos que sea una pagina valida
		if(necessaryPage < 0 || necessaryPage >= this.pdfPages.size())
			return;

		// Si no tenemos la pagina que necesitamos, la cargamos
		if (this.pdfPages.get(necessaryPage) == null) {
			try {
				this.pdfDocument.loadNewPages(this.pdfPages, getCurrentPage() - 1);
			}
			catch (final OutOfMemoryError | Exception e) {
				throw new IOException("No se ha podido cargar la previsualizacion de la pagina", e); //$NON-NLS-1$
			}
		}
	}
}
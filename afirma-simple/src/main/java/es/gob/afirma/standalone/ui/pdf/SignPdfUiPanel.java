package es.gob.afirma.standalone.ui.pdf;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.image.BufferedImage;
import java.util.List;
import java.util.Properties;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.ui.pdf.PageLabel.PageLabelListener;

final class SignPdfUiPanel extends JPanel implements PageLabel.PageLabelListener, KeyListener, ActionListener {

	static interface SignPdfUiPanelListener {
		void positionSelected(final Properties extraParams);
		void positionCancelled();
	}

	private static final long serialVersionUID = 8109653789776305491L;

	private Properties extraParamsForLocation = null;
	Properties getExtraParamsForLocation() {
		return this.extraParamsForLocation;
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
	private final List<BufferedImage> pdfPages;
	private final List<Dimension> pdfPageSizes;
	private JLabel pageLabel;
	private final JButton okButton = new JButton(SignPdfUiMessages.getString("SignPdfUiPanel.0")); //$NON-NLS-1$
	private final JTextField posX = new JTextField(4);
	private final JTextField posY = new JTextField(4);
	private final JLabel indexLabel = new JLabel();

	final JButton firstPageButton = new JButton("<<"); //$NON-NLS-1$
	final JButton previousPageButton = new JButton("<"); //$NON-NLS-1$
	final JButton nextPageButton = new JButton(">"); //$NON-NLS-1$
	final JButton lastPageButton = new JButton(">>"); //$NON-NLS-1$

	SignPdfUiPanel(final List<BufferedImage> pages,
				   final List<Dimension> pageSizes,
			       final SignPdfUiPanelListener spul) {

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


		SwingUtilities.invokeLater(
			new Runnable() {
				@Override
				public void run() {
					createUI();
				}
			}
		);
	}

	private void setProperties(final Properties p) {
		this.extraParamsForLocation = p;
	}

	private JLabel createPageLabel(final BufferedImage page,
			                       final PageLabelListener pll,
			                       final KeyListener kl,
			                       final Component parent,
			                       final Dimension pdfPageOriginalDimension) {

		int pageWidth = page.getWidth();
		int pageHeight = page.getHeight();

		final Rectangle screen = SignPdfUiUtil.getScreenBounds(parent);
		while (pageWidth > screen.width || pageHeight > screen.height) {
			pageWidth = pageWidth / 2;
			pageHeight = pageHeight / 2;
		}

		// Comprobacion de si las paginas estan rotadas
		final int pdfOriginalHeight;
		if (page.getWidth() <= page.getHeight() && pdfPageOriginalDimension.width <= pdfPageOriginalDimension.height) {
			// No rotada
			pdfOriginalHeight = pdfPageOriginalDimension.height;
		}
		else {
			// Pagina rotada
			pdfOriginalHeight = pdfPageOriginalDimension.width;
		}

		this.currentScale = Math.round(100 * ((float) pageWidth / page.getWidth()));

		final JLabel ret = new PageLabel(
			page.getScaledInstance(pageWidth, pageHeight, Image.SCALE_SMOOTH),
			pageWidth,
			pageHeight,
			pll,
			(float)pdfOriginalHeight / page.getHeight()
		);

		ret.addKeyListener(kl);
		ret.setFocusable(false);

		ret.getAccessibleContext().setAccessibleDescription(
				SignPdfUiMessages.getString("SignPdfUiPanel.6") //$NON-NLS-1$
		);

		return ret;
	}

	void createUI() {

		addKeyListener(this);

		getAccessibleContext().setAccessibleDescription(
			SignPdfUiMessages.getString("SignPdfUiPanel.1") //$NON-NLS-1$
		);

		setLayout(new GridBagLayout());

		final GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.BOTH;
		gbc.weightx = 1.0;
		gbc.gridy = 0;

		final JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new GridBagLayout());
		mainPanel.setBorder(BorderFactory.createTitledBorder(SignPdfUiMessages.getString("SignPdfUiPanel.7"))); //$NON-NLS-1$

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1.0;
		c.gridy = 0;

		mainPanel.add(createCoordenatesPanel(), c);

		c.gridy++;

		mainPanel.add(createPagePanel(), c);

		c.gridy++;
		c.ipady = 11;

		mainPanel.add(createPaginationPanel(), c);

		add(mainPanel, gbc);

		gbc.gridy++;
		gbc.weighty = 1.0;

		add(new JPanel(), gbc); // Relleno en blanco
		gbc.gridy++;
		gbc.weighty = 0.0;
		gbc.ipady = 6;
		add(createButtonsPanel(), gbc);
	}

	/** Crea el panel con la previsualizaci&oacute;n de las p&aacute;ginas del PDF.
	 * @return Panel con la previsualizaci&oacute;n. */
	private JPanel createPagePanel() {

		this.pagePanel = new JPanel();
		this.pagePanel.setLayout(new FlowLayout(FlowLayout.CENTER));

		// Creamos la etiqueta y establecemos la primera pagina
		this.pageLabel = createPageLabel(
			this.pdfPages.get(0),
			this,
			this,
			this,
			this.pdfPageSizes.get(0)
		);
		this.pagePanel.add(this.pageLabel);

		return this.pagePanel;
	}

	/** Crea el panel para la paginaci&oacute;n del PDF.
	 * @return Panel para la paginaci&oacute;n del PDF. */
	private JPanel createPaginationPanel() {

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.CENTER));

		this.indexLabel.setText(
			SignPdfUiMessages.getString(
				"SignPdfUiPanel.5", //$NON-NLS-1$
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

		panel.add(this.firstPageButton);
		panel.add(this.previousPageButton);
		panel.add(this.indexLabel);
		panel.add(this.nextPageButton);
		panel.add(this.lastPageButton);

		return panel;
	}

	/** Crea el panel con los elementos que muestran las coordenadas del cursor dentro
	 * del panel de firma.
	 * @return Panel con los componentes para la visualizacion de coordenadas. */
	private JPanel createCoordenatesPanel() {

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.CENTER));

		panel.add(new JLabel(SignPdfUiMessages.getString("SignPdfUiPanel.8"))); //$NON-NLS-1$

		this.posX.setEnabled(false);
		this.posX.setFocusable(false);
		this.posX.addKeyListener(this);
		panel.add(this.posX);

		panel.add(new JLabel(SignPdfUiMessages.getString("SignPdfUiPanel.9"))); //$NON-NLS-1$

		this.posY.setEnabled(false);
		this.posY.setFocusable(false);
		this.posY.addKeyListener(this);
		panel.add(this.posY);

		return panel;
	}

	/** Crea el panel con los botones de aceptar y cancelar.
	 * @return Panel de botones. */
	private JPanel createButtonsPanel() {

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		this.okButton.setEnabled(false);
		this.okButton.setMnemonic('A');
		this.okButton.getAccessibleContext().setAccessibleDescription(
			SignPdfUiMessages.getString("SignPdfUiPanel.2") //$NON-NLS-1$
		);
		this.okButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent e) {
					final Properties p = new Properties();
					p.put("signaturePage", Integer.toString(getCurrentPage())); //$NON-NLS-1$
					p.putAll(getExtraParamsForLocation());
					getListener().positionSelected(p);
				}
			}
		);
		this.okButton.addKeyListener(this);
		panel.add(this.okButton);

		final JButton cancelButton = new JButton(SignPdfUiMessages.getString("SignPdfUiPanel.3")); //$NON-NLS-1$
		cancelButton.setMnemonic('C');
		cancelButton.getAccessibleContext().setAccessibleDescription(
			SignPdfUiMessages.getString("SignPdfUiPanel.4") //$NON-NLS-1$
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
				changePage();
			}
			else if (e.getSource() == this.nextPageButton && getCurrentPage() < this.pdfPages.size()) {
				this.currentPage++;
				changePage();
			}
			else if (e.getSource() == this.lastPageButton && getCurrentPage() < this.pdfPages.size()) {
				this.currentPage = this.pdfPages.size();
				changePage();
			}
		}
	}

	private void changePage() {
		this.pagePanel.remove(this.pageLabel);
		this.posX.setText(""); //$NON-NLS-1$
		this.posY.setText(""); //$NON-NLS-1$
		this.pageLabel = createPageLabel(
			this.pdfPages.get(getCurrentPage() - 1),
			this,
			this,
			this,
			this.pdfPageSizes.get(getCurrentPage() - 1)
		);

		this.indexLabel.setText(
			SignPdfUiMessages.getString(
				"SignPdfUiPanel.5", //$NON-NLS-1$
				Integer.toString(getCurrentPage()),
				Integer.toString(this.pdfPages.size()),
				Integer.toString(this.currentScale)
			)
		);
		this.pagePanel.add(this.pageLabel);
		this.pagePanel.repaint();
	}

}

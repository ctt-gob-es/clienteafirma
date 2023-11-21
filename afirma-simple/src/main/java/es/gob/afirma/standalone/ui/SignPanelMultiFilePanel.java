/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.IOException;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.ListCellRenderer;
import javax.swing.ListModel;
import javax.swing.Scrollable;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;

import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.VisorFirma;

final class SignPanelMultiFilePanel extends JPanel implements Scrollable {

    /** Serial Id. */
	private static final long serialVersionUID = 6644719944157037807L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private String accessibleDescription = ""; //$NON-NLS-1$

	SignPanelMultiFilePanel(final List<SignOperationConfig> operations) {
        super(true);

        SwingUtilities.invokeLater(() -> createUI(operations));
    }

    void createUI(final List<SignOperationConfig> operations) {

        setLayout(new GridBagLayout());

        final FileOperationTitleRenderer titlePanel = new FileOperationTitleRenderer();
        titlePanel.setFileNameColumnTitle(SimpleAfirmaMessages.getString("SignDataPanel.43")); //$NON-NLS-1$
        titlePanel.setSignatureFormatColumnTitle(SimpleAfirmaMessages.getString("SignDataPanel.44")); //$NON-NLS-1$
        titlePanel.setSizeColumnTitle(SimpleAfirmaMessages.getString("SignDataPanel.45")); //$NON-NLS-1$
        titlePanel.setBorder(BorderFactory.createMatteBorder(0,  0,  1,  0, LookAndFeelManager.WINDOWS_HIGH_CONTRAST ? Color.white : Color.black));

        final SignOperationConfig [] operationsArray = operations.toArray(new SignOperationConfig[operations.size()]);
        this.accessibleDescription += SimpleAfirmaMessages.getString("SignDataPanel.47"); //$NON-NLS-1$
        for (final SignOperationConfig opConfig : operationsArray) {
        	this.accessibleDescription += SimpleAfirmaMessages.getString("SignDataPanel.43") + opConfig.getDataFile().getName() //$NON-NLS-1$
        							+ SimpleAfirmaMessages.getString("SignDataPanel.44") + opConfig.getSignatureFormatName() //$NON-NLS-1$
        							+ SimpleAfirmaMessages.getString("SignDataPanel.45") + opConfig.getDataFile().length() / 1024 + "KB"; //$NON-NLS-1$ //$NON-NLS-2$
        }
        final JList<SignOperationConfig> fileList = new JList<>(operationsArray);
        fileList.getAccessibleContext().setAccessibleDescription(this.accessibleDescription);
        fileList.setCellRenderer(new FileOperationCellRenderer());

        // Definimos que al hacer doble clic sobre una firma del listado, se visualicen sus datos
        fileList.addMouseListener(new MouseListener() {
			@Override public void mouseReleased(final MouseEvent e) { /* No hacemos nada */ }
			@Override public void mousePressed(final MouseEvent e) { /* No hacemos nada */ }
			@Override public void mouseExited(final MouseEvent e) { /* No hacemos nada */ }
			@Override public void mouseEntered(final MouseEvent e) { /* No hacemos nada */ }
			@Override
			public void mouseClicked(final MouseEvent e) {
				if (e.getSource() instanceof JList<?>) {
					final JList<SignOperationConfig> list = (JList<SignOperationConfig>) e.getSource();
					if (e.getButton() == MouseEvent.BUTTON3) {
						final int index = list.locationToIndex(e.getPoint());
						if (index >= 0) {
							final SignOperationConfig signConfig = list.getModel().getElementAt(index);
							new ContextualMenu(signConfig, (Component) e.getSource()).show(
									e.getComponent(), e.getX(), e.getY());
						}
					}
					else if (e.getButton() == MouseEvent.BUTTON1 && e.getClickCount() == 2) {
						final int index = list.locationToIndex(e.getPoint());
						if (index >= 0) {
							openDataFileItem(list.getModel().getElementAt(index));
						}
					}
				}
			}
        });

        fileList.addKeyListener(new KeyListener() {
			@Override public void keyTyped(final KeyEvent e) { /* No hacemos nada */ }
			@Override public void keyPressed(final KeyEvent e) { /* No hacemos nada */ }
			@Override
			public void keyReleased(final KeyEvent e) {
				if (e.getKeyCode() == KeyEvent.VK_ENTER && e.getSource() instanceof JList<?>) {
					final JList<SignOperationConfig> list = (JList<SignOperationConfig>) e.getSource();
					final int index = fileList.getSelectedIndex();
					if (index >= 0) {
						openDataFileItem(list.getModel().getElementAt(index));
					}
				}
			}
		});

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;
        this.add(titlePanel, c);
        c.weighty = 1.0;
        c.gridy++;
        this.add(fileList, c);
    }

	/**
	 * Abre el fichero al que corresponde una configuraci&oacute;n de firma.
	 * @param signConfig Configuraci&oacute;n de operaci&oacute;n de firma.
	 */
    static void openDataFileItem(final SignOperationConfig signConfig) {
    	if (signConfig.getDataFile() != null) {
    		// Los ficheros de los que se tenga informacion de validacion son documentos de firma
    		// y se abriran con el visor, mientras que el resto a traves de la aplicacion asociada
    		// en el sistema
    		if (signConfig.getSignValidity() != null) {
    			new VisorFirma(false, null).initialize(false, signConfig.getDataFile());
    		}
    		else {
    			SwingUtilities.invokeLater(() -> {
    				try {
    					Desktop.getDesktop().open(signConfig.getDataFile());
    				} catch (final IOException ex) {
    					LOGGER.log(Level.WARNING, "No se pudo abrir el fichero: " + LoggerUtil.getCleanUserHomePath(signConfig.getDataFile().getAbsolutePath()), ex); //$NON-NLS-1$
    				}
    			});
    		}
    	}
    }

    /**
     * Panel de t&iacute;tulo de las columnas del listado de ficheros.
     */
    class FileOperationTitleRenderer extends JPanel {

    	/** Serial Id. */
		private static final long serialVersionUID = -8317461712461916374L;

    	private final JLabel fileNameLabel;
    	private final JLabel sizeLabel;
    	private final JLabel formatNameLabel;

    	public FileOperationTitleRenderer() {

    		this.fileNameLabel = new JLabel();

    		this.formatNameLabel = new JLabel();
    		this.formatNameLabel.setPreferredSize(new Dimension(120, 14));

    		this.sizeLabel = new JLabel();
    		this.sizeLabel.setPreferredSize(new Dimension(60, 14));

    		// Configuramos los colores
    		if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST && !Platform.OS.MACOSX.equals(Platform.getOS())) {
    			final Color bgColor = LookAndFeelManager.DEFAULT_COLOR;
    			setBackground(bgColor);
    		}

    		setLayout(new GridBagLayout());

    		final GridBagConstraints c = new GridBagConstraints();
    		c.fill = GridBagConstraints.HORIZONTAL;
    		c.insets = new Insets(3, 11, 3, 0);

    		c.gridx = 0;
    		c.weightx = 1.0;
    		add(this.fileNameLabel, c);

    		c.weightx = 0;

    		c.gridx++;
    		add(this.formatNameLabel, c);
    		c.gridx++;
    		add(this.sizeLabel, c);
    	}

    	void setFileNameColumnTitle(final String title) {
    		this.fileNameLabel.setText(title);
    	}

    	void setSignatureFormatColumnTitle(final String title) {
    		this.formatNameLabel.setText(title);
    	}

    	void setSizeColumnTitle(final String title) {
    		this.sizeLabel.setText(title);
    	}
    }

    class FileOperationCellRenderer extends JPanel
    								implements ListCellRenderer<SignOperationConfig> {

    	/** Serial Id. */
		private static final long serialVersionUID = -6210265681086564451L;

		private final Dimension iconDimension;

    	private final JLabel icon;
    	private final JLabel fileNameLabel;
    	private final JLabel sizeLabel;
    	private final JLabel formatNameLabel;

    	private final NumberFormat kbFormatter;
    	private final DecimalFormat mbFormatter;

    	private final Border focusedBorder;
    	private final Border unfocusedBorder;

    	private int basePathLength = 0;

    	public FileOperationCellRenderer() {

    		this.iconDimension = new Dimension(32, 32);

    		this.icon = new JLabel();
    		this.icon.setPreferredSize(this.iconDimension);

			this.fileNameLabel = new JLabel();

			this.formatNameLabel = new JLabel();
			this.formatNameLabel.setPreferredSize(new Dimension(120, 32));

			this.sizeLabel = new JLabel();
			this.sizeLabel.setPreferredSize(new Dimension(60, 32));

			this.kbFormatter = NumberFormat.getNumberInstance();

			this.mbFormatter = new DecimalFormat("#.#"); //$NON-NLS-1$
			this.mbFormatter.setRoundingMode(RoundingMode.CEILING);

			this.focusedBorder = BorderFactory.createDashedBorder(Color.GRAY);
			this.unfocusedBorder = BorderFactory.createEmptyBorder(1,  1,  1,  1);

            // Configuramos los colores
            if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
            	setBackground(Color.WHITE);
            }

			setLayout(new GridBagLayout());

			final GridBagConstraints c = new GridBagConstraints();
			c.fill = GridBagConstraints.HORIZONTAL;
			c.insets = new Insets(3, 11, 3, 0);

			c.gridx = 0;
			add(this.icon, c);

			c.gridx++;
			c.weightx = 1.0;
			add(this.fileNameLabel, c);

			c.weightx = 0;

			c.gridx++;
			add(this.formatNameLabel, c);
			c.gridx++;
			add(this.sizeLabel, c);
		}

		@Override
		public Component getListCellRendererComponent(final JList<? extends SignOperationConfig> list,
				final SignOperationConfig value, final int index, final boolean isSelected, final boolean cellHasFocus) {

			if (this.basePathLength == 0) {
				this.basePathLength = calculateBasePathLength(list.getModel());
			}

			final ScalablePane typeIcon = (ScalablePane) value.getFileType().getIcon();
			typeIcon.setPreferredSize(this.iconDimension);


			this.icon.setIcon(new ImageIcon(typeIcon.getScaledInstanceToFit(typeIcon.getMaster(), this.iconDimension)));

			if (value.getInvalidSignatureText() == null) {
				this.fileNameLabel.setText(value.getDataFile().getAbsolutePath().substring(this.basePathLength));
				if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
					this.fileNameLabel.setForeground(Color.BLACK);
				}
			}
			else if (value.getSignValidity() != null
					&& SignValidity.SIGN_DETAIL_TYPE.PENDING_CONFIRM_BY_USER.equals(value.getSignValidity().get(0).getValidity())) {
				this.fileNameLabel.setText(
						(value.getDataFile().getAbsolutePath() + " (" + value.getSignValidity().get(0).validityTypeToString() + ")").substring(this.basePathLength)); //$NON-NLS-1$ //$NON-NLS-2$
				if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
					this.fileNameLabel.setForeground(Color.RED);
				}
			}
			else {
				this.fileNameLabel.setText(
						(value.getDataFile().getAbsolutePath() + " " + SimpleAfirmaMessages.getString("SignPanel.147")).substring(this.basePathLength)); //$NON-NLS-1$ //$NON-NLS-2$
				if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
					this.fileNameLabel.setForeground(Color.RED);
				}
			}

			this.formatNameLabel.setText(value.getSignatureFormatName());
			this.sizeLabel.setText(calculateSize(value.getDataFile().length()));

			setBorder(cellHasFocus ? this.focusedBorder : this.unfocusedBorder);

			return this;
		}

		/**
		 * Calcula el n&uacute;mero de caracteres del directorio padre com&uacute;n a todos los
		 * ficheros seleccionados para poder omitir de la visualizaci&oacute;n ese fragmento de
		 * las rutas.
		 * @param signConfigs Configuraci&oacute;n con todos los ficheros a procesar.
		 * @return Longitud del directorio padre com&uacute;n.
		 */
		private int calculateBasePathLength(final ListModel<? extends SignOperationConfig> signConfigs) {
			int parentLength = Integer.MAX_VALUE;
			for (int i = 0; i < signConfigs.getSize(); i++) {
				final SignOperationConfig config = signConfigs.getElementAt(i);
				if (config.getDataFile() != null &&
						config.getDataFile().getParentFile() != null) {
					final int length = config.getDataFile().getParentFile().getAbsolutePath().length();
					if (length < parentLength) {
						parentLength = length;
					}
				}
			}
	        if (parentLength > 0) {
	        	parentLength++;
	        }

	        return parentLength;
		}

		/**
		 * Obtiene el tama&ntilde;o formateado del fichero en KiloBytes.
		 * @param size Bytes del fichero.
		 * @return Tama&ntilde;o del fichero.
		 */
		private String calculateSize(final long size) {
			if (size == 0) {
				return "0 KB"; //$NON-NLS-1$
			}
			if (size < 1024) {
				return "1 KB"; //$NON-NLS-1$
			}
			if (size < 1024 * 1000) { // Con 1000Kb o menos, lo  expresamos en kilobytes
				return this.kbFormatter.format(size / 1024) + " KB"; //$NON-NLS-1$
			}

			return this.mbFormatter.format(size / (1024 * 1024)) + " MB"; //$NON-NLS-1$
		}
    }

	@Override
	public Dimension getPreferredScrollableViewportSize() {
		return getPreferredSize();
	}

	@Override
	public int getScrollableUnitIncrement(final Rectangle visibleRect, final int orientation, final int direction) {
		return 30;
	}

	@Override
	public int getScrollableBlockIncrement(final Rectangle visibleRect, final int orientation, final int direction) {
		return 30;
	}

	@Override
	public boolean getScrollableTracksViewportWidth() {
		return true;
	}

	@Override
	public boolean getScrollableTracksViewportHeight() {
		return false;
	}

	private class ContextualMenu extends JPopupMenu implements ActionListener {

		/** Serial Id. */
		private static final long serialVersionUID = 4803487514272887434L;

		private final SignOperationConfig config;
		private final Component parent;

		private final JMenuItem openFileItem;
		private final JMenuItem seeAttributesItem;

	    public ContextualMenu(final SignOperationConfig signConfig, final Component element) {

	    	this.config = signConfig;
	    	this.parent = element;

	    	this.openFileItem = new JMenuItem(SimpleAfirmaMessages.getString("SignPanel.51")); //$NON-NLS-1$
	    	this.openFileItem.setFont(this.openFileItem.getFont().deriveFont(Font.BOLD));
	    	this.openFileItem.addActionListener(this);
	        add(this.openFileItem);
	        this.seeAttributesItem = new JMenuItem(SimpleAfirmaMessages.getString("SignPanel.148")); //$NON-NLS-1$
	        this.seeAttributesItem.addActionListener(this);
	        add(this.seeAttributesItem);
	    }

	    @Override
	    public void actionPerformed(final ActionEvent e) {
	    	if (e.getSource() == this.openFileItem) {
	    		openDataFileItem(this.config);
	    	}
	    	else if (e.getSource() == this.seeAttributesItem) {
	    		final JDialog dialog = SignatureAttributesDialog.newInstance(this.parent, this.config);
	    		dialog.getAccessibleContext().setAccessibleDescription(SignatureAttributesDialog.getAccessibleDescription());
	    		dialog.setVisible(true);
	    	}
	    }
	}
}

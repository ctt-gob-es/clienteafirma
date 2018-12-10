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
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.dnd.DropTarget;
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
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.LookAndFeelManager;

final class SignPanelMultiFilePanel extends JPanel {

    /** Serial Id. */
	private static final long serialVersionUID = 6644719944157037807L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	SignPanelMultiFilePanel(List<SignOperationConfig> operations,
                       final DropTarget dropTarget) {
        super(true);

        // Puede arrastrarse un fichero a cualquiera de estos componentes para cargarlo
        setDropTarget(dropTarget);

        SwingUtilities.invokeLater(() -> createUI(operations));
    }

    void createUI(List<SignOperationConfig> operations) {

        setBorder(BorderFactory.createLineBorder(Color.black));
        setLayout(new GridBagLayout());

        final FileOperationTitleRenderer titlePanel = new FileOperationTitleRenderer();
        titlePanel.setFileNameColumnTitle("Fichero");
        titlePanel.setSignatureFormatColumnTitle("Formato");
        titlePanel.setSizeColumnTitle("Tama\u00F1o");
        titlePanel.setBorder(BorderFactory.createMatteBorder(0,  0,  1,  0, Color.black));

        final JList<SignOperationConfig> fileList =
        		new JList<>(operations.toArray(new SignOperationConfig[operations.size()]));
        fileList.setCellRenderer(new FileOperationCellRenderer());

        // Definimos que al hacer doble clic sobre una firma del listado, se visualicen sus datos
        fileList.addMouseListener(new MouseListener() {
			@Override public void mouseReleased(MouseEvent e) { /* No hacemos nada */ }
			@Override public void mousePressed(MouseEvent e) { /* No hacemos nada */ }
			@Override public void mouseExited(MouseEvent e) { /* No hacemos nada */ }
			@Override public void mouseEntered(MouseEvent e) { /* No hacemos nada */ }
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					if (e.getSource() instanceof JList<?>) {
						final JList<SignOperationConfig> list = (JList<SignOperationConfig>) e.getSource();
						final int index = list.locationToIndex(e.getPoint());
						openDataFileItem(list, index);
					}
				}
			}
		});

        fileList.addKeyListener(new KeyListener() {
			@Override public void keyTyped(KeyEvent e) { /* No hacemos nada */ }
			@Override public void keyPressed(KeyEvent e) { /* No hacemos nada */ }
			@Override
			public void keyReleased(KeyEvent e) {
				if (e.getKeyCode() == KeyEvent.VK_ENTER && e.getSource() instanceof JList<?>) {
					final JList<SignOperationConfig> list = (JList<SignOperationConfig>) e.getSource();
					final int index = fileList.getSelectedIndex();
					openDataFileItem(list, index);
				}
			}
		});

        final JScrollPane scrollPane = new JScrollPane(fileList);
        scrollPane.setBorder(BorderFactory.createEmptyBorder());

        // En Apple siempre hay barras, y es el SO el que las pinta o no depende de si hacen falta
        if (Platform.OS.MACOSX.equals(Platform.getOS())) {
        	scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
        	scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
        }
        else {
        	scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        }

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;
        this.add(titlePanel, c);
        c.weighty = 1.0;
        c.gridy++;
        this.add(scrollPane, c);
    }

	/**
	 * Abre un fichero de la lista.
	 * @param list Lista con los ficheros cargados.
	 * @param index Indice del fichero seleccionado.
	 */
    static void openDataFileItem(final JList<SignOperationConfig> list, final int index) {
		if (index >= 0) {
			final SignOperationConfig item = list.getModel().getElementAt(index);
			if (item.getDataFile() != null) {
				SwingUtilities.invokeLater(new Runnable() {
					@Override
					public void run() {
						try {
							Desktop.getDesktop().open(item.getDataFile());
						} catch (final IOException ex) {
							LOGGER.log(Level.WARNING, "No se pudo abrir el fichero: " + item.getDataFile().getAbsolutePath(), ex); //$NON-NLS-1$
						}
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

    		// Establecemos la configuracion de color
    		Color bgColor = Color.WHITE;
    		// Configuramos los colores
    		if (!LookAndFeelManager.HIGH_CONTRAST && !Platform.OS.MACOSX.equals(Platform.getOS())) {
    			bgColor = LookAndFeelManager.WINDOW_COLOR;
    		}

    		setBackground(bgColor);

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

    	void setFileNameColumnTitle(String title) {
    		this.fileNameLabel.setText(title);
    	}

    	void setSignatureFormatColumnTitle(String title) {
    		this.formatNameLabel.setText(title);
    	}

    	void setSizeColumnTitle(String title) {
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

			// Establecemos la configuracion de color
            Color bgColor = Color.WHITE;
            // Configuramos los colores
            if (!LookAndFeelManager.HIGH_CONTRAST && !Platform.OS.MACOSX.equals(Platform.getOS())) {
            	bgColor = LookAndFeelManager.WINDOW_COLOR;
            }

            setBackground(bgColor);

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
		public Component getListCellRendererComponent(JList<? extends SignOperationConfig> list,
				SignOperationConfig value, int index, boolean isSelected, boolean cellHasFocus) {

			if (this.basePathLength == 0) {
				this.basePathLength = calculateBasePathLength(list.getModel());
			}

			final ScalablePane typeIcon = (ScalablePane) value.getFileType().getIcon();
			typeIcon.setPreferredSize(this.iconDimension);

			this.icon.setIcon(new ImageIcon(typeIcon.getScaledInstanceToFit(typeIcon.getMaster(), this.iconDimension)));

			this.fileNameLabel.setText(value.getDataFile().getAbsolutePath().substring(this.basePathLength));
			this.formatNameLabel.setText(value.getSignatureFormatName());
			this.sizeLabel.setText(calculateSize(value.getDataFile().length()));

			setBorder(cellHasFocus ? this.focusedBorder : this.unfocusedBorder);

			return this;
		}

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
		private String calculateSize(long size) {
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
}

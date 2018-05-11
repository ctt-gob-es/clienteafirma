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
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.dnd.DropTarget;
import java.text.NumberFormat;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.LookAndFeelManager;

final class SignPanelMultiFilePanel extends JPanel {

    /** Serial Id. */
	private static final long serialVersionUID = 6644719944157037807L;

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

        final JList<SignOperationConfig> fileList =
        		new JList<>(operations.toArray(new SignOperationConfig[operations.size()]));
        fileList.setCellRenderer(new FileOperationCellRenderer());

        final JScrollPane scrollPane = new JScrollPane(fileList);

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.weighty = 1.0;
        this.add(scrollPane, c);
    }

    class FileOperationCellRenderer extends JPanel
    								implements ListCellRenderer<SignOperationConfig> {

    	private final Dimension iconDimension;

    	private final JPanel icon;
    	private final JLabel fileNameLabel;
    	private final JLabel sizeLabel;
    	private final JLabel formatNameLabel;

    	private final NumberFormat formatter;

    	public FileOperationCellRenderer() {

    		this.iconDimension = new Dimension(32, 32);

    		this.icon = new JPanel();
    		this.icon.setPreferredSize(this.iconDimension);

			this.fileNameLabel = new JLabel();

			this.formatNameLabel = new JLabel();
			this.formatNameLabel.setPreferredSize(new Dimension(120, 32));

			this.sizeLabel = new JLabel();
			this.sizeLabel.setPreferredSize(new Dimension(60, 32));

			this.formatter = NumberFormat.getNumberInstance();

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
//			add(this.icon, c);
//
//			c.gridx++;
			c.weightx = 1.0;
			add(this.fileNameLabel, c);

			c.weightx = 0;

			c.gridx++;
			add(this.formatNameLabel, c);
			c.gridx++;
			add(this.sizeLabel, c);
		}

		/** Serial Id. */
		private static final long serialVersionUID = -2216450346454299685L;

		@Override
		public Component getListCellRendererComponent(JList<? extends SignOperationConfig> list,
				SignOperationConfig value, int index, boolean isSelected, boolean cellHasFocus) {

			final ScalablePane typeIcon = (ScalablePane) value.getFileType().getIcon();
			typeIcon.setPreferredSize(this.iconDimension);

//			this.icon.removeAll();
//			this.icon.add(typeIcon);
			this.fileNameLabel.setIcon(new ImageIcon(typeIcon.getMaster()));
			this.fileNameLabel.setText(value.getDataFile().getName());
			this.formatNameLabel.setText(value.getSignatureFormatName());
			this.sizeLabel.setText(calculateSize(value.getDataFile().length()));

			return this;
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
			return this.formatter.format(size / 1024) + " KB"; //$NON-NLS-1$
		}
    }
}

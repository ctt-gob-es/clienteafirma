/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.beans.PropertyChangeListener;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.batik.swing.JSVGCanvas;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.Messages;

/** Panel para la espera y detecci&oacute;n autom&aacute;tica de insercci&oacute;n de DNIe.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class DNIeWaitPanel extends JPanel implements KeyListener {

	/** Evento de DNIe solicitado. */
	public static final String PROP_HELP_REQUESTED = "F1"; //$NON-NLS-1$

	/** Evento de DNIe rechazado. */
	public static final String PROP_DNIE_REQUESTED = "DNI"; //$NON-NLS-1$

	/** Evento de Ayuda solicitada. */
	public static final String PROP_DNIE_REJECTED = "NoDNI"; //$NON-NLS-1$

    private static final long serialVersionUID = -8543615798397861866L;


//    private final SimpleAfirma saf;

    private void createUI(final PropertyChangeListener pcl) {

    	this.addPropertyChangeListener(pcl);

        this.setLayout(new GridBagLayout());
        this.setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));

        final JPanel dniePanel = new JPanel();
        dniePanel.setLayout(new GridBagLayout());

        // Boton para cargar DNIe
        final JButton DNIButton = new JButton(Messages.getString("DNIeWaitPanel.4")); //$NON-NLS-1$
        DNIButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				DNIeWaitPanel.this.firePropertyChange(PROP_DNIE_REQUESTED, false, true);
			}
		});
        DNIButton.setMnemonic('C');
        DNIButton.getAccessibleContext().setAccessibleDescription(Messages.getString(Messages.getString("DNIeWaitPanel.5"))); //$NON-NLS-1$
        DNIButton.getAccessibleContext().setAccessibleName(Messages.getString(Messages.getString("DNIeWaitPanel.6"))); //$NON-NLS-1$
        DNIButton.requestFocus();
        DNIButton.addKeyListener(this);
        dniePanel.add(DNIButton);

        // Boton para saltar de pantalla
        final JButton noDNIButton = new JButton(Messages.getString("DNIeWaitPanel.0")); //$NON-NLS-1$
        noDNIButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent e) {
				DNIeWaitPanel.this.firePropertyChange(PROP_DNIE_REJECTED, false, true);
			}
		});
        noDNIButton.setMnemonic('n');
        noDNIButton.getAccessibleContext().setAccessibleDescription(Messages.getString("DNIeWaitPanel.1")); //$NON-NLS-1$
        noDNIButton.getAccessibleContext().setAccessibleName(Messages.getString("DNIeWaitPanel.2")); //$NON-NLS-1$
        noDNIButton.addKeyListener(this);
        dniePanel.add(noDNIButton);

        // Texto informativo
        final ResizingTextPanel textPanel = new ResizingTextPanel(Messages.getString("DNIeWaitPanel.3")); //$NON-NLS-1$

        textPanel.setFocusable(false);

        // Imagen central
        final JSVGCanvas vectorDNIeHelpPicture = new JSVGCanvas();
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        try {
        	vectorDNIeHelpPicture.setDocument(
    			dbf.newDocumentBuilder().parse(
					this.getClass().getResourceAsStream("/resources/lectordnie.svg") //$NON-NLS-1$
    			)
        	);
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
              "No se ha podido cargar la imagen explicativa de insercion de DNIe, esta no se mostrara: " + e //$NON-NLS-1$
            );
        }
        vectorDNIeHelpPicture.setFocusable(false);

        // Configuramos los colores
        if (!LookAndFeelManager.HIGH_CONTRAST) {
            this.setBackground(LookAndFeelManager.WINDOW_COLOR);
            dniePanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
            textPanel.setBackground(LookAndFeelManager.WINDOW_COLOR);
        }

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.weighty = 1.0;
        c.gridx = 0;
        c.gridy = 0;
        this.add(vectorDNIeHelpPicture, c);
        c.weighty = 0.0;
        c.insets = new Insets(10, 0, 5, 0);
        c.gridy = 1;
        c.ipady = 60;
        this.add(textPanel, c);
        c.weightx = 1.0;
        c.insets = new Insets(0, 0, 0, 0);
        c.gridy = 2;
        c.ipady = 0;
        this.add(dniePanel, c);

//        // Listado de idiomas disponibles
//        final Locale[] locales = SimpleAfirma.getAvailableLocales();
//        if (locales != null && locales.length > 1) {
//            final JComboBox languagesList = new JComboBox(locales);
//            languagesList.setRenderer(new LocaleCellRenderer());
//            languagesList.setSelectedItem(Locale.getDefault());
//            languagesList.addItemListener(new ItemListener() {
//                @Override
//                public void itemStateChanged(ItemEvent e) {
//                    if (e.getStateChange() == ItemEvent.SELECTED) {
//                        try {
//                            DNIeWaitPanel.this.saf.setDefaultLocale((Locale) e.getItem());
//                        }
//                        catch (final Exception ex) {
//                            Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
//                               "No se ha podido cambiar el idioma de la interfaz: " + ex //$NON-NLS-1$
//                            );
//                            return;
//                        }
//                    }
//                }
//            });
//            if (kl != null) languagesList.addKeyListener(kl);
//            c.fill = GridBagConstraints.NONE;
//            c.gridx = 1;
//            c.gridy = 0;
//            c.anchor = GridBagConstraints.EAST;
//            this.noDNIPanel.add(languagesList, c);
//        }

    }

    /** Construye un panel de espera a insercci&oacute;n de DNIe.
     * @param pcl <code>PropertyChangeListener</code> para la detecci&oacute;n de las teclas ESC para el
     *        cierre del aplicativo y F1 para mostrar la ayuda y para el control de los botones */
    // @param safirma SimpleAfirma para establecer el <code>Locale</code> seleccionado en el men&uacute; desplegable
    public DNIeWaitPanel(final PropertyChangeListener pcl) {
        super(true);
//        this.saf = safirma;
        createUI(pcl);
    }

	@Override
	public void keyPressed(final KeyEvent ke) {
		if (ke != null && ke.getKeyCode() == KeyEvent.VK_ESCAPE) {
			DNIeWaitPanel.this.firePropertyChange(PROP_DNIE_REJECTED, false, true);
        }
        else if (ke != null && ke.getKeyCode() == KeyEvent.VK_F1 && (!Platform.OS.MACOSX.equals(Platform.getOS()))) {
        	DNIeWaitPanel.this.firePropertyChange(PROP_HELP_REQUESTED, false, true);
        }
	}

	@Override
	public void keyReleased(final KeyEvent arg0) { /* No necesario */ }

	@Override
	public void keyTyped(final KeyEvent arg0) { /* No necesario */ }

//    private static final class LocaleCellRenderer extends DefaultListCellRenderer {
//
//        private static final long serialVersionUID = -6516072256082631760L;
//
//        @Override
//        public Component getListCellRendererComponent(final JList list,
//                                                      final Object value,
//                                                      final int index,
//                                                      final boolean isSelected,
//                                                      final boolean cellHasFocus) {
//            String language = null;
//            if (value instanceof Locale) {
//                final Locale locale = (Locale) value;
//                language = locale.getDisplayName(locale).substring(0, 1).toUpperCase() + locale.getDisplayName(locale).substring(1);
//            }
//            return super.getListCellRendererComponent(list, language != null ? language : value, index, isSelected, cellHasFocus);
//        }
//    }

}


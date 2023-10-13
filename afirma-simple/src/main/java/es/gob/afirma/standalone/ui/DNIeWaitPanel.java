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
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.beans.PropertyChangeListener;
import java.io.InputStream;
import java.util.logging.Logger;

import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JPanel;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.LookAndFeelManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;

/** Panel para la espera y detecci&oacute;n autom&aacute;tica de insercci&oacute;n de DNIe.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class DNIeWaitPanel extends JPanel implements KeyListener {

    private static final long serialVersionUID = -8543615798397861866L;

	/** Evento de Ayuda solicitada. */
	public static final String PROP_HELP_REQUESTED = "F1"; //$NON-NLS-1$

	/** Evento de DNIe solicitado. */
	public static final String PROP_DNIE_REQUESTED = "DNI"; //$NON-NLS-1$

	/** Evento de DNIe rechazado. */
	public static final String PROP_DNIE_REJECTED = "NoDNI"; //$NON-NLS-1$

    /** Anchura m&iacute;nima que deber&aacute; tener el panel. */
	private static final int DEFAULT_WINDOW_WIDTH = 600;
	/** Altura m&iacute;nima que deber&aacute; tener el panel. */
	private static final int DEFAULT_WINDOW_HEIGHT = 420;

    /** Construye un panel de espera a insercci&oacute;n de DNIe.
     * @param pcl <code>PropertyChangeListener</code> para la detecci&oacute;n de las teclas ESC para el
     *        cierre del aplicativo y F1 para mostrar la ayuda y para el control de los botones */
    public DNIeWaitPanel(final PropertyChangeListener pcl) {
        super(true);
        createUI(pcl);
    }

    private void createUI(final PropertyChangeListener pcl) {

    	this.addPropertyChangeListener(pcl);

        setLayout(new GridBagLayout());

        setPreferredSize(new Dimension(DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT));
        setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));

        final JPanel dniePanel = new JPanel();
        dniePanel.setLayout(new GridBagLayout());

        // Boton para cargar DNIe
        final JButton dniButton = new JButton(SimpleAfirmaMessages.getString("DNIeWaitPanel.4")); //$NON-NLS-1$
        dniButton.addActionListener(
    		e -> DNIeWaitPanel.this.firePropertyChange(PROP_DNIE_REQUESTED, false, true)
		);
        dniButton.setMnemonic('C');
        dniButton.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString(SimpleAfirmaMessages.getString("DNIeWaitPanel.5")) //$NON-NLS-1$
		);
        dniButton.getAccessibleContext().setAccessibleName(
    		SimpleAfirmaMessages.getString(SimpleAfirmaMessages.getString("DNIeWaitPanel.6")) //$NON-NLS-1$
		);
        dniButton.requestFocus();
        dniButton.addKeyListener(this);
        dniePanel.add(dniButton);

        // Boton para saltar de pantalla
        final JButton noDNIButton = new JButton(SimpleAfirmaMessages.getString("DNIeWaitPanel.0")); //$NON-NLS-1$
        noDNIButton.addActionListener(
    		e -> DNIeWaitPanel.this.firePropertyChange(PROP_DNIE_REJECTED, false, true)
		);
        noDNIButton.setMnemonic('u');
        noDNIButton.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("DNIeWaitPanel.1") //$NON-NLS-1$
		);
        noDNIButton.getAccessibleContext().setAccessibleName(
    		SimpleAfirmaMessages.getString("DNIeWaitPanel.2") //$NON-NLS-1$
		);
        noDNIButton.addKeyListener(this);
        dniePanel.add(noDNIButton);

        // Texto informativo
        final ResizingTextPanel textPanel = new ResizingTextPanel(
    		SimpleAfirmaMessages.getString("DNIeWaitPanel.3") //$NON-NLS-1$
		);
        final ResizingTextPanel textPanelExtra = new ResizingTextPanel(
    		SimpleAfirmaMessages.getString("DNIeWaitPanel.7") //$NON-NLS-1$
		);

        textPanel.setFocusable(false);
        textPanelExtra.setFocusable(false);

        // Imagen central
        ScalablePane vectorDNIeHelpPicture;
        try {
        	final Image image;
        	try ( final InputStream is = this.getClass().getResourceAsStream("/resources/lectordnie.png"); ) { //$NON-NLS-1$
        		image = ImageIO.read(is);
        	}
        	vectorDNIeHelpPicture = new ScalablePane(image, true);
        	vectorDNIeHelpPicture.setBackground(new Color(255, 255, 255, 0));
        	vectorDNIeHelpPicture.setFocusable(false);
        }
        catch (final Exception e) {
        	Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
    			"No se ha podido cargar la imagen explicativa de insercion de DNIe, esta no se mostrara: " + e //$NON-NLS-1$
			);
        	vectorDNIeHelpPicture = null;
        }

        // Configuramos los colores
        if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
        	setBackground(LookAndFeelManager.SECUNDARY_COLOR);
        	dniePanel.setBackground(LookAndFeelManager.SECUNDARY_COLOR);
        	textPanel.setBackground(LookAndFeelManager.SECUNDARY_COLOR);
        	textPanelExtra.setBackground(LookAndFeelManager.SECUNDARY_COLOR);
        }

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.weighty = 1.0;
        c.gridx = 0;
        c.gridy = 0;
        if (vectorDNIeHelpPicture != null) {
        	this.add(vectorDNIeHelpPicture, c);
        }
        c.weighty = 0.0;
        c.insets = new Insets(0, 0, 5, 0);
        c.gridy = 1;
        c.ipady = 50;
        this.add(textPanel, c);
        c.gridy = 2;
        c.ipady = 30;
        this.add(textPanelExtra, c);
        c.weightx = 1.0;
        c.insets = new Insets(20, 0, 0, 0);
        c.gridy = 3;
        c.ipady = 0;
        this.add(dniePanel, c);

        c.gridy = 4;
        final JCheckBox hideDniWaitScreen = new JCheckBox(
    		SimpleAfirmaMessages.getString("DNIeWaitPanel.8"), //$NON-NLS-1$
    		PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN)
		);
        hideDniWaitScreen.addKeyListener(this);
        hideDniWaitScreen.addChangeListener(
    		e -> PreferencesManager.putBoolean(
				PreferencesManager.PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN,
				hideDniWaitScreen.isSelected()
			)
    	);
        if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
        	hideDniWaitScreen.setBackground(LookAndFeelManager.SECUNDARY_COLOR);
        }
        add(hideDniWaitScreen, c);
    }

    /** {@inheritDoc} */
	@Override
	public void keyPressed(final KeyEvent ke) {
		if (ke != null && ke.getKeyCode() == KeyEvent.VK_ESCAPE) {
			DNIeWaitPanel.this.firePropertyChange(PROP_DNIE_REJECTED, false, true);
        }
        else if (ke != null && ke.getKeyCode() == KeyEvent.VK_F1 && !Platform.OS.MACOSX.equals(Platform.getOS())) {
        	DNIeWaitPanel.this.firePropertyChange(PROP_HELP_REQUESTED, false, true);
        }
	}

	/** {@inheritDoc} */
	@Override
	public void keyReleased(final KeyEvent arg0) { /* No necesario */ }

	/** {@inheritDoc} */
	@Override
	public void keyTyped(final KeyEvent arg0) { /* No necesario */ }

}

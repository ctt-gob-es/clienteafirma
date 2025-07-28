/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.WindowListener;
import java.util.logging.Logger;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import es.gob.afirma.standalone.DesktopUtil;
import es.gob.afirma.standalone.LookAndFeelManager;

/** Pantalla principal de la aplicaci&oacute;n de Autofirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class MainScreen extends JFrame {

    private static final long serialVersionUID = -3288572031446592104L;

    private JScrollPane scrollPane = null;

    /** Muestra la pantalla principal de la aplicaci&oacute;n.
     * @param wlist WindowListener para el control del cierre de la ventana
     * @param firstPanel Primera pantalla de la aplicaci&oacute;n.
     * @param width Ancho de la ventana
     * @param height Alto de la ventana */
    public void showMainScreen(final WindowListener wlist,
    		                   final JPanel firstPanel,
    		                   final int width,
    		                   final int height) {

    	SwingUtilities.invokeLater(() -> createUI(wlist, firstPanel, width, height));
    }

    void createUI(final WindowListener wlist, final JPanel firstPanel, final int width, final int height) {
        if (!LookAndFeelManager.WINDOWS_HIGH_CONTRAST) {
            setBackground(LookAndFeelManager.DEFAULT_COLOR);
        }

        if (LookAndFeelManager.needMaximizeWindow()) {
        	setExtendedState(getExtendedState() | Frame.MAXIMIZED_BOTH);
        }

        setLayout(new BorderLayout());
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        if (wlist != null) {
            addWindowListener(wlist);
        }

        // Creamos un panel con barras de desplazamiento y establecemos su tamano inicial
        this.scrollPane = new JScrollPane();
        this.scrollPane.setPreferredSize(new Dimension(width, height));
        this.scrollPane.getHorizontalScrollBar().setUnitIncrement(30);
        this.scrollPane.getVerticalScrollBar().setUnitIncrement(30);
        this.scrollPane.setViewportView(firstPanel);

        add(this.scrollPane, BorderLayout.CENTER);

        try {
            setIconImages(DesktopUtil.getIconImages());
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
        		"No se ha podido cargar el icono de la aplicacion: " + e  //$NON-NLS-1$
    		);
        }

        // Ajustamos el tamano de la pantalla al de su contenido y
        // la localizamos centrada en base al nuevo tamano que adquiera
        pack();
        setLocationRelativeTo(null);

        setVisible(true);
    }

    /**
     * Reemplaza el panel que se muestra actualmente por el indicado.
     * @param panel Panel a mostrar.
     */
    public void replaceShowingPanel(final JPanel panel ) {
    	if (this.scrollPane != null) {
    		this.scrollPane.setViewportView(panel);
    		pack();
    	}
    }
}

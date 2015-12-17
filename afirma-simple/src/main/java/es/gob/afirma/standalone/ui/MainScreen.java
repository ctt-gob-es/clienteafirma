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

import java.awt.BorderLayout;
import java.awt.Toolkit;
import java.awt.event.WindowListener;
import java.util.logging.Logger;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import es.gob.afirma.standalone.LookAndFeelManager;

/** Pantalla principal de la aplicaci&oacute;n de AutoFirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class MainScreen extends JFrame {

    private static final long serialVersionUID = -3288572031446592104L;

    /** Muestra la pantalla principal de la aplicaci&oacute;n.
     * @param wlist WindowListener para el control del cierre de la ventana
     * @param firstPanel Primera pantalla de la aplicaci&oacute;n.
     * @param width Ancho de la ventana
     * @param height Alto de la ventana */
    public void showMainScreen(final WindowListener wlist,
    		                   final JPanel firstPanel,
    		                   final int width,
    		                   final int height) {

    	SwingUtilities.invokeLater(
			new Runnable() {
	    		@Override
	    		public void run() {
	    			createUI(wlist, firstPanel, width, height);
	    		}
	    	}
		);
    }

    void createUI(final WindowListener wlist, final JPanel firstPanel, final int width, final int height) {
        if (!LookAndFeelManager.HIGH_CONTRAST) {
            this.setBackground(LookAndFeelManager.WINDOW_COLOR);
        }
        this.setSize(width, height);
        this.setLayout(new BorderLayout());
        this.setLocationRelativeTo(null);
        this.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        if (wlist != null) {
            this.addWindowListener(wlist);
        }

        this.add(firstPanel, BorderLayout.CENTER);

        try {
            setIconImage(
        		Toolkit.getDefaultToolkit().getImage(
    				this.getClass().getResource("/resources/afirma_ico.png") //$NON-NLS-1$
				)
            );
        }
        catch (final Exception e) {
            Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
        		"No se ha podido cargar el icono de la aplicacion: " + e  //$NON-NLS-1$
    		);
        }

        this.setVisible(true);
    }
}

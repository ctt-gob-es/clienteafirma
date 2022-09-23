/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.configurator;

import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.logging.Logger;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

/** Pantalla principal de la aplicaci&oacute;n. Muestra una consola para la notificaci&oacute;n de las accesiones
 * llevadas a cabo por el navegador. */
final class GraphicConfiguratorConsole extends JFrame implements Console {

	private static final int DEFAULT_WINDOW_WIDTH = 480;
	private static final int DEFAULT_WINDOW_HEIGHT = 350;

	/** Serial Id. */
	private static final long serialVersionUID = 398187262022150395L;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final JTextArea console;

	private final ConsoleListener listener;
	ConsoleListener getConsoleListener() {
		return this.listener;
	}


	/**
	 * Crea la pantalla.
	 * @param l Tipo de pantalla a crear.
	 */
	GraphicConfiguratorConsole(final ConsoleListener l) {
		this.listener = l;
    	this.console = new JTextArea();
	}

	/** Muestra la pantalla principal de la aplicaci&oacute;n. */
    @Override
	public void showConsole() {
    	SwingUtilities.invokeLater(
			() -> createUI(
				new WindowListener() {
				    @Override
				    public void windowClosing(final WindowEvent we) {
				    	if (getConsoleListener() != null) {
				    		getConsoleListener().close();
				    	}
				    }
				    @Override public void windowOpened(final WindowEvent we) { /* No implementado */ }
				    @Override public void windowClosed(final WindowEvent we) { /* No implementado */ }
				    @Override public void windowActivated(final WindowEvent we) { /* No implementado */ }
				    @Override public void windowIconified(final WindowEvent we) { /* No implementado */ }
				    @Override public void windowDeiconified(final WindowEvent we) { /* No implementado */ }
				    @Override public void windowDeactivated(final WindowEvent we) { /* No implementado */ }
				},
				DEFAULT_WINDOW_WIDTH,
				DEFAULT_WINDOW_HEIGHT
			)
		);
    }

    void createUI(final WindowListener wlist, final int width, final int height) {
        if (!LookAndFeelManager.HIGH_CONTRAST) {
            setBackground(LookAndFeelManager.WINDOW_COLOR);
        }
        setTitle(Messages.getString("ConfiguratorConsole.0")); //$NON-NLS-1$
        this.setSize(width, height);
        setLayout(new GridBagLayout());
        setLocationRelativeTo(null);
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        if (wlist != null) {
            addWindowListener(wlist);
        }

        this.console.setMargin(new Insets(5,  5,  5,  5));
        this.console.setEditable(false);

        final JScrollPane scrollPane = new JScrollPane(this.console);

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.weighty = 1.0;
        c.insets = new Insets(11,  11,  11,  11);

        this.add(scrollPane, c);

        try {
            setIconImage(
        		Toolkit.getDefaultToolkit().getImage(this.getClass().getResource("/logo_cliente_256.png")) //$NON-NLS-1$
            );
        }
        catch (final Exception e) {
        	LOGGER.warning("No se ha podido cargar el icono de la aplicacion: " + e);  //$NON-NLS-1$
        }

        setVisible(true);
    }

    /** Muestra un texto por consola.
     * @param text Texto a mostrar. */
    @Override
	public void print(final String text) {
    	try {
    		this.console.append(text);
    		this.console.append("\n"); //$NON-NLS-1$
    		LOGGER.info("Mensaje de consola: " + text); //$NON-NLS-1$
    	}
    	catch (final Exception e) {
		LOGGER.warning("No se pudo mostrar por consola el mensaje '" + text + "': " + e);  //$NON-NLS-1$ //$NON-NLS-2$
    	}
    }


	@Override
	public Component getParentComponent() {
		return this;
	}
}

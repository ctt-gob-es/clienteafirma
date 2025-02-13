/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.restoreconfig;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.logging.Logger;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/**
 * Clase que gestiona los componentes gr&aacute;ficos de las ventanas de restauraci&oacute;n de la
 * configuraci&oacute;n de navegadores de Autofirma.
 *
 */
public final class RestoreConfigPanel extends JPanel implements KeyListener, DisposableInterface {

	/**
	 * Identificador de la versi&oacute;n de serializaci&oacute;n.
	 */
	private static final long serialVersionUID = 5353477830742383848L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/**
     * Caracter de salto de l&iacute;nea para los mensajes de la consola de restauraci&oacute;n
     */
	static String newline  = System.getProperty("line.separator"); //$NON-NLS-1$

	private final Window window;

	/**
	 * &Aacute;rea de texto para mostrar los mensajes de progreso de la tarea de restauraci&oacute;n
	 */
	JTextArea taskOutput;

	/**
	 * Casilla de verificaci&oacute;n con la que se puede activar o desactivar la opci&oacute;n de que Firefox
	 * utilice el almac&eacute;n del sistema.
	 */
	JCheckBox firefoxIntegrationCb;


	/**Constructor con par&aacute;metro de la clase
	 * @param w Objeto Window para inicializar la instancia de la clase
	 */
	RestoreConfigPanel(final Window w) {

		this.window = w;
		createUI();
	}

	/**
	 * Dibuja la ventana con las opciones de restauraci&oacute;n
	 */
	private void createUI() {

		setLayout(new GridBagLayout());

		final GridBagConstraints c = new GridBagConstraints();


        // Creamos un panel para el texto de introduccion
		final JLabel introText = new JLabel(SimpleAfirmaMessages.getString("RestoreConfigPanel.4")); //$NON-NLS-1$

    	c.fill = GridBagConstraints.HORIZONTAL;
    	c.insets = new Insets(15,  15,  11,  15);
    	c.weightx = 1.0;
    	c.gridy++;
        this.add(introText, c);

    	// Configuramos el area de texto para los mensajes de configuracion
    	this.taskOutput = new JTextArea();
    	this.taskOutput.setMargin(new Insets(5,5,5,5));
    	this.taskOutput.setEditable(false);
    	this.taskOutput.setLineWrap(true);
    	final JScrollPane jsPanel = new JScrollPane(
    			this.taskOutput,
    			ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
    			ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

    	this.taskOutput.addKeyListener(this);

    	c.fill = GridBagConstraints.BOTH;
    	c.insets = new Insets(0, 15,  0,  15);
    	c.weighty = 1.0;
    	c.gridy++;
        this.add(jsPanel, c);

        this.firefoxIntegrationCb = new JCheckBox(SimpleAfirmaMessages.getString("RestoreConfigPanel.7")); //$NON-NLS-1$
        this.firefoxIntegrationCb.setToolTipText(SimpleAfirmaMessages.getString("RestoreConfigPanel.8")); //$NON-NLS-1$
        this.firefoxIntegrationCb.getAccessibleContext().setAccessibleName(SimpleAfirmaMessages.getString("RestoreConfigPanel.9")); //$NON-NLS-1$
        this.firefoxIntegrationCb.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("RestoreConfigPanel.8")); //$NON-NLS-1$

        // La opcion de configurar Firefox para que use el almacen de confianza del sistema solo estara disponible
        // en sistemas Windows y macOS.
        if (Platform.getOS() == Platform.OS.WINDOWS || Platform.getOS() == Platform.OS.MACOSX) {
        	c.fill = GridBagConstraints.HORIZONTAL;
        	c.insets = new Insets(7, 15,  0,  15);
        	c.gridy++;
        	c.weighty = 0.0;
        	this.add(this.firefoxIntegrationCb, c);
        }

        // Creamos un panel para el boton de restauracion
        final JPanel buttonsPanel = createButtonsPanel();

        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(7, 11,  0,  11);
        c.gridy++;
        c.weighty = 0.0;
		c.ipady = 11;
		add(buttonsPanel, c);
	}

	/** Construye el objeto gr&aacute;fico que representa el panel
	 * donde se ubican los botones de la ventana de restauraci&oacute;n.
	 * @return Panel donde se ubican los botones de la ventana de restauraci&oacute;n. */
	private JPanel createButtonsPanel() {

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.TRAILING));

		final JButton closeButton = new JButton(SimpleAfirmaMessages.getString("RestoreConfigPanel.5")); //$NON-NLS-1$
		closeButton.setMnemonic('C');
		closeButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("RestoreConfigPanel.6") //$NON-NLS-1$
		);
		closeButton.addKeyListener(this);
		closeButton.addActionListener(
			ae -> disposeInterface()
		);

		final JButton restoreButton = new JButton(SimpleAfirmaMessages.getString("RestoreConfigPanel.1")); //$NON-NLS-1$
		restoreButton.setMnemonic('R');
		restoreButton.getAccessibleContext().setAccessibleDescription(
				SimpleAfirmaMessages.getString("RestoreConfigPanel.2") //$NON-NLS-1$
		);
		restoreButton.addKeyListener(this);
		restoreButton.addActionListener(ae -> {

			// Limpiamos el area de texto antes de comenzar con la restauracion
			// para eliminar posibles mensajes de ejecuciones anteriores.
			RestoreConfigPanel.this.taskOutput.setText(null);
			// Deshabilito el boton mientras el proceso se esta ejecutando
			restoreButton.setEnabled(false);
			new Thread(() -> new RestoreConfigManager().restoreConfig(RestoreConfigPanel.this)).start();

			restoreButton.setEnabled(true);

		});

		// En Mac OS X el orden de los botones es distinto
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			panel.add(closeButton);
			panel.add(restoreButton);
		}
		else {
			panel.add(restoreButton);
			panel.add(closeButton);
		}

		return panel;
	}

	/** Devuelve la ventana desde donde se abri&oacute; la ventana actual.
	 * @return Ventana desde donde se abri&oacute; la ventana actual. */
	Window getParentWindow() {
		return this.window;
	}

	@Override
	public void disposeInterface() {
		RestoreConfigPanel.this.getParentWindow().dispose();
	}

	/** {@inheritDoc} */
	@Override
	public void keyPressed(final KeyEvent e) {
		/* Vacio */ }

	/** {@inheritDoc} */
	@Override
	public void keyReleased(final KeyEvent ke) {
		// En Mac no cerramos los dialogos con Escape
		if (ke != null && ke.getKeyCode() == KeyEvent.VK_ESCAPE && !Platform.OS.MACOSX.equals(Platform.getOS())) {
			disposeInterface();
		}
	}

	/** {@inheritDoc} */
	@Override
	public void keyTyped(final KeyEvent e) {
		/* Vacio */ }

	/**
	 * Muestra un mensaje en el panel de trazas de configuraci&oacute;n.
	 * @param message Mensaje a mostrar.
	 */
	public void appendMessage(final String message) {

		SwingUtilities.invokeLater(() -> {
			RestoreConfigPanel.this.taskOutput.setText(
					RestoreConfigPanel.this.taskOutput.getText() + message + newline);
			RestoreConfigPanel.this.taskOutput.setCaretPosition(
					RestoreConfigPanel.this.taskOutput.getDocument().getLength());
		});
	}
}

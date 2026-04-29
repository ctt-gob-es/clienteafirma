/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.ui.core.jse.errors;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import es.gob.afirma.ui.core.jse.JSEUIMessages;


/**
 * Panel gr&aacute;fico con las opciones de gesti&oacute;n de errores.
 */
public final class ErrorManagementPanel extends JPanel {

	/** Serial Id. */
	private static final long serialVersionUID = 8384222173067817402L;

	private final Window window;

	private JPanel errorInfoPanel;
	private JPanel buttonsPanel;

	private JButton closeButton;

	final ErrorManagementHandler eventsHandler;

	/**
	 * Constructor con la ventana padre.
	 * @param w Di&aacute;logo sobre el que mostrar el panel.
	 * @param t Informaci&oacute;n sobre el error.
	 * @param message Mensaje sobre el error.
	 * @param messageType Tipo de mensaje (precauci&oacute;n o error).
	 */
	ErrorManagementPanel(final Window w, final Throwable t, final Object message, final int messageType) {
		this.window = w;
		this.eventsHandler = new ErrorManagementHandler(this);
		createUI(t, message, messageType);
		this.eventsHandler.registerComponents();
	}

	/**
	 * Dibuja la ventana con la informaci&oacute;n sobre el error.
	 * @param t Datos sobre el error.
	 * @param message Mensaje del error.
	 * @param messageType Tipo de mensaje (precauci&oacute;n o error).
	 */
	private void createUI(final Throwable t, final Object message, final int messageType) {

		setLayout(new GridBagLayout());

		this.errorInfoPanel = createInfoTextPanel(t, message, messageType);

        // Creamos un panel para los botones de detalles y cerrar
        this.buttonsPanel = createButtonsPanel();

        // Anadimos cada elemento al panel
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
		c.weighty = 0;
		c.weightx = 1.0;
		c.gridx = 0;
		c.gridy = 0;
		c.insets = new Insets(15, 5, 11, 15);
		add(this.errorInfoPanel, c);

        c.gridy++;
		c.ipady = 0;
        add(this.buttonsPanel, c);
	}

	/**
	 * Crea un panel con el texto de informaci&oacute;n del error preparado para
	 * agregar a un panel con scroll.
	 * @param t Datos sobre el error
	 * @param message Mensaje del error
	 * @param messageType Tipo de mensaje (error o warning)
	 * @return Componente para la visualizaci&oacute;n del texto.
	 */
	private static JPanel createInfoTextPanel(final Throwable t, final Object message, final int messageType) {

		ImageIcon icon = null;
		final JLabel errorIconLabel = new JLabel();
		if (messageType == JOptionPane.ERROR_MESSAGE) {
			icon =  new ImageIcon(ErrorManagementPanel.class.getResource("error_icon.png")); //$NON-NLS-1$
			errorIconLabel.setIcon(icon);
		} else if (messageType == JOptionPane.WARNING_MESSAGE) {
			icon =  new ImageIcon(ErrorManagementPanel.class.getResource("warning_icon.png")); //$NON-NLS-1$
			errorIconLabel.setIcon(icon);
		}
		
		final String newMessage = "<html>" + message.toString().replace("\n", "<br>") + "</html>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		final JLabel errorInfoLabel = new JLabel();
		errorInfoLabel.setText(newMessage);

		final JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEADING, 11, 0));
		panel.add(errorIconLabel);
		panel.add(errorInfoLabel);

		return panel;
	}

	/** Construye el objeto gr&aacute;fico que representa el panel
	 * donde se ubican los botones de detalles y cerrar
	 * @return Panel donde se ubican los botones de detalles y cerrar*/
	private JPanel createButtonsPanel() {

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.TRAILING));

		this.closeButton = new JButton(JSEUIMessages.getString("JSEUIManager.1")); //$NON-NLS-1$
		this.closeButton.getAccessibleContext().setAccessibleDescription(
				JSEUIMessages.getString("JSEUIManager.1") //$NON-NLS-1$
		);
		panel.add(this.closeButton);

		return panel;
	}

	/** Devuelve la ventana desde donde se abri&oacute; la ventana actual.
	 * @return Ventana desde donde se abri&oacute; la ventana actual. */
	public Window getWindow() {
		return this.window;
	}

	/**
	 * Devuelve el componente de texto en el que se debe mostrar la informaci&oacute;n
	 * del error.
	 * @return Panel de texto.
	 */
	public JPanel getErrorInfoPanel() {
		return this.errorInfoPanel;
	}

	/**
	 * Devuelve el bot&oacute;n de cierre del di&aacute;logo.
	 * @return Bot&oacute;n de cierre.
	 */
	public JButton getCloseButton() {
		return this.closeButton;
	}

}

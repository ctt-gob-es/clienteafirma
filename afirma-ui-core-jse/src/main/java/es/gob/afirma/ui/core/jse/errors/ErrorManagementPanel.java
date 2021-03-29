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
import java.io.PrintWriter;
import java.io.StringWriter;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import es.gob.afirma.ui.core.jse.JSEUIMessages;


/**
 * Panel gr&aacute;fico con las opciones de gesti&oacute;n de errores.
 */
public final class ErrorManagementPanel extends JPanel {

	/** Serial Id. */
	private static final long serialVersionUID = 8384222173067817402L;

	private final Window window;

	private JLabel errorInfoLabel;
	private JPanel buttonsPanel;

	private JButton closeButton;
	private JButton detailsButton;

	private JScrollPane scrollPane;

	final ErrorManagementHandler eventsHandler;

	private boolean expandedDetails;

	/** Constructor con la ventana padre.
	 * @param w Ventana sobre la que mostrar la clase.
	 * @param t Informaci&oacute;n sobre el error
	 * @param message Mensaje sobre el error
	 */
	ErrorManagementPanel(final Window w, final Throwable t, final Object message, final int messageType) {
		this.window = w;
		this.eventsHandler = new ErrorManagementHandler(this);
		createUI(t, message, messageType);
		this.eventsHandler.registerComponents();
	}

	/**
	 * Dibuja la ventana con la informaci&oacute;n sobre el error
	 * @param t Datos sobre el error
	 * @param message Mensaje del error
	 * @param messageType Tipo de mensaje (error o warning)
	 */
	private void createUI(final Throwable t, final Object message, final int messageType) {

		setLayout(new GridBagLayout());

		createInfoTextPanel(t, message, messageType);

		final JTextArea area = new JTextArea();
		area.setEditable(false);

		if(t != null && t.getStackTrace().length != 0) {

			final StringWriter errors = new StringWriter();
			t.printStackTrace(new PrintWriter(errors));
			area.append(errors.toString().replaceAll("\t", "      "));  //$NON-NLS-1$//$NON-NLS-2$
		}

		this.scrollPane = new JScrollPane(area);
		//Por defecto no se mostraran los detalles
		this.scrollPane.setVisible(false);

        // Creamos un panel para los botones de detalles y cerrar
        this.buttonsPanel = createButtonsPanel();

        // Anadimos cada elemento al panel
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
		c.weighty = 0;
		c.weightx = 1.0;
		c.gridx = 0;
		c.gridy = 0;
		c.insets = new Insets(11, 11,  11,  11);
		add(this.errorInfoLabel, c);

		c.fill = GridBagConstraints.HORIZONTAL;
		c.gridy = 1;
		c.ipady = 140;
		add(this.scrollPane, c);

        c.fill = GridBagConstraints.HORIZONTAL;
        c.weighty = 0;
        c.gridx = 0;
        c.gridy = 2;
		c.ipady = 11;
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
	private JPanel createInfoTextPanel(final Throwable t, final Object message, final int messageType) {

		this.errorInfoLabel = new JLabel();
		this.errorInfoLabel.setFocusable(false);
		final String newMessage = new String("<html>"+message.toString()+"</html>"); //$NON-NLS-1$ //$NON-NLS-2$
		this.errorInfoLabel.setText(newMessage.replace("\n", "<br>")); //$NON-NLS-1$ //$NON-NLS-2$

		if(messageType == 0) {
			this.errorInfoLabel.setIcon(new ImageIcon(getClass().
		        getResource("images/error_icon.png"))); //$NON-NLS-1$
		}else if (messageType == 2) {
			this.errorInfoLabel.setIcon(new ImageIcon(getClass().
			    getResource("images/warning_icon.png"))); //$NON-NLS-1$
		}

		final JPanel textPanel = new JPanel();

		textPanel.add(this.errorInfoLabel);

		return textPanel;
	}

	/** Construye el objeto gr&aacute;fico que representa el panel
	 * donde se ubican los botones de detalles y cerrar
	 * @return Panel donde se ubican los botones de detalles y cerrar*/
	private JPanel createButtonsPanel() {

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.TRAILING));

		this.detailsButton = new JButton(JSEUIMessages.getString("JSEUIManager.90")); //$NON-NLS-1$
		this.detailsButton.getAccessibleContext().setAccessibleDescription(
				JSEUIMessages.getString("JSEUIManager.90")  //$NON-NLS-1$
		);
		panel.add(this.detailsButton);

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
	JLabel getErrorInfoLabel() {
		return this.errorInfoLabel;
	}

	/**
	 * Devuelve el componente del panel de scroll
	 * @return Panel de scroll.
	 */
	JScrollPane getScrollPane() {
		return this.scrollPane;
	}

	/**
	 * Devuelve el bot&oacute;n de cierre del di&aacute;logo.
	 * @return Bot&oacute;n de cierre.
	 */
	JButton getCloseButton() {
		return this.closeButton;
	}

	/**
	 * Devuelve el bot&oacute;n de abrir o cerrar de los detalles.
	 * @return Bot&oacute;n de abrir o cerrar los detalles.
	 */
	JButton getDetailsButton() {
		return this.detailsButton;
	}

	/**
	 * Variable que indica si los detalles est&aacute;n expandidos
	 * @return Devuelve true si los detalles est&oacute;n expandidos
	 */
	public boolean isExpandedDetails() {
		return this.expandedDetails;
	}

	/**
	 * Da valor a la variable que indica si los detalles est&aacute;n expandidos
	 * @param expandedDetails Valor para indicar si los detalles est&aacute;n expandidos
	 */
	public void setExpandedDetails(final boolean expandedDetails) {
		this.expandedDetails = expandedDetails;
	}

}

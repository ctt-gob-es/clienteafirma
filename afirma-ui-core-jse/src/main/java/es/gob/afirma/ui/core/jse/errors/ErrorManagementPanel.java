/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.ui.core.jse.errors;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.Window;
import java.io.PrintWriter;
import java.io.StringWriter;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

import es.gob.afirma.ui.core.jse.JSEUIMessages;


/**
 * Panel gr&aacute;fico con las opciones de gesti&oacute;n de errores.
 */
public final class ErrorManagementPanel extends JPanel {

	/** Serial Id. */
	private static final long serialVersionUID = 8384222173067817402L;

	private static final int DETAILS_PREFERRED_WIDTH = 700;

	private static final int DETAILS_PREFERRED_HEIGHT = 200;

	private final Window window;

	private JPanel errorInfoPanel;
	private JPanel buttonsPanel;

	private JButton closeButton;
	private JButton detailsButton;
	private JButton copyErrorButton;

	private JScrollPane scrollPane;
	private JTextArea errorTextArea;

	final ErrorManagementHandler eventsHandler;

	private boolean expandedDetails;

	/**
	 * Constructor con la ventana padre.
	 * @param w Di&aacute;logo sobre el que mostrar el panel.
	 * @param t Informaci&oacute;n sobre el error.
	 * @param message Mensaje sobre el error.
	 * @param messageType Tipo de mensaje (precauci&oacute;n o error).
	 */
	ErrorManagementPanel(final ErrorManagementDialog w, final Throwable t, final Object message, final int messageType) {
		this.window = w;
		this.eventsHandler = new ErrorManagementHandler(this, w);
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

		this.errorTextArea = new JTextArea();
		this.errorTextArea.setEditable(false);

		boolean withTrace = false;

		if (t != null && t.getStackTrace().length != 0) {
			final StringWriter errors = new StringWriter();
			t.printStackTrace(new PrintWriter(errors));
			this.errorTextArea.append(errors.toString().replaceAll("\t", "      "));  //$NON-NLS-1$//$NON-NLS-2$
			withTrace = true;
		}

		this.scrollPane = new JScrollPane(this.errorTextArea);
		this.scrollPane.getVerticalScrollBar().setValue(this.scrollPane.getVerticalScrollBar().getMinimum());
		final double screenHeight = Toolkit.getDefaultToolkit().getScreenSize().getHeight();
		final Dimension preferedFrameSize = new Dimension(
				DETAILS_PREFERRED_WIDTH,
				(int) Math.min(DETAILS_PREFERRED_HEIGHT, screenHeight * 0.7));
		this.scrollPane.setPreferredSize(preferedFrameSize);

		//Las barras de scroll se dejan al principio despues de escribir
		SwingUtilities.invokeLater(new Runnable() {
            @Override
			public void run() {
        		getScrollPane().getVerticalScrollBar().setValue(getScrollPane().getVerticalScrollBar().getMinimum());
        		getScrollPane().getHorizontalScrollBar().setValue(getScrollPane().getHorizontalScrollBar().getMinimum());
            }
        });

		//Por defecto no se mostraran los detalles
		this.scrollPane.setVisible(false);

        // Creamos un panel para los botones de detalles y cerrar
        this.buttonsPanel = createButtonsPanel(withTrace);

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
		c.ipady = 140;
		c.insets = new Insets(0, 15,  11,  15);
		add(this.scrollPane, c);

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
	 * @param withTrace indica si viene con informaci&oacute;n sobre el error
	 * @return Panel donde se ubican los botones de detalles y cerrar*/
	private JPanel createButtonsPanel(final boolean withTrace) {

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.TRAILING));

		if(withTrace) {

			this.copyErrorButton = new JButton(JSEUIMessages.getString("JSEUIManager.92")); //$NON-NLS-1$
			this.copyErrorButton.getAccessibleContext().setAccessibleDescription(
				JSEUIMessages.getString("JSEUIManager.92")  //$NON-NLS-1$
			);
			this.copyErrorButton.setVisible(false);
			panel.add(this.copyErrorButton);

			this.detailsButton = new JButton(JSEUIMessages.getString("JSEUIManager.90")); //$NON-NLS-1$
			this.detailsButton.getAccessibleContext().setAccessibleDescription(
				JSEUIMessages.getString("JSEUIManager.90")  //$NON-NLS-1$
			);
			panel.add(this.detailsButton);
		}

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
	 * Devuelve el componente del panel de scroll
	 * @return Panel de scroll.
	 */
	public JScrollPane getScrollPane() {
		return this.scrollPane;
	}

	/**
	 * Devuelve el bot&oacute;n de cierre del di&aacute;logo.
	 * @return Bot&oacute;n de cierre.
	 */
	public JButton getCloseButton() {
		return this.closeButton;
	}

	/**
	 * Devuelve el bot&oacute;n de abrir o cerrar de los detalles.
	 * @return Bot&oacute;n de abrir o cerrar los detalles.
	 */
	public JButton getDetailsButton() {
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

	/**
	 * Devuelve el bot&oacute; de copiar el error del textarea
	 * @return Bot&oacute; de copiar el error del textarea
	 */
	public JButton getCopyErrorButton() {
		return this.copyErrorButton;
	}

	/**
	 * Devuelve el textarea con la informaci&oacute; sobre el error
	 * @return Textarea con la informaci&oacute; sobre el error
	 */
	public JTextArea getErrorTextArea() {
		return this.errorTextArea;
	}
}

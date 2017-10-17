/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.awt.Component;
import java.awt.Dialog;
import java.awt.Dialog.ModalityType;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Window;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;

import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.SwingConstants;

import es.gob.afirma.standalone.SimpleAfirmaMessages;

/** Componente para mostrar di&aacute;logos de error. En caso de no indicar
 * la excepci&oacute;n que origin&oacute; el error, su comportamiento es
 * equivalente a
 * {@code JOptionPane.showMessageDialog(parentComponent, message)}.
 * En caso de indicarlo, se mostrar&aacute; el mensaje de error y se
 * dar&aacute; la opci&oacute;n de ampliar la informaci&oacute;n para ver
 * la traza del error que la origin&oacute;. */
public class ErrorPane {

	private static JPanel mainPanel;

	private static JLabel detailsLabel;

	/**
	 * Muestra el mensaje indicado de forma equivalente a
	 * {@code JOptionPane.showMessageDialog(parentComponent, message)}
	 * @param parentComponent Componente padre sobre el que mostrar el mensaje.
	 * @param message Objeto que mostrar a modo de mensaje.
	 */
	public static void showError(final Component parentComponent, final Object message) {
		showError(parentComponent, message, null);
	}

	/** Muestra el mensaje indicado de forma similar a
	 * {@code JOptionPane.showMessageDialog(parentComponent, message)} y da la
	 * opci&oacute;n de ampliar la informaci&oacute;n del error.
	 * @param parentComponent Componente padre sobre el que mostrar el mensaje.
	 * @param message Objeto que mostrar a modo de mensaje.
	 * @param t Excepci&oacute;n o error que origin&oacute; el mensaje. */
	public static void showError(final Component parentComponent, final Object message, final Throwable t) {

		JDialog dialog;
		if (parentComponent instanceof Window) {
			dialog = new JDialog((Window) parentComponent, ModalityType.APPLICATION_MODAL);
		}
		else if (parentComponent instanceof Frame) {
			dialog = new JDialog((Frame) parentComponent, true);
		}
		else if (parentComponent instanceof Dialog) {
			dialog = new JDialog((Dialog) parentComponent, true);
		}
		else {
			dialog = new JDialog();
		}

		if (t == null) {
			final JOptionPane optionPane = new JOptionPane(
				message,
                JOptionPane.ERROR_MESSAGE,
                JOptionPane.DEFAULT_OPTION
            );
			dialog.setContentPane(optionPane);
			dialog.pack();
			dialog.setVisible(true);

			return;
		}

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 1.0;
		c.gridy = 0;

		// Mostramos el mensaje
		mainPanel = new JPanel(new GridBagLayout());
		if (message instanceof Component) {
			mainPanel.add((Component) message, c);
		}
		else {
			mainPanel.add(new JLabel(message.toString()), c);
		}

		// Mostramos el boton para ampliar informacion
		final ImageIcon showDetailsIcon = new ImageIcon(ErrorPane.class.getResource("/resources/default_cert_ico.png")); //$NON-NLS-1$
		detailsLabel = new JLabel(
			SimpleAfirmaMessages.getString("ErrorPane.1"), //$NON-NLS-1$
			showDetailsIcon,
			SwingConstants.LEFT
		);

		final JPanel detailsPanel = new JPanel();
		detailsPanel.add(detailsLabel);

		c.gridy = 1;

		mainPanel.add(detailsPanel, c);

		detailsLabel.addMouseListener(
			new ErrorPane.DetailsLinkListener(detailsPanel, detailsLabel, getTrace(t))
		);

		JOptionPane.showMessageDialog(parentComponent, mainPanel);
	}

	/** Obtiene la traza de una excepci&oacute;n o error.
	 * @param t Excepci&oacute;n o error del que obtener la traza.
	 * @return Traza. */
	private static String getTrace(final Throwable t) {
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try ( final PrintWriter writer = new PrintWriter(baos); ) {
			t.printStackTrace(writer);
		}
    	return new String(baos.toByteArray());
	}

	private static class DetailsLinkListener implements MouseListener {

		private final JPanel parentPanel;

		private final JLabel showDetailsLabel;

		private boolean isShowingDetails = false;

		private String trace = null;

		private JTextArea stackTraceArea = null;


		public DetailsLinkListener(final JPanel mainPanel, final JLabel detailsLabel, final String trace) {
			this.parentPanel = mainPanel;
			this.showDetailsLabel = detailsLabel;
			this.trace = trace;
		}

		/** Muestra los detalles del error. */
		private void showDetails() {

			if (this.stackTraceArea == null) {
				this.stackTraceArea = new JTextArea();
				this.stackTraceArea.setEditable(false);
			}

			this.stackTraceArea.setText(this.trace);

			final GridBagConstraints c = new GridBagConstraints();
			c.fill = GridBagConstraints.HORIZONTAL;
			c.weightx = 1.0;
			c.gridy = 2;

			this.parentPanel.add(this.stackTraceArea, c);

			final ImageIcon hideDetailsIcon = new ImageIcon(ErrorPane.class.getResource("/resources/dnie_cert_ico.png")); //$NON-NLS-1$
			this.showDetailsLabel.setIcon(hideDetailsIcon);

			this.isShowingDetails = true;
		}

		/**
		 * Oculta los detalles del error.
		 */
		private void hideDetails() {

			this.parentPanel.remove(this.stackTraceArea);

			final ImageIcon showDetailsIcon = new ImageIcon(ErrorPane.class.getResource("/resources/default_cert_ico.png")); //$NON-NLS-1$
			this.showDetailsLabel.setIcon(showDetailsIcon);

			this.isShowingDetails = false;
		}

		@Override
		public void mouseClicked(final MouseEvent e) {
			if (this.isShowingDetails) {
				hideDetails();
			}
			else {
				showDetails();
			}
		}

		@Override
		public void mouseEntered(final MouseEvent e) { /* No hacemos nada. */ }

		@Override
		public void mouseExited(final MouseEvent e) { /* No hacemos nada. */ }

		@Override
		public void mousePressed(final MouseEvent e) { /* No hacemos nada. */ }

		@Override
		public void mouseReleased(final MouseEvent e) { /* No hacemos nada. */ }
	}

}

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
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.geom.RoundRectangle2D;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.SwingConstants;

import es.gob.afirma.standalone.configurator.common.PreferencesManager;

/** Di&aacute;logo que muestra mensajes con la informaci&oacute;n
 * de la tarea que esta realizando la aplicaci&oacute;n */
public final class ProgressInfoDialog extends JDialog {

	private static final long serialVersionUID = -6137113502471587689L;

	private final JLabel labelProgress = new JLabel();

	private final boolean enabledDialog;

	/** Crea un di&aacute;logo de espera indeterminada.
	 * @param parent Marco padre para la modalidad. */
	public ProgressInfoDialog(final Frame parent) {

	    super(parent);
	    setUndecorated(true);
	    setResizable(false);
	    setShape(new RoundRectangle2D.Double(
	        0, 0, getWidth(), getHeight(), 20, 20
	    ));

	    final RoundedPanel panel = new RoundedPanel(20);
	    panel.setLayout(new GridBagLayout());
	    panel.setBackground(Color.WHITE);

	    final GridBagConstraints c = new GridBagConstraints();
	    c.gridx = 0;
	    c.gridy = 0;
	    c.anchor = GridBagConstraints.CENTER;
	    c.fill = GridBagConstraints.HORIZONTAL;
	    c.weightx = 1;

	    this.labelProgress.setHorizontalAlignment(SwingConstants.CENTER);
	    this.labelProgress.setFont(
	        this.labelProgress.getFont().deriveFont(Font.PLAIN, 18f)
	    );
	    panel.add(this.labelProgress, c);

	    setContentPane(panel);

	    this.enabledDialog = PreferencesManager.getBoolean(PreferencesManager.PREFERENCE_GENERAL_ENABLE_PROGRESS_DIALOG);
	}

	/** Establece el mensaje del di&aacute;logo.
	 * @param message Mensaje del di&aacute;logo. */
	public void setMessage(final String message) {
		this.labelProgress.setText(message);
		pack();

		setLocationRelativeTo(null);
		revalidate();

		setVisible(this.enabledDialog);
	}

	/** Obtiene el mensaje del di&aacute;logo.
	 * @return Mensaje del di&aacute;logo. */
	public String getMessage() {
		return this.labelProgress.getText();
	}

	public boolean isEnabledDialog() {
		return this.enabledDialog;
	}

}

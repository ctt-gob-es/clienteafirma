/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.preferences;

import java.awt.Frame;

import javax.swing.JDialog;
import javax.swing.WindowConstants;

import es.gob.afirma.standalone.DesktopUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

/** Di&aacute;logo de preferencias de Autofirma. */
public final class PreferencesDialog extends JDialog{

	private static final long serialVersionUID = 1L;

	/** Constructor del panel de preferencias.
	 * @param parent Padre del panel de preferencias.
	 * @param modal Indica si las preferencias deben mostrarse de forma <i>modal</i>.
	 * @param selectedPreferencesTabIndex &Iacute;ndice de la pesta&ntilde;a de configuraci&oacute;n seleccionada por defecto. */
	public PreferencesDialog(final Frame parent, final boolean modal, final int selectedPreferencesTabIndex) {
		super(parent, modal);
		setTitle(SimpleAfirmaMessages.getString("MainMenu.24")); //$NON-NLS-1$
		setIconImages(DesktopUtil.getIconImages());
		add(new PreferencesPanel(this, selectedPreferencesTabIndex));
		pack();
		setLocationRelativeTo(parent);
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
	}

	/** Constructor del panel de preferencias.
	 * @param parent Padre del panel de preferencias.
	 * @param modal Indica si las preferencias deben mostrarse de forma <i>modal</i>. */
	public PreferencesDialog(final Frame parent, final boolean modal) {
		this(parent, modal, 0);
	}

	/** Muestra el di&aacute;logo de preferencias.
	 * @param parent Componente padre del panel de preferencias.
	 * @param modal Indica si las preferencias deben mostrarse de forma <i>modal</i>.
	 * @param selectedPreferencesTabIndex &Iacute;dice de la pesta&ntilde;a de configuraci&oacute;n seleccionada por defecto. */
	public static void show(final Frame parent, final boolean modal, final int selectedPreferencesTabIndex) {
		new PreferencesDialog(parent, modal, selectedPreferencesTabIndex).setVisible(true);
	}

	/** Muestra el di&aacute;logo de preferencias.
	 * @param parent Componente padre del panel de preferencias.
	 * @param modal Indica si las preferencias deben mostrarse de forma <i>modal</i>. */
	public static void show(final Frame parent, final boolean modal) {
		new PreferencesDialog(parent, modal, 0).setVisible(true);
	}

}

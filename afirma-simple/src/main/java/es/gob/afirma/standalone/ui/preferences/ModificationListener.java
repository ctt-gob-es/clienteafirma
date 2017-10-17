/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.preferences;

import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;



/** Clase que escucha los cambios realizados a los elementos a los que se asigne
 * y notifica a un panel de preferencias en caso de producirse. */
final class ModificationListener extends KeyAdapter implements ItemListener {

    private final PreferencesPanel prefPanel;

    /** Construye un listener capaz de advertir cambios en componentes
     * a partir de listeners de tipo {@code KeyAdapter} y {@code ItemListener}.
     * @param pp Panel de preferencias al que se notifican los cambios. */
    ModificationListener(final PreferencesPanel pp) {
        if (pp == null) {
            throw new IllegalArgumentException(
        		"Se necesita un ModificationListener para indicar que ha habido modificaciones, no puede ser nulo" //$NON-NLS-1$
    		);
        }
        this.prefPanel = pp;
    }

    /** {@inheritDoc} */
    @Override
    public void keyReleased(final KeyEvent arg0) {
        this.prefPanel.setModified(true);
    }

    /** {@inheritDoc} */
	@Override
	public void itemStateChanged(final ItemEvent ie) {
		this.prefPanel.setModified(true);
	}

}

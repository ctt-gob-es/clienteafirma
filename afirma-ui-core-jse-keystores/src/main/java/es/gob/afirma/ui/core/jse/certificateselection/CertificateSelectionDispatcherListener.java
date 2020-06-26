/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.ui.core.jse.certificateselection;

import java.awt.Component;
import java.awt.KeyEventDispatcher;
import java.awt.event.KeyEvent;

final class CertificateSelectionDispatcherListener implements KeyEventDispatcher {

	private final Component parent;
	private final CertificateSelectionDialog selectionDialog;

	CertificateSelectionDispatcherListener(final Component p,
			                               final CertificateSelectionDialog selectionDialog) {
		this.parent = p;
		this.selectionDialog = selectionDialog;
	}

	@Override
	public boolean dispatchKeyEvent(final KeyEvent ke) {
		if (ke.getID() == KeyEvent.KEY_RELEASED) {

			if (KeyEvent.VK_F1 == ke.getKeyCode()) {
				UtilActions.doHelp();
				return false;
			}

			if (KeyEvent.VK_F5 == ke.getKeyCode()) {
				UtilActions.doRefresh(this.selectionDialog, this.parent);
			}
		}
		return false;
	}
}

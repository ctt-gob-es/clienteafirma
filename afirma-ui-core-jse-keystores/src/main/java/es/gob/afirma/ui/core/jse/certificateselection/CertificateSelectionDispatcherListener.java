package es.gob.afirma.ui.core.jse.certificateselection;

import java.awt.Component;
import java.awt.KeyEventDispatcher;
import java.awt.event.KeyEvent;

import es.gob.afirma.core.misc.Platform;

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

			// En OS X el modificador es distinto (la tecla Meta es el "Command" de Mac)
			if (!Platform.OS.MACOSX.equals(Platform.getOS()) && ke.isControlDown() || ke.isMetaDown()) {
				if (KeyEvent.VK_O == ke.getKeyCode()) {
					UtilActions.doOpen(this.selectionDialog, this.parent);
				}
			}
			else if (KeyEvent.VK_F5 == ke.getKeyCode()) {
				UtilActions.doRefresh(this.selectionDialog, this.parent);
			}
		}
		return false;
	}
}

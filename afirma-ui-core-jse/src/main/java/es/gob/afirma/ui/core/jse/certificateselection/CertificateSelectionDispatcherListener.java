package es.gob.afirma.ui.core.jse.certificateselection;

import java.awt.Component;
import java.awt.Desktop;
import java.awt.KeyEventDispatcher;
import java.awt.event.KeyEvent;
import java.net.URI;
import java.util.logging.Logger;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.keystores.KeyStoreRefresher;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.core.jse.JSEUIManager;
import es.gob.afirma.ui.core.jse.JSEUIMessages;

final class CertificateSelectionDispatcherListener implements KeyEventDispatcher {

	private final KeyStoreRefresher localKeyStoreEnabler;

	private static final String[] EXTS;
	private static final String EXTS_DESC;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String HELP_URI = "http://incidencias-ctt.administracionelectronica.gob.es/wiki/doku.php?id=forja-ctt_wiki:clienteafirma:start"; //$NON-NLS-1$
	static {
		if (Platform.OS.MACOSX.equals(Platform.getOS())) {
			EXTS = new String[] {
				"pfx", "p12", "jks", "dylib", "so" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
			};
			EXTS_DESC = " (*.p12, *.pfx, *.jks, *.dylib, *.so)"; //$NON-NLS-1$
		}
		else if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
			EXTS = new String[] {
				"pfx", "p12", "jks", "dll" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			};
			EXTS_DESC = " (*.p12, *.pfx, *.jks, *.dll)"; //$NON-NLS-1$
		}
		else {
			EXTS = new String[] {
				 "pfx", "p12", "jks", "so" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			};
			EXTS_DESC = " (*.p12, *.pfx, *.jks, *.so)"; //$NON-NLS-1$
		}
	}

	private final Component parent;

	CertificateSelectionDispatcherListener(final Component p,
			                               final KeyStoreRefresher lke) {
		this.parent = p;
		this.localKeyStoreEnabler = lke;
	}

	@Override
	public boolean dispatchKeyEvent(final KeyEvent ke) {
		if (ke.getID() == KeyEvent.KEY_RELEASED) {

			if (KeyEvent.VK_F1 == ke.getKeyCode()) {
				if (Desktop.isDesktopSupported()) {
					try {
						Desktop.getDesktop().browse(new URI(HELP_URI));
					}
					catch (final Exception e) {
						LOGGER.severe("No se ha podido abrir la pagina Web de ayuda: " + e); //$NON-NLS-1$
					}
				}
				else {
					LOGGER.warning(
						"No se soporta la apertura de paginas Web, por lo que no se puede abrir la ayuda" //$NON-NLS-1$
					);
				}
				return false;
			}

			if (this.localKeyStoreEnabler != null) {

				// En OS X el modificador es distinto (la tecla Meta es el "Command" de Mac)
				if (!Platform.OS.MACOSX.equals(Platform.getOS()) && ke.isControlDown() || ke.isMetaDown()) {
					if (KeyEvent.VK_O == ke.getKeyCode()) {
						try {
							this.localKeyStoreEnabler.changeToFileStore(new JSEUIManager().getLoadFiles(
								JSEUIMessages.getString("CertificateSelectionDispatcherListener.0"), //$NON-NLS-1$
								null,
								null,
								EXTS,
								JSEUIMessages.getString("CertificateSelectionDispatcherListener.1") + EXTS_DESC, //$NON-NLS-1$
								false,
								false,
								this.parent
							)[0]);
						}
						catch(final AOCancelledOperationException e) {
							return false;
						}
					}
				}
				else if (KeyEvent.VK_F5 == ke.getKeyCode()) {
					this.localKeyStoreEnabler.refresh();
				}
			}

		}
		return false;
	}

}

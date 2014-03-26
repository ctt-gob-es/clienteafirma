package es.gob.afirma.ui.core.jse.certificateselection;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.KeyEventDispatcher;
import java.awt.event.KeyEvent;
import java.io.IOException;
import java.net.URI;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.keystores.KeyStoreRefresher;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.core.jse.JSEUIManager;

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
							new JSEUIManager().getLoadFiles(
								CertificateSelectionDialogMessages.getString("CertificateSelectionDispatcherListener.0"), //$NON-NLS-1$
								null,
								null,
								EXTS,
								CertificateSelectionDialogMessages.getString("CertificateSelectionDispatcherListener.1") + EXTS_DESC, //$NON-NLS-1$
								false,
								false,
								this.parent
							);
						}
						catch(final AOCancelledOperationException e) {
							return false;
						}
					}
				}
				else if (KeyEvent.VK_F5 == ke.getKeyCode()) {
					if (this.parent != null) {
						this.parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
					}
					try {
						Thread.sleep(5000);
					} catch (final InterruptedException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
					try {
						this.localKeyStoreEnabler.refresh();
					}
					catch (final IOException e) {
						LOGGER.severe("Error al refrescar el almacen actual: " + e); //$NON-NLS-1$
						new JSEUIManager().showMessageDialog(
							this.parent,
							CertificateSelectionDialogMessages.getString("CertificateSelectionDispatcherListener.2"), //$NON-NLS-1$
							CertificateSelectionDialogMessages.getString("CertificateSelectionDispatcherListener.3"), //$NON-NLS-1$
							JOptionPane.ERROR_MESSAGE
						);
					}
					finally {
						if (this.parent != null) {
							this.parent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
						}
					}
				}
			}

		}
		return false;
	}

}

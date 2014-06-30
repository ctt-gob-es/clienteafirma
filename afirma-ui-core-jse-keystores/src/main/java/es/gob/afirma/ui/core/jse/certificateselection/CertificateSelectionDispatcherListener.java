package es.gob.afirma.ui.core.jse.certificateselection;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.KeyEventDispatcher;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.ui.core.jse.JSEUIManager;

final class CertificateSelectionDispatcherListener implements KeyEventDispatcher {

	private static final String HELP_URI = "http://incidencias-ctt.administracionelectronica.gob.es/wiki/doku.php?id=forja-ctt_wiki:clienteafirma:start"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	
	private static final String[] EXTS;
	private static final String EXTS_DESC;
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
	private final CertificateSelectionDialog selectionDialog;
	private KeyStoreManager localKeyStore;
	

	CertificateSelectionDispatcherListener(final Component p,
			                               final KeyStoreManager lks,
			                               final CertificateSelectionDialog selectionDialog) {
		this.parent = p;
		this.localKeyStore = lks;
		this.selectionDialog = selectionDialog;
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

			if (this.localKeyStore != null) {

				// En OS X el modificador es distinto (la tecla Meta es el "Command" de Mac)
				if (!Platform.OS.MACOSX.equals(Platform.getOS()) && ke.isControlDown() || ke.isMetaDown()) {
					if (KeyEvent.VK_O == ke.getKeyCode()) {
						final File[] ksFile; 
						try {
							ksFile = new JSEUIManager().getLoadFiles(
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
						
						if (ksFile != null && ksFile.length > 0) {
							AOKeyStoreManager ksm;
							try {
								ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
										AOKeyStore.PKCS12,
										ksFile[0].getAbsolutePath(),
										null,
										AOKeyStore.PKCS12.getStorePasswordCallback(this.parent),
										this.parent);
							} catch (final Exception e) {
								LOGGER.warning("No se ha podido cargar el almacen de certificados seleccionado: " + e); //$NON-NLS-1$
								return false;
							}
							
							this.localKeyStore = ksm;
							this.selectionDialog.changeKeyStore(ksm);
						}
					}
				}
				else if (KeyEvent.VK_F5 == ke.getKeyCode()) {

					if (this.parent != null) {
						this.parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
					}
					try {
						this.localKeyStore.refresh();
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
					
					this.selectionDialog.refresh();
				}
			}

		}
		return false;
	}

}

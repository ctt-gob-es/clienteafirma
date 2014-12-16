package es.gob.afirma.ui.core.jse.certificateselection;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Desktop;
import java.io.File;
import java.net.URI;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;

final class UtilActions {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String HELP_URI;
	static {
		final Properties p = new Properties();
		try {
			p.load(UtilActions.class.getResourceAsStream("/resources/selectiondialogutil.properties")); //$NON-NLS-1$
		}
		catch (final Exception e) {
			LOGGER.severe(
				"No se han podido cargar las propiedades del dialogo de seleccion de certificados, se utilizaran los valores por defecto: " + e //$NON-NLS-1$
			);
		}
		HELP_URI = p.getProperty(
			"helpUrl", //$NON-NLS-1$
			"http://incidencias-ctt.administracionelectronica.gob.es/wiki/doku.php?id=forja-ctt_wiki:clienteafirma:start" //$NON-NLS-1$
		);
	}

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

	private UtilActions() {
		// No instanciable
	}

	static void doHelp() {
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
	}

	static void doRefresh(final CertificateSelectionDialog selectionDialog, final Component parent) {
		if (parent != null) {
			parent.setCursor(new Cursor(Cursor.WAIT_CURSOR));
		}

		selectionDialog.refresh();

		if (parent != null) {
			parent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
		}
	}

	static void doOpen(final CertificateSelectionDialog selectionDialog, final Object parent) {
		final File[] ksFile;
		try {
			ksFile = AOUIFactory.getLoadFiles(
				CertificateSelectionDialogMessages.getString("CertificateSelectionDispatcherListener.0"), //$NON-NLS-1$
				null,
				null,
				EXTS,
				CertificateSelectionDialogMessages.getString("CertificateSelectionDispatcherListener.1") + EXTS_DESC, //$NON-NLS-1$
				false,
				false,
				null,
				parent
			);
		}
		catch(final AOCancelledOperationException e) {
			// Se ignora
			return;
		}

		if (ksFile != null && ksFile.length > 0) {
			AOKeyStoreManager ksm;
			try {
				ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
						AOKeyStore.PKCS12,
						ksFile[0].getAbsolutePath(),
						null,
						AOKeyStore.PKCS12.getStorePasswordCallback(parent),
						parent);
			}
			catch (final Exception e) {
				LOGGER.warning("No se ha podido cargar el almacen de certificados seleccionado: " + e); //$NON-NLS-1$
				AOUIFactory.showErrorMessage(
					parent,
					CertificateSelectionDialogMessages.getString("CertificateSelectionDispatcherListener.4"), //$NON-NLS-1$
					CertificateSelectionDialogMessages.getString("CertificateSelectionDispatcherListener.3"), //$NON-NLS-1$
					AOUIFactory.ERROR_MESSAGE
				);
				return;
			}

			selectionDialog.changeKeyStore(ksm);
		}
	}

}

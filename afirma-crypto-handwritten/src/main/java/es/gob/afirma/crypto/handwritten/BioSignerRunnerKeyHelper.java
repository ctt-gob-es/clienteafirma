package es.gob.afirma.crypto.handwritten;

import java.awt.Container;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOCertificatesNotFoundException;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.callbacks.NullPasswordCallback;

final class BioSignerRunnerKeyHelper {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	static PrivateKeyEntry getKeyFromPkcs12(final String p12File,
			                                final String p12password,
			                                final String p12alias) {
		System.out.println("P12: " + p12File + ", " + p12password + ", " + p12alias); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		return null;

	}

	static PrivateKeyEntry getKey(final Container parent) {
		// Obtenemos el gestor del almac&eacute;n de claves (KeyStoreManager)
		final AOKeyStoreManager ksm;
		try {
			if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
				ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
					AOKeyStore.WINDOWS,
					null,
					"Windows", //$NON-NLS-1$
					AOKeyStore.WINDOWS.getStorePasswordCallback(parent),
					parent
				);
			}
			else if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
						AOKeyStore.APPLE,
						null,
						"Apple", //$NON-NLS-1$
						AOKeyStore.APPLE.getStorePasswordCallback(parent),
						parent
					);
			}
			else {
				ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
						AOKeyStore.MOZ_UNI,
						null,
						"NSS", //$NON-NLS-1$
						AOKeyStore.MOZ_UNI.getStorePasswordCallback(parent),
						parent
					);
			}
		}
		catch(final Exception e) {
			AOUIFactory.showErrorMessage(
					parent,
					HandwrittenMessages.getString("BioSignerRunner.25"), //$NON-NLS-1$
					HandwrittenMessages.getString("BioSignerRunner.24"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
			return null;
		}

		final AOKeyStoreDialog dialog = new AOKeyStoreDialog(ksm, null, true, true, false);
		String alias;
		try {
			alias = dialog.show();
		}
		catch (AOCertificatesNotFoundException e) {
			LOGGER.warning("Error, no hay certificados para firmar. " + e); //$NON-NLS-1$
			AOUIFactory
				.showErrorMessage(
					parent,
					HandwrittenMessages.getString("BioSignerRunner.26"), //$NON-NLS-1$
					HandwrittenMessages.getString("BioSignerRunner.24"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
			return null;
		}

		// Obtenemos la entrada de la clave privada
		final PrivateKeyEntry signKey;
		try {
			signKey = ksm.getKeyEntry(alias, NullPasswordCallback.getInstance());
		}
		catch (Exception e) {
			LOGGER.warning("Error accediendo a la clave de firma. " + e); //$NON-NLS-1$
			AOUIFactory
				.showErrorMessage(
					parent,
					HandwrittenMessages.getString("BioSignerRunner.27"), //$NON-NLS-1$
					HandwrittenMessages.getString("BioSignerRunner.24"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
			return null;
		}

		return signKey;
	}

}

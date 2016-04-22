package es.gob.afirma.standalone.configurator;

import java.io.File;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.cert.Certificate;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.standalone.configurator.CertUtil.CertPack;
import es.gob.afirma.standalone.configurator.ConfiguratorFirefox.MozillaProfileNotFoundException;

/** Configura la instalaci&oacute;n en Windows para la correcta ejecuci&oacute;n de AutoFirma. */
final class ConfiguratorWindows implements Configurator {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String KS_FILENAME = "autofirma.pfx"; //$NON-NLS-1$
	private static final String FILE_AUTOFIRMA_CERTIFICATE = "AutoFirma_ROOT.cer"; //$NON-NLS-1$
	private static final String KS_PASSWORD = "654321"; //$NON-NLS-1$

	@Override
	public void configure(final Console window) throws IOException, GeneralSecurityException {

		window.print(Messages.getString("ConfiguratorWindows.2")); //$NON-NLS-1$

		final File appDir = ConfiguratorUtil.getApplicationDirectory();

		window.print(Messages.getString("ConfiguratorWindows.3") + appDir.getAbsolutePath()); //$NON-NLS-1$

		if (!checkSSLKeyStoreGenerated(appDir)) {
			uninstall();
			window.print(Messages.getString("ConfiguratorWindows.5")); //$NON-NLS-1$
			final CertPack certPack = CertUtil.getCertPackForLocalhostSsl(
				ConfiguratorUtil.CERT_ALIAS,
				KS_PASSWORD
			);

			window.print(Messages.getString("ConfiguratorWindows.11")); //$NON-NLS-1$

			//Generacion del certificado pfx
			ConfiguratorUtil.installFile(
				certPack.getPkcs12(),
				new File(appDir, KS_FILENAME)
			);
			
			//Generacion del certificado raiz .cer
			ConfiguratorUtil.installFile(
					certPack.getCaCertificate().getEncoded(), 
					new File(appDir, FILE_AUTOFIRMA_CERTIFICATE));

			try {
				window.print(Messages.getString("ConfiguratorWindows.13")); //$NON-NLS-1$
				ConfiguratorFirefox.installRootCAMozillaKeyStore(appDir, certPack.getCaCertificate());
				ConfiguratorFirefox.removeConfigurationFiles(appDir);
				window.print(Messages.getString("ConfiguratorWindows.4")); //$NON-NLS-1$
				window.print(Messages.getString("ConfiguratorWindows.9")); //$NON-NLS-1$
				window.print(Messages.getString("ConfiguratorWindows.7")); //$NON-NLS-1$
			}
			catch(final MozillaProfileNotFoundException e) {
				window.print(Messages.getString("ConfiguratorWindows.12") + ": " + e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		else {
			window.print(Messages.getString("ConfiguratorWindows.14")); //$NON-NLS-1$
		}

		window.print(Messages.getString("ConfiguratorWindows.8")); //$NON-NLS-1$

	}

	/** Comprueba si ya existe un almac&eacute;n de certificados generado.
	 * @param appDir Directorio de la aplicaci&oacute;n.
	 * @return {@code true} si ya existe un almacen de certificados SSL, {@code false} en caso contrario. */
	private static boolean checkSSLKeyStoreGenerated(final File appDir) {
		return new File(appDir, KS_FILENAME).exists();
	}

	private static void importCARootOnWindowsKeyStore(final Certificate cert) throws GeneralSecurityException, IOException {

		final KeyStore ks = KeyStore.getInstance("Windows-ROOT"); //$NON-NLS-1$
		ks.load(null,  null);

		boolean installed = false;
		boolean cancelled = false;
		do {
			try {
				ks.setCertificateEntry(CertUtil.AF_ROOT_SUBJECT_PRINCIPAL, cert);
				installed = true;
			}
			catch (final KeyStoreException e) {
				LOGGER.warning(
					"No se pudo instalar la CA del certificado SSL para el socket en el almacen de Windows: " + e //$NON-NLS-1$
				);
				final int result = JOptionPane.showConfirmDialog(
					null,
					Messages.getString("ConfiguratorWindows.0"), //$NON-NLS-1$
					Messages.getString("ConfiguratorWindows.1"), //$NON-NLS-1$
					JOptionPane.OK_CANCEL_OPTION,
					JOptionPane.WARNING_MESSAGE
				);
				if (result == JOptionPane.CANCEL_OPTION) {
					cancelled = true;
					LOGGER.severe("El usuario cancelo la instalacion del certificado SSL para el socket: " + e); //$NON-NLS-1$
				}
			}
		}
		while (!installed && !cancelled);
	}

	@Override
	public void uninstall() {

		LOGGER.info("Desinstalamos el certificado raiz del almacen de Windows"); //$NON-NLS-1$
		uninstallRootCAWindowsKeyStore();

		LOGGER.info("Desinstalamos el certificado raiz del almacen de Firefox"); //$NON-NLS-1$
		ConfiguratorFirefox.uninstallRootCAMozillaKeyStore(ConfiguratorUtil.getApplicationDirectory());

		// No es necesario eliminar nada mas porque el verdadero proceso de desinstalacion
        // eliminara el directorio de aplicacion con todo su contenido
	}

	private static void uninstallRootCAWindowsKeyStore() {
		try {
			final KeyStore ks = KeyStore.getInstance("Windows-ROOT"); //$NON-NLS-1$
			ks.load(null,  null);
			ks.deleteEntry(CertUtil.AF_ROOT_SUBJECT_PRINCIPAL);
		}
		catch (final Exception e) {
			LOGGER.warning("No se pudo desinstalar el certificado SSL raiz del almacen de Windows: " + e); //$NON-NLS-1$
		}
	}

}

package es.gob.afirma.standalone.configurator;

import java.io.File;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.logging.Logger;

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

	@Override
	public void uninstall() {

		LOGGER.info("Desinstalamos el certificado raiz del almacen de Firefox"); //$NON-NLS-1$
		ConfiguratorFirefox.uninstallRootCAMozillaKeyStore(ConfiguratorUtil.getApplicationDirectory());

		// No es necesario eliminar nada mas porque el verdadero proceso de desinstalacion
        // eliminara el directorio de aplicacion con todo su contenido
	}

}

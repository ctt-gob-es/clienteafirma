package es.gob.afirma.standalone.configurator;

import java.io.File;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.cert.Certificate;
import java.util.logging.Logger;

import es.gob.afirma.standalone.configurator.CertUtil.CertPack;
import es.gob.afirma.standalone.configurator.ConfiguratorFirefox.MozillaProfileNotFoundException;

/**
 * Configura la instalaci&oacute;n en Linux para la correcta ejecuci&oacute;n de
 * AutoFirma.
 */
final class ConfiguratorMacOSX implements Configurator {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final String KS_FILENAME = "autofirma.pfx"; //$NON-NLS-1$
	private static final char[] KS_PASSWORD = "654321".toCharArray(); //$NON-NLS-1$
	private static final String CERT_CN = "127.0.0.1"; //$NON-NLS-1$
	static final String GET_USER_SCRIPT = "/getUsers.sh";//$NON-NLS-1$
	private static final String MACOSX_CERTIFICATE = "/autofirma.cer";//$NON-NLS-1$
	private static final String KEYCHAIN_PATH = "/Library/Keychains/System.keychain"; //$NON-NLS-1$
	private static final String OSX_SEC_COMMAND = "security add-trusted-cert -d -r trustRoot -k %KEYCHAIN% %CERT%"; //$NON-NLS-1$
	private static final String OSX_SEC_COMMAND_UNINSTALL = "sudo security delete-certificate -c %CERT% %KEYCHAIN%"; //$NON-NLS-1$
	static final String OSX_GET_USERS_COMMAND = "dscacheutil -q user"; //$NON-NLS-1$
	static final String MAC_SCRIPT_NAME = "/installCerScript.sh"; //$NON-NLS-1$
	static final String MAC_PATH_SCRIPT = "/var/temp" + MAC_SCRIPT_NAME; //$NON-NLS-1$
	static final String EXPORT_PATH = "export PATH=$PATH:";//$NON-NLS-1$
	static final String EXPORT_LIBRARY_LD = "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:";//$NON-NLS-1$


	@Override
	public void configure(final Console window) throws IOException, ConfigurationException, GeneralSecurityException {


		window.print(Messages.getString("ConfiguratorMacOSX.2")); //$NON-NLS-1$

		final File appDir = ConfiguratorUtil.getApplicationDirectory();

		window.print(Messages.getString("ConfiguratorMacOSX.3") + appDir.getAbsolutePath()); //$NON-NLS-1$

		if (!checkSSLKeyStoreGenerated(appDir)) {
			window.print(Messages.getString("ConfiguratorMacOSX.5")); //$NON-NLS-1$

			final CertPack certPack = CertUtil.generateSSLCertificate(CERT_CN, ConfiguratorUtil.CERT_ALIAS, KS_PASSWORD,
					false);

			window.print(Messages.getString("ConfiguratorMacOSX.11")); //$NON-NLS-1$

			ConfiguratorUtil.installFile(certPack.getPkcs12(),
					new File(ConfiguratorUtil.getApplicationDirectory(), KS_FILENAME));

			window.print(Messages.getString("ConfiguratorMacOSX.6")); //$NON-NLS-1$

			// damos permisos al script
			ConfiguratorFirefox.addExexPermissionsToAllFilesOnDirectory(ConfiguratorUtil.getApplicationDirectory());
			try {
				window.print(Messages.getString("ConfiguratorMacOSX.13")); //$NON-NLS-1$
				// generamos el script para instalar el certificado en mozilla
				ConfiguratorFirefox.installRootCAMozillaKeyStore(appDir, certPack.getCertificate(),new String[] { OSX_GET_USERS_COMMAND});
				// hay que instalar el certificado en el almacen de Apple
				importCARootOnMacOSXKeyStore(certPack.getCertificate());
				ConfiguratorFirefox.addExexPermissionsToFile(new File(MAC_PATH_SCRIPT));
				ConfiguratorFirefox.executeScriptMacOsx(MAC_PATH_SCRIPT, true, true);
			} catch (final MozillaProfileNotFoundException e) {
				window.print(Messages.getString("ConfiguratorMacOSX.12")); //$NON-NLS-1$
			}

		} else {
			window.print(Messages.getString("ConfiguratorMacOSX.14")); //$NON-NLS-1$
		}

		window.print(Messages.getString("ConfiguratorMacOSX.8")); //$NON-NLS-1$
	}

	/**
	 * Comprueba si ya existe un almac&eacute;n de certificados generado.
	 *
	 * @param appConfigDir
	 *            Directorio de configuraci&oacute;n de la aplicaci&oacute;n.
	 * @return {@code true} si ya existe un almacen de certificados SSL,
	 *         {@code false} en caso contrario.
	 */
	private static boolean checkSSLKeyStoreGenerated(final File appConfigDir) {
		return new File(appConfigDir, KS_FILENAME).exists();
	}

	/**
	 * Genera el comando de instalaci&oacute;n del certificado en el almac&eacute;n de apple en el script de instalaci&oacute;n.
	 * @param cert Certificado a instalar.
	 * @throws GeneralSecurityException Se produce si hay un problema de seguridad durante el proceso.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero.
	 */
	static void importCARootOnMacOSXKeyStore(final Certificate cert) throws GeneralSecurityException, IOException {
		final File f = new File(ConfiguratorUtil.getApplicationDirectory() + MACOSX_CERTIFICATE);
		final String cmd = OSX_SEC_COMMAND.replace("%KEYCHAIN%", KEYCHAIN_PATH).replace("%CERT%", //$NON-NLS-1$ //$NON-NLS-2$
				f.getAbsolutePath().replace(" ", "\\ ")); //$NON-NLS-1$ //$NON-NLS-2$
		LOGGER.info("comando de instalacion del certificado en el almacen de apple: " + cmd); //$NON-NLS-1$
		ConfiguratorFirefox.writeScriptFile(MAC_PATH_SCRIPT, new StringBuilder(cmd), true);
	}

	@Override
	public void uninstall() {
		LOGGER.info("Desinstalamos el certificado raiz del almacen de Windows"); //$NON-NLS-1$

		 try {
			// generamos script borrar almacen de certificados de apple
			 uninstallRootCAMacOSXKeyStore();
			 // generamos script para borrar el almacen certificados firefox
			ConfiguratorFirefox.generateUninstallScriptMac(ConfiguratorUtil.getApplicationDirectory());
			ConfiguratorFirefox.addExexPermissionsToAllFilesOnDirectory(ConfiguratorUtil.getApplicationDirectory());
			ConfiguratorFirefox.executeScriptMacOsx(ConfiguratorUtil.getApplicationDirectory().getAbsolutePath() + MAC_SCRIPT_NAME, true, true);
		} catch (MozillaProfileNotFoundException | IOException e) {
			LOGGER.severe("Se ha producido un error durante la desinstalaci&oacute;n :" + e); //$NON-NLS-1$
		}
	}

	/**
	 * Genera el script de desinstalaci&oacute;n del llavero OS X mediante AppleScript del certificado generado.
	 * @throws IOException Se produce cuando hay un error en la creaci&oacute;n del fichero.
	 */
	private static void uninstallRootCAMacOSXKeyStore() throws IOException {
		String cmd = OSX_SEC_COMMAND_UNINSTALL.replace("%KEYCHAIN%", KEYCHAIN_PATH); //$NON-NLS-1$
		cmd = cmd.replace("%CERT%", CERT_CN); //$NON-NLS-1$
		ConfiguratorFirefox.writeScriptFile(ConfiguratorUtil.getApplicationDirectory() + MAC_SCRIPT_NAME, new StringBuilder(cmd), true);
	}
}

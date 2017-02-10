package es.gob.afirma.standalone.configurator;

import java.io.File;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.logging.Logger;

import es.gob.afirma.standalone.configurator.CertUtil.CertPack;
import es.gob.afirma.standalone.configurator.ConfiguratorFirefox.MozillaProfileNotFoundException;

/** Configura la instalaci&oacute;n en Linux para la correcta ejecuci&oacute;n de AutoFirma. */
final class ConfiguratorLinux implements Configurator {

    static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static final String KS_FILENAME = "autofirma.pfx"; //$NON-NLS-1$
    private static final String FILE_AUTOFIRMA_CERTIFICATE = "AutoFirma_ROOT.cer"; //$NON-NLS-1$
    private static final String KS_PASSWORD = "654321"; //$NON-NLS-1$
    static final String EXPORT_PATH = "export PATH=$PATH:"; //$NON-NLS-1$
    static final String EXPORT_LD_LIBRARY ="export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:"; //$NON-NLS-1$

    @Override
    public void configure(final Console window) throws IOException, GeneralSecurityException {

        LOGGER.info(Messages.getString("ConfiguratorLinux.2")); //$NON-NLS-1$

        final File appDir = ConfiguratorUtil.getApplicationDirectory();

        LOGGER.info(Messages.getString("ConfiguratorLinux.3") + appDir.getAbsolutePath()); //$NON-NLS-1$

        if (!checkSSLKeyStoreGenerated(appDir)) {
            LOGGER.info(Messages.getString("ConfiguratorLinux.5")); //$NON-NLS-1$

            final CertPack certPack = CertUtil.getCertPackForLocalhostSsl(
                ConfiguratorUtil.CERT_ALIAS,
                KS_PASSWORD
            );

            LOGGER.info(Messages.getString("ConfiguratorLinux.11")); //$NON-NLS-1$

           //Generacion del certificado pfx
            ConfiguratorUtil.installFile(certPack.getPkcs12(), new File(
            		ConfiguratorUtil.getApplicationDirectory(), KS_FILENAME));

          //Generacion del certificado raiz .cer
            ConfiguratorUtil.installFile(
            		certPack.getCaCertificate().getEncoded(),
            		new File(ConfiguratorUtil.getApplicationDirectory(), FILE_AUTOFIRMA_CERTIFICATE));

            // comando para sacar los usuarios del sistema
            final String[] command = new String[] {
    				"cut", //$NON-NLS-1$
    				"-d:", //$NON-NLS-1$
    				"-f6", //$NON-NLS-1$
    				"/etc/passwd" //$NON-NLS-1$
    				};

            try {
                LOGGER.info(Messages.getString("ConfiguratorLinux.13")); //$NON-NLS-1$
                ConfiguratorFirefox.removeAppExecutionWarningInChrome(appDir, command);
                ConfiguratorFirefox.installRootCAChromeKeyStore(appDir, command);
                ConfiguratorFirefox.installRootCAMozillaKeyStore(appDir, command);
              }
            catch(final MozillaProfileNotFoundException e) {
                LOGGER.warning(Messages.getString("ConfiguratorLinux.12")); //$NON-NLS-1$
            }
        }
        else {
            LOGGER.info(Messages.getString("ConfiguratorLinux.14")); //$NON-NLS-1$
        }

        LOGGER.info(Messages.getString("ConfiguratorLinux.8")); //$NON-NLS-1$
    }

    /** Comprueba si ya existe un almac&eacute;n de certificados generado.
     * @param appConfigDir Directorio de configuraci&oacute;n de la aplicaci&oacute;n.
     * @return {@code true} si ya existe un almacen de certificados SSL, {@code false} en caso contrario. */
    private static boolean checkSSLKeyStoreGenerated(final File appConfigDir) {
        return new File(appConfigDir, KS_FILENAME).exists();
    }

    @Override
    public void uninstall() {
        // No es necesario hacer nada porque el  proceso de desinstalacion de linux
        // eliminara el directorio de aplicacion con todo su contenido.
    }
}

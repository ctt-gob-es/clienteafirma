/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 2 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.install;

import java.awt.Component;
import java.io.File;
import java.net.URL;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.logging.Logger;

import javax.swing.JOptionPane;


/** Instalador del cliente AFirma.
 * @version 2.0 */
final class Installer {
    
    /** Gestor de registro. */
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$;

    /** Directorio de instalaci&oacute;n. */
    static final String INSTALL_DIR = ".cafirma"; //$NON-NLS-1$

    /** Construcci&oacute;n LITE del cliente Afirma. */
    static final String LITE = "LITE"; //$NON-NLS-1$

    /** Componente padre sobre el que se situar&aacute;n los mensajes del instalador. */
    private Component parentComponent = null;

    /** Componente para la comprobaci&oacute;n e instalaci&oacute;n de las dependencias
     * de entorno del Cliente. */
    private final CheckAndInstallMissingParts enviromentInstaller;

    /** Crea el objeto instalador tomando un componente padre como referencia para mostrar los mensajes
     * modales y unos par&aacute;metros de instalaci&oacute;n.
     * @param parentComponent Componente padre, puede ser nulo.
     * @param params Parametros de instalaci&oacute;n.
     * @exception NullPointerException Si no se han indicado los par&aacute;metros de la instalaci&oacute;n. */
    Installer(final Component parentComponent, final URL codeBase, final String build) {

        // Establecemos el componente padre sobre el que se mostraran los dialogos del instalador
        // (puede ser nulo).
        this.parentComponent = parentComponent;

        // Manejador para la comprobacion e instalacion de las dependencias del cliente
        // propias del entorno (Manejadores de repositorios, bibliotecas de terceros,...)
        this.enviromentInstaller = new CheckAndInstallMissingParts(Platform.getOS(), Platform.getJavaVersion(), build, codeBase);

    }

    /** Desinstala el cliente de firma al completo. */
    void uninstall() {
        final File afirmaDir = new File(Platform.getUserHome() + File.separator + Installer.INSTALL_DIR);

        AccessController.doPrivileged(new PrivilegedAction<Void>() {
            /** {@inheritDoc} */
            public Void run() {
                if (!afirmaDir.exists()) {
                    LOGGER.info("El directorio de instalacion no existe, se omitira la operacion"); //$NON-NLS-1$
                    return null;
                }
                try {
                    fileDelete(afirmaDir);
                    JOptionPane.showMessageDialog(Installer.this.parentComponent, BootLoaderMessages.getString("Installer.11"), //$NON-NLS-1$
                                                  BootLoaderMessages.getString("Installer.12"), //$NON-NLS-1$
                                                  JOptionPane.INFORMATION_MESSAGE);
                    return null;
                }
                catch (final Exception e) {
                    LOGGER.warning("No se ha podido eliminar el directorio de instalacion: " + e); //$NON-NLS-1$
                    JOptionPane.showMessageDialog(Installer.this.parentComponent, BootLoaderMessages.getString("Installer.13"), //$NON-NLS-1$
                                                  BootLoaderMessages.getString("Installer.12"), //$NON-NLS-1$
                                                  JOptionPane.ERROR_MESSAGE);
                    return null;
                }
            }

            private void fileDelete(final File srcFile) {
                if (srcFile.isDirectory()) {
                    for (final File f : srcFile.listFiles()) {
                        fileDelete(f);
                    }
                    srcFile.delete();
                }
                else {
                    srcFile.delete();
                }
            }
        });

    }

    /** Informa al usuario que se va a proceder a instalar el cliente Afirma y, a continuaci&oacute;n,
     * muestra al usuario el acuerdo de licencia. La funci&oacute;n devuelve {@code true} o {@code false} seg&uacute;n se acepte o no el
     * di&aacute;logo.
     * @param build Construcci&oacute;n que se de desea instalar.
     * @param parentComponent Componente padre sobre el que se muestran los di&aacute;logos.
     * @return Indica si se ha aceptado o no el acuerdo de licencia. */
    private boolean prepareInstall() {
        JOptionPane.showMessageDialog(this.parentComponent,
                                      BootLoaderMessages.getString("Installer.0"), BootLoaderMessages.getString("Installer.1"), JOptionPane.INFORMATION_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
        final boolean accepted = new LicenceDialogPanel(this.parentComponent).showDisclaimer();
        if (accepted) {
            LOGGER.info("Se ha aceptado el acuerdo de licencia"); //$NON-NLS-1$
        }
        else {
            LOGGER.info("No se ha aceptado el acuerdo de licencia, no se instalara el Cliente"); //$NON-NLS-1$
        }
        return accepted;
    }

    void install() {

        boolean allOK = true;
        boolean licenciaMostrada = false;

        try {
            if (this.enviromentInstaller.isEndorsedJava5AFirmaDependenciesNeeded()) {
                if (!licenciaMostrada && !prepareInstall()) {
                    return;
                }
                licenciaMostrada = true;
                LOGGER.info("Instalando dependencias de @firma para Java 5..."); //$NON-NLS-1$
                this.enviromentInstaller.installEndorsedJava5AFirmaDependencies();
            }
        }
        catch (final Exception e) {
            LOGGER.severe("Error instalando las dependencias para Java 5, la ejecucion sobre Java 5 puede fallar: " + e); //$NON-NLS-1$
            if (AfirmaBootLoader.DEBUG) {
                final java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
                e.printStackTrace(new java.io.PrintStream(baos));
                LOGGER.warning(new String(baos.toByteArray()));
            }
            allOK = false;
        }

        try {
            if (this.enviromentInstaller.isEndorsedXalanNeeded()) {
                if (!licenciaMostrada && !prepareInstall()) {
                    return;
                }
                licenciaMostrada = true;
                LOGGER.info("Instalando Apache XALAN..."); //$NON-NLS-1$
                this.enviromentInstaller.installEndorsedXalan();
            }
        }
        catch (final Exception e) {
            LOGGER.severe("Error instalando Apache Xalan, la ejecucion sobre Java 5 puede fallar: " + e); //$NON-NLS-1$
            if (AfirmaBootLoader.DEBUG) {
                final java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
                e.printStackTrace(new java.io.PrintStream(baos));
                LOGGER.warning(new String(baos.toByteArray()));
            }
            allOK = false;
        }

        try {
            if (this.enviromentInstaller.isNSSNeeded()) {
                if (!licenciaMostrada && !prepareInstall()) {
                    return;
                }
                licenciaMostrada = true;
                LOGGER.info("Instalando NSS..."); //$NON-NLS-1$
                this.enviromentInstaller.installNSS();
            }
        }
        catch (final Exception e) {
            LOGGER.severe("Error instalando NSS, la ejecucion sobre Firefox puede fallar: " + e); //$NON-NLS-1$
            if (AfirmaBootLoader.DEBUG) {
                final java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
                e.printStackTrace(new java.io.PrintStream(baos));
                LOGGER.warning(new String(baos.toByteArray()));
            }
            allOK = false;
        }

        try {
            if (this.enviromentInstaller.isSunMSCAPINeeded()) {
                if (!licenciaMostrada && !prepareInstall()) {
                    return;
                }
                licenciaMostrada = true;
                LOGGER.info("Instalando SunMSCAPI..."); //$NON-NLS-1$
                this.enviromentInstaller.installSunMSCAPI();
            }
        }
        catch (final Exception e) {
            LOGGER.severe("Error instalando SunMSCAPI, la ejecucion sobre Java 64 bits o Java 5 puede fallar: " + e); //$NON-NLS-1$
            if (AfirmaBootLoader.DEBUG) {
                final java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
                e.printStackTrace(new java.io.PrintStream(baos));
                LOGGER.warning(new String(baos.toByteArray()));
            }
            allOK = false;
        }

        try {
            if (this.enviromentInstaller.isSunPKCS11Needed()) {
                if (!licenciaMostrada && !prepareInstall()) {
                    return;
                }
                licenciaMostrada = true;
                LOGGER.info("Instalando SunPKCS11..."); //$NON-NLS-1$
                this.enviromentInstaller.installSunPKCS11();
            }
        }
        catch (final Exception e) {
            LOGGER.severe("Error instalando SunPKCS11, la ejecucion sobre Java 64 bits puede fallar: " + e); //$NON-NLS-1$
            if (AfirmaBootLoader.DEBUG) {
                final java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
                e.printStackTrace(new java.io.PrintStream(baos));
                LOGGER.warning(new String(baos.toByteArray()));
            }
            allOK = false;
        }

        if (licenciaMostrada && allOK) {
            JOptionPane.showMessageDialog(this.parentComponent, BootLoaderMessages.getString("Installer.23"), //$NON-NLS-1$
                                          BootLoaderMessages.getString("Installer.24"), //$NON-NLS-1$
                                          JOptionPane.INFORMATION_MESSAGE);
        }
        else if (licenciaMostrada) {
            JOptionPane.showMessageDialog(this.parentComponent, BootLoaderMessages.getString("Installer.25"), //$NON-NLS-1$
                                          BootLoaderMessages.getString("Installer.26"), //$NON-NLS-1$
                                          JOptionPane.ERROR_MESSAGE);
        }

    }

}

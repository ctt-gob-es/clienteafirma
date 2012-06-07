/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.install;

import java.awt.Component;
import java.net.URL;
import java.util.logging.Logger;

import javax.swing.JOptionPane;


/** Instalador del cliente AFirma.
 * @version 2.0 */
final class Installer {

    /** Gestor de registro. */
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$;

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
     * @param codeBase Ruta en la cual se encuentran los instalables.
     * @param build Construcci&oacute;n del Cliente que se desea cargar.
     * @exception NullPointerException Si no se han indicado los par&aacute;metros de la instalaci&oacute;n. */
    Installer(final Component parentComponent, final URL codeBase, final String build) {

    	System.out.println("DEBUG: Constructor de Installer");

        // Establecemos el componente padre sobre el que se mostraran los dialogos del instalador
        // (puede ser nulo).
        this.parentComponent = parentComponent;

        // Manejador para la comprobacion e instalacion de las dependencias del cliente
        // propias del entorno (Manejadores de repositorios, bibliotecas de terceros,...)
        this.enviromentInstaller = new CheckAndInstallMissingParts(BootPlatform.getOS(), BootPlatform.getJavaVersion(), build, codeBase);

    }

    /** Informa al usuario que se va a proceder a instalar el cliente Afirma y, a continuaci&oacute;n,
     * muestra al usuario el acuerdo de licencia. La funci&oacute;n devuelve {@code true} o {@code false} seg&uacute;n se acepte o no el
     * di&aacute;logo.
     * @param build Construcci&oacute;n que se de desea instalar.
     * @param parentComponent Componente padre sobre el que se muestran los di&aacute;logos.
     * @return Indica si se ha aceptado o no el acuerdo de licencia. */
    private boolean prepareInstall() {
        JOptionPane.showMessageDialog(
    		this.parentComponent,
            BootLoaderMessages.getString("Installer.0"), //$NON-NLS-1$
            BootLoaderMessages.getString("Installer.1"), //$NON-NLS-1$
            JOptionPane.INFORMATION_MESSAGE
        );
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

        System.out.println("DEBUG: Installer.install");

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
            if (CheckAndInstallMissingParts.isSunPKCS11Needed()) {
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

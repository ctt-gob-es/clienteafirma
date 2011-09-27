/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.install;

import java.net.URL;
import java.security.AccessController;
import java.util.logging.Logger;

import javax.swing.JApplet;
import javax.swing.SwingUtilities;


/** Clase encargada de instalar el cliente de firma del Ministerio de la Presidencia, tanto
 * las librer&iacute;as de las que depende como el propio applet cliente (cliente + librerias
 * de compatibilidad con afirma 5). */
public final class AfirmaBootLoader extends JApplet {

    /** Activar a <code>true</code> &uacute;nicamente para pruebas. */
    static final boolean DEBUG = false;

    private static final long serialVersionUID = -2570412953683244702L;

    /** Gestor de registro. */
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$;

    @Override
    public void init() {
        LOGGER.info("BootLoader de @firma iniciado"); //$NON-NLS-1$
        LOGGER.info("Arquitectura del JRE: " + BootPlatform.getJavaArch()); //$NON-NLS-1$
        LOGGER.info("Arquitectura del sistema operativo: " + BootPlatform.getOsArch()); //$NON-NLS-1$
        
        if (DEBUG) {
            LOGGER.warning("Modo de depuracion activado"); //$NON-NLS-1$
        }
                
        URL codeBase = null;
        String baseDownloadURL = getParameter("baseDownloadURL"); //$NON-NLS-1$
        if (baseDownloadURL != null) {
            try {
                codeBase = new URL(baseDownloadURL);
            }
            catch(final Exception e) {
                LOGGER.warning("No se puede usar " + baseDownloadURL + " como URL de descarga se usara la por defecto del Applet: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
        if (codeBase == null) {
            codeBase = this.getCodeBase();
        }
        
        install(getParameter("installType"), codeBase); //$NON-NLS-1$
        
    }

    /** Instala las dependencias del Cliente @firma respecto al entorno operativo
     * del cliente de firma, incluyendo las partes nativas y clases Java necesarias.
     * @param installType Contrucci&oacute;n que se desea instalar.
     * @param codeBase Direcci&oacute;n de donde descargar las bibliotecas a instralar
     */
    private void install(final String build, final URL codeBase) {
        LOGGER.info("Invocando instalar(" + build + ", " + codeBase + ")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

        // Realizamos la instalacion
        AccessController.doPrivileged(new java.security.PrivilegedAction<Void>() {
            /** {@inheritDoc} */
            public Void run() {
                SwingUtilities.invokeLater(new Runnable() {
                    /** {@inheritDoc} */
                    public void run() {
                        // Configuramos el instalador e instalamos
                        new Installer(AfirmaBootLoader.this, codeBase, build).install();
                    }
                });
                return null;
            }
        });

    }

}

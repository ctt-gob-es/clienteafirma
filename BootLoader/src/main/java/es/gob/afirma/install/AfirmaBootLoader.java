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

import java.io.InputStream;
import java.net.URL;
import java.security.AccessController;
import java.util.logging.Logger;

import javax.swing.JApplet;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import es.gob.afirma.BootLoaderMessages;
import es.gob.afirma.misc.AOBootUtil;
import es.gob.afirma.misc.Platform;

/** Clase encargada de instalar el cliente de firma del Ministerio de la Presidencia, tanto
 * las librer&iacute;as de las que depende como el propio applet cliente (cliente + librerias
 * de compatibilidad con afirma 5). */
public final class AfirmaBootLoader extends JApplet {

    /** Activar a <code>true</code> &uacute;nicamente para pruebas. */
    public static final boolean DEBUG = false;

    private static final long serialVersionUID = -2570412953683244702L;

    /** Gestor de registro para todo el proyecto. */
    public static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$;

    /** Direcci&oacute;n remota del applet. */
    private URL codeBase = null;

    @Override
    public void init() {
        this.codeBase = this.getCodeBase();
        LOGGER.info("BootLoader de @firma iniciado"); //$NON-NLS-1$
        LOGGER.info("Arquitectura del JRE: " + Platform.getJavaArch()); //$NON-NLS-1$
        LOGGER.info("Arquitectura del sistema operativo: " + Platform.getOsArch()); //$NON-NLS-1$
        if (DEBUG) {
            LOGGER.warning("Modo de depuracion activado"); //$NON-NLS-1$
        }
    }

    /** Establece la URL desde la cual se bajar&aacute;n los ficheros para instalar.
     * @param url URL desde la cual se bajar&aacute;n los ficheros para instalar
     * @return <code>true</code> si se ha establecido correctamente, <code>false</code> si la URL est&aacute; mal formada */
    public boolean setBaseDownloadURL(final String url) {
        LOGGER.info("Invocando setBaseDownloadURL(String)"); //$NON-NLS-1$
        try {
            this.codeBase = new URL(url);
        }
        catch (final Exception e) {
            LOGGER.severe("La URL establecida es incorrecta, se usara la original (" + this.codeBase.toString() + "): " + e //$NON-NLS-1$ //$NON-NLS-2$
            );
            return false;
        }
        return true;
    }

    /** Instala las dependencias del Cliente @firma respecto al entorno operativo
     * del cliente de firma, incluyendo las partes nativas y clases Java necesarias.
     * @return <code>true</code> si la instalaci&oacute;n termin&oacute; sin problemas, <code>false</code> en caso contrario */
    public boolean instalar() {
        // Realizamos la instalacion
        return instalar(Installer.LITE);
    }

    /** Instala las dependencias del Cliente @firma respecto al entorno operativo
     * del cliente de firma, incluyendo las partes nativas y clases Java necesarias.
     * @param build Contrucci&oacute;n que se desea instalar.
     * @return <code>true</code> si la instalaci&oacute;n termin&oacute; sin problemas, <code>false</code> en caso contrario */
    public boolean instalar(final String build) {
        return instalar(build, null, null);
    }

    /** Instala las dependencias del Cliente @firma respecto al entorno operativo
     * del cliente de firma, incluyendo las partes nativas y clases Java necesarias.
     * @param build Contrucci&oacute;n que se desea instalar.
     * @return <code>true</code> si la instalaci&oacute;n termin&oacute; sin problemas, <code>false</code> en caso contrario
     * @param jsMethodName M&eacute;todo JavaScript a ejecutar al terminar la
     *        instalaci&oacute;n (puede ser nulo)
     * @param jsMethodParams Par&aacute;metros del m&eacute;todo JavaScript */
    public boolean instalar(final String build, final String jsMethodName, final Object jsMethodParams) {
        LOGGER.info("Invocando instalar(" + build + ", " + jsMethodName + ", " + jsMethodParams + ")" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$  //$NON-NLS-4$
        );

        // Realizamos la instalacion
        return AccessController.doPrivileged(new java.security.PrivilegedAction<Boolean>() {
            /** {@inheritDoc} */
            public Boolean run() {
                SwingUtilities.invokeLater(new Runnable() {
                    /** {@inheritDoc} */
                    public void run() {

                        // Configuramos el instalador
                        final Installer installer = new Installer(AfirmaBootLoader.this, AfirmaBootLoader.this.codeBase, build);

                        // Instalamos
                        installer.install();

                        // Ejecuto la funcion JavaScript si se especifico
                        invoqueAsynchronousJSOperation(jsMethodName, jsMethodParams);
                    }
                });
                return Boolean.TRUE;
            }
        }).booleanValue();

    }

    /** Desinstala localmente las dependecias de entorno operativo del Cliente de Firma.
     * @return Devuelve <code>true</code> si se desinstalaron correctamente, <code>false</code> en caso contrario */
    public boolean desinstalar() {
        LOGGER.info("Invocando desinstalar()"); //$NON-NLS-1$
        return new Installer(AfirmaBootLoader.this, this.codeBase, null).uninstall();
    }

    /** Recupera la versi&oacute;n del BootLoader.
     * @return Versi&oacute;n del BootLoader. */
    public String getVersion() {
        final InputStream is = AfirmaBootLoader.class.getResourceAsStream("/version.properties"); //$NON-NLS-1$
        final String idVersion = AOBootUtil.getVersion(is);
        try {
            is.close();
        }
        catch (final Exception e) {}
        return idVersion;
    }

    /** Invoca una operaci&oacute;n JavaScript. En caso de producirse un error durante la
     * ejecuci&oacute;n se pedir&aacute; reiniar el navegador Web.
     * @param jsMethodName Nombre del m&eacute;todo JavaScript.
     * @param jsMethodParams Par&aacute;metros para la llamada en forma e Object[] o
     *        Cadenas separadas por comas (','). */
    private void invoqueAsynchronousJSOperation(final String jsMethodName, final Object jsMethodParams) {

        // Si se ha definido una accion posterior, la ejecutamos
        if (jsMethodName != null && !"".equals(jsMethodName)) { //$NON-NLS-1$

            final Object[] jsPreparedParams = AOBootUtil.prepareJSParams(jsMethodParams);
            try {
                netscape.javascript.JSObject.getWindow(AfirmaBootLoader.this).call(jsMethodName, jsPreparedParams);
            }
            catch (final Exception e) {
                LOGGER.severe("No se ha podido realizar la llamada al metodo JavaScript '" + jsMethodName + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
                JOptionPane.showMessageDialog(AfirmaBootLoader.this,
                                              BootLoaderMessages.getString("AfirmaBootLoader.6"), //$NON-NLS-1$
                                              BootLoaderMessages.getString("AfirmaBootLoader.7"), //$NON-NLS-1$
                                              JOptionPane.WARNING_MESSAGE);
            }
        }
    }

}

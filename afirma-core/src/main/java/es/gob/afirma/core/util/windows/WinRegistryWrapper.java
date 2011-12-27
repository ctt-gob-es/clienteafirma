/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.core.util.windows;

import java.util.logging.Logger;

import com.sun.deploy.util.WinRegistry;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;

/** Clase envoltorio para las clases de Sun para acceso al registro de Windows.
 * Garantizan la carga est&aacute;tica de la biblioteca nativa */
public final class WinRegistryWrapper {
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    static {

        if (Platform.getOS().equals(Platform.OS.WINDOWS)) {
            // Cargamos la libreria nativa 'deploy.dll'
            try {
                AOUtil.loadNativeLibrary(Platform.getJavaHome() + "\\bin\\deploy.dll"); //$NON-NLS-1$
            }
            catch (final Exception e) {
                LOGGER.warning("No se ha podido cargar la libreria de despliegue 'deploy.dll': " + e);  //$NON-NLS-1$
            }
        }
    }

//    /** Carga la DLL <i>deploy.dll</i>, o la original de Java o la propia del
//     * Cliente @firma. */
//    public static void loadDeployDll() {
//        // Cuerpo vacio, unicamente para cargar la DLL de forma estatica
//    }

    private WinRegistryWrapper() {
        // No permitimos la instanciacion
    }

    /** Clave <i>HKEY_LOCAL_MACHINE</i> del registro de Windows. */
    public static final int HKEY_LOCAL_MACHINE = 0x80000002;

    /** Clave <i>HKEY_CURRENT_USER</i> del registro de Windows. */
    public static final int HKEY_CURRENT_USER = 0x80000001;
    
    /** Clave <i>HKEY_CLASSES_ROOT</i> del registro de Windows. */
    public static final int HKEY_CLASSES_ROOT = 0x80000000;

    /** Obtiene una clave del registro de Windows.
     * @param hKey
     *        Ra&iacute;z de la clave a obtener del registro
     * @param path
     *        Ruta de la clave a obtener
     * @param name
     *        Nombre de la clave a obtener
     * @return Valor de la clave obtenida, <i>null</i> si no se encontr&oacute;
     *         la clave o si se produjeron errores durante la obtenci&oacute;n */
    public static Object get(final int hKey, final String path, final String name) {
        try {
            return WinRegistry.get(hKey, path, name);
        }
        catch (final Exception e) {
            LOGGER.severe("No se ha podido obtener la clave de registro con ruta '" + path //$NON-NLS-1$ 
                                                     + "' y nombre '" //$NON-NLS-1$
                                                     + name
                                                     + "', se devolvera null: " //$NON-NLS-1$
                                                     + e);
        }
        return null;
    }

    /** Obtiene una clave de tipo cadena de texto del registro de Windows.
     * @param hKey
     *        Ra&iacute;z de la clave a obtener del registro
     * @param path
     *        Ruta de la clave a obtener
     * @param name
     *        Nombre de la clave a obtener
     * @return Texto correspondiente a la clave obtenida, <i>null</i> si no se
     *         encontr&oacute; la clave o si se produjeron errores durante la
     *         obtenci&oacute;n */
    public static String getString(final int hKey, final String path, final String name) {
        try {
            return WinRegistry.getString(hKey, path, name);
        }
        catch (final Exception e) {
            LOGGER.severe("No se ha podido obtener la clave de registro con ruta '" + path  //$NON-NLS-1$
                                                     + "' y nombre '" //$NON-NLS-1$
                                                     + name
                                                     + "', se devolvera null: " //$NON-NLS-1$
                                                     + e);
        }
        return null;
    }

    /** Establece un valor textual a una clave del registro de Windows. Si la
     * clave no existe la crea
     * @param hKey
     *        Ra&iacute;z de la clave a establecer en el registro
     * @param path
     *        Ruta de la clave
     * @param name
     *        Nombre de la clave
     * @param value
     *        Valor textual que se desea dar a la clave
     * @return <code>true</code> si se estableci&oacute; el valor, <code>false</code> si no se pudo establecer */
    public static boolean setStringValue(final int hKey, final String path, final String name, final String value) {
        try {
            return WinRegistry.setStringValue(hKey, path, name, value);
        }
        catch (final Exception e) {
            LOGGER.severe("No se ha podido establecer la clave de registro con ruta '" + path //$NON-NLS-1$ 
                                                     + "' y nombre '" //$NON-NLS-1$
                                                     + name
                                                     + "', se devolvera false: " //$NON-NLS-1$
                                                     + e);
        }
        return false;
    }

}

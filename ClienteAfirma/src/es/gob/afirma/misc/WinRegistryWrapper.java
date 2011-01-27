/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.misc;

import java.io.File;
import java.util.logging.Logger;

import com.sun.deploy.util.WinRegistry;



/**
 * Clase envoltorio para las clases de Sun para acceso al registro de Windows.
 * Garantizan la carga est&aacute;tica de la biblioteca nativa
 */
public final class WinRegistryWrapper {

    static {

        if (System.getProperty("os.name").contains("indows")) {
            // En caso de java 6 o superior, cargamos la libreria 'deploy.dll' que incorporan
            boolean loaded = false;
//            if(!System.getProperty("java.version").startsWith("1.5")) {	// Caso de Java 6 o superior (Con Java 4 no carga la aplicacion)
                try {
                    AOUtil.loadNativeLibrary(es.gob.afirma.misc.AOUtil.getJavaHome() + File.separator + "bin" + File.separator + "deploy.dll");
                    loaded = true;
                }
                catch(Throwable e) {
                    Logger.getLogger("es.gob.afirma").warning("No se ha podido cargar la libreria de despliegue 'deploy.dll', se cargara la libreria incluida en el cliente: "+e);
                }
//            }

            if(!loaded) {
                try {
                	if(new File(AOInstallParameters.getDeployDllDir() + File.separator + "aodeploy.dll").exists())
                    AOUtil.loadNativeLibrary(AOInstallParameters.getDeployDllDir() + File.separator + "aodeploy.dll");
                }
                catch(Throwable e) {
                    Logger.getLogger("es.gob.afirma").warning("No se ha podido cargar la libreria de despliegue 'aodeploy.dll' incluida en el cliente: "+e);
                }
            }
        }
    }

    private WinRegistryWrapper() {}

    /** Clave <i>HKEY_LOCAL_MACHINE</i> del registro de Windows. */
    public static final int HKEY_LOCAL_MACHINE = 0x80000002;

    /** Clave <i>HKEY_CURRENT_USER</i> del registro de Windows. */
    public static final int HKEY_CURRENT_USER = 0x80000001; 

    /**
     * Obtiene una clave del registro de Windows.
     * @param hKey Ra&iacute;z de la clave a obtener del registro
     * @param path Ruta de la clave a obtener
     * @param name Nombre de la clave a obtener
     * @return Valor de la clave obtenida, <i>null</i> si no se encontr&oacute; la clave o si se produjeron errores
     *         durante la obtenci&oacute;n
     */
    public static Object get(int hKey, String path, String name) {
        try {
            return WinRegistry.get(hKey, path, name);
        }
        catch(Throwable e) {
            Logger.getLogger("es.gob.afirma").severe(
                    "No se ha podido obtener la clave de registro con ruta '" +
                    path + "' y nombre '" + name + "', se devolvera null: " + e 
            );
        }
        return null;
    }

    /**
     * Obtiene una clave de tipo cadena de texto del registro de Windows.
     * @param hKey Ra&iacute;z de la clave a obtener del registro
     * @param path Ruta de la clave a obtener
     * @param name Nombre de la clave a obtener
     * @return Texto correspondiente a la clave obtenida, <i>null</i> si no se encontr&oacute; la clave o si se produjeron errores
     *         durante la obtenci&oacute;n
     */
    public static String getString(int hKey, String path, String name) {
        try {
            return WinRegistry.getString(hKey, path, name);
        }
        catch(Throwable e) {
            Logger.getLogger("es.gob.afirma").severe(
                    "No se ha podido obtener la clave de registro con ruta '" +
                    path + "' y nombre '" + name + "', se devolvera null: " + e 
            );
        }
        return null;
    }

    /**
     * Establece un valor textual a una clave del registro de Windows.
     * Si la clave no existe la crea
     * @param hKey Ra&iacute;z de la clave a establecer en el registro
     * @param path Ruta de la clave
     * @param name Nombre de la clave
     * @param value Valor textual que se desea dar a la clave
     * @return <code>true</code> si se estableci&oacute; el valor, <code>false</code> si no se pudo establecer
     */
    public static boolean setStringValue(int hKey, String path, String name, String value) {
        try {
            return WinRegistry.setStringValue(hKey, path, name, value);
        }
        catch(Throwable e) {
            Logger.getLogger("es.gob.afirma").severe(
                    "No se ha podido establecer la clave de registro con ruta '" +
                    path + "' y nombre '" + name + "', se devolvera false: " + e 
            );
        }
        return false;
    }

}

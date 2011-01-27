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
 * Clase intermedia para el acceso al registro de Windows mediante las
 * clases de Sun Microsystems.
 */
public final class WinRegistryWrapper {
	
	static {
		if (System.getProperty("os.name").contains("indows")) { //$NON-NLS-1$ //$NON-NLS-2$
			try {
				AOBootUtil.loadNativeLibrary(AOBootUtil.getJavaHome() + File.separator + "bin" + File.separator + "deploy.dll"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			catch(Throwable e) {
				Logger.getLogger("es.gob.afirma").warning("No se ha podido cargar la libreria de despliegue 'deploy.dll', no se podra acceder al registro de Windows: "+e); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
	}
	
	private WinRegistryWrapper() {}
	
	/** Clave HKEY_LOCAL_MACHINE del registro de Windows. */
	public static final int HKEY_LOCAL_MACHINE = 0x80000002;
	
	/** Clave HKEY_CURRENT_USER del registro de Windows. */
	public static final int HKEY_CURRENT_USER = 0x80000001; 
	
	/**
	 * Obtiene una entrada del registro de Windows.
	 * @param hKey Tipo de clave a obtener
	 * @param path Ruta en el registro hacia la clave
	 * @param name Nombre de la clave
	 * @return Clave (entrada) del registro de Windows
	 */
	public static Object get(int hKey, String path, String name) {
		try {
			return WinRegistry.get(hKey, path, name);
		}
		catch(Throwable e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"No se ha podido obtener la clave de registro con ruta '" + //$NON-NLS-1$
				path + "' y nombre '" + name + "', se devolvera null: " + e  //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		return null;
	}

	/**
	 * Obtiene una entrada de texto del registro de Windows.
	 * @param hKey Tipo de clave a obtener
	 * @param path Ruta en el registro hacia la clave
	 * @param name Nombre de la clave
	 * @return Clave (entrada) del registro de Windows
	 */
	public static String getString(int hKey, String path, String name) {
		try {
			return WinRegistry.getString(hKey, path, name);
		}
		catch(Throwable e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"No se ha podido obtener la clave de registro con ruta '" + //$NON-NLS-1$
				path + "' y nombre '" + name + "', se devolvera null: " + e  //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		return null;
	}
	
	/**
	 * establece una entrada de texto en el registro de Windows.
	 * @param hKey Tipo de clave
	 * @param path Ruta en el registro de la clave
	 * @param name Nombre de la clave
	 * @param value Valor a establecer para la clave
	 * @return <code>true</code> si se estableci&oacute; correctamente, <code>false</code>
	 *         en caso contrario
	 */
	public static boolean setStringValue(int hKey, String path, String name, String value) {
		try {
			return WinRegistry.setStringValue(hKey, path, name, value);
		}
		catch(Throwable e) {
			Logger.getLogger("es.gob.afirma").severe( //$NON-NLS-1$
				"No se ha podido establecer la clave de registro con ruta '" + //$NON-NLS-1$
				path + "' y nombre '" + name + "', se devolvera false: " + e  //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		return false;
	}
	
}

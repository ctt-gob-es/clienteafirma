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
				AOBootUtil.loadNativeLibrary(Platform.getJavaHome() + File.separator + "bin" + File.separator + "deploy.dll"); //$NON-NLS-1$ //$NON-NLS-2$
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
	
}

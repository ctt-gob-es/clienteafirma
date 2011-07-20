package es.gob.afirma.misc;

import com.sun.deploy.util.WinRegistry;

import es.gob.afirma.install.AfirmaBootLoader;

/** Clase intermedia para el acceso al registro de Windows mediante las
 * clases de Sun Microsystems. */
public final class WinRegistryWrapper {

    static {
        if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
            try {
                AOBootUtil.loadNativeLibrary(Platform.getJavaHome() + "\\bin\\deploy.dll"); //$NON-NLS-1$
            }
            catch (final Exception e) {
                AfirmaBootLoader.LOGGER.warning("No se ha podido cargar la libreria de despliegue 'deploy.dll', no se podra acceder al registro de Windows: " + e); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
    }

    private WinRegistryWrapper() {}

    /** Clave HKEY_LOCAL_MACHINE del registro de Windows. */
    public static final int HKEY_LOCAL_MACHINE = 0x80000002;

    /** Clave HKEY_CURRENT_USER del registro de Windows. */
    public static final int HKEY_CURRENT_USER = 0x80000001;

    /** Obtiene una entrada de texto del registro de Windows.
     * @param hKey Tipo de clave a obtener
     * @param path Ruta en el registro hacia la clave
     * @param name Nombre de la clave
     * @return Clave (entrada) del registro de Windows */
    public static String getString(final int hKey, final String path, final String name) {
        try {
            return WinRegistry.getString(hKey, path, name);
        }
        catch (final Exception e) {
            AfirmaBootLoader.LOGGER.severe( //$NON-NLS-1$
                                                      "No se ha podido obtener la clave de registro con ruta '" + //$NON-NLS-1$
                                                      path
                                                      + "' y nombre '" + name + "', se devolvera null: " + e //$NON-NLS-1$ //$NON-NLS-2$
            );
        }
        return null;
    }

}

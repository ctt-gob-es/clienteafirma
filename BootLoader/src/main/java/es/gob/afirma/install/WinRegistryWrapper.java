package es.gob.afirma.install;

import java.io.File;
import java.util.logging.Logger;

import com.sun.deploy.util.WinRegistry;

/** Clase intermedia para el acceso al registro de Windows mediante las
 * clases de Sun Microsystems. */
final class WinRegistryWrapper {

    /** Gestor de registro. */
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$;
    
    static {
        if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
            try {
                loadNativeLibrary(Platform.getJavaHome() + "\\bin\\deploy.dll"); //$NON-NLS-1$
            }
            catch (final Throwable e) {
                LOGGER.warning("No se ha podido cargar la libreria de despliegue 'deploy.dll', no se podra acceder al registro de Windows: " + e); //$NON-NLS-1$
            }
        }
    }

    private WinRegistryWrapper() {
        // No permitimos la instanciacion
    }

    /** Clave HKEY_LOCAL_MACHINE del registro de Windows. */
    static final int HKEY_LOCAL_MACHINE = 0x80000002;

    /** Clave HKEY_CURRENT_USER del registro de Windows. */
    static final int HKEY_CURRENT_USER = 0x80000001;

    /** Obtiene una entrada de texto del registro de Windows.
     * @param hKey Tipo de clave a obtener
     * @param path Ruta en el registro hacia la clave
     * @param name Nombre de la clave
     * @return Clave (entrada) del registro de Windows */
    static String getString(final int hKey, final String path, final String name) {
        try {
            return WinRegistry.getString(hKey, path, name);
        }
        catch (final Exception e) {
            LOGGER.severe(
                  "No se ha podido obtener la clave de registro con ruta '" + //$NON-NLS-1$
                  path
                  + "' y nombre '" + name + "', se devolvera null: " + e //$NON-NLS-1$ //$NON-NLS-2$
            );
        }
        return null;
    }

    /** Carga una librer&iacute;a nativa del sistema.
     * @param path Ruta a la libreria de sistema. */
    private static void loadNativeLibrary(final String path) {

        boolean copyOK = false;
        final int pos = path.lastIndexOf('.');
        final File file = new File(path);
        File tempLibrary = null;
        try {
            tempLibrary =
                File.createTempFile(pos < 1 ? file.getName() : file.getName().substring(0, file.getName().indexOf('.')),
                                            pos < 1 || pos == path.length() - 1 ? null : path.substring(pos));

            // Copiamos el fichero
            copyOK = AOBootUtil.copyFile(file, tempLibrary);
        }
        catch (final Exception e) {
            LOGGER.warning("Error al generar una nueva instancia de la libreria " + path + " para su carga: " + e); //$NON-NLS-1$ //$NON-NLS-2$
        }
        LOGGER.info("Cargamos " + (tempLibrary == null ? path : tempLibrary.getAbsolutePath())); //$NON-NLS-1$
        System.load((copyOK && tempLibrary != null) ? tempLibrary.getAbsolutePath() : path);
        if (tempLibrary != null) {
            tempLibrary.deleteOnExit();
        }
    }

}

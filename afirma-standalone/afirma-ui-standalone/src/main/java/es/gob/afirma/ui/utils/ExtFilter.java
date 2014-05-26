package es.gob.afirma.ui.utils;

import java.io.File;

import javax.swing.filechooser.FileFilter;

/** Filtra los ficheros por extensi&oacute;n para los di&aacute;logos de
 * carga y guardado. Se declara como p&uacute;blico para que pueda ser usado
 * tambi&eacute;n por el interfaz de aplicaci&oacute;n de escritorio. No
 * usamos <code>FileNameExtensionFilter</code> directamente para
 * compatibilizar con Java 1.4
 * @version 0.3 */
public final class ExtFilter extends FileFilter implements java.io.FileFilter {

    /** Devuelve la extensi&oacute;n de un fichero.
     * @param f
     *        Fichero del cual queremos conocer la extensi&oacute;n
     * @return Extensi&oacute;n del fichero o cadena vac&iacute;a si este no
     *         tiene extensi&oacute;n */
    private static String getExtension(final File f) {
        final String s = f.getName();
        final int i = s.lastIndexOf('.');
        if (i > 0 && i < s.length() - 1) {
            return s.substring(i + 1).toLowerCase();
        }
        return ""; //$NON-NLS-1$
    }

    /** Devuleve las extensiones de fichero aceptadas.
     * @return Extensiones de fichero aceptadas */
    public String[] getExtensions() {
    	return this.extensions;
    }

    private final String description;

    private final String[] extensions;

    /** Construye un filtro para la selecci&oacute;n de ficheros en un <code>JFileChooser</code>.
     * @param exts
     *        Extensiones de fichero permitidas
     * @param desc
     *        Descripci&oacute;n del tipo de fichero correspondiente a
     *        las extensiones */
    public ExtFilter(final String[] exts, final String desc) {
        if (exts == null || exts.length < 1) {
            throw new IllegalArgumentException("No se puede crear un filtro vacio"); //$NON-NLS-1$
        }
        this.extensions = exts.clone();
        this.description = desc != null ? desc : Messages.getString("ExtFiler.fileTypes.all"); //$NON-NLS-1$
    }

    /** {@inheritDoc}  */
    @Override
    public boolean accept(final File f) {
        if (f.isDirectory()) {
            return true;
        }
        // getExtension() pasa la extension a minusculas, no hace falta
        // el "ignoreCase"
        final String extension = getExtension(f);
        for (final String extension2 : this.extensions) {
            if (extension2.equalsIgnoreCase(extension)) {
                return true;
            }
        }
        return false;
    }

    /** {@inheritDoc}  */
    @Override
    public String getDescription() {
        return this.description;
    }

}

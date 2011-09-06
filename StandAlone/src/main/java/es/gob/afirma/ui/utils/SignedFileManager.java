package es.gob.afirma.ui.utils;

import java.io.File;

import javax.swing.filechooser.FileFilter;

import es.gob.afirma.core.signers.AOSignConstants;

public class SignedFileManager {

    /** Obtiene el nombre que le corresponde a un fichero tras su firma
     * seg&uacute;n su nombre original y el formato de la firma. En caso de no
     * reconocerse el formato de firma o existir alg&uacute;n problema con el
     * nombre de fichero, se agregar&aacute; al nombre la extension ".sig".
     * @param inName
     *        Nombre original del fichero.
     * @param signFormat
     *        Formato de firma recogido en {@link AOSignConstants AOSignConstants}.
     * @return Nombre de salida del fichero */
    public static final String getOutFileName(final String inName, final String signFormat) {

        if (inName == null || inName.equals("")) { //$NON-NLS-1$
            throw new IllegalArgumentException("El nombre de fichero no puede estar vacio"); //$NON-NLS-1$ 
        }


        if (signFormat == null) {
            throw new IllegalArgumentException("El formato de firma no puede ser nulo"); //$NON-NLS-1$
        }

        if (signFormat.equals(AOSignConstants.SIGN_FORMAT_CMS) || signFormat.equals(AOSignConstants.SIGN_FORMAT_CADES)) {
            return inName + ".csig"; //$NON-NLS-1$
        }
        if (signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED) || signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)
            || signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING)
            || signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED)
            || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_DETACHED)
            || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED)
            || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING)
            || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED)) {
            return inName + ".xsig"; //$NON-NLS-1$
        }
        if (signFormat.equals(AOSignConstants.SIGN_FORMAT_PDF) || signFormat.equals(AOSignConstants.SIGN_FORMAT_ODF)
            || signFormat.equals(AOSignConstants.SIGN_FORMAT_OOXML)) {
            final int i = inName.lastIndexOf('.');
            if (i > 0 && i < inName.length() - 1) {
                return inName.substring(0, i) + ".signed" + inName.substring(i).toLowerCase(); //$NON-NLS-1$
            }
        }

        return inName + ".sig"; //$NON-NLS-1$
    }
    
    /** Obtiene un filtro de fichero correspondiente para el almacenamiento de un
     * fichero de firma.
     * @param signFormat
     *        Formato de firma.
     * @return Filtro con las extensiones de fichero v&aacute;lidas para el
     *         formato de firma especificado. */
    public static final FileFilter getOutFileFilter(final String signFormat) {
        if (signFormat.equals(AOSignConstants.SIGN_FORMAT_CMS)) {
            return new ExtFilter(new String[] {
                "csig"}, Messages.getString("SignedFileManager.43")); //$NON-NLS-1$ //$NON-NLS-2$
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_CADES)) {
            return new ExtFilter(new String[] {
                "csig"}, Messages.getString("SignedFileManager.1")); //$NON-NLS-1$ //$NON-NLS-2$
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_PDF)) {
            return new ExtFilter(new String[] {
                "pdf"}, Messages.getString("SignedFileManager.30")); //$NON-NLS-1$ //$NON-NLS-2$
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_ODF)) {
            return new ExtFilter(new String[] {
                    "odt", "ods", "odp"}, Messages.getString("SignedFileManager.16")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_OOXML)) {
            return new ExtFilter(new String[] {
                    "docx", "xlsx", "pptx", "ppsx"}, Messages.getString("SignedFileManager.50")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED) || signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING)
                 || signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)
                 || signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED)
                 || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_DETACHED)
                 || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING)
                 || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED)
                 || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED)) {
            return new ExtFilter(new String[] {
                "xsig"}, Messages.getString("SignedFileManager.17")); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return new ExtFilter(new String[] {
            "sig"}, Messages.getString("SignedFileManager.52")); //$NON-NLS-1$ //$NON-NLS-2$
    }
    
    /** Filtra los ficheros por extensi&oacute;n para los di&aacute;logos de
     * carga y guardado. Se declara como p&uacute;blico para que pueda ser usado
     * tambi&eacute;n por el interfaz de aplicaci&oacute;n de escritorio. No
     * usamos <code>FileNameExtensionFilter</code> directamente para
     * compatibilizar con Java 1.4
     * @version 0.3 */
    public final static class ExtFilter extends FileFilter implements java.io.FileFilter {

        private String[] extensions;
        private String description;

        /** Construye un filtro para la selecci&oacute;n de ficheros en un <code>JFileChooser</code>.
         * @param exts
         *        Extensiones de fichero permitidas
         * @param desc
         *        Descripci&oacute;n del tipo de fichero correspondiente a
         *        las extensiones */
        public ExtFilter(final String[] exts, String desc) {
            if (exts == null || exts.length < 1) {
                throw new IllegalArgumentException("No se puede crear un filtro vacio"); //$NON-NLS-1$
            }
            if (desc == null || desc.length() < 1) {
                desc = Messages.getString("SignedFileManager.0"); //$NON-NLS-1$
            }
            this.extensions = exts.clone();
            this.description = desc;
        }

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

        // public String[] getExtensions() {
        // return extensions;
        // }

        @Override
        public String getDescription() {
            return this.description;
        }

        /** Devuelve la extensi&oacute;n de un fichero.
         * @param f
         *        Fichero del cual queremos conocer la extensi&oacute;n
         * @return Extensi&oacute;n del fichero o cadena vac&iacute;a si este no
         *         tiene extensi&oacute;n */
        private final static String getExtension(final File f) {
            final String s = f.getName();
            final int i = s.lastIndexOf('.');
            if (i > 0 && i < s.length() - 1) {
                return s.substring(i + 1).toLowerCase();
            }
            return ""; //$NON-NLS-1$
        }

    }
}

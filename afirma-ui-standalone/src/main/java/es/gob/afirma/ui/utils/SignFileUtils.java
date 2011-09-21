package es.gob.afirma.ui.utils;

import javax.swing.filechooser.FileFilter;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.ui.jse.JSEUIManager;


public class SignFileUtils {

    /** Obtiene un filtro de fichero correspondiente para el almacenamiento de un
     * fichero de firma.
     * @param signFormat
     *        Formato de firma.
     * @return Filtro con las extensiones de fichero v&aacute;lidas para el
     *         formato de firma especificado. */
    public static final FileFilter getOutFileFilter(final String signFormat) {
        if (signFormat.equals(AOSignConstants.SIGN_FORMAT_CMS)) {
            return JSEUIManager.getFileFilter(new String[] {
                "csig"}, Messages.getString("AOUIManager.43")); //$NON-NLS-1$ //$NON-NLS-2$
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_CADES)) {
            return JSEUIManager.getFileFilter(new String[] {
                "csig"}, Messages.getString("AOUIManager.1")); //$NON-NLS-1$ //$NON-NLS-2$
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_PDF)) {
            return JSEUIManager.getFileFilter(new String[] {
                "pdf"}, Messages.getString("AOUIManager.30")); //$NON-NLS-1$ //$NON-NLS-2$
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_ODF)) {
            return JSEUIManager.getFileFilter(new String[] {
                    "odt", "ods", "odp"}, Messages.getString("AOUIManager.16")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_OOXML)) {
            return JSEUIManager.getFileFilter(new String[] {
                    "docx", "xlsx", "pptx", "ppsx"}, Messages.getString("AOUIManager.50")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED)
                 || signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING)
                 || signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)
                 || signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED)
                 || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_DETACHED)
                 || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING)
                 || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED)
                 || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED)) {
            return JSEUIManager.getFileFilter(new String[] {
                "xsig"}, Messages.getString("AOUIManager.17")); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return JSEUIManager.getFileFilter(new String[] { "sig" },  //$NON-NLS-1$
                Messages.getString("AOUIManager.52")); //$NON-NLS-1$
    }
}

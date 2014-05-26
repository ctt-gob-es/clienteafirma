package es.gob.afirma.ui.utils;

import es.gob.afirma.core.signers.AOSignConstants;

/** Utilidades para la gesti&oacute;n de ficheros de firma. */
public final class SignFileUtils {

	private SignFileUtils() {
		// No permitimos la instanciacion
	}

    /** Obtiene un filtro de fichero correspondiente para el almacenamiento de un
     * fichero de firma.
     * @param signFormat
     *        Formato de firma.
     * @return Filtro con las extensiones de fichero v&aacute;lidas para el
     *         formato de firma especificado. */
    public static ExtFilter getOutFileFilter(final String signFormat) {
        if (signFormat.equals(AOSignConstants.SIGN_FORMAT_CMS)) {
            return new ExtFilter(new String[] {
            "csig"}, Messages.getString("AOUIManager.43")); //$NON-NLS-1$ //$NON-NLS-2$
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_CADES)) {
            return new ExtFilter(new String[] {
            "csig"}, Messages.getString("AOUIManager.1")); //$NON-NLS-1$ //$NON-NLS-2$
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_PDF)) {
            return new ExtFilter(new String[] {
            "pdf"}, Messages.getString("AOUIManager.30")); //$NON-NLS-1$ //$NON-NLS-2$
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_ODF)) {
            return new ExtFilter(new String[] {
                                               "odt", "ods", "odp"}, Messages.getString("AOUIManager.16")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_OOXML)) {
            return new ExtFilter(new String[] {
                                               "docx", "xlsx", "pptx", "ppsx"}, Messages.getString("AOUIManager.50")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED) || signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING)
                || signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)
                || signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED)
                || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_DETACHED)
                || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING)
                || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED)
                || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED)) {
            return new ExtFilter(new String[] {
            "xsig"}, Messages.getString("AOUIManager.17")); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return new ExtFilter(new String[] {
        "sig"}, //$NON-NLS-1$
        Messages.getString("AOUIManager.52")); //$NON-NLS-1$
    }
}

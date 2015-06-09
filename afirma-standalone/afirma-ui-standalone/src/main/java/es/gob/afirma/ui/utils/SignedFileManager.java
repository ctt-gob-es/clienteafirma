package es.gob.afirma.ui.utils;

import javax.swing.filechooser.FileFilter;

import es.gob.afirma.core.signers.AOSignConstants;

/** Operaciones de utilidad para la gesti&oacute;n de ficheros firmados. */
public final class SignedFileManager {

	private SignedFileManager() {
		// No permitimos la instanciacion
	}

    /** Obtiene un filtro de fichero correspondiente para los ficheros mas comunes que contienen una firma.
     * @return filtro */
    public static FileFilter getCommonSignedFileFilter() {
        final FileFilter fileFilter = new ExtFilter(new String[] {
          "csig", "xsig", "pdf" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }, Messages.getString("SignedFileManager.commonSignedFile")); //$NON-NLS-1$
        return fileFilter;
    }

    /** Obtiene un filtro de fichero correspondiente para el almacenamiento de un
     * fichero de firma.
     * @param signFormat
     *        Formato de firma.
     * @return Filtro con las extensiones de fichero v&aacute;lidas para el
     *         formato de firma especificado. */
    public static ExtFilter getOutFileFilter(final String signFormat) {

        if (signFormat.equals(AOSignConstants.SIGN_FORMAT_CMS) || signFormat.equals(AOSignConstants.SIGN_FORMAT_CADES)) {
            return new ExtFilter(
        		new String[] {
        			"csig" //$NON-NLS-1$
				},
				Messages.getString("SignedFileManager.43") //$NON-NLS-1$
			);
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_PDF)) {
            return new ExtFilter(
        		new String[] {
        			"pdf" //$NON-NLS-1$
				},
				Messages.getString("SignedFileManager.30") //$NON-NLS-1$
			);
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_ODF)) {
            return new ExtFilter(
        		new String[] {
                   "odt", "ods", "odp" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
               },
               Messages.getString("SignedFileManager.16") //$NON-NLS-1$
           );
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_OOXML)) {
            return new ExtFilter(
        		new String[] {
    				"docx", "xlsx", "pptx", "ppsx" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				},
				Messages.getString("SignedFileManager.50") //$NON-NLS-1$
			);
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_DETACHED)
        	  || signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPING)
              || signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_ENVELOPED)
              || signFormat.equals(AOSignConstants.SIGN_FORMAT_XMLDSIG_EXTERNALLY_DETACHED)
              || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_DETACHED)
              || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING)
              || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED)
              || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED)) {
            return new ExtFilter(
        		new String[] {
        			"xsig" //$NON-NLS-1$
				},
				Messages.getString("SignedFileManager.17") //$NON-NLS-1$
			);
        }
        else if (signFormat.equals(AOSignConstants.SIGN_FORMAT_FACTURAE)) {
            return new ExtFilter(
        		new String[] {
        			"xsig" //$NON-NLS-1$
				},
				Messages.getString("SignedFileManager.18") //$NON-NLS-1$
			);
        }
        return new ExtFilter(
    		new String[] {
    			"sig" //$NON-NLS-1$
			},
			Messages.getString("SignedFileManager.52") //$NON-NLS-1$
		);
    }

    /** Obtiene el nombre que le corresponde a un fichero tras su firma
     * seg&uacute;n su nombre original y el formato de la firma. En caso de no
     * reconocerse el formato de firma o existir alg&uacute;n problema con el
     * nombre de fichero, se agregar&aacute; al nombre la extension ".sig".
     * @param inName
     *        Nombre original del fichero.
     * @param signFormat
     *        Formato de firma recogido en {@link AOSignConstants AOSignConstants}.
     * @return Nombre de salida del fichero */
    public static String getOutFileName(final String inName, final String signFormat) {

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
                || signFormat.equals(AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED)
                || signFormat.equals(AOSignConstants.SIGN_FORMAT_FACTURAE)) {
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
}

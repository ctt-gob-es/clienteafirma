package es.gob.afirma.signature;

import com.lowagie.text.pdf.AcroFields;
import com.lowagie.text.pdf.PdfReader;

/**
 * Validador de firmas Adobe PDF.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class ValidatePDFSignature {

    /**
     * Valida las firma de un documento PDF.
     * @param sign Documento PDF
     * @return <code>true</code> si la firmas son v&aacute;lidas, <code>false</code> en caso contrario
     * @throws Exception Si ocurre cualquier problema durante la validaci&oacute;n
     */
    public static boolean validate(final byte[] sign) throws Exception {
    	if (sign == null) {
    		throw new NullPointerException("El PDF a validar no puede ser nulo"); //$NON-NLS-1$
    	}
    	final AcroFields af = new PdfReader(sign).getAcroFields();
    	if (af.getSignatureNames().size() < 1) {
    		throw new Exception("El documento PDF no contiene ninguna firma electronica"); //$NON-NLS-1$
    	}
    	for (final Object o : af.getSignatureNames()) {
		   // Con una unica firma que falle damos por malo todo el PDF
		   if (!af.verifySignature(o.toString()).verify()) {
			   return false;
		   }
    	}
    	return true;
    }
}

package es.gob.afirma.signers.pades;

import java.util.Calendar;
import java.util.Properties;


/** Resultado de una pre-firma (como primera parte de un firma trif&aacute;sica) o una firma completa PAdES. */
/** <i>JavaBean</i> que encapsula los resultados de la pre-firma o firma completa PDF. */
public final class PdfSignResult {

    private final String fileID;
    private final byte[] sign;
    private final Calendar signTime;
    private final Properties extraParams;

    PdfSignResult(final String pdfFileId,
    		         final byte[] preSignature,
    		         final Calendar signingTime,
    		         final Properties xParams) {
        if (signingTime == null || pdfFileId == null || preSignature == null || "".equals(pdfFileId) || preSignature.length < 1) { //$NON-NLS-1$
            throw new IllegalArgumentException("Es obligatorio proporcionar un MAC, una pre-firma y un momento de firmado"); //$NON-NLS-1$
        }
        this.fileID = pdfFileId;
        this.sign = preSignature.clone();
        this.signTime = signingTime;
        this.extraParams = xParams != null ? xParams : new Properties();
    }

    /** Obtiene las opciones adicionales de la firma.
     * @return Opciones adicionales de la firma */
    public Properties getExtraParams() {
    	return this.extraParams;
    }

    /** Obtiene el FileID (<i>/ID</i>) del diccionario PDF generado.
     * @return FileID del diccionario PDF generado */
    public String getFileID() {
        return this.fileID;
    }

    /** Obtiene los atributos CAdES a firmar.
     * @return Atributos CAdES a firmar (pre-firma) */
    public byte[] getSign() {
        return this.sign;
    }

    /** Obtiene el momento en el que se realiz&oacute; la firma.
     * @return Momento en el que se realiz&oacute; la firma */
    public Calendar getSignTime() {
    	return this.signTime;
    }

}
package es.gob.afirma.services;

import javax.xml.bind.annotation.XmlRootElement;

/** Resultado de una prefirma PAdES. */
@XmlRootElement
public class PreSignatureResult {
    
    private byte[] preSignData;
    private String fileID;
    
    /** Construye el resultado de una prefirma CAdES.
     * @param preSignData Atributos firmados de CAdES
     * @param fileID FileID del PDF prefirmado
     */
    public PreSignatureResult(final byte[] preSignData, final String fileID) {
        this.fileID = fileID;
        this.preSignData = preSignData;
    }

    /** Obtiene los atributos firmados CAdES de la prefirma PAdES.
     * @return Atributos firmados CAdES de la prefirma PAdES
     */
    public byte[] getPreSignData() {
        return this.preSignData;
    }

    /** Establece los atributos firmados CAdES de la prefirma PAdES.
     * @param preSignData Atributos firmados CAdES de la prefirma PAdES
     */
    public void setPreSignData(final byte[] preSignData) {
        this.preSignData = preSignData;
    }

    /** Obtiene el FileID del PDF prefirmado.
     * @return FileID del PDF prefirmado
     */
    public String getFileID() {
        return this.fileID;
    }

    /** Obtiene el FileID del PDF prefirmado.
     * @param fileID FileID del PDF prefirmado
     */
    public void setFileID(String fileID) {
        this.fileID = fileID;
    }
}

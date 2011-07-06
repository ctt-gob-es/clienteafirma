package es.gob.afirma.signers.aobinarysignhelper;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;

/** Clase utilizada desde la clase Utils.java para manejar las variables de la
 * funcion initVariables */
final class Info {

    private ASN1EncodableVector recipientInfos = null;
    private EncryptedContentInfo encInfo = null;

    public ASN1EncodableVector getRecipientInfos() {
        return recipientInfos;
    }

    public void setRecipientInfos(final ASN1EncodableVector recipientInfos) {
        this.recipientInfos = recipientInfos;
    }

    public EncryptedContentInfo getEncInfo() {
        return encInfo;
    }

    public void setEncInfo(final EncryptedContentInfo encInfo) {
        this.encInfo = encInfo;
    }
}

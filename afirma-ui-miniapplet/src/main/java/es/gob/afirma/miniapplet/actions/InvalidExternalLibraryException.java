package es.gob.afirma.miniapplet.actions;

import es.gob.afirma.miniapplet.MiniAppletMessages;

/** Indica que en el sistema hay una biblioteca que puede causar un mal funcionamiento
 * del MiniApplet de Afirma.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class InvalidExternalLibraryException extends Exception {

    private static final long serialVersionUID = 8785012357200277893L;
    
    private final String localizedMessageKey;
    private final String[] localizedMessageParams;
    
    InvalidExternalLibraryException(final String message, final String localizedStringCode, final String[] localizedStringParams) {
        super(message);
        this.localizedMessageKey = localizedStringCode;
        this.localizedMessageParams = (localizedStringParams != null) ? localizedStringParams.clone() : null;
    }
    
    @Override
    public String getLocalizedMessage() {
        return MiniAppletMessages.getString(this.localizedMessageKey, this.localizedMessageParams);
    }

}

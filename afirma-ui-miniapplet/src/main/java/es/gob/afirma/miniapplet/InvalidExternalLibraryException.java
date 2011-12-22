package es.gob.afirma.miniapplet;


/** Indica que en el sistema hay una biblioteca que puede causar un mal funcionamiento
 * del MiniApplet de Afirma.
 * @deprecated Se externaliza las comprobaciones de entorno.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
@Deprecated
final class InvalidExternalLibraryException extends Exception {

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

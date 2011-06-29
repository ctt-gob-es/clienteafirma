package es.gob.afirma.signature;

/**
 * Indica si la firma es v&aacute;lida o no.
 * @author Carlos Gamuci
 */
public class SignValidity {

    /** Tipo del resultado de la firma. */
    public enum SIGN_DETAIL_TYPE {
        /** Firma v&aacute;lida. */
        OK,
        /** Firma inv&aacute;lida. */
        KO,
        /** Validez desconocida. */
        UNKNOWN,
        /** Firma generada en la misma aplicaci&oacute;n, se considera siempre v&aacute;lida. */
        GENERATED
    }
    
    /** Errores que invalidan una firma o impiden conocer si es v&aacute;lida o no. */
    public enum VALIDITY_ERROR {
        /** Cuando no se puede comprobar la validez por no tener los datos firmados. */
        NO_DATA,
        /** Cuando la informacion contenida en la firma no sea consistente (certificados corruptos,...). */
        CORRUPTED_SIGN,
        /** Cuando no se encuentra la firma dentro del documento. */
        NO_SIGN
    }
    
    /** Validez de la firma. */
    private SIGN_DETAIL_TYPE validity = SIGN_DETAIL_TYPE.UNKNOWN;
    
    /** Error que invalida la firma o hace que la validez sea desconocida. */
    private VALIDITY_ERROR error = null;
    
    /**
     * Identifica la validez de una firma.
     * @param type Validez de la firma.
     * @param error Error que invalida o impide comprobar la firma.
     */
    public SignValidity(SIGN_DETAIL_TYPE type, VALIDITY_ERROR error) {
        this.validity = type;
        this.error = error;
    }

    /**
     * Recupera la validez de la firma.
     * @return Validez de la firma.
     */
    public SIGN_DETAIL_TYPE getValidity() {
        return validity;
    }

    /**
     * Recupera el error que invalida la firma. Si no existe ning&uacute;n error o este es desconocido,
     * se devolver&aacute; {@code null}.
     * @return Error que invalida la firma o impide comprobar su validez.
     */
    public VALIDITY_ERROR getError() {
        return error;
    }
}

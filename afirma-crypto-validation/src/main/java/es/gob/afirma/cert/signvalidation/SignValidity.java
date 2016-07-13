/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.cert.signvalidation;

/**
 * Indica si la firma es v&aacute;lida o no.
 * @author Carlos Gamuci
 */
public final class SignValidity {

	@Override
	public String toString() {
		if (this.validity.equals(SIGN_DETAIL_TYPE.OK)) {
			return "Firma valida"; //$NON-NLS-1$
		}
		if (this.validity.equals(SIGN_DETAIL_TYPE.UNKNOWN)) {
			return "Validez de la firma desconocida"; //$NON-NLS-1$
		}
		final String ret = "Firma no valida"; //$NON-NLS-1$
		if (this.error == null) {
			return ret;
		}

		switch (this.error) {
			case NO_DATA:
				return ret + ": no se puede comprobar la validez por no tener los datos firmados"; //$NON-NLS-1$
			case CORRUPTED_SIGN:
				return ret + ": la informacion contenida en la firma no es consistente (certificados corruptos, etc.)"; //$NON-NLS-1$
			case NO_MATCH_DATA:
				return ret + ": la firma no se corresponde con los datos firmados"; //$NON-NLS-1$
			case NO_SIGN:
				return ret + ": no se encuentra la firma dentro del documento"; //$NON-NLS-1$
			case CERTIFICATE_PROBLEM:
				return ret + ": no se puede extraer un certificado o este no es valido"; //$NON-NLS-1$
			case CERTIFICATE_EXPIRED:
				return ret + ": existe un certificado de firma caducado"; //$NON-NLS-1$
			case CERTIFICATE_NOT_VALID_YET:
				return ret + ": existe un certificado de firma que aun no es valido"; //$NON-NLS-1$
			case ALGORITHM_NOT_SUPPORTED:
				return ret + ": la firma contiene un algoritmo no reconocido o no soportado"; //$NON-NLS-1$
			case CA_NOT_SUPPORTED:
				return ret + ": no se soporta la CA de expedición."; //$NON-NLS-1$
			case CRL_PROBLEM:
				return ret + ": existe algun problema con las CRLs incrustadas en la firma"; //$NON-NLS-1$
			case PDF_UNKOWN_VALIDITY:
				return ret + ": no se puede comprobar la validez de la firma PDF"; //$NON-NLS-1$
			case OOXML_UNKOWN_VALIDITY:
				return ret + ": no se puede comprobar la validez de la firma OOXML"; //$NON-NLS-1$
			case ODF_UNKOWN_VALIDITY:
				return ret + ": no se puede comprobar la validez de la firma ODF"; //$NON-NLS-1$
			case UNKOWN_ERROR:
				return ret + ": no se puede comprobar la validez de la firma"; //$NON-NLS-1$
			case UNKOWN_SIGNATURE_FORMAT:
				return ret + ": los datos proporcionados no se corresponden con ningun formato de firma reconocido"; //$NON-NLS-1$
			default:
				return ret;
		}
	}

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
        /** Cuando la informacion contenida en la firma no sea consistente (certificados corruptos, etc.). */
        CORRUPTED_SIGN,
        /** Cuando la firma no se corresponde con los datos firmados. */
        NO_MATCH_DATA,
        /** Cuando no se encuentra la firma dentro del documento. */
        NO_SIGN,
        /** Cuando no se puede extraer un certificado o este no es v&aacute;lido. */
        CERTIFICATE_PROBLEM,
        /** Cuando existe un certificado de firma caducado. */
        CERTIFICATE_EXPIRED,
        /** Cuando existe un certificado de firma que aun no es v&aacute;lido. */
        CERTIFICATE_NOT_VALID_YET,
        /** Cuando la firma contiene un algoritmo no reconocido o no soportado. */
        ALGORITHM_NOT_SUPPORTED,
        /** Cuando el emisor del certificado no es v&aacute;lido. */
        CA_NOT_SUPPORTED,
        /** Cuando existe alg&uacute;n problema con las CRLs incrustadas en la firma. */
        CRL_PROBLEM,
        /** Cuando se trata de una firma PDF. */
        PDF_UNKOWN_VALIDITY,
        /** Cuando se trata de una firma OOXML. */
        OOXML_UNKOWN_VALIDITY,
        /** Cuando se trata de una firma ODF. */
        ODF_UNKOWN_VALIDITY,
        /** Cuando la firma es inv&aacute;lida pero no se sabe la raz&oacute;n. */
        UNKOWN_ERROR,
        /** Cuando los datos proporcionado no sean ning&uacute;n tipo de firma reconocida. */
        UNKOWN_SIGNATURE_FORMAT
    }

    /** Validez de la firma. */
    private final SIGN_DETAIL_TYPE validity;

    /** Error que invalida la firma o hace que la validez sea desconocida. */
    private final VALIDITY_ERROR error;

    /**
     * Identifica la validez de una firma.
     * @param type Validez de la firma.
     * @param error Error que invalida o impide comprobar la firma.
     */
    public SignValidity(final SIGN_DETAIL_TYPE type, final VALIDITY_ERROR error) {
        this.validity = type;
        this.error = error;
    }

    /**
     * Recupera la validez de la firma.
     * @return Validez de la firma.
     */
    public SIGN_DETAIL_TYPE getValidity() {
        return this.validity;
    }

    /**
     * Recupera el error que invalida la firma. Si no existe ning&uacute;n error o este es desconocido,
     * se devolver&aacute; {@code null}.
     * @return Error que invalida la firma o impide comprobar su validez.
     */
    public VALIDITY_ERROR getError() {
        return this.error;
    }
}

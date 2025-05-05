/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signvalidation;

/**
 * Indica si la firma es v&aacute;lida o no.
 * @author Carlos Gamuci
 */
public final class SignValidity {

	@Override
	public String toString() {
		String ret;
		ret = validityTypeToString();
		if (this.error == null) {
			return ret;
		}

		switch (this.error) {
			case NO_DATA:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.4"); //$NON-NLS-1$ //$NON-NLS-2$
			case CORRUPTED_SIGN:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.5"); //$NON-NLS-1$ //$NON-NLS-2$
			case NO_MATCH_DATA:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.6"); //$NON-NLS-1$ //$NON-NLS-2$
			case NO_SIGN:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.7"); //$NON-NLS-1$ //$NON-NLS-2$
			case CERTIFICATE_PROBLEM:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.8"); //$NON-NLS-1$ //$NON-NLS-2$
			case CERTIFICATE_EXPIRED:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.9"); //$NON-NLS-1$ //$NON-NLS-2$
			case CERTIFICATE_NOT_VALID_YET:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.10"); //$NON-NLS-1$ //$NON-NLS-2$
			case CANT_VALIDATE_CERT:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.11"); //$NON-NLS-1$ //$NON-NLS-2$
			case ALGORITHM_NOT_SUPPORTED:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.12"); //$NON-NLS-1$ //$NON-NLS-2$
			case CA_NOT_SUPPORTED:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.13"); //$NON-NLS-1$ //$NON-NLS-2$
			case CRL_PROBLEM:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.14"); //$NON-NLS-1$ //$NON-NLS-2$
			case PDF_UNKOWN_VALIDITY:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.15"); //$NON-NLS-1$ //$NON-NLS-2$
			case OOXML_UNKOWN_VALIDITY:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.16"); //$NON-NLS-1$ //$NON-NLS-2$
			case ODF_UNKOWN_VALIDITY:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.17"); //$NON-NLS-1$ //$NON-NLS-2$
			case UNKOWN_ERROR:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.18"); //$NON-NLS-1$ //$NON-NLS-2$
			case UNKOWN_SIGNATURE_FORMAT:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.19"); //$NON-NLS-1$ //$NON-NLS-2$
			case MODIFIED_DOCUMENT:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.20"); //$NON-NLS-1$ //$NON-NLS-2$
			case OVERLAPPING_SIGNATURE:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.21"); //$NON-NLS-1$ //$NON-NLS-2$
			case MODIFIED_FORM:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.22"); //$NON-NLS-1$ //$NON-NLS-2$
			case SUSPECTED_SIGNATURE:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.23"); //$NON-NLS-1$ //$NON-NLS-2$
			case SIGN_PROFILE_NOT_CHECKED:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.24"); //$NON-NLS-1$ //$NON-NLS-2$
			case CANT_VALIDATE_EXTERNALLY_DETACHED:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.25"); //$NON-NLS-1$ //$NON-NLS-2$
			case BAD_BUILD_SIGN:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.26"); //$NON-NLS-1$ //$NON-NLS-2$
			case CERTIFIED_SIGN_REVISION:
				return ret + ": " + ValidationMessages.getString("SignValidationMessage.27"); //$NON-NLS-1$ //$NON-NLS-2$
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
        /** Error a confirmar por parte del usuario. */
        PENDING_CONFIRM_BY_USER,
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
        /** No se ha podido validar el certificado. */
        CANT_VALIDATE_CERT,
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
        UNKOWN_SIGNATURE_FORMAT,
        /** Cuando un formualrio del documento ha sufrido alguna modificaci&oacute;n desde la &uacute;ltima firma. */
        MODIFIED_FORM,
        /** Cuando el documento ha sufrido alguna modificaci&oacute;n desde la &uacute;ltima firma. */
        MODIFIED_DOCUMENT,
        /** Cuando una firma est&aacute; solapando a otra en un documento PDF. */
        OVERLAPPING_SIGNATURE,
        /** El usuario debe confirmar la validez de la firma. */
        SUSPECTED_SIGNATURE,
        /** La firma no se ha comprobado completamente debido al nivel del perfil de firma. */
        SIGN_PROFILE_NOT_CHECKED,
        /** No se permite la validacion de firmas con referencias externas. */
        CANT_VALIDATE_EXTERNALLY_DETACHED,
        /** Cuando la firma es inv&aacute;lida porque no est&aacute; formada correctamente */
        BAD_BUILD_SIGN,
        /** Cuando la firma es inv&aacute;lida porque no es la &uacute;ltima firma*/
        CERTIFIED_SIGN_REVISION,
    }

    /** Validez de la firma. */
    private final SIGN_DETAIL_TYPE validity;

    /** Error que invalida la firma o hace que la validez sea desconocida. */
    private final VALIDITY_ERROR error;

    /** Excepci&oacute;n por la que se detect&oacute; el error de validaci&oacute;n. */
    private final Exception errorException;

    /** Identifica la validez de una firma.
     * @param type Validez de la firma.
     * @param error Error que invalida o impide comprobar la firma.
     * @param ex Excepci&oacute;n por la que se detect&oacute; el error de validaci&oacute;n.
     *           Si la validaci&oacute;n fue correcta, puede indicarse <code>null</code>. */
    public SignValidity(final SIGN_DETAIL_TYPE type, final VALIDITY_ERROR error, final Exception ex) {
        this.validity = type;
        this.error = error;
        this.errorException = ex;
    }

    /** Identifica la validez de una firma.
     * @param type Validez de la firma.
     * @param error Error que invalida o impide comprobar la firma. */
    public SignValidity(final SIGN_DETAIL_TYPE type, final VALIDITY_ERROR error) {
    	this(type, error, null);
    }

    /** Recupera la validez de la firma.
     * @return Validez de la firma. */
    public SIGN_DETAIL_TYPE getValidity() {
        return this.validity;
    }

    /** Recupera el error que invalida la firma. Si no existe ning&uacute;n error o este es desconocido,
     * se devolver&aacute; {@code null}.
     * @return Error que invalida la firma o impide comprobar su validez. */
    public VALIDITY_ERROR getError() {
        return this.error;
    }

    /** Recupera la excepci&oacute;n por la que se detect&oacute; el error de validaci&oacute;n.
     * Devuelve <code>null</code> en una validaci&oacute;n sin errores.
     * @return Excepci&oacute;n por la que se detect&oacute; el error de validaci&oacute;n. */
    public Exception getErrorException() {
    	return this.errorException;
    }

    public String validityTypeToString() {
		if (this.validity.equals(SIGN_DETAIL_TYPE.OK) || this.validity.equals(SIGN_DETAIL_TYPE.GENERATED)) {
			return ValidationMessages.getString("SignValidationMessage.0"); //$NON-NLS-1$
		}
		if (this.validity.equals(SIGN_DETAIL_TYPE.UNKNOWN)) {
			return ValidationMessages.getString("SignValidationMessage.1"); //$NON-NLS-1$
		}
		if (this.validity.equals(SIGN_DETAIL_TYPE.PENDING_CONFIRM_BY_USER)) {
			return ValidationMessages.getString("SignValidationMessage.2"); //$NON-NLS-1$
		}
		if (this.validity.equals(SIGN_DETAIL_TYPE.KO)) {
			return ValidationMessages.getString("SignValidationMessage.3"); //$NON-NLS-1$
		}
		return null;
    }
}

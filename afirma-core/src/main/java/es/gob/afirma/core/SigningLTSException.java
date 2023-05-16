/* Copyright (C) 2022 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core;

/**
 * Excepci&oacute;n que denota que no se puede cofirmar o contrafirmar
 * una firma porque alguna firma previa contiene informaci&oacute;n de
 * archivo que quedar&iacute;a invalidada.
 */
public class SigningLTSException extends RuntimeConfigNeededException {

	/** Serial Id. */
	private static final long serialVersionUID = 995443738935981665L;

	public static final String REQUESTOR_MSG_CODE = "signingLts"; //$NON-NLS-1$
	public static final String REQUESTOR_POSSIBLE_MSG_CODE = "signingLtsPossible"; //$NON-NLS-1$

	private static final String EXTRA_PARAM_NEEDED = "allowSignLTSignature"; //$NON-NLS-1$

	/**
	 * Crear la excepci&oacute;n motivada por lo indicado en el mensaje.
	 * @param message Mensaje descriptivo del problema.
	 */
	public SigningLTSException(final String message) {
		super(message, RuntimeConfigNeededException.RequestType.CONFIRM, REQUESTOR_MSG_CODE, EXTRA_PARAM_NEEDED);
	}

	/**
	 * Crear la excepci&oacute;n motivada por lo indicado en el mensaje.
	 * @param message Mensaje descriptivo del problema.
	 * @param possible {@code true} indica que esto es un posible problema, {@code false} indica que
	 * hay certeza de ello.
	 */
	public SigningLTSException(final String message, final boolean possible) {
		super(message, RuntimeConfigNeededException.RequestType.CONFIRM,
				possible ? REQUESTOR_POSSIBLE_MSG_CODE : REQUESTOR_MSG_CODE , EXTRA_PARAM_NEEDED);
	}

	/**
	 * Crear la excepci&oacute;n motivada por lo indicado en el mensaje.
	 * @param message Mensaje descriptivo del problema.
	 * @param cause Origen del problema.
	 * @param possible {@code true} indica que esto es un posible problema, {@code false} indica que
	 * hay certeza de ello.
	 */
	public SigningLTSException(final String message, final Throwable cause, final boolean possible) {
		super(message, RuntimeConfigNeededException.RequestType.CONFIRM,
				possible ? REQUESTOR_POSSIBLE_MSG_CODE : REQUESTOR_MSG_CODE, EXTRA_PARAM_NEEDED, cause);
	}
}

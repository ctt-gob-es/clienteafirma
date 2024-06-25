/* Copyright (C) 2023 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core;

import java.util.Properties;

/**
 * Excepci&oacute;n que denota que se va a generar una firma que declara la pol&iacute;tica de firma
 * de la AGE, pero es incompatible con ella.
 */
public class AGEPolicyIncompatibilityException extends CustomRuntimeConfigNeededException {

	/** Serial Id. */
	private static final long serialVersionUID = 8278487852684799319L;

	public static final String REQUESTOR_SIGN_MSG_CODE = "agePolicyIncompatibilitySign"; //$NON-NLS-1$
	public static final String REQUESTOR_COSIGN_MSG_CODE = "agePolicyIncompatibilityCosign"; //$NON-NLS-1$
	public static final String REQUESTOR_COUNTERSIGN_MSG_CODE = "agePolicyIncompatibilityCounterSign"; //$NON-NLS-1$

	private static final String EXTRA_PARAM_NEEDED = "avoidAGEPolicyIncompatibilities"; //$NON-NLS-1$

	/** Error identificado durante una operaci&oacute;n de firma. */
	public static final int OP_SIGN = 1;
	/** Error identificado durante una operaci&oacute;n de cofirma. */
	public static final int OP_COSIGN = 2;
	/** Error identificado durante una operaci&oacute;n de contrafirma. */
	public static final int OP_COUNTERSIGN = 3;

	/**
	 * Crear la excepci&oacute;n motivada por lo indicado en el mensaje.
	 * @param message Mensaje descriptivo del problema.
	 */
	public AGEPolicyIncompatibilityException(final String message) {
		this(message, OP_SIGN);
	}

	/**
	 * Crear la excepci&oacute;n motivada por lo indicado en el mensaje.
	 * @param message Mensaje descriptivo del problema.
	 * @param operation C&oacute;digo de operaci&oacute;n que identifica el mensaje de solicitud
	 * de confirmaci&oacute;n al usuario.
	 */
	public AGEPolicyIncompatibilityException(final String message, final int operation) {
		super(message, RuntimeConfigNeededException.RequestType.CONFIRM, getMessageCode(operation), EXTRA_PARAM_NEEDED);
	}

	/**
	 * Crear la excepci&oacute;n motivada por lo indicado en el mensaje.
	 * @param message Mensaje descriptivo del problema.
	 * @param cause Origen del problema.
	 */
	public AGEPolicyIncompatibilityException(final String message, final Throwable cause) {
		this(message, OP_SIGN, cause);
	}

	/**
	 * Crear la excepci&oacute;n motivada por lo indicado en el mensaje.
	 * @param message Mensaje descriptivo del problema.
	 * @param operation C&oacute;digo de operaci&oacute;n que identifica el mensaje de solicitud
	 * de confirmaci&oacute;n al usuario.
	 * @param cause Origen del problema.
	 */
	public AGEPolicyIncompatibilityException(final String message, final int operation, final Throwable cause) {
		super(message, RuntimeConfigNeededException.RequestType.CONFIRM, getMessageCode(operation), EXTRA_PARAM_NEEDED, cause);
	}

	@Override
	public void prepareOperationWithConfirmation(final Properties extraParams) {
		if (extraParams != null) {
			extraParams.remove("policyIdentifier");//$NON-NLS-1$
			extraParams.remove("policyIdentifierHash");//$NON-NLS-1$
			extraParams.remove("policyIdentifierHashAlgorithm");//$NON-NLS-1$
			extraParams.remove("policyDescription");//$NON-NLS-1$
			extraParams.remove("policyQualifier"); //$NON-NLS-1$
		}
	}

	/**
	 * Obtiene el c&oacute;digo del mensaje que se debe mostrar a un usuario para una
	 * operaci&oacute;n.
	 * @param operation C&oacute;digo de operaci&oacute;n.
	 * @return Mensaje que mostrar al usuario.
	 */
	private static String getMessageCode(final int operation) {
		String msg;
		switch (operation) {
		case OP_COSIGN:
			msg = REQUESTOR_COSIGN_MSG_CODE;
			break;
		case OP_COUNTERSIGN:
			msg = REQUESTOR_COUNTERSIGN_MSG_CODE;
			break;
		default:
			msg = REQUESTOR_SIGN_MSG_CODE;
			break;
		}
		return msg;
	}
}

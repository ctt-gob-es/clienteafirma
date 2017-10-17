/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.cadestri.client;

final class ProtocolConstants {

	private ProtocolConstants() {
		// No instanciable
	}

	static final String HTTP_CGI = "?"; //$NON-NLS-1$
	static final String HTTP_EQUALS = "="; //$NON-NLS-1$
	static final String HTTP_AND = "&"; //$NON-NLS-1$

	// Parametros que necesitamos para la URL de las llamadas al servidor de firma
	static final String PARAMETER_NAME_DOCID = "doc"; //$NON-NLS-1$
	static final String PARAMETER_NAME_ALGORITHM = "algo"; //$NON-NLS-1$
	static final String PARAMETER_NAME_FORMAT = "format"; //$NON-NLS-1$
	static final String PARAMETER_NAME_CERT = "cert"; //$NON-NLS-1$
	static final String PARAMETER_NAME_EXTRA_PARAM = "params"; //$NON-NLS-1$
	static final String PARAMETER_NAME_SESSION_DATA = "session"; //$NON-NLS-1$

	/** Nombre de la propiedad de URL del servidor de firma trif&aacute;sica. */
	static final String PROPERTY_NAME_SIGN_SERVER_URL = "serverUrl"; //$NON-NLS-1$

	/** Indica si la postfirma requiere el identificador o contenido del documento. */
	static final String PROPERTY_NAME_NEED_DATA = "NEED_DATA"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro de c&oacute;digo de operaci&oacute;n en la URL de llamada al servidor de firma. */
	static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro que identifica la operaci&oacute;n criptogr&aacute;fica en la URL del servidor de firma. */
	static final String PARAMETER_NAME_CRYPTO_OPERATION = "cop"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n de prefirma en servidor. */
	static final String OPERATION_PRESIGN = "pre"; //$NON-NLS-1$
}

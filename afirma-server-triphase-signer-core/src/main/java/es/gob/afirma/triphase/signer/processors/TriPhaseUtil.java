/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.signer.processors;

import java.util.Properties;
import java.util.UUID;

/**
 * Funciones de utilidad en el proceso de firma en 3 fases.
 * @author Carlos Gamuci
 */
public final class TriPhaseUtil {

	private static final String PROP_ID = "SignatureId"; //$NON-NLS-1$

	private TriPhaseUtil() {
		// No instanciable
	}

	/**
	 * Recupera de los par&aacute;metros de configuraci&oacute;n el identificado de la firma
	 * que se est&aacute; procesando y, en caso de no estar reflejado, genera un identificador
	 * y lo agrega al extraParams.
	 * @param extraParams Par&aacute;metro de configuraci&oacute;n de la firma.
	 * @return Identificador de la firma.
	 */
	public static String getSignatureId(final Properties extraParams) {
		if (extraParams == null || !extraParams.containsKey(PROP_ID)) {
			return UUID.randomUUID().toString();
		}
		return extraParams.getProperty(PROP_ID);
	}

}

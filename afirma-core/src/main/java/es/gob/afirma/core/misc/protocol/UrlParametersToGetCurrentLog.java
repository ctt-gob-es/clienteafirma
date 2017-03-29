/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.core.misc.protocol;

import java.util.Map;

/** Par&aacute;metros de la URL de llamada a la aplicaci&oacute;n. */
public final class UrlParametersToGetCurrentLog extends UrlParameters {
	
	/** Par&aacute;metro de entrada con la m&iacute;nima versi&oacute;n requerida del aplicativo a usar en la invocaci&oacute;n por protocolo. */
	private static final String VER_PARAM = "ver"; //$NON-NLS-1$
				
	private String minimumVersion;
	
	/** Obtiene la versi&oacute;n m&iacute;nima requerida del aplicativo.
	 * @return Versi&oacute;n m&iacute;nima requerida del aplicativo. */
	public String getMinimumVersion() {
		return this.minimumVersion;
	}
	
	/**
	 * Constructor sin argumentos
	 */
	UrlParametersToGetCurrentLog() {
		setMinimumVersion(null);
	}
	
	/** Establece la versi&oacute;n m&iacute;nima exigida del protocolo de comunicaci&oacute;n.
	 * @param minVer Versi&oacute;n m&iacute;nima del protocolo.
	 */
	void setMinimumVersion(final String minVer) {
		this.minimumVersion = minVer;
	}
	
	/**
	 * M&eacute;todo que establece los par&aacute;metros propios de la operaci&oacute;n de obtenci&oacute;n
	 * de log actual de la aplicaci&oacute;n
	 * @param params Mapa de valores obtenidos de la URL de invocaci&oacute;n de la operaci&oacute;n
	 * @throws ParameterException
	 */
	void setGetCurrentLogParameters(final Map<String, String> params) throws ParameterException {
		
		// Version minima requerida del protocolo que se debe soportar
		if (params.containsKey(VER_PARAM)) {
			setMinimumVersion(params.get(VER_PARAM));
		}
		else {
			setMinimumVersion(Integer.toString(ProtocolVersion.VERSION_0.getVersion()));
		}
			
	}	

}

/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc.protocol;

import java.util.Map;

/** Par&aacute;metros de la URL de llamada a la aplicaci&oacute;n. */
public final class UrlParametersToGetCurrentLog extends UrlParameters {

	/** Par&aacute;metro de entrada con la m&iacute;nima versi&oacute;n requerida del aplicativo a usar en la invocaci&oacute;n por protocolo. */
	private static final String VER_PARAM = "ver"; //$NON-NLS-1$

	private String minimumProtocolVersion;

	/**
	 * Construye el conjunto de par&aacute;metros vac&iacute;o.
	 */
	public UrlParametersToGetCurrentLog() {
		setMinimumProtocolVersion(null);
	}

	/** Obtiene la versi&oacute;n m&iacute;nima requerida del aplicativo.
	 * @return Versi&oacute;n m&iacute;nima requerida del aplicativo. */
	public String getMinimumProtocolVersion() {
		return this.minimumProtocolVersion;
	}

	/** Establece la versi&oacute;n m&iacute;nima exigida del protocolo de comunicaci&oacute;n.
	 * @param minVer Versi&oacute;n m&iacute;nima del protocolo. */
	void setMinimumProtocolVersion(final String minVer) {
		this.minimumProtocolVersion = minVer;
	}

	/** Establece los par&aacute;metros propios de la operaci&oacute;n de obtenci&oacute;n
	 * de registro (<i>log</i>) actual de la aplicaci&oacute;n
	 * @param params Mapa de valores obtenidos de la URL de invocaci&oacute;n de la operaci&oacute;n
	 * @throws ParameterException Si alguno de los par&aacute;metros proporcionados no es v&aacute;lido. */
	public void setGetCurrentLogParameters(final Map<String, String> params) throws ParameterException {
		// Version minima requerida del protocolo que se debe soportar
		if (params.containsKey(VER_PARAM)) {
			setMinimumProtocolVersion(params.get(VER_PARAM));
		}
		else {
			setMinimumProtocolVersion(Integer.toString(ProtocolVersion.VERSION_0.getVersion()));
		}
	}

}

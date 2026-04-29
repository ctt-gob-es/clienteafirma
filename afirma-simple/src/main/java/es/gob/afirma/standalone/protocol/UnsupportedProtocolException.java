/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.protocol;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.protocol.ProtocolVersion;
import es.gob.afirma.standalone.SimpleErrorCode;

/**
 * Excepci&oacute;n con la que se indica si una versi&oacute;n de protocolo de comunicaci&oacute;n
 * no esta soportado y si es necesario actualizar la aplicaci&oacute;n para soportarlo (protocolo
 * m&aacute;s avanzada que el soportado por la aplicaci&oacute;n) o si se trata de un protocolo
 * obsoleto.
 */
public class UnsupportedProtocolException extends AOException {

	/** Serial Id. */
	private static final long serialVersionUID = -7429271911165454430L;

	private final boolean newVersionNeeded;
	private final ProtocolVersion version;

	UnsupportedProtocolException(final ProtocolVersion version, final boolean newVersionNeeded) {
		super("Version del protocolo no soportada", SimpleErrorCode.Request.UNSUPPORTED_PROTOCOL_VERSION); //$NON-NLS-1$
		this.version = version;
		this.newVersionNeeded = newVersionNeeded;
	}

	/**
	 * Indica si la version del protocolo es superior a la soportada y, por tanto, hay que actualizar
	 * a una nueva versi&oacute;n del n&uacute;cleo.
	 * @return {@code true} si se requiere actualizar a una nueva versi&oacute;n, {@code false} cuando
	 * la el protocolo es antiguo y no compatible con esta versi&oacute;n.
	 */
	public boolean isNewVersionNeeded() {
		return this.newVersionNeeded;
	}

	/**
	 * Recupera la versi&oacute;n de protocolo necesaria.
	 * @return Versi&oacute;n del protocolo solicitada.
	 */
	public ProtocolVersion getVersion() {
		return this.version;
	}
}

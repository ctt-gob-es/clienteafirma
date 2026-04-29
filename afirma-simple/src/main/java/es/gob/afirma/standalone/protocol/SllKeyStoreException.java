/* Copyright (C) 2020 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */
package es.gob.afirma.standalone.protocol;

import es.gob.afirma.core.AOException;
import es.gob.afirma.standalone.SimpleErrorCode;

/**
 * Error producido cuando no se puede cargar el almac&eacute;n de claves con el certificado
 * SSL utilizado para cifrar la conexi&oacute;n por socket o websocket.
 */
public class SllKeyStoreException extends AOException {

	/** Serial Id. */
	private static final long serialVersionUID = 5056838661534858313L;

	/**
	 * Construye una excepci&oacute;n.
	 *
	 * @param msg Mensaje de la excepci&oacute;n.
	 * @param e Excepci&oacute;n de la que proviene.
	 */
	public SllKeyStoreException(final String msg, final Exception e) {
		super(msg, e, SimpleErrorCode.Internal.LOADING_SSL_KEYSTORE_ERROR);
	}
}

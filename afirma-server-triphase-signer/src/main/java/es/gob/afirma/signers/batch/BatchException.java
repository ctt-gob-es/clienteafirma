/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.batch;

/** Error en el proceso de firma por lotes.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class BatchException extends Exception {

	private static final long serialVersionUID = 1L;

	public BatchException(final String msg) {
		super(msg);
	}

	public BatchException(final String msg, final Throwable e) {
		super(msg, e);
	}

}

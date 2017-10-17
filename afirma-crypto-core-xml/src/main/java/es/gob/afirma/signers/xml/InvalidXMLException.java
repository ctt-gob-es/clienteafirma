/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xml;

import es.gob.afirma.core.AOFormatFileException;

/** Excepci&oacute;n para notificar que se ha encontrado un objeto que no es un XML v&aacute;lido.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class InvalidXMLException extends AOFormatFileException {

	private static final long serialVersionUID = -8682543966969351726L;

	/** Construye una excepci&oacute;n para notificar que se ha encontrado un objeto
	 * que no es un XML apto para ser firmado con los par&aacute;metros indicados.
	 * @param e Causa de la excepci&oacute;n */
	public InvalidXMLException(final Throwable e) {
		super("Los datos proporcionados no son un XML apto para su firma con los parametros indicados: " + e, e); //$NON-NLS-1$
	}

	/** Construye una excepci&oacute;n para notificar que se ha encontrado un objeto
	 * que no es un XML apto para ser firmado con los par&aacute;metros indicados.
	 * @param msg Mensaje de la excepci&oacute;n */
	public InvalidXMLException(final String msg) {
		super(msg);
	}

	/** Construye una excepci&oacute;n para notificar que se ha encontrado un objeto
	 * que no es un XML apto para ser firmado con los par&aacute;metros indicados.
	 * @param msg Mensaje de la excepci&oacute;n.
	 * @param e Causa de la excepci&oacute;n. */
	public InvalidXMLException(final String msg, final Throwable e) {
		super(msg, e);
	}

}

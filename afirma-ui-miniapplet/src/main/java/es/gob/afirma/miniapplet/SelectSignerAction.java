/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.miniapplet;

import java.io.IOException;
import java.security.PrivilegedExceptionAction;

import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.AOSignerFactory;

/**
 * Acci&oacute;n para la seleccion de un manejador de firma.
 * @author Carlos Gamuci Mill&aacute;n
 */
final class SelectSignerAction implements PrivilegedExceptionAction<AOSigner> {

    private final String format;
    private final byte[] data;

    /**
     * Crea la acci&oacute;n para la obtenci&oacute;n de un manejador de firma compatible
     * con el formato de firma indicado.
     * @param format Formato de firma.
     */
    SelectSignerAction(final String format) {
        this.format = format;
        this.data = null;
    }

    /**
     * Crea la acci&oacute;n para la obtenci&oacute;n de un manejador de firma compatible
     * con la firma indicada.
     * @param data Firma electr&oacute;nica para la que se desea el manejador.
     */
    SelectSignerAction(final byte[] data) {
        this.data = (data != null ? data.clone() : null);
        this.format = null;
    }

    /**
     * Selecciona el manejador de firma adecuado para el formato o los datos indicados.
     * Si no se encuentra un manejador compatible, se devuelve {@code null}.
     * @return Manejador de firma.
     * @throws IOException Cuando se produce un error durante la lectura de los datos.
     */
	@Override
	public AOSigner run() throws IOException {
		if (this.format != null) {
			return AOSignerFactory.getSigner(this.format);
		}
		else if (this.data != null) {
			return AOSignerFactory.getSigner(this.data);
		}
        return null;
	}
}

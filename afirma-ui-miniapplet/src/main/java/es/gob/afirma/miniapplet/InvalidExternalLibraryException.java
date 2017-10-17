/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.miniapplet;


/** Indica que en el sistema hay una biblioteca que puede causar un mal funcionamiento
 * del MiniApplet de Afirma.
 * @deprecated Se externaliza las comprobaciones de entorno.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
@Deprecated
final class InvalidExternalLibraryException extends Exception {

    private static final long serialVersionUID = 8785012357200277893L;

    private final String localizedMessageKey;
    private final String[] localizedMessageParams;

    InvalidExternalLibraryException(final String message, final String localizedStringCode, final String[] localizedStringParams) {
        super(message);
        this.localizedMessageKey = localizedStringCode;
        this.localizedMessageParams = (localizedStringParams != null) ? localizedStringParams.clone() : null;
    }

    /** {@inheritDoc} */
    @Override
    public String getLocalizedMessage() {
    	if (this.localizedMessageParams != null) {
    		return MiniAppletMessages.getString(this.localizedMessageKey, this.localizedMessageParams);
    	}
    	return MiniAppletMessages.getString(this.localizedMessageKey);
    }

}

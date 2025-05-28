/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ErrorCode;

/**
 * Indica que ocurri&oacute; un error intentando obtener un <code>KeyStore</code>, pero que se obtuvo uno de un tipo alternativo.
 * @version 0.1
 */
public final class KeystoreAlternativeException extends AOException {

    private static final long serialVersionUID = -1536411480952188376L;

    /** Almac&eacute;n alternativo. */
    private final AOKeyStore alternativeKs;

    /**
     * Crea la excepci&oacute;n con un mensaje determinado y un almac&eacute;n
     * alternativo que podr&iacute;a usarse como sustituto.
     * @param ks
     *        Almac&eacute;n de claves y certificados alternativo
     * @param desc
     *        Mensaje descriptivo de la excepci&oacute;n.
     * @param cause
     *        Causa del error.
	 * @param code
	 * 		  C&oacute;dico que identifica el error.
     */
    public KeystoreAlternativeException(final AOKeyStore ks, final String desc, final Throwable cause, final ErrorCode code) {
        super(desc, cause, code);
        this.alternativeKs = ks;
    }

    /**
     * Crea la excepci&oacute;n con un mensaje determinado y un almac&eacute;n
     * alternativo que podr&iacute;a usarse como sustituto.
     * @param ks
     *        Almac&eacute;n de claves y certificados alternativo
     * @param desc
     *        Mensaje descriptivo de la excepci&oacute;n.
	 * @param code
	 *        C&oacute;dico que identifica el error.
     */
    public KeystoreAlternativeException(final AOKeyStore ks, final String desc, final ErrorCode code) {
        super(desc, code);
        this.alternativeKs = ks;
    }

    /**
     * Obtiene el almac&eacute;n alternativo que podr&iacute;a usarse como
     * sustituto.
     * @return Almac&eacute;n de claves y certificados alternativo
     */
    public AOKeyStore getAlternativeKsm() {
        return this.alternativeKs;
    }

}

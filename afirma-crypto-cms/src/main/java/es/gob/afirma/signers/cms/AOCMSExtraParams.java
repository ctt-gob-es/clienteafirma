/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.cms;

/** Par&aacute;metros adicionales para firmas CMS. */
public final class AOCMSExtraParams {

    /** Modo de firma. */
    static final String MODE = "mode"; //$NON-NLS-1$

    /** Algoritmo de huella digital cuando este se proporciona precalculada. */
    public static final String PRECALCULATED_HASH_ALGORITHM = "precalculatedHashAlgorithm";//$NON-NLS-1$

    /** <code>true</code> si se desea usar la hora y fecha del sistema como hora y fecha de firma,
     * <code>false</code> en caso contrario. */
    static final String APPLY_SYSTEM_DATE = "applySystemDate"; //$NON-NLS-1$

    /** Constructor vac&iacute;o privado para que no se pueda instanciar la clase ya que es est&aacute;tico. */
    private AOCMSExtraParams(){
        // No instanciable
    }
}

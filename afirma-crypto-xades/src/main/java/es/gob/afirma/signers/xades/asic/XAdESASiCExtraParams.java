/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xades.asic;

final class XAdESASiCExtraParams {

    /** Determina el nombre del fichero de datos dentro del contenedor.
     * Si no se estabkece este par&aacute;metro se usa el nombre <code>dataobject</code> con la extensi&oacute;n correspondiente al tipo de datos,
     * o <code>.bin</code> si esta primera no puede determinarse de forma autom&aacute;tica. */
    public static final String ASICS_FILENAME = "asicsFilename"; //$NON-NLS-1$

    /** Constructor vac&iacute;o privado para que no se pueda instanciar la clase ya que es est&aacute;tico. */
    private XAdESASiCExtraParams(){
        // No instanciable
    }
}

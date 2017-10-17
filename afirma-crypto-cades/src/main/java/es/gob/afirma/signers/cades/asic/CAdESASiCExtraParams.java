/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.cades.asic;

/** Par&aacute;metros adicionales aceptados para las firmas CAdES ASiC-S. */
final class CAdESASiCExtraParams {

    /** En las firmas simples CAdES ASiC-S, determina el nombre del fichero de datos dentro del contenedor.<br>
     * Si no se estabkece este par&aacute;metro se usa el nombre <i>dataobject</i> con la extensi&oacute;n correspondiente
     * al tipo de datos, o <i>.bin</i> si esta primera no puede determinarse de forma autom&aacute;tica. */
    static final String ASICS_FILENAME = "asicsFilename"; //$NON-NLS-1$

    /** Constructor vac&iacute;o privado para que no se pueda instanciar la clase ya que es est&aacute;tico. */
    private CAdESASiCExtraParams(){
        // No instanciable
    }
}

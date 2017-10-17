/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.ooxml;

final class OOXMLExtraParams {

    /** Ciudad en la que se realiza la firma. */
    static final String SIGNATURE_PRODUCTION_CITY = "signatureProductionCity";//$NON-NLS-1$

    /** Provincia en la que se realiza la firma. */
    static final String SIGNATURE_PRODUCTION_PROVINCE = "signatureProductionProvince";//$NON-NLS-1$

    /** C&oacute;digo postal en el que se realiza la firma. */
    static final String SIGNATURE_PRODUCTION_POSTAL_CODE = "signatureProductionPostalCode";//$NON-NLS-1$

    /** Pa&iacute;s en el que se realiza la firma. */
    static final String SIGNATURE_PRODUCTION_COUNTRY = "signatureProductionCountry";//$NON-NLS-1$

    /** Cargo atribuido para el firmante. */
    static final String SIGNER_CLAIMED_ROLES = "signerClaimedRoles";//$NON-NLS-1$

    /** Comentarios sobre la firma (normalmente la raz&oacute;n de la firma). */
    static final String SIGNATURE_COMMENTS = "signatureComments";//$NON-NLS-1$

    /** Primera l&iacute;nea de la direcci&oacute;n en la que se ha realizado la firma. */
    static final String SIGNATURE_ADDRESS1 = "signatureAddress1";//$NON-NLS-1$

    /** Segunda l&iacute;nea de la direcci&oacute;n en la que se ha realizado la firma. */
    static final String SIGNATURE_ADDRESS2 = "signatureAddress2";//$NON-NLS-1$

    /** Constructor vac&iacute;o privado para que no se pueda instanciar la clase ya que es est&aacute;tico. */
    private OOXMLExtraParams(){
        // No instanciable
    }
}

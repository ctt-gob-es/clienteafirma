/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signvalidation;

import java.io.IOException;
import java.util.Map;

import es.gob.afirma.core.misc.protocol.ConfirmationNeededException;

/** Valida una firma del tipo del validador instanciado.
 * @author Sergio Mart&iacute;nez Rico. */
public interface SignValider {

	/** Valida una firma del tipo del validador instanciado.
     * @param sign Firma a validar
     * @return Validez de la firma.
	 * @throws IOException Fallo durante la validaci&oacute;n de la firma. */
    SignValidity validate(final byte[] sign) throws IOException;

	/** Valida una firma del tipo del validador instanciado.
     * @param sign Firma a validar
	 * @param checkCertificates Indica si debe comprobarse o no el periodo de validez de los certificados.
     * @return Validez de la firma.
	 * @throws IOException Fallo durante la validaci&oacute;n de la firma. */
    SignValidity validate(final byte[] sign, final boolean checkCertificates) throws IOException;

	/** Valida una firma del tipo del validador instanciado.
     * @param sign Firma a validar
	 * @param params Indica propiedades a indicar para tener en cuenta en la validaci&oacute;n.
     * @return Validez de la firma.
	 * @throws ConfirmationNeededException Excepci&oacute;n con las opciones para el di&aacute;logo
	 * de confirmaci&oacute;n para la operaci&oacute;n.
	 * @throws IOException Fallo durante la validaci&oacute;n de la firma. */
    SignValidity validate(final byte[] sign, final Map<String, String> params) throws ConfirmationNeededException, IOException;
}

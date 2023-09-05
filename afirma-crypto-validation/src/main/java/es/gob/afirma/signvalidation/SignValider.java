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
import java.util.Properties;

import es.gob.afirma.core.RuntimeConfigNeededException;

/** Valida una firma del tipo del validador instanciado.
 * @author Sergio Mart&iacute;nez Rico. */
public abstract class SignValider {

	private boolean relaxed = false;

	/**
	 * Establece si la validacion debe ser relajada. Esta se utiliza cuando
	 * se admiten problemas en la firma de cara a una operaci&oacute;n posterior.
	 * @param relaxed {@code true} para habilitar el modo relajado, {@code false}
	 * en caso contrario.
	 */
	public void setRelaxed(final boolean relaxed) {
		this.relaxed = relaxed;
	}

	/**
	 * Devuelve si la validacion ser&aacute; relajada. Esta se utiliza cuando
	 * se admiten problemas en la firma de cara a una operaci&oacute;n posterior.
	 * @return {@code true} para habilitar el modo relajado, {@code false}
	 * en caso contrario.
	 */
	public boolean isRelaxed() {
		return this.relaxed;
	}

	/** Valida una firma del tipo del validador instanciado.
     * @param sign Firma a validar
     * @return Validez de la firma.
     * @throws RuntimeConfigNeededException Antes de realizar una
     * operaci&oacute;n sobre la firma ser&iacute;a necesaria
     * confirmaci&oacute;n del usuario. S&oacute;lo se lanza en modo relajado.
	 * @throws IOException Fallo durante la validaci&oacute;n de la firma. */
    public abstract SignValidity validate(final byte[] sign) throws RuntimeConfigNeededException, IOException;

	/** Valida una firma del tipo del validador instanciado.
     * @param sign Firma a validar
	 * @param checkCertificates Indica si debe comprobarse o no el periodo de validez de los certificados.
     * @return Validez de la firma.
     * @throws RuntimeConfigNeededException Antes de realizar una
     * operaci&oacute;n sobre la firma ser&iacute;a necesaria
     * confirmaci&oacute;n del usuario. S&oacute;lo se lanza en modo relajado.
	 * @throws IOException Fallo durante la validaci&oacute;n de la firma. */
    public abstract SignValidity validate(final byte[] sign, final boolean checkCertificates) throws RuntimeConfigNeededException, IOException;

	/** Valida una firma del tipo del validador instanciado.
     * @param sign Firma a validar
	 * @param params Indica propiedades a indicar para tener en cuenta en la validaci&oacute;n.
     * @return Validez de la firma.
	 * @throws RuntimeConfigNeededException Antes de realizar una
     * operaci&oacute;n sobre la firma ser&iacute;a necesaria
     * confirmaci&oacute;n del usuario. S&oacute;lo se lanza en modo relajado.
	 * @throws IOException Fallo durante la validaci&oacute;n de la firma. */
    public abstract SignValidity validate(final byte[] sign, final Properties params) throws RuntimeConfigNeededException, IOException;
}

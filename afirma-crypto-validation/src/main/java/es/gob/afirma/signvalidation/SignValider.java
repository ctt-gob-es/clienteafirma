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
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import es.gob.afirma.core.RuntimeConfigNeededException;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.SignValidity.VALIDITY_ERROR;

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
    public abstract List<SignValidity> validate(final byte[] sign) throws RuntimeConfigNeededException, IOException;

	/** Valida una firma del tipo del validador instanciado.
     * @param sign Firma a validar
	 * @param checkCertificates Indica si debe comprobarse o no el periodo de validez de los certificados.
     * @return Validez de la firma.
     * @throws RuntimeConfigNeededException Antes de realizar una
     * operaci&oacute;n sobre la firma ser&iacute;a necesaria
     * confirmaci&oacute;n del usuario. S&oacute;lo se lanza en modo relajado.
	 * @throws IOException Fallo durante la validaci&oacute;n de la firma. */
    public abstract List<SignValidity> validate(final byte[] sign, final boolean checkCertificates) throws RuntimeConfigNeededException, IOException;

	/** Valida una firma del tipo del validador instanciado.
     * @param sign Firma a validar
	 * @param params Indica propiedades a indicar para tener en cuenta en la validaci&oacute;n.
     * @return Validez de la firma.
	 * @throws RuntimeConfigNeededException Antes de realizar una
     * operaci&oacute;n sobre la firma ser&iacute;a necesaria
     * confirmaci&oacute;n del usuario. S&oacute;lo se lanza en modo relajado.
	 * @throws IOException Fallo durante la validaci&oacute;n de la firma. */
    public abstract List<SignValidity> validate(final byte[] sign, final Properties params) throws RuntimeConfigNeededException, IOException;

    /**
     * Reordena la lista de validaciones para que en caso de que una firma sea logeva y solo se encuentren errores de
     * certificados caducados, se le de prioridad a la validaci&oacute;n de tipo UNKNOWN.
     * @param validityList Lista de validaciones.
     * @return Lista de validaciones reordenada.
     */
    protected static List<SignValidity> checkLongStandingValiditySign(final List<SignValidity> validityList) {
		final List<SignValidity> validityListResult = new ArrayList<>(validityList);
		int certKOIndex = -1;
		int longStandingWarningIndex = -1;
		boolean isAnotherKOType = false;
		for (int i = 0 ; i < validityList.size() ; i++) {
			if (SIGN_DETAIL_TYPE.KO.equals(validityList.get(i).getValidity())) {
				if(VALIDITY_ERROR.CERTIFICATE_EXPIRED.equals(validityList.get(i).getError())) {
					certKOIndex = i;
				} else {
					isAnotherKOType = true;
				}
			} else if (SIGN_DETAIL_TYPE.UNKNOWN.equals(validityList.get(i).getValidity())
						&& VALIDITY_ERROR.SIGN_PROFILE_NOT_CHECKED.equals(validityList.get(i).getError())) {
				longStandingWarningIndex = i;
			}
		}

		// Si se ha encontrado solo errores de certificado y un error de atributos longevos, se le dara prioridad a este ultimo
		if (!isAnotherKOType && certKOIndex != -1 && longStandingWarningIndex != -1) {
			final SignValidity v = validityListResult.get(0);
			validityListResult.set(0, validityList.get(longStandingWarningIndex));
			validityListResult.set(longStandingWarningIndex, v);
		}

		return validityListResult;
	}

    /**
     * Ordena una lista a la que ya se le ha dado prioridad a validaciones incompletas
     * para darle prioridad a validaciones KO pr encima de estas.
     * @param validityList Lista con validaciones incompletas ya ordenadas.
     * @return Lista con las prioridades KO e incompletas ordenadas.
     */
	protected static List<SignValidity> checkValidityKOPriority(final List<SignValidity> validityList) {
		final List<SignValidity> signValidityResult = new ArrayList<>();
		for (final SignValidity sv : validityList) {
			if (!signValidityResult.contains(sv)) {
				if (SIGN_DETAIL_TYPE.KO.equals(sv.getValidity())) {
					signValidityResult.add(0, sv);
				} else {
					signValidityResult.add(sv);
				}
			}
		}
		return signValidityResult;
	}
}

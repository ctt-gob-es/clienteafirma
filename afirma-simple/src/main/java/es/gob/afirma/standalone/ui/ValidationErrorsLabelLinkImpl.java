/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui;

import java.util.List;

import es.gob.afirma.signvalidation.SignValidity;

/**
 * Implementaci&oacute;n para gestionar errores en validaciones de firma.
 * @author Jos&eacute;s Montero Rivero.
 * */
public final class ValidationErrorsLabelLinkImpl implements LabelLinkListener{

	private final byte [] signData;
	private final List<SignValidity> generalValidation;

    public ValidationErrorsLabelLinkImpl (final byte [] signData, final List<SignValidity> generalValidation) {
    	this.generalValidation = generalValidation;
    	this.signData = signData;
    }

	@Override
	public void openLink() {
		final ValidationInfoDialog validationDialog = new ValidationInfoDialog(null, this.signData, this.generalValidation);
    	validationDialog.setVisible(true);
	}

}

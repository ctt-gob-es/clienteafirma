/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.cades;

import java.util.ArrayList;
import java.util.List;

/** Indicaci&oacute;n sobre el tipo de compromiso adquirido con la firma (<i>CommitmentTypeIndication</i>).
 * <pre>
 * CommitmentTypeIndication ::= SEQUENCE {
 *   commitmentTypeId           CommitmentTypeIdentifier,
 *   commitmentTypeQualifier    SEQUENCE SIZE (1..MAX) OF CommitmentTypeQualifier OPTIONAL
 * }
 *
 * CommitmentTypeIdentifier ::= OBJECT IDENTIFIER
 *
 * CommitmentTypeQualifier ::= SEQUENCE {
 *   commitmentTypeIdentifier   CommitmentTypeIdentifier,
 *   qualifier                  ANY DEFINED BY commitmentTypeIdentifier
 * }
 * </pre>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class CommitmentTypeIndicationBean {

	private final String identifier;
	private final List<String> qualifiers;

	CommitmentTypeIndicationBean(final String id, final List<String> quals) {
		if (id == null) {
			throw new IllegalArgumentException(
				"El obligatorio proporcionar un identificador no nulo" //$NON-NLS-1$
			);
		}
		this.identifier = id;
		this.qualifiers = quals;
	}

	String getIdentifier() {
		return this.identifier;
	}

	List<String> getQualifiers() {
		if (this.qualifiers == null) {
			return new ArrayList<String>(0);
		}
		return this.qualifiers;
	}

}
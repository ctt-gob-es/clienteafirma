/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone.ui.preferences;

import es.gob.afirma.core.signers.AOSignConstants;

/**
 * Clase auxiliar para tratar las pol&iacute;ticas de firma de la AGE.
 */
public class AgePolicy {

	private static final String URN_OID_PREFIX = "urn:oid:"; //$NON-NLS-1$
	private static final String OID_AGE_1_9 = "2.16.724.1.3.1.1.2.1.9"; //$NON-NLS-1$
	private static final String OID_AGE_1_8 = "2.16.724.1.3.1.1.2.1.8"; //$NON-NLS-1$

	/**
	 * Indica si un identificador de pol&iacute;tica se corresponde con el de alguna
	 * pol&oacute;tica de la AGE.
	 * @param oid Identificador de pol&iacute;tica.
	 * @param format Nombre del formato de firma para el que se eval&uacute; la pol&iacute;tica.
	 * @return {@code true} si el identificador se corresponde con el de alguna
	 * pol&iacute;tica de la AGE, {@code false} en caso contrario.
	 */
	public static boolean isAGEPolicy(final String oid, final String format) {

		if (AOSignConstants.SIGN_FORMAT_CADES.equalsIgnoreCase(format) ||
				AOSignConstants.SIGN_FORMAT_XADES.equalsIgnoreCase(format)) {
			return OID_AGE_1_9.equals(oid) || (URN_OID_PREFIX + OID_AGE_1_9).equalsIgnoreCase(oid) ||
					OID_AGE_1_8.equals(oid) || (URN_OID_PREFIX + OID_AGE_1_8).equalsIgnoreCase(oid);
		}
		else if (AOSignConstants.SIGN_FORMAT_PADES.equalsIgnoreCase(format)) {
				return OID_AGE_1_9.equals(oid) || (URN_OID_PREFIX + OID_AGE_1_9).equalsIgnoreCase(oid);
		}
		return false;
	}

	/**
	 * Comprueba si el identificador proporcionado es el correspondiente al de la
	 * pol&iacute;tica de firma de la AGE versi&oacute;n 1.8.
	 * @param oid Identificador de pol&iacute;tica.
	 * @return {@code true} si el identificador es el de la  pol&iacute;tica de la AGE v1.8,
	 * {@code false} en caso contrario.
	 */
	public static boolean isAGEPolicy18(final String oid) {
		return OID_AGE_1_8.equals(oid) || (URN_OID_PREFIX + OID_AGE_1_8).equalsIgnoreCase(oid);
	}

	/**
	 * Comprueba si el identificador proporcionado es el correspondiente al de la
	 * pol&iacute;tica de firma de la AGE versi&oacute;n 1.9.
	 * @param oid Identificador de pol&iacute;tica.
	 * @return {@code true} si el identificador es el de la  pol&iacute;tica de la AGE v1.9,
	 * {@code false} en caso contrario.
	 */
	public static boolean isAGEPolicy19(final String oid) {
		return OID_AGE_1_9.equals(oid) || (URN_OID_PREFIX + OID_AGE_1_9).equalsIgnoreCase(oid);
	}
}

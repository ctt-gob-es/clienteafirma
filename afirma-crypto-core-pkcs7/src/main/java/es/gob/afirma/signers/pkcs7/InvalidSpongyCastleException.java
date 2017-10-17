/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pkcs7;

import es.gob.afirma.core.InvalidLibraryException;

/** Indica que hay un SpongyCastle inv&aacute;lido en el CLASSPATH o en el BOOTCLASSPATH, a menudo
 * porque se ha instalado el JAR inapropiadamente como extensi&oacute;n del JRE.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class InvalidSpongyCastleException extends InvalidLibraryException {

	private static final long serialVersionUID = -322997692480101275L;

	private final String exp;
	private final String fnd;

	/** Crea una instancia de la excepci&oacute;n.
	 * @param expected Versi&oacute;n esperada de SpongyCastle
	 * @param found Versi&oacute;n encontrada (actual) de SpongyCastle
	 * @param e Excepci&oacute;n original */
	public InvalidSpongyCastleException(final String expected, final String found, final Throwable e) {
		super("Se necesitaba SpongyCastle version " + expected + ", pero se encontro la version " + found, e); //$NON-NLS-1$ //$NON-NLS-2$
		this.exp = expected;
		this.fnd = found;
	}

	/** Obtiene la versi&oacute;n esperada de SpongyCastle.
	 * @return Versi&oacute;n esperada de SpongyCastle */
	public String getExpectedVersion() {
		return this.exp;
	}

	/** Obtiene la versi&oacute;n encontrada (actual) de SpongyCastle.
	 * @return Versi&oacute;n encontrada (actual) de SpongyCastle */
	public String getFoundVersion() {
		return this.fnd;
	}

}

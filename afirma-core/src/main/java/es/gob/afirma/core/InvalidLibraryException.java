/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.core;

/** Indica que hay una biblioteca inv&aacute;lida en el CLASSPATH o en el BOOTCLASSPATH, a menudo
 * poque se ha instalado un JAR inapropiadamente como extensi&oacute;n del JRE.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class InvalidLibraryException extends RuntimeException {
	private static final long serialVersionUID = -8418397871871426778L;
}

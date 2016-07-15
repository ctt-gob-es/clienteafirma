/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */
package es.gob.afirma.core.keystores;

/** Almac&eacute;n de claves que puede cerrarse autom&aacute;ticamente tras una cantidad determinada de tiempo y recuperarse
 * (reiniciando todos sus almacenes subyacentes y pidiendo de nuevo la contrase&ntilde;a si es preciso) cuando se vuelve a
 * necesitar su uso.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public interface AutoCloseable {

	/** Establece una cantidad de tiempo en el que el almac&eacute;n se cerrar&aacute;a.
	 * @param seconds Segundos transcurridos los cuales el almac&eacute;n se cerrar&aacute;a. */
	void closeIn(final int seconds);
}

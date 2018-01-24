/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.signers;

import java.io.IOException;
import java.util.Properties;

/** Interfaz que define los m&eacute;todos necesarios para la mejora de firmas (agregado de
 * sellos de tiempo, firma longeva,...). */
public interface SignEnhancer {

	/** Completa una firma electr&oacute;nica agreg&aacute;ndole opciones adicionales
	 * postfirma.
	 * @param signature Firma que se desea mejorar.
	 * @param options Opciones y configuraci&oacute;n necesarias para la mejora de la firma.
	 * @return Firma mejorada.
	 * @throws IOException Cuando ocurre algun error al generar la firma. */
	byte[] enhance(byte[] signature, Properties options) throws IOException;
}

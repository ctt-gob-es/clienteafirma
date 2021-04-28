/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.server.document;

import java.io.IOException;
import java.util.Properties;

/** Interfaz para la recuperaci&oacute;n de documentos desde un servidor o repositorio documental mediante cach&eacute.
 * <u>ES OBLIGATORIO</u>, que las clases que implementen esta interfaz dispongan de un constructor que reciba
 * &uacute;nicamente un objeto Properties (<code>java.util.Properties</code>). */
public interface DocumentCacheManager {

	/** Obtiene un documento de cach&eacute en base a su identificador.
	 * Si no es posible recuperar el fichero se debe lanzar una excepci&oacute;n. El mensaje se recibir&aacute;
	 * como parte del mensaje de error en el cliente de firma.
	 * @param id Identificador del documento
	 * @param config Par&aacute;metros para la configuraci&oacute;n de la recuperaci&oacute;n del documento.
	 * @throws IOException Cuando ocurre alg&uacute;n problema con la recuperaci&oacute;n. */
	byte[] getDocumentFromCache(String id, Properties config) throws IOException;

	/** Almacena un documento firmado en cach&eacute.
	 * Si no es posible almacenar el fichero se debe lanzar una excepci&oacute;n. El mensaje se recibir&aacute;
	 * como parte del mensaje de error en el cliente de firma.
	 * @param id Identificador del documento original no firmado.
	 * @param data Datos firmados.
	 * @param config Par&aacute;metros para la configuraci&oacute;n del guardado del documento.
	 * @return Identificador del nuevo documento codificado en base 64.
	 * @throws IOException Cuando ocurre alg&uacute;n problema con el guardado. */
	String storeDocumentToCache(String id, byte[] data, Properties config) throws IOException;

	/** Permite la limpieza de cach&eacute.
	 * @throws IOException Cuando ocurre alg&uacute;n problema en el borrado de archivos */
	void cleanCache() throws IOException;

}

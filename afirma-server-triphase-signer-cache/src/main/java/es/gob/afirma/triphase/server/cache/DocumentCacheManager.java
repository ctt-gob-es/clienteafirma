/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.server.cache;

import java.io.IOException;

/**
 * Interfaz para la gesti&oacute;n de cach&eacute; del servicio de firma trif&aacute;sica.
 */
public interface DocumentCacheManager {

	/**
	 * Obtiene un documento de cach&eacute; en base a su identificador y lo elimina.
	 * @param id Identificador del documento.
	 * @return Array con los datos del archivo cacheado o {@code null} si no se encontr&oacute;.
	 * @throws IOException Cuando ocurre alg&uacute;n problema con la recuperaci&oacute;n.
	 */
	byte[] getDocumentFromCache(String id) throws IOException;

	/**
	 * Almacena un documento en cach&eacute;.
	 * @param data Datos a guardar.
	 * @return Identificador con el que referenciar al documento guardado.
	 * @throws IOException Cuando ocurre alg&uacute;n problema con el guardado.
	 */
	String storeDocumentToCache(byte[] data) throws IOException;

	/**
	 * Permite la limpieza de cach&eacute;.
	 * @throws IOException Cuando ocurre alg&uacute;n problema en el borrado de archivos.
	 */
	void cleanCache() throws IOException;

}

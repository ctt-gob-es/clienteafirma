/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet.io;

/**
 * Clase que almacena el nombre y contenido de un fichero.
 */
public class FileBean {

	private final String path;

	private final byte[] content;

	/**
	 * Crea un objeto con ruta de fichero y contenido.
	 * @param path Ruta al fichero.
	 * @param content Contenido del fichero.
	 */
	public FileBean(final String path, final byte[] content) {
		this.path = path;
		this.content = content;
	}

	/**
	 * Recupera la ruta indicada del fichero.
	 * @return Ruta del fichero.
	 */
	public String getPath() {
		return this.path;
	}

	/**
	 * Recupera el contenido del fichero.
	 * @return Contenido del fichero.
	 */
	public byte[] getContent() {
		return this.content;
	}
}

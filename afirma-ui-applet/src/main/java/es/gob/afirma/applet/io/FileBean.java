/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet.io;

import java.io.File;

/**
 * Clase que almacena un fichero y su contenido.
 */
public class FileBean {

	private final File file;

	private final byte[] content;

	/**
	 * Crea un objeto un fichero y su contenido.
	 * @param file Fichero.
	 * @param content Contenido del fichero.
	 */
	public FileBean(final File file, final byte[] content) {
		this.file = file;
		this.content = content == null ? null : content.clone();
	}

	/**
	 * Recupera la ruta indicada del fichero.
	 * @return Ruta del fichero.
	 */
	public String getPath() {
		return this.file.getAbsolutePath();
	}

	/**
	 * Recupera el contenido del fichero.
	 * @return Contenido del fichero.
	 */
	public byte[] getContent() {
		return this.content;
	}
}

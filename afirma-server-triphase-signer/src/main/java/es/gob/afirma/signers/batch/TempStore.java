/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.batch;

import java.io.IOException;

import es.gob.afirma.signers.batch.xml.SingleSign;

public interface TempStore {

	void store(final byte[] data, final SingleSign ss, final String batchId) throws IOException;

	byte[] retrieve(final SingleSign ss, final String batchId) throws IOException;

	/** Borra una firma del almacenamiento temporal.
	 * Si falla el borrado debe gestionarse sin interrumpir el proceso (no deben
	 * lanzarse excepciones).
	 * @param ss Firma cuyos datos temporales hay que borrar.
	 * @param batchId Identificador del lote al que pertenece la firma. */
	void delete(final SingleSign ss, final String batchId);

	void store(byte[] dataToSave, String filename) throws IOException;

	byte[] retrieve(String filename) throws IOException;

	void delete(String filename);

}

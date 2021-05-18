/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.batchV2;

import java.io.IOException;

interface JSONTempStore {

	void store(final byte[] data, final JSONSingleSign ss, final String batchId) throws IOException;

	byte[] retrieve(final JSONSingleSign ss, final String batchId) throws IOException;

	/** Borra una firma del almacenamiento temporal.
	 * Si falla el borrado debe gestionarse sin interrumpir el proceso (no deben
	 * lanzarse excepciones).
	 * @param ss Firma cuyos datos temporales hay que borrar.
	 * @param batchId Identificador del lote al que pertenece la firma. */
	void delete(final JSONSingleSign ss, final String batchId);

	void store(byte[] dataToSave, String filename) throws IOException;

	byte[] retrieve(String filename) throws IOException;

	void delete(String filename);

}

package es.gob.afirma.signers.batch;

import java.io.IOException;

interface TempStore {

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

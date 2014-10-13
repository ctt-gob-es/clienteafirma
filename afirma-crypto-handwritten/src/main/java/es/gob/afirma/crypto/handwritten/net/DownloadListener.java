package es.gob.afirma.crypto.handwritten.net;

/** Clase a notificar el resultado de una descarga. */
public interface DownloadListener {

	/** Descarga finalizada.
	 * @param data Datos descargados. */
	void downloadComplete(byte[] data);

	/** Descarga abortada por error.
	 * @param t Error producido. */
	void downloadError(Throwable t);

}

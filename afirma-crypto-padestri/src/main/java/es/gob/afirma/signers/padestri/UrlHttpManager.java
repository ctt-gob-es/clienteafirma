package es.gob.afirma.signers.padestri.client;

import java.io.IOException;

/** Utilidades para el acceso a URL por HTTP o HTTPS.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public interface UrlHttpManager {

	/** Lee el contenido de una URL HTTP o HTTPS.
	 * @param url URL a abrir y leer
	 * @return Lectura completa de la URL
	 * @throws IOException Si ocurre alg&uacute;n problema en la conexi&oacute;n de red */
	byte[] readUrl(String url) throws IOException;

}

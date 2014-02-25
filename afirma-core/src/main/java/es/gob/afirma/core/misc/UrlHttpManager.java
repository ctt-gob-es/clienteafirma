package es.gob.afirma.core.misc;

import java.io.IOException;

/** Clase abstracta para la lectura y env&iacute;o de datos a URL remotas.
 * @author Carlos Gamuci */
public interface UrlHttpManager {

	/** Lee una URL HTTP o HTTPS por POST. Los par&aacute;metros se indican en la URL.
	 * @param url URL a leer
	 * @return Contenido de la URL
	 * @throws IOException Si no se puede leer la URL */
	byte[] readUrlByPost(final String url) throws IOException;

	/** Lee una URL HTTP o HTTPS por POST estableciendo un tiempo m&aacute;ximo para la comunicaci&oacute;n.
	 * Los par&aacute;metros se indican en la URL.
	 * @param url URL a leer
	 * @param timeout Timeout.
	 * @return Contenido de la URL
	 * @throws IOException Si no se puede leer la URL */
	byte[] readUrlByPost(final String url, final int timeout) throws IOException;

	/** Lee una URL HTTP o HTTPS por GET.
	 * @param url URL a leer.
	 * @return Contenido de la URL.
	 * @throws IOException Si no se puede leer la URL */
	byte[] readUrlByGet(final String url) throws IOException;
}

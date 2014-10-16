package es.gob.afirma.crypto.handwritten.net;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.UrlHttpManagerImpl;

/** Clase utilizada para descarga de documentos.
 * @author Astrid Idoate **/
public final class ProgressUrlHttpManagerImpl extends UrlHttpManagerImpl {

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Descarga el contenido de una URL de forma as&iacute;ncrona mediante HTTP/HTTPS GET.
	 * @param url URL a descargar.
	 * @param dl Clase a la que notificar el resultado de la descarga.
	 * @throws IOException Si hay problemas preparando la descarga, los errores durante la
	 *                     descarga en si se notifican al <i>listener</i>. */
	public static void readUrlByGetAsync(final String url, final DownloadListener dl) throws IOException {

		if (url == null) {
			throw new IllegalArgumentException("La URL de descarga no puede ser nula"); //$NON-NLS-1$
		}
		if (dl == null) {
			throw new IllegalArgumentException(
				"Es obligatorio indicar una clase a la que notificar el resulatdo de la descarga" //$NON-NLS-1$
			);
		}

		final URL uri = new URL(url);

		new Thread(
			new Runnable() {
				@Override
				public void run() {

					if (uri.getProtocol().equals("https")) { //$NON-NLS-1$
						try {
							disableSslChecks();
						}
						catch(final Exception e) {
							Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
								"No se ha podido ajustar la confianza SSL, es posible que no se pueda completar la conexion: " + e //$NON-NLS-1$
							);
						}
					}
					try{
						final InputStream is = uri.openStream();
						dl.downloadComplete(AOUtil.getDataFromInputStream(is));
						is.close();
					}
					catch(final Exception e) {
						LOGGER.severe("Error en la descarga del contenido: " + e); //$NON-NLS-1$
						dl.downloadError(e);
					}

					if (uri.getProtocol().equals("https")) { //$NON-NLS-1$
						enableSslChecks();
					}
				}
			}
		).start();

	}
}

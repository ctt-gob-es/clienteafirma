package es.gob.afirma.core.misc.http;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;

/** Case de utilidad para la descarga de datos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class DataDownloader {

	private DataDownloader() {
		// No instanciable
	}

	/** Descarga datos del origen indicado.
	 * @param dataSource Origen de los datos. Este puede ser, y se eval&uacute;a en este orden:
	 *                   <ol>
	 *                    <li>Una URL de tipo HTTP, HTTPS o FTP.</li>
	 *                    <li>Datos en formato Base64.</li>
	 *                    <li>Datos textuales (en este caso de devuelven ellos mismos como binario).</li>
	 *                   </ol>
	 * @return Datos obtenidos del origen indicado.
	 * @throws IOException Si no se pueden obtener los datos. */
	public static byte[] downloadData(final String dataSource) throws IOException {

		if (dataSource == null) {
			throw new IllegalArgumentException("La fuente de datos no puede ser nula"); //$NON-NLS-1$
		}

		// Miramos primero si los datos son una URL, en cuyo caso descargamos los datos
		if (dataSource.startsWith("http://") || dataSource.startsWith("https://")) { //$NON-NLS-1$ //$NON-NLS-2$
			return UrlHttpManagerFactory.getInstalledManager().readUrlByGet(dataSource);
		}
		if (dataSource.startsWith("ftp://")) { //$NON-NLS-1$
		 	final InputStream ftpStream = new URL(dataSource).openStream();
			final byte[] data = AOUtil.getDataFromInputStream(ftpStream);
			ftpStream.close();
			return data;
		}

		// No son URL, son los datos en si

		// Comprobamos que los datos se pueden tratar como base 64
		try {
			return Base64.decode(dataSource.replace("_", "/").replace("-", "+")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").warning("Los datos introducidos no se pueden tratar como base 64: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}

		return dataSource.getBytes();

	}

}

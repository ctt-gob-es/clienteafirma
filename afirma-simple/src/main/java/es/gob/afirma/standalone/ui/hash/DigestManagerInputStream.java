/*
* Este fichero forma parte de la plataforma de @firma.
* La plataforma de @firma es de libre distribución cuyo código fuente puede ser consultado
* y descargado desde http://administracionelectronica.gob.es
*
* Copyright 2005-2019 Gobierno de España
* Este fichero se distribuye bajo las licencias EUPL versión 1.1, según las
* condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este
* fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
*/

/**
 * <p>Fichero: DigestManagerInputStream.java</p>
 * <p>Descripción: </p>
 * <p>Empresa: Telvent Interactiva </p>
 * <p>Fecha creación: 23-may-2006</p>
 * @author SEJLHA
 * @version 1.0
 *
 */
package es.gob.afirma.standalone.ui.hash;

import java.io.IOException;
import java.io.InputStream;

/**
 * @author SEJLHA
 *
 */
public class DigestManagerInputStream extends InputStream {

	private final InputStream myInputStream;
	private final DigestManager myMessageDigest;

	public DigestManagerInputStream(final InputStream is, final DigestManager md) {
		super();
		this.myInputStream = is;
		this.myMessageDigest = md;
	}

	@Override
	public int read() throws IOException {
		int b;
		b = this.myInputStream.read();
		if (b != -1) {
			this.myMessageDigest.addDataToCompute((byte) b);
		}

		return b;
	}

	@Override
	public int read(final byte[ ] buff) throws IOException {
		int read;
		read = this.myInputStream.read(buff);
		if (read > 0) {
			this.myMessageDigest.addDataToCompute(buff, 0, read);
		}

		return read;
	}

	@Override
	public int read(final byte[ ] buff, final int offset, final int len) throws IOException {
		int read;
		read = this.myInputStream.read(buff, offset, len);
		if (read > 0) {
			this.myMessageDigest.addDataToCompute(buff, offset, read);
		}

		return read;
	}

	public void readOptimized(final int size) throws IOException {
		final byte[ ] buff = new byte[size];
		int buf = read(buff);
		while (buf > 0) {
			buf = read(buff);
		}
	}

	public byte[] digest() {
		return this.myMessageDigest.computeHash();
	}

	@Override
	public void close() throws IOException {
		this.myInputStream.close();
	}
}

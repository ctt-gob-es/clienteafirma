package es.gob.afirma.signers.batch;

import java.io.IOException;
import java.util.Properties;

import es.gob.afirma.signers.batch.xml.SingleSign;

/** Guardador que siempre da error al guardar.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class FaultySignSaver implements SignSaver {

	@Override
	public void saveSign(final SingleSign sign, final byte[] dataToSave) throws IOException {
		throw new IOException("provocada"); //$NON-NLS-1$
	}

	@Override
	public void init(final Properties config) {
		// Vacio
	}

	@Override
	public Properties getConfig() {
		return new Properties();
	}

	@Override
	public void rollback(final SingleSign sign) {
		// Vacio
	}

	@Override
	public boolean isInitialized() {
		return true;
	}
}

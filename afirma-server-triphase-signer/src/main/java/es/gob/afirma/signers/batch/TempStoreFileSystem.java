package es.gob.afirma.signers.batch;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.MessageDigest;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;

final class TempStoreFileSystem implements TempStore {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
	private static final MessageDigest MD;
	static {
		try {
			MD = MessageDigest.getInstance("SHA-1"); //$NON-NLS-1$
		}
		catch (final Exception e) {
			throw new IllegalStateException(
				"No se ha podido cargar el motor de huellas para SHA-1: " + e, e //$NON-NLS-1$
			);
		}
	}

	@Override
	public void store(final byte[] dataToSave, final SingleSign ss, final String batchId) throws IOException {
		final OutputStream fos = new FileOutputStream(
			new File(
				BatchConfigManager.getTempDir(),
				getFilename(ss, batchId)
			)
		);
		final BufferedOutputStream bos = new BufferedOutputStream(
			fos,
			dataToSave.length
		);
		bos.write(dataToSave);
		bos.flush();
		bos.close();
		LOGGER.info("Firma '" + ss.getId() + "' almacenada temporalmente en " + getFilename(ss, batchId)); //$NON-NLS-1$ //$NON-NLS-2$
	}

	@Override
	public byte[] retrieve(final SingleSign ss, final String batchId) throws IOException {
		final InputStream fis = new FileInputStream(
				new File(
						BatchConfigManager.getTempDir(),
						getFilename(ss, batchId)
						)
				);
		final InputStream bis = new BufferedInputStream(fis);
		final byte[] ret = AOUtil.getDataFromInputStream(bis);
		bis.close();
		fis.close();
		return ret;
	}

	@Override
	public void delete(final SingleSign ss, final String batchId) {
		final File f = new File(
				BatchConfigManager.getTempDir(),
				getFilename(ss, batchId)
				);
		if (f.exists()) {
			f.delete();
		}
	}

	private static String getFilename(final SingleSign ss, final String batchId) {
		return AOUtil.hexify(MD.digest(ss.getId().getBytes()), false) + "." + batchId; //$NON-NLS-1$
	}
}

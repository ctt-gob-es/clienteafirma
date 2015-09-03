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
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.triphase.server.SignatureService;

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

	private static final File TMPDIR;
	private static final String CONFIG_FILE = "signbatch.properties"; //$NON-NLS-1$

	static {
		final Properties config = new Properties();
		try {
			final InputStream configIs = SignatureService.class.getClassLoader().getResourceAsStream(CONFIG_FILE);
			if (configIs == null) {
				throw new RuntimeException("No se encuentra el fichero de configuracion del servicio: " + CONFIG_FILE); //$NON-NLS-1$
			}
			config.load(configIs);
			configIs.close();
		}
		catch(final Exception e) {
			LOGGER.severe(
				"No se ha podido cargar el fichero de configuracion (" + CONFIG_FILE + "), se usaran los valores por defecto: " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		final String defaultDir = System.getProperty("java.io.tmpdir"); //$NON-NLS-1$
		final File f = new File(config.getProperty("tmpdir", defaultDir)); //$NON-NLS-1$
		if (!f.isDirectory() || !f.canRead() || !f.canWrite()) {
			LOGGER.severe(
				"El directorio temporal configurado (" + f.getAbsolutePath() + ") no es valido, se usaran el por defecto: " + defaultDir //$NON-NLS-1$ //$NON-NLS-2$
			);
			TMPDIR = new File(defaultDir);
		}
		else {
			TMPDIR = f;
		}
	}


	@Override
	public void store(final byte[] dataToSave, final SingleSign ss, final String batchId) throws IOException {
		final OutputStream fos = new FileOutputStream(
			new File(
				TMPDIR,
				getFilename(ss, batchId)
			)
		);
		final BufferedOutputStream bos = new BufferedOutputStream(
			fos,
			dataToSave.length
		);
		bos.write(dataToSave);
		bos.flush();
		fos.close();
		LOGGER.info("Firma '" + ss.getId() + "' almacenada temporalmente en " + getFilename(ss, batchId)); //$NON-NLS-1$ //$NON-NLS-2$
	}

	@Override
	public byte[] retrieve(final SingleSign ss, final String batchId) throws IOException {
		final InputStream fis = new FileInputStream(
			new File(
				TMPDIR,
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
			TMPDIR,
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

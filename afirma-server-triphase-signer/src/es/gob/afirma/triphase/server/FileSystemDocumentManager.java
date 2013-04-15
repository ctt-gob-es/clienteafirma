package es.gob.afirma.triphase.server;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;

import com.sun.org.apache.xml.internal.security.utils.Base64;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;

final public class FileSystemDocumentManager implements DocumentManager {

	private static final String IN_DIR_PARAM = "indir"; //$NON-NLS-1$
	private static final String OUT_DIR_PARAM = "outdir"; //$NON-NLS-1$
	private static final String OVERWRITE_PARAM = "overwrite"; //$NON-NLS-1$

	private static final String FORMAT_PROPERTY = "format"; //$NON-NLS-1$

	final String inDir;
	final String outDir;
	final boolean overwrite;

	public FileSystemDocumentManager(final Properties config) {

		this.inDir = config.getProperty(IN_DIR_PARAM);
		this.outDir = config.getProperty(OUT_DIR_PARAM);
		this.overwrite = Boolean.parseBoolean(config.getProperty(OVERWRITE_PARAM));
	}

	@Override
	public byte[] getDocument(final String id, final Properties prop) throws IOException {

		final File file = new File(this.inDir, id);
		if (!file.exists() || !file.isFile() || !file.canRead()) {
			throw new IOException("No se puede cargar el documento"); //$NON-NLS-1$
		}

		final FileInputStream fis = new FileInputStream(file);
		final byte[] data = AOUtil.getDataFromInputStream(fis);
		fis.close();

		return data;
	}

	@Override
	public String storeDocument(final String id, final byte[] data, final Properties prop) throws IOException {

		final String initialId = id != null ? id : "signature"; //$NON-NLS-1$
		String newId = initialId;
		final int lastDotPos = initialId.lastIndexOf('.');
		if (lastDotPos != -1) {
			newId = initialId.substring(0,  lastDotPos) + "_signed"; //$NON-NLS-1$
		}

		final String format = prop.getProperty(FORMAT_PROPERTY);
		if (AOSignConstants.SIGN_FORMAT_CADES.equalsIgnoreCase(format)) {
			newId += ".csig";  //$NON-NLS-1$
		} else if (AOSignConstants.SIGN_FORMAT_XADES.equalsIgnoreCase(format)) {
			newId += ".xsig"; //$NON-NLS-1$
		} else if (lastDotPos < initialId.length() - 1) {
			newId += initialId.substring(lastDotPos + 1);
		}

		final File file = new File(this.outDir, newId);
		if (file.exists() && !this.overwrite) {
			throw new IOException("Se ha obtenido un nombre de documento existente en el sistema de ficheros."); //$NON-NLS-1$
		}

		final FileOutputStream fos = new FileOutputStream(file);
		fos.write(data);
		fos.close();

		return Base64.encode(newId.getBytes());
	}


}

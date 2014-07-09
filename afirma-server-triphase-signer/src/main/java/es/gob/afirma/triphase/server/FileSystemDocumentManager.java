package es.gob.afirma.triphase.server;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;

/** Implementaci&oacute;n de acceso a gestor documental usando simplemente el sistema de ficheros.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final public class FileSystemDocumentManager implements DocumentManager {

	private static final String IN_DIR_PARAM = "indir"; //$NON-NLS-1$
	private static final String OUT_DIR_PARAM = "outdir"; //$NON-NLS-1$
	private static final String OVERWRITE_PARAM = "overwrite"; //$NON-NLS-1$

	private static final String FORMAT_PROPERTY = "format"; //$NON-NLS-1$

	final String inDir;
	final String outDir;
	final boolean overwrite;

	final static Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Construye la clase de acceso a gestor documental usando sistema de ficheros.
	 * @param config Configuraci&oacute;n del gestor (directorios, etc.) */
	public FileSystemDocumentManager(final Properties config) {

		this.inDir = config.getProperty(IN_DIR_PARAM);
		this.outDir = config.getProperty(OUT_DIR_PARAM);
		this.overwrite = Boolean.parseBoolean(config.getProperty(OVERWRITE_PARAM));

		LOGGER.info("Directorio de entrada de ficheros: " + this.inDir); //$NON-NLS-1$
		LOGGER.info("Directorio de salida de ficheros: " + this.outDir); //$NON-NLS-1$
	}

	@Override
	public byte[] getDocument(final String id, final X509Certificate cert, final Properties prop) throws IOException {

		LOGGER.info("Recuperamos el documento con identificador: " + id); //$NON-NLS-1$

		final File file = new File(this.inDir, new String(Base64.decode(id)));

		LOGGER.info("Buscamos el fichero: " + file.getAbsolutePath()); //$NON-NLS-1$


		if (!file.exists() || !file.isFile() || !file.canRead()) {
			throw new IOException("No se puede cargar el documento"); //$NON-NLS-1$
		}

		try (final FileInputStream fis = new FileInputStream(file)) {
			return AOUtil.getDataFromInputStream(fis);
		}
	}

	@Override
	public String storeDocument(final String id, final X509Certificate cert, final byte[] data, final Properties prop) throws IOException {

		final String initialId = id != null ? new String(Base64.decode(id)) : "signature"; //$NON-NLS-1$
		String newId = initialId;
		final int lastDotPos = initialId.lastIndexOf('.');
		if (lastDotPos != -1) {
			newId = initialId.substring(0,  lastDotPos);
		}

		final String format = prop.getProperty(FORMAT_PROPERTY);
		if (AOSignConstants.SIGN_FORMAT_CADES.equalsIgnoreCase(format)) {
			newId += ".csig";  //$NON-NLS-1$
		} else if (AOSignConstants.SIGN_FORMAT_XADES.equalsIgnoreCase(format)) {
			newId += ".xsig"; //$NON-NLS-1$
		} else if (lastDotPos < initialId.length() - 1) {
			newId += initialId.substring(lastDotPos);
		}

		final File file = new File(this.outDir, newId);
		if (file.exists() && !this.overwrite) {
			throw new IOException("Se ha obtenido un nombre de documento existente en el sistema de ficheros."); //$NON-NLS-1$
		}

		try (final FileOutputStream fos = new FileOutputStream(file)) {
			fos.write(data);
		}
		LOGGER.info("Escribiendo el fichero: " + file.getAbsolutePath()); //$NON-NLS-1$
		return Base64.encode(newId.getBytes());
	}


}

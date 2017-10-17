/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.server.document;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;

/** Implementaci&oacute;n de acceso a gestor documental usando simplemente el sistema de ficheros.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class FileSystemDocumentManager implements DocumentManager {

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
	public byte[] getDocument(final String id, final X509Certificate[] certChain, final Properties prop) throws IOException {

		LOGGER.info("Recuperamos el documento con identificador: " + id); //$NON-NLS-1$

		final File file = new File(this.inDir, new String(Base64.decode(id)));

		if( !isParent(new File(this.inDir), file ) ) {
		    throw new IOException(
	    		"Se ha pedido un fichero fuera del directorio configurado: " + file.getAbsolutePath() //$NON-NLS-1$
    		);
		}

		LOGGER.info("Buscamos el fichero: " + file.getAbsolutePath()); //$NON-NLS-1$

		if (!file.exists()) {
			throw new IOException("No se puede cargar el documento, no existe"); //$NON-NLS-1$
		}

		if (!file.isFile()) {
			throw new IOException(
				"No se puede cargar el documento, el elmento existe, pero no es un fichero" //$NON-NLS-1$
			);
		}

		if (!file.canRead()) {
			throw new IOException(
				"No se puede cargar el documento, no se tienen permisos de lectura sobre el" //$NON-NLS-1$
			);
		}

		final byte[] data;
		InputStream fis = null;
		try {
			fis = new FileInputStream(file);
			data = AOUtil.getDataFromInputStream(fis);
			fis.close();
		}
		catch (final IOException e) {
			LOGGER.warning("Error en la lectura del fichero '" + file.getAbsolutePath() + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			if (fis != null) {
				try {
					fis.close();
				}
				catch (final IOException e2) {
					LOGGER.warning("El fichero queda sin cerrar: " + file.getAbsolutePath()); //$NON-NLS-1$
				}
			}
			throw e;
		}

		return data;
	}

	@Override
	public String storeDocument(final String id,
			                    final X509Certificate[] certChain,
			                    final byte[] data,
			                    final Properties prop) throws IOException {

		final String initialId = id != null ? new String(Base64.decode(id)) : "signature"; //$NON-NLS-1$
		String newId = initialId;
		final int lastDotPos = initialId.lastIndexOf('.');
		if (lastDotPos != -1) {
			newId = initialId.substring(0,  lastDotPos);
		}

		final String format = prop.getProperty(FORMAT_PROPERTY);
		if (format != null && AOSignConstants.SIGN_FORMAT_CADES.equalsIgnoreCase(format)) {
			newId += ".csig";  //$NON-NLS-1$
		}
		else if (format != null && format.toLowerCase().startsWith(AOSignConstants.SIGN_FORMAT_XADES.toLowerCase())) {
			newId += ".xsig"; //$NON-NLS-1$
		}
		else if (lastDotPos < initialId.length() - 1) {
			newId += initialId.substring(lastDotPos);
		}

		final File file = new File(this.outDir, newId);
		if (file.exists() && !this.overwrite) {
			throw new IOException("Se ha obtenido un nombre de documento existente en el sistema de ficheros."); //$NON-NLS-1$
		}

		FileOutputStream fos = null;
		try {
			fos = new FileOutputStream(file);
			fos.write(data);
			fos.close();
		}
		catch (final IOException e) {
			LOGGER.severe("Error al almacenar los datos en el fichero '" + file.getAbsolutePath() + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			if (fos != null) {
				try {
					fos.close();
				}
				catch (final IOException e2) {
					LOGGER.warning("El fichero queda sin cerrar: " + file.getAbsolutePath()); //$NON-NLS-1$
				}
			}
			throw e;
		}

		LOGGER.info("Escribiendo el fichero: " + file.getAbsolutePath()); //$NON-NLS-1$
		return Base64.encode(newId.getBytes());
	}

	private static boolean isParent(final File p, final File file) {
	    File f;
	    final File parent;
	    try {
	        parent = p.getCanonicalFile();
	        f = file.getCanonicalFile();
	    }
	    catch( final IOException e ) {
	        return false;
	    }

	    while( f != null ) {
	        if(parent.equals(f)) {
	            return true;
	        }
	        f = f.getParentFile();
	    }
	    return false;
	}

}

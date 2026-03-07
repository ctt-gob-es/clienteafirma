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
import java.nio.file.Files;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.triphase.server.ConfigManager;
import es.gob.afirma.triphase.server.FileSystemUtils;

/** Implementaci&oacute;n de acceso a gestor documental usando simplemente el sistema de ficheros.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class FileSystemDocumentManager implements BatchDocumentManager {

	private static final String CONFIG_PARAM_IN_DIR = "docmanager.filesystem.indir"; //$NON-NLS-1$
	private static final String CONFIG_PARAM_IN_DIR_LEGACY = "indir"; //$NON-NLS-1$
	private static final String CONFIG_PARAM_OUT_DIR = "docmanager.filesystem.outdir"; //$NON-NLS-1$
	private static final String CONFIG_PARAM_OUT_DIR_LEGACY = "outdir"; //$NON-NLS-1$
	private static final String CONFIG_PARAM_OVERWRITE = "docmanager.filesystem.overwrite"; //$NON-NLS-1$
	private static final String CONFIG_PARAM_OVERWRITE_LEGACY = "overwrite"; //$NON-NLS-1$
	private static final String CONFIG_PARAM_MAXDOCSIZE = "docmanager.filesystem.maxDocSize"; //$NON-NLS-1$

	/** Tama&ntilde;o m&aacute;ximo de documento permitido. Cero (0) indica sin l&iacute;mite. */
	private static final long DEFAULT_MAXDOCSIZE = 0;

	private static final int MAX_REF_LENGTH = 64;

	private static final String PROPERTY_FORMAT = "format"; //$NON-NLS-1$

	private final static Logger LOGGER = Logger.getLogger(ConfigManager.LOGGER_NAME);

	File inDir;
	File outDir;
	boolean overwrite;
	long maxDocSize;


	@Override
	public void init(final Properties config) {
		final String inDirPath = config.getProperty(CONFIG_PARAM_IN_DIR, config.getProperty(CONFIG_PARAM_IN_DIR_LEGACY));
		try {
			this.inDir = new File(inDirPath).getCanonicalFile();
		} catch (final IOException e) {
			LOGGER.log(Level.WARNING, "No se ha podido normalizar la ruta del directorio de entrada de documentos", e); //$NON-NLS-1$
		}
		final String outDirPath = config.getProperty(CONFIG_PARAM_OUT_DIR, config.getProperty(CONFIG_PARAM_OUT_DIR_LEGACY));
		try {
			this.outDir = new File(outDirPath).getCanonicalFile();
		} catch (final IOException e) {
			LOGGER.log(Level.WARNING, "No se ha podido normalizar la ruta del directorio de salida de documentos", e); //$NON-NLS-1$
		}

		this.overwrite = Boolean.parseBoolean(config.getProperty(CONFIG_PARAM_OVERWRITE, config.getProperty(CONFIG_PARAM_OVERWRITE_LEGACY)));

		if (config.containsKey(CONFIG_PARAM_MAXDOCSIZE)) {
			try {
			this.maxDocSize = Long.parseLong(config.getProperty(CONFIG_PARAM_MAXDOCSIZE));
			}
			catch (final Exception e) {
				this.maxDocSize = DEFAULT_MAXDOCSIZE;
			}
		} else {
			this.maxDocSize = DEFAULT_MAXDOCSIZE;
		}

		LOGGER.info("Directorio de entrada de ficheros: " + this.inDir); //$NON-NLS-1$
		LOGGER.info("Directorio de salida de ficheros: " + this.outDir); //$NON-NLS-1$
	}


	@Override
	public byte[] getDocument(final String dataRef, final X509Certificate[] certChain, final Properties prop) throws IOException, SecurityException {

		if (dataRef.length() > MAX_REF_LENGTH) {
			throw new IOException("El nombre Base 64 del fichero excede el tamano prefijado"); //$NON-NLS-1$
		}

		LOGGER.info("Recuperamos el documento con referencia: " + dataRef); //$NON-NLS-1$

		final File file;
		try {
			file = FileSystemUtils.composeTargetFile(this.inDir, new String(Base64.decode(dataRef)));
		}
		catch (final SecurityException e) {
		    throw new IOException("Se ha pedido leer un fichero fuera del directorio configurado", e); //$NON-NLS-1$
		}
		catch (final Exception e) {
		    throw new IOException("No se ha podido componer la ruta del fichero", e); //$NON-NLS-1$
		}

		LOGGER.info("Buscamos el fichero: " + LoggerUtil.getTrimStr(file.getAbsolutePath())); //$NON-NLS-1$

		if (!file.exists()) {
			throw new IOException("No se puede cargar el documento, no existe"); //$NON-NLS-1$
		}

		if (!file.isFile() || Files.isSymbolicLink(file.toPath())) {
			throw new IOException(
				"No se ha encontrado un documento valido" //$NON-NLS-1$
			);
		}

		if (!file.canRead()) {
			throw new IOException(
				"No se puede cargar el documento, no se tienen permisos de lectura sobre el" //$NON-NLS-1$
			);
		}

		if (Files.isSymbolicLink(file.toPath())) {
			throw new IOException(
				"No se admite la carga de datos a traves de enlaces simbolicos" //$NON-NLS-1$
			);
		}

		if (this.maxDocSize > 0 && file.length() > this.maxDocSize) {
			throw new SecurityException(
					"El tamano del documento es superior al permitido. Tamano del documento: " + file.length()); //$NON-NLS-1$
		}

		final byte[] data;
		try (
			final InputStream fis = new FileInputStream(file);
		) {
			data = AOUtil.getDataFromInputStream(fis);
			fis.close();
		}
		catch (final IOException e) {
			LOGGER.warning("Error en la lectura del fichero '" + file.getAbsolutePath() + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$
			throw e;
		}

		return data;
	}

	@Override
	public String storeDocument(final String dataRef,
			                    final X509Certificate[] certChain,
			                    final byte[] data,
			                    final Properties prop) throws IOException {

		if (dataRef.length() > MAX_REF_LENGTH) {
			throw new IOException("El nombre Base 64 del fichero excede el tamano prefijado"); //$NON-NLS-1$
		}

		final String signatureFilename = buildOutputFilename(dataRef, prop);

		final File file;
		try {
			file = FileSystemUtils.composeTargetFile(this.outDir, signatureFilename);
		}
		catch (final SecurityException e) {
		    throw new IOException("Se ha pedido guardar un fichero fuera del directorio configurado", e); //$NON-NLS-1$
		}
		catch (final Exception e) {
		    throw new IOException("No se ha podido componer la ruta del fichero", e); //$NON-NLS-1$
		}

		if (file.exists() && !this.overwrite) {
			throw new IOException("Ya existe un fichero con el nombre de salida indicado"); //$NON-NLS-1$
		}

		LOGGER.info("Escribiendo el fichero: " + LoggerUtil.getTrimStr(file.getAbsolutePath())); //$NON-NLS-1$

		try (
			final FileOutputStream fos = new FileOutputStream(file);
		) {
			fos.write(data);
			fos.close();
		}
		catch (final IOException e) {
			throw new IOException("Error al almacenar los datos en el fichero", e); //$NON-NLS-1$
		}

		return Base64.encode(file.getName().getBytes());
	}

	@Override
	public void rollback(final String id,
            final X509Certificate[] certChain,
            final Properties prop) throws IOException {

		final String filename = buildOutputFilename(id, prop);
		final File file;
		try {
			file = FileSystemUtils.composeTargetFile(this.inDir, filename);
		}
		catch (final SecurityException e) {
		    throw new IOException("Se ha pedido guardar un fichero fuera del directorio configurado", e); //$NON-NLS-1$
		}
		catch (final Exception e) {
		    throw new IOException("No se ha podido componer la ruta del fichero", e); //$NON-NLS-1$
		}

		if (file.isFile()) {
			LOGGER.info("Borrando el fichero: " + file.getAbsolutePath()); //$NON-NLS-1$
			file.delete();
		}
	}

	/**
	 * Compone el nombre y la ruta del fichero donde debe encontrarse la firma de un documento dato.
	 * @param id Identificador del documento.
	 * @param prop Configuraci&oacute;n de firma.
	 * @return Fichero de salida.
	 * @throws IOException Cuando falla la composici&oacute;n del fichero.
	 */
	private static String buildOutputFilename(final String id, final Properties prop) throws IOException {

		final String initialId = id != null ? new String(Base64.decode(id)) : "signature"; //$NON-NLS-1$
		String newId = initialId;
		final int lastDotPos = initialId.lastIndexOf('.');
		if (lastDotPos != -1) {
			newId = initialId.substring(0,  lastDotPos);
		}

		final String format = prop.getProperty(PROPERTY_FORMAT);
		if (AOSignConstants.SIGN_FORMAT_CADES.equalsIgnoreCase(format)) {
			newId += ".csig";  //$NON-NLS-1$
		}
		else if (format != null && format.toLowerCase().startsWith(AOSignConstants.SIGN_FORMAT_XADES.toLowerCase())) {
			newId += ".xsig"; //$NON-NLS-1$
		}
		else if (format != null
				&& (format.toLowerCase().startsWith(AOSignConstants.SIGN_FORMAT_CADES_ASIC_S.toLowerCase())
						|| format.toLowerCase().startsWith(AOSignConstants.SIGN_FORMAT_XADES_ASIC_S.toLowerCase()))) {
			newId += ".asics"; //$NON-NLS-1$
		}
		else if (lastDotPos < initialId.length() - 1) {
			newId += initialId.substring(lastDotPos);
		}

		return newId;
	}
}

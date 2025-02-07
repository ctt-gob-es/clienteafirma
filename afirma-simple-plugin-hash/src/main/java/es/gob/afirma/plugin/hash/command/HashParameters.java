/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.plugin.hash.command;

import java.io.File;

import es.gob.afirma.plugin.hash.Messages;

/** Clase que engloba todos los par&aacute;metros admitidos por
 * l&iacute;nea de comandos. */
final class HashParameters {

	public static final String PARAM_HELP		  = "-help"; //$NON-NLS-1$

	private static final String PARAM_INPUT       = "-i"; //$NON-NLS-1$
	private static final String PARAM_OUTPUT      = "-o"; //$NON-NLS-1$
	private static final String PARAM_XML         = "-xml"; //$NON-NLS-1$
	private static final String PARAM_GUI         = "-gui"; //$NON-NLS-1$
	private static final String PARAM_HASH_FORMAT = "-hformat"; //$NON-NLS-1$
	private static final String PARAM_HASH_ALGO   = "-halgorithm"; //$NON-NLS-1$
	private static final String PARAM_RECURSIVE   = "-r"; //$NON-NLS-1$

	// Parametro oculto para indicar que se desea validar un directorio
	private static final String PARAM_DIRECTORY   = "-d"; //$NON-NLS-1$

	public static final String FORMAT_HASH_FILE_HEX     = "hex"; //$NON-NLS-1$
	public static final String FORMAT_HASH_FILE_BASE64  = "b64"; //$NON-NLS-1$
	public static final String FORMAT_HASH_FILE_BIN     = "bin"; //$NON-NLS-1$
	public static final String FORMAT_HASH_DIR_PLAIN    = "txt"; //$NON-NLS-1$
	public static final String FORMAT_HASH_DIR_XML      = "xml"; //$NON-NLS-1$
	public static final String DEFAULT_FORMAT_FILE_HASH = FORMAT_HASH_FILE_HEX;
	public static final String DEFAULT_FORMAT_DIR_HASH  = FORMAT_HASH_DIR_XML;


	private static final String DEFAULT_HASH_ALGORITHM = "SHA-256"; //$NON-NLS-1$

	private File mainFile = null;
	private File inputFile = null;
	private File outputFile = null;
	private String hashFormat = null;
	private String hashAlgorithm = null;
	private boolean xml = false;
	private boolean gui = false;
	private boolean recursive = false;
	private boolean directory = false;

	public HashParameters(final HashCommands command, final String[] params)
			throws IllegalArgumentException {

		// El comando exige que se haya indicado un fichero principal de entrada
		int i = 0;
		if (command.isMainFileNeeded()) {
			if (params.length < 1) {
				throw new IllegalArgumentException(Messages.getString("CommandLine.120")); //$NON-NLS-1$
			}

			this.mainFile = readMainFile(params[i]);
			this.mainFile = new File(params[i]);

			if (!this.mainFile.exists()) {
				throw new IllegalArgumentException(Messages.getString("CommandLine.121", params[i])); //$NON-NLS-1$
			}
			if (!this.mainFile.canRead()) {
				throw new IllegalArgumentException(Messages.getString("CommandLine.122", params[i])); //$NON-NLS-1$
			}
			i++;
		}

		// Parseamos el resto de parametros
		for (; i < params.length; i++) {

			if (PARAM_XML.equals(params[i])) {
				this.xml = true;
			}
			else if (PARAM_GUI.equals(params[i])) {
				this.gui = true;
			}
			else if (PARAM_RECURSIVE.equals(params[i])) {
				this.recursive = true;
			}
			else if (PARAM_DIRECTORY.equals(params[i])) {
				this.directory = true;
			}
			else if (PARAM_HASH_ALGO.equals(params[i])) {
				if (this.hashAlgorithm != null) {
					throw new IllegalArgumentException(Messages.getString("CommandLine.26", params[i])); //$NON-NLS-1$
				}
				this.hashAlgorithm = params[i+1];
				i++;
			}
			else if (PARAM_INPUT.equals(params[i])) {

				if (this.inputFile != null) {
					throw new IllegalArgumentException(Messages.getString("CommandLine.26", params[i])); //$NON-NLS-1$
				}

				if (i >= params.length - 1) {
					throw new IllegalArgumentException(Messages.getString("CommandLine.74")); //$NON-NLS-1$
				}

				this.inputFile = new File(params[i + 1]);

				if (!this.inputFile.exists()) {
					throw new IllegalArgumentException(Messages.getString("CommandLine.0", params[i + 1])); //$NON-NLS-1$
				}
				if (!this.inputFile.canRead()) {
					throw new IllegalArgumentException(Messages.getString("CommandLine.1", params[i + 1])); //$NON-NLS-1$
				}
				i++;
			}
			else if (PARAM_HASH_FORMAT.equals(params[i])) {
				if (this.hashFormat != null) {
					throw new IllegalArgumentException(Messages.getString("CommandLine.26", params[i])); //$NON-NLS-1$
				}

				this.hashFormat = params[i+1].toLowerCase();
				if (!this.hashFormat.equals(FORMAT_HASH_FILE_HEX) &&
						!this.hashFormat.equals(FORMAT_HASH_FILE_BIN) &&
						!this.hashFormat.equals(FORMAT_HASH_FILE_BASE64) &&
						!this.hashFormat.equals(FORMAT_HASH_DIR_XML) &&
						!this.hashFormat.equals(FORMAT_HASH_DIR_PLAIN)) {
					throw new IllegalArgumentException(Messages.getString("CommandLine.89", params[i + 1])); //$NON-NLS-1$
				}
				i++;
			}
			else if (PARAM_OUTPUT.equals(params[i])) {
				if (this.outputFile != null) {
					throw new IllegalArgumentException(Messages.getString("CommandLine.26", params[i])); //$NON-NLS-1$
				}

				if (i >= params.length - 1) {
					throw new IllegalArgumentException(Messages.getString("CommandLine.75")); //$NON-NLS-1$
				}

				this.outputFile = new File(params[i + 1]);
				final String parent = this.outputFile.getParent();
				if (parent != null && !new File(parent).canWrite()) {
					throw new IllegalArgumentException(Messages.getString("CommandLine.3", params[i + 1])); //$NON-NLS-1$
				}
				i++;
			}
			// Suponemos que el fichero principal era opcional y se ha indicado
			else if (i == 0) {
				this.mainFile = readMainFile(params[i]);
			}
			else {
				throw new IllegalArgumentException(Messages.getString("CommandLine.25", params[i])); //$NON-NLS-1$
			}
		}
	}

	private static File readMainFile(final String path) {
		final File file = new File(path);
		if (!file.exists()) {
			throw new IllegalArgumentException(Messages.getString("CommandLine.121", path)); //$NON-NLS-1$
		}
		if (!file.canRead()) {
			throw new IllegalArgumentException(Messages.getString("CommandLine.122", path)); //$NON-NLS-1$
		}
		return file;
	}

	/**
	 * Recupera el fichero de entrada establecido inmediatemente a continuaci&oacute;n del comando.
	 * @return Fichero de entrada o {@code null} si no se ha establecido.
	 */
	public File getMainFile() {
		return this.mainFile;
	}

	/**
	 * Recupera el fichero de entrada configurado.
	 * @return Fichero de entrada o {@code null} si no se ha establecido.
	 */
	public File getInputFile() {
		return this.inputFile;
	}

	/**
	 * Recupera el fichero de salida configurado.
	 * @return Fichero en el que almacenar la firma o {@code null} si no se ha establecido.
	 */
	public File getOutputFile() {
		return this.outputFile;
	}

	/** Recupera el formato de salida de la huella digital configurado. Si no se ha indicado,
	 * devuelve {@code null}.
	 * @return Formato de huella digital configurado por par&aacute;metro o {@code null}. */
	public String getHashFormat() {
		return this.hashFormat;
	}

	/**
	 * Recupera el formato de salida de huella digital cuando la entrada es un directorio.
	 * @return Formato del documento con las huellas digitales del directorio.
	 * @throws IllegalArgumentException Cuando el formato de salida indicado no era v&aacute;lido.
	 */
	String getHashDirectoryFormat() throws IllegalArgumentException {
		String formatResult;
		if (this.hashFormat == null) {
			formatResult = DEFAULT_FORMAT_DIR_HASH;
		}
		else if (this.hashFormat.equals(FORMAT_HASH_DIR_XML) ||
				this.hashFormat.equals(FORMAT_HASH_DIR_PLAIN)) {
			formatResult = this.hashFormat;
		}
		else {
			throw new IllegalArgumentException(Messages.getString("CommandLine.90", this.hashFormat)); //$NON-NLS-1$
		}
		return formatResult;
	}

	/**
	 * Recupera el formato de salida de huella digital cuando la entrada es un fichero.
	 * @return Formato del documento con la huella digital del fichero.
	 * @throws IllegalArgumentException Cuando el formato de salida indicado no era v&aacute;lido.
	 */
	String getHashFileFormat() throws IllegalArgumentException {
		String formatResult;
		if (this.hashFormat == null) {
			formatResult = DEFAULT_FORMAT_FILE_HASH;
		}
		else if (this.hashFormat.equals(FORMAT_HASH_FILE_HEX) ||
				this.hashFormat.equals(FORMAT_HASH_FILE_BASE64) ||
				this.hashFormat.equals(FORMAT_HASH_FILE_BIN)) {
			formatResult = this.hashFormat;
		}
		else {
			throw new IllegalArgumentException(Messages.getString("CommandLine.91", this.hashFormat)); //$NON-NLS-1$
		}
		return formatResult;
	}

	/** Recupera el algoritmo de huella digital configurado o, si no se ha indicado, el
	 * algoritmo por defecto.
	 * @return Algoritmo de huella digital. */
	public String getHashAlgorithm() {
		return this.hashAlgorithm != null ? this.hashAlgorithm : DEFAULT_HASH_ALGORITHM;
	}

	/**
	 * Indica si se ha solicitado que la salida sea XML.
	 * @return {@code true} si se ha solicitado que la salida sea XML,
	 * {@code false} en caso contrario.
	 */
	public boolean isXml() {
		return this.xml;
	}

	/**
	 * Indica si se ha solicitado mostrar la interfaz gr&aacute;fica.
	 * @return {@code true} si se ha solicitado mostrar la interfaz gr&aacute;fica,
	 * {@code false} en caso contrario.
	 */
	public boolean isGui() {
		return this.gui;
	}

	/**
	 * Indica si se ha solicitado procesar los ficheros de los subdirectorios.
	 * @return {@code true} si se ha solicitado procesar los ficheros de los subdirectorios,
	 * {@code false} en caso contrario.
	 */
	public boolean isRecursive() {
		return this.recursive;
	}

	/**
	 * Indica si se ha se&ntilde;alado que se quiere procesar un directorio.
	 * @return {@code true} si se ha se&ntilde;alado que se quiere procesar un directorio,
	 * {@code false} en caso contrario.
	 */
	public boolean isDirectory() {
		return this.directory;
	}

	/**
	 * Construye la respuesta de s&iacute;ntaxis incorrecta.
	 * @param cmds Comandos introducidos.
	 * @param errorMessage Mensaje con el error detectado.
	 * @return Mensaje completo con la s&iacute;ntaxis.
	 */
	public static String buildSyntaxError(final HashCommands cmds, final String errorMessage) {
		switch (cmds) {
			case CHECKHASH:
				return buildOperationCheckHashSyntaxError(cmds.getOp(), errorMessage);
			case CREATEHASH:
				return buildOperationCreateHashSyntaxError(cmds.getOp(), errorMessage);
			default:
				return errorMessage;
		}
	}

	/** Construye la cadena de texto que explica la sintaxis para el uso del comando de
	 * verificaci&oacute;n de huellas digitales de ficheros.
	 * @param op Comando.
	 * @param errorMessage Mensaje que explica el error cometido.
	 * @return Texto con el error de sintaxis y la explicaci&oacute;n de la sintaxis correcta. */
	private static String buildOperationCheckHashSyntaxError(final String op, final String errorMessage) {
		final StringBuilder sb = new StringBuilder();
		if (errorMessage != null) {
			sb.append(errorMessage).append("\n"); //$NON-NLS-1$
		}
		// Sintaxis
		sb.append(Messages.getString("CommandLine.7")).append(":\n") //$NON-NLS-1$ //$NON-NLS-2$
		.append("  Autofirma ").append(op).append(" [FICHERO] [opciones...]\t\t- ").append(Messages.getString("CommandLine.108")).append("\n")  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  Autofirma ").append(op).append(" [DIRECTORIO] [opciones...]\t- ").append(Messages.getString("CommandLine.109")).append("\n\n")  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		// Parametros
		.append(Messages.getString("CommandLine.114")).append(":\n") //$NON-NLS-1$ //$NON-NLS-2$
		.append("  ").append("FICHERO").append("\t\t(").append(Messages.getString("CommandLine.106")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
		.append("  ").append("DIRECTORIO").append("\t\t(").append(Messages.getString("CommandLine.107")).append(")\n\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
		// Opciones de fichero
		.append(Messages.getString("CommandLine.116")).append(":\n") //$NON-NLS-1$ //$NON-NLS-2$
		.append("  ").append(PARAM_GUI).append("\t\t\t(").append(Messages.getString("CommandLine.100")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_INPUT).append(" FICHERO\t\t(").append(Messages.getString("CommandLine.67")).append(")\n\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		// Opciones de directorio
		.append(Messages.getString("CommandLine.117")).append(":\n") //$NON-NLS-1$ //$NON-NLS-2$
		.append("  ").append(PARAM_GUI).append("\t\t\t(").append(Messages.getString("CommandLine.100")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_INPUT).append(" FICHERO\t\t(").append(Messages.getString("CommandLine.105")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_OUTPUT).append(" FICHERO\t\t(").append(Messages.getString("CommandLine.97")).append(")\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

		return sb.toString();
	}

	/** Construye la cadena de texto que explica la sintaxis para el uso del comando de
	 * generacion de huellas digitales de un fichero o de los ficheros de un directorio.
	 * @param op Comando.
	 * @param errorMessage Mensaje que explica el error cometido.
	 * @return Texto con el error de sintaxis y la explicaci&oacute;n de la sintaxis correcta. */
	private static String buildOperationCreateHashSyntaxError(final String op, final String errorMessage) {
		final StringBuilder sb = new StringBuilder();
		if (errorMessage != null) {
			sb.append(errorMessage).append("\n"); //$NON-NLS-1$
		}
		// Sintaxis
		sb.append(Messages.getString("CommandLine.7")).append(":\n") //$NON-NLS-1$ //$NON-NLS-2$
		.append("  Autofirma ").append(op).append(" FICHERO [opciones...]\t\t- ").append(Messages.getString("CommandLine.110")).append("\n")  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  Autofirma ").append(op).append(" DIRECTORIO [opciones...]\t- ").append(Messages.getString("CommandLine.111")).append("\n\n")  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		// Parametros
		.append(Messages.getString("CommandLine.114")).append(":\n") //$NON-NLS-1$ //$NON-NLS-2$
		.append("  ").append("FICHERO").append("\t\t(").append(Messages.getString("CommandLine.112")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
		.append("  ").append("DIRECTORIO").append("\t\t(").append(Messages.getString("CommandLine.113")).append(")\n\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
		// Opciones de fichero
		.append(Messages.getString("CommandLine.116")).append(":\n") //$NON-NLS-1$ //$NON-NLS-2$
		.append("  ").append(PARAM_GUI).append("\t\t\t (").append(Messages.getString("CommandLine.95")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_OUTPUT).append(" FICHERO\t\t (").append(Messages.getString("CommandLine.118")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_HASH_ALGO).append(" ALGORITMO\t (").append(Messages.getString("CommandLine.77")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_HASH_FORMAT).append(" FORMATO\t (").append(Messages.getString("CommandLine.78")).append(")\n")  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  \t ").append(FORMAT_HASH_FILE_HEX).append("\t\t (").append(Messages.getString("CommandLine.81")).append(") (").append(Messages.getString("CommandLine.79")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
		.append("  \t ").append(FORMAT_HASH_FILE_BASE64).append("\t\t (").append(Messages.getString("CommandLine.82")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  \t ").append(FORMAT_HASH_FILE_BIN).append("\t\t (").append(Messages.getString("CommandLine.83")).append(")\n\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		// Opciones de directorio
		.append(Messages.getString("CommandLine.117")).append(":\n") //$NON-NLS-1$ //$NON-NLS-2$
		.append("  ").append(PARAM_GUI).append("\t\t\t (").append(Messages.getString("CommandLine.95")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_OUTPUT).append(" FICHERO\t\t (").append(Messages.getString("CommandLine.96")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_HASH_ALGO).append(" ALGORITMO\t (").append(Messages.getString("CommandLine.77")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_HASH_FORMAT).append(" FORMATO\t (").append(Messages.getString("CommandLine.78")).append(")\n")  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  \t ").append(FORMAT_HASH_DIR_XML).append("\t\t (").append(Messages.getString("CommandLine.85")).append(") (").append(Messages.getString("CommandLine.79")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
		.append("  \t ").append(FORMAT_HASH_DIR_PLAIN).append("\t\t (").append(Messages.getString("CommandLine.86")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_RECURSIVE).append("\t\t (").append(Messages.getString("CommandLine.92")).append(")\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

		return sb.toString();
	}
}
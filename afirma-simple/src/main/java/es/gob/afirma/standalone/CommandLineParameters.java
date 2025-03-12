/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import es.gob.afirma.core.misc.Platform;

/** Clase que engloba todos los par&aacute;metros admitidos por
 * l&iacute;nea de comandos. */
final class CommandLineParameters {

	private static final String PARAM_INPUT       = "-i"; //$NON-NLS-1$
	private static final String PARAM_OUTPUT      = "-o"; //$NON-NLS-1$
	private static final String PARAM_ALIAS       = "-alias"; //$NON-NLS-1$
	private static final String PARAM_FILTER      = "-filter"; //$NON-NLS-1$
	private static final String PARAM_STORE       = "-store"; //$NON-NLS-1$
	private static final String PARAM_FORMAT      = "-format"; //$NON-NLS-1$
	private static final String PARAM_PASSWD      = "-password"; //$NON-NLS-1$
	private static final String PARAM_ALGO        = "-algorithm"; //$NON-NLS-1$
	private static final String PARAM_CONFIG      = "-config"; //$NON-NLS-1$
	private static final String PARAM_OP	      = "-operation"; //$NON-NLS-1$
	private static final String PARAM_GUI         = "-gui"; //$NON-NLS-1$
	private static final String PARAM_CERT_GUI    = "-certgui"; //$NON-NLS-1$
	private static final String PARAM_PREURL      = "-preurl"; //$NON-NLS-1$
	private static final String PARAM_POSTURL     = "-posturl"; //$NON-NLS-1$
	private static final String PARAM_HASH_FORMAT = "-hformat"; //$NON-NLS-1$
	private static final String PARAM_HASH_ALGO   = "-halgorithm"; //$NON-NLS-1$
	private static final String PARAM_RECURSIVE   = "-r"; //$NON-NLS-1$

	public static final String PARAM_XML         = "-xml"; //$NON-NLS-1$

	public static final String FORMAT_AUTO     = "auto"; //$NON-NLS-1$
	public static final String FORMAT_XADES    = "xades"; //$NON-NLS-1$
	public static final String FORMAT_PADES    = "pades"; //$NON-NLS-1$
	public static final String FORMAT_CADES    = "cades"; //$NON-NLS-1$
	public static final String FORMAT_FACTURAE = "facturae"; //$NON-NLS-1$
	public static final String FORMAT_OOXML    = "ooxml"; //$NON-NLS-1$
	public static final String FORMAT_ODF      = "odf"; //$NON-NLS-1$
	public static final String DEFAULT_FORMAT  = FORMAT_AUTO;

	public static final String FORMAT_HASH_FILE_HEX     = "hex"; //$NON-NLS-1$
	public static final String FORMAT_HASH_FILE_BASE64  = "b64"; //$NON-NLS-1$
	public static final String FORMAT_HASH_FILE_BIN     = "bin"; //$NON-NLS-1$
	public static final String FORMAT_HASH_DIR_PLAIN    = "txt"; //$NON-NLS-1$
	public static final String FORMAT_HASH_DIR_XML      = "xml"; //$NON-NLS-1$
	public static final String DEFAULT_FORMAT_FILE_HASH = FORMAT_HASH_FILE_HEX;
	public static final String DEFAULT_FORMAT_DIR_HASH  = FORMAT_HASH_DIR_XML;

	public static final String MASSIVE_OP_SIGN			= "sign"; //$NON-NLS-1$
	public static final String MASSIVE_OP_COSIGN		= "cosign"; //$NON-NLS-1$
	public static final String MASSIVE_OP_COUNTERSIGN	= "countersign"; //$NON-NLS-1$
	private static final String DEFAULT_MASSIVE_OP      = MASSIVE_OP_SIGN;



	public static final String ALGO_SHA1		= "sha1"; //$NON-NLS-1$
	public static final String ALGO_SHA256		= "sha256"; //$NON-NLS-1$
	public static final String ALGO_SHA384		= "sha384"; //$NON-NLS-1$
	public static final String ALGO_SHA512		= "sha512"; //$NON-NLS-1$
	private static final String DEFAULT_ALGO	= ALGO_SHA512;

	private String store = null;
	private String alias = null;
	private String filter = null;
	private File mainFile = null;
	private File inputFile = null;
	private File outputFile = null;
	private String format = null;
	private String hashFormat = null;
	private String password = null;
	private String algorithm = null;
	private String hashAlgorithm = null;
	private String extraParams = null;
	private String massiveOp = null;
	private boolean xml = false;
	private boolean gui = false;
	private boolean certgui = false;
	private boolean recursive = false;
	private final boolean help = false;
	private URL postUrl = null;
	private URL preUrl = null;

	// Variables temporales para la carga de los parametros
	private String formatParam = null;
	private String hashFormatParam = null;
	private String preUrlParam = null;
	private String postUrlParam = null;
	private String inputFileParam = null;
	private String outputFileParam = null;

	private final Platform.OS system;

	public CommandLineParameters(final CommandLineCommand command, final String[] params) throws CommandLineException {

		this.system = Platform.getOS();

		// El comando exige que se haya indicado un fichero principal de entrada
		int i = 1;
		if (command.isMainFileNeeded()) {
			if (params.length < 2) {
				throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.120")); //$NON-NLS-1$
			}

			this.mainFile = new File(params[i]);

			if (!this.mainFile.exists()) {
				throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.121", params[i])); //$NON-NLS-1$
			}
			if (!this.mainFile.canRead()) {
				throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.122", params[i])); //$NON-NLS-1$
			}
			i++;
		}

		// Parseamos el resto de parametros
		for (; i < params.length; i++) {

			if (PARAM_XML.equals(params[i])) {
				this.xml = true;
			}
			else if (PARAM_PREURL.equals(params[i])) {
				if (this.preUrlParam != null) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.26", params[i]), this.gui); //$NON-NLS-1$
				}
				if (i >= params.length - 1) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.128"), this.gui); //$NON-NLS-1$
				}
				this.preUrlParam = params[i+1];
				i++;
			}
			else if (PARAM_POSTURL.equals(params[i])) {
				if (this.postUrlParam != null) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.26", params[i]), this.gui); //$NON-NLS-1$
				}
				if (i >= params.length - 1) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.129"), this.gui); //$NON-NLS-1$
				}
				this.postUrlParam = params[i+1];
				i++;
			}
			else if (PARAM_GUI.equals(params[i])) {
				this.gui = true;
			}
			else if (PARAM_CERT_GUI.equals(params[i])) {
				this.certgui = true;
			}
			else if (PARAM_RECURSIVE.equals(params[i])) {
				this.recursive = true;
			}
			else if (PARAM_STORE.equals(params[i])) {
				if (this.store != null) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.26", params[i]), this.gui); //$NON-NLS-1$
				}
				if (i >= params.length - 1) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.130"), this.gui); //$NON-NLS-1$
				}
				this.store = params[i+1];
				i++;
			}
			else if (PARAM_OP.equals(params[i])) {
				if (this.massiveOp != null) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.26", params[i]), this.gui); //$NON-NLS-1$
				}
				if (i >= params.length - 1) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.131"), this.gui); //$NON-NLS-1$
				}
				this.massiveOp = params[i+1];
				i++;
			}
			else if (PARAM_ALGO.equals(params[i])) {
				if (this.algorithm != null) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.26", params[i]), this.gui); //$NON-NLS-1$
				}
				if (i >= params.length - 1) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.132"), this.gui); //$NON-NLS-1$
				}
				this.algorithm = params[i+1];
				i++;
			}
			else if (PARAM_HASH_ALGO.equals(params[i])) {
				if (this.hashAlgorithm != null) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.26", params[i]), this.gui); //$NON-NLS-1$
				}
				if (i >= params.length - 1) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.133"), this.gui); //$NON-NLS-1$
				}
				this.hashAlgorithm = params[i+1];
				i++;
			}
			else if (PARAM_CONFIG.equals(params[i])) {
				if (this.extraParams != null) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.26", params[i]), this.gui); //$NON-NLS-1$
				}
				if (i >= params.length - 1) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.134"), this.gui); //$NON-NLS-1$
				}
				this.extraParams = params[i+1];
				i++;
			}
			else if (PARAM_PASSWD.equals(params[i])) {
				if (this.password != null) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.26", params[i]), this.gui); //$NON-NLS-1$
				}
				if (i >= params.length - 1) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.135"), this.gui); //$NON-NLS-1$
				}
				this.password = params[i+1];
				i++;
			}
			else if (PARAM_ALIAS.equals(params[i])) {
				if (this.alias != null) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.26", params[i]), this.gui); //$NON-NLS-1$
				}
				if (this.filter != null) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.28"), this.gui); //$NON-NLS-1$
				}
				if (i >= params.length - 1) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.136"), this.gui); //$NON-NLS-1$
				}
				this.alias = params[i+1];
				i++;
			}
			else if (PARAM_FILTER.equals(params[i])) {
				if (this.filter != null) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.26", params[i]), this.gui); //$NON-NLS-1$
				}
				if (this.alias != null) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.28"), this.gui); //$NON-NLS-1$
				}
				if (i >= params.length - 1) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.137"), this.gui); //$NON-NLS-1$
				}
				this.filter = params[i+1];
				i++;
			}
			else if (PARAM_INPUT.equals(params[i])) {
				if (this.inputFileParam != null) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.26", params[i]), this.gui); //$NON-NLS-1$
				}
				if (i >= params.length - 1) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.74"), this.gui); //$NON-NLS-1$
				}
				this.inputFileParam = params[i + 1];
				i++;
			}
			else if (PARAM_FORMAT.equals(params[i])) {
				if (this.formatParam != null) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.26", params[i]), this.gui); //$NON-NLS-1$
				}
				if (i >= params.length - 1) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.138"), this.gui); //$NON-NLS-1$
				}
				this.formatParam = params[i+1];
				i++;
			}
			else if (PARAM_HASH_FORMAT.equals(params[i])) {
				if (this.hashFormatParam != null) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.26", params[i]), this.gui); //$NON-NLS-1$
				}
				if (i >= params.length - 1) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.139"), this.gui); //$NON-NLS-1$
				}
				this.hashFormatParam = params[i + 1];
				i++;
			}
			else if (PARAM_OUTPUT.equals(params[i])) {
				if (this.outputFileParam != null) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.26", params[i]), this.gui); //$NON-NLS-1$
				}
				if (i >= params.length - 1) {
					throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.75"), this.gui); //$NON-NLS-1$
				}
				this.outputFileParam = params[i + 1];
				i++;
			}
			else {
				throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.25", params[i]), this.gui); //$NON-NLS-1$
			}
		}

		// Hacemos las comprobaciones finales sobre los parametros
		checkParams();
	}

	/**
	 * Hace las comprobaciones de los distintos parametros una vez ya se han cargado todos.
	 * @throws CommandLineException Cuando se detecta un error en un par&aacute;metro.
	 */
	private void checkParams() throws CommandLineException {

		// Se comprueba el formato de firma
		if (this.formatParam != null) {
			this.format = this.formatParam.toLowerCase();
			if (!this.format.equals(FORMAT_XADES) &&
					!this.format.equals(FORMAT_CADES) &&
					!this.format.equals(FORMAT_PADES) &&
					!this.format.equals(FORMAT_FACTURAE) &&
					!this.format.equals(FORMAT_OOXML) &&
					!this.format.equals(FORMAT_ODF) &&
					!this.format.equals(FORMAT_AUTO)) {
				throw new CommandLineParameterException(CommandLineMessages.getString("CommandLineLauncher.4", this.formatParam), this.gui); //$NON-NLS-1$
			}
		}

		// Se comprueba el formato de hash
		if (this.hashFormatParam != null) {
			this.hashFormat = this.hashFormatParam.toLowerCase();
			if (!this.hashFormat.equals(FORMAT_HASH_FILE_HEX) &&
					!this.hashFormat.equals(FORMAT_HASH_FILE_BIN) &&
					!this.hashFormat.equals(FORMAT_HASH_FILE_BASE64) &&
					!this.hashFormat.equals(FORMAT_HASH_DIR_XML) &&
					!this.hashFormat.equals(FORMAT_HASH_DIR_PLAIN)) {
				throw new CommandLineParameterException(CommandLineMessages.getString("CommandLineLauncher.89", this.hashFormatParam), this.gui); //$NON-NLS-1$
			}
		}

		// Se comprueba la URL de prefirma
		if (this.preUrlParam != null) {
			try {
				this.preUrl = new URL(this.preUrlParam);
			}
			catch (final MalformedURLException e) {
				throw new CommandLineParameterException(
						CommandLineMessages.getString(
								"CommandLineLauncher.59", this.preUrlParam, e.toString() //$NON-NLS-1$
								), this.gui
						);
			}
		}


		// Se comprueba la URL de prefirma
		if (this.postUrlParam != null) {
			try {
				this.postUrl = new URL(this.postUrlParam);
			}
			catch (final MalformedURLException e) {
				throw new CommandLineParameterException(
						CommandLineMessages.getString(
								"CommandLineLauncher.59", this.postUrlParam, e.toString() //$NON-NLS-1$
								), this.gui
						);
			}
		}

		// Se comprueba el fichero de entrada
		if (this.inputFileParam != null) {
			this.inputFile = getFile(this.inputFileParam);

			if (!this.inputFile.exists()) {
				throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.0", this.inputFileParam), this.gui); //$NON-NLS-1$
			}
			if (!this.inputFile.canRead()) {
				throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.1", this.inputFileParam), this.gui); //$NON-NLS-1$
			}
		}

		// Se comprueba el parametro del fichero de salida
		if (this.outputFileParam != null) {
			this.outputFile = getFile(this.outputFileParam);
			final File parentFile = this.outputFile.getParentFile();
			if (parentFile != null && !parentFile.canWrite()) {
				throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.3", this.outputFileParam), this.gui); //$NON-NLS-1$
			}
		}
	}

	public URL getPostSignUrl() {
		return this.postUrl;
	}

	public URL getPreSignUrl() {
		return this.preUrl;
	}

	public String getStore() {
		return this.store;
	}

	public String getAlias() {
		return this.alias;
	}

	public String getFilter() {
		return this.filter;
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

	/** Recupera el formato de firma configurado o, si no se ha indicado, el
	 * formato por defecto.
	 * @return Formato de firma o "auto" para indicar que se seleccione el m&aacute;s apropiado. */
	public String getFormat() {
		return this.format != null ? this.format : DEFAULT_FORMAT;
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
	 * @throws CommandLineException Cuando el formato de salida indicado no era v&aacute;lido.
	 */
	String getHashDirectoryFormat() throws CommandLineException {
		String formatResult;
		if (this.hashFormat == null) {
			formatResult = DEFAULT_FORMAT_DIR_HASH;
		}
		else if (this.hashFormat.equals(FORMAT_HASH_DIR_XML) ||
				this.hashFormat.equals(FORMAT_HASH_DIR_PLAIN)) {
			formatResult = this.hashFormat;
		}
		else {
			throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.90", this.hashFormat)); //$NON-NLS-1$
		}
		return formatResult;
	}

	/**
	 * Recupera el formato de salida de huella digital cuando la entrada es un fichero.
	 * @return Formato del documento con la huella digital del fichero.
	 * @throws CommandLineException Cuando el formato de salida indicado no era v&aacute;lido.
	 */
	String getHashFileFormat() throws CommandLineException {
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
			throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.91", this.hashFormat)); //$NON-NLS-1$
		}
		return formatResult;
	}

	private File getFile(final String path) {
		File file;
		if (path.startsWith("\u007E") && Platform.isUnixSystem(this.system)) { //$NON-NLS-1$
			file = new File(Platform.getUserHome(), path.substring(1));
		}
		else {
			file = new File(path);
		}
		return file;
	}

	/** Recupera la contrase&ntilde;a configurada para acceder al almac&eacute;n de claves.
	 * @return Contrase&ntilde;a del almac&eacute;n o {@code null} si no se configur&oacute;.
	 */
	public String getPassword() {
		return this.password;
	}

	/** Recupera la operaci&oacute;n masiva configurada o, si no se ha indicado, la
	 * operaci&oacute;n por defecto.
	 * @return Operaci&oacute;n masiva. */
	public String getMassiveOperation() {
		return this.massiveOp != null ? this.massiveOp : DEFAULT_MASSIVE_OP;
	}

	/** Recupera el algoritmo de firma configurado o, si no se ha indicado, el
	 * algoritmo por defecto.
	 * @return Algoritmo de firma. */
	public String getAlgorithm() {
		return this.algorithm != null ? this.algorithm : DEFAULT_ALGO;
	}

	public String getExtraParams() {
		return this.extraParams;
	}

	public boolean isXml() {
		return this.xml;
	}

	public boolean isGui() {
		return this.gui;
	}

	public boolean isCertGui() {
		return this.certgui;
	}

	public boolean isRecursive() {
		return this.recursive;
	}

	public boolean isHelp() {
		return this.help;
	}

	public static String buildSyntaxError(final CommandLineCommand op, final String errorMessage) {
		switch (op) {
			case SIGN:
			case COSIGN:
			case COUNTERSIGN:
				return buildOperationSignSyntaxError(op.getOp(), errorMessage);
			case LIST:
				return buildOperationListSyntaxError(op.getOp(), errorMessage);
			case VERIFY:
				return buildOperationVerifySyntaxError(op.getOp(), errorMessage);
			case BATCHSIGN:
				return buildOperationBatchSignSyntaxError(op.getOp(), errorMessage);
			default:
				return errorMessage;
		}
	}

	/** Construye la cadena de texto que explica la sintaxis para el uso de los comandos de firma
	 * cofirma y contrafirma por l&iacute;nea de comandos.
	 * @param op Comando.
	 * @param errorMessage Mensaje que explica el error cometido.
	 * @return Texto con el error de sintaxis y la explicaci&oacute;n de la sintaxis correcta. */
	private static String buildOperationSignSyntaxError(final String op, final String errorMessage) {

		final String appName = DesktopUtil.getApplicationFilename();

		final StringBuilder sb = new StringBuilder();
		if (errorMessage != null) {
			sb.append(errorMessage).append("\n"); //$NON-NLS-1$
		}
		sb.append(CommandLineMessages.getString("CommandLineLauncher.7")) //$NON-NLS-1$
			.append(": ").append(appName).append(" ").append(op).append(" [").append(CommandLineMessages.getString("CommandLineLauncher.140")).append("...]\n\n")  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
			.append(CommandLineMessages.getString("CommandLineLauncher.115")).append(":\n") //$NON-NLS-1$ //$NON-NLS-2$
			.append("  ").append(PARAM_GUI).append("\t\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.23")).append(" ").append(CommandLineMessages.getString("CommandLineLauncher.146", PARAM_CERT_GUI)).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
			.append("  ").append(PARAM_CERT_GUI).append("\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.76")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  ").append(PARAM_INPUT).append(" inputfile\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.13")).append(")\n")  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  ").append(PARAM_OUTPUT).append(" outputfile\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.14")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  ").append(PARAM_ALGO).append(" algo\t (").append(CommandLineMessages.getString("CommandLineLauncher.20")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  \t ").append(ALGO_SHA1).append("\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.142")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  \t ").append(ALGO_SHA256).append("\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.143")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  \t ").append(ALGO_SHA384).append("\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.144")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  \t ").append(ALGO_SHA512).append("\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.145")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  ").append(PARAM_FORMAT).append("\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.32")).append(")\n")  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  \t ").append(FORMAT_AUTO).append("\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.42")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  \t ").append(FORMAT_CADES).append("\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.43")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  \t ").append(FORMAT_PADES).append("\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.44")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  \t ").append(FORMAT_XADES).append("\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.45")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  \t ").append(FORMAT_FACTURAE).append("\t (").append(CommandLineMessages.getString("CommandLineLauncher.46")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  \t ").append(FORMAT_OOXML).append("\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.72")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  \t ").append(FORMAT_ODF).append("\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.73")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  ").append(PARAM_CONFIG).append(" extraParams\t (").append(CommandLineMessages.getString("CommandLineLauncher.27")).append(")\n")  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  ").append(PARAM_STORE).append("\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.31")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  \t auto\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.36")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			.append("  \t windows\t (").append(CommandLineMessages.getString("CommandLineLauncher.37")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			.append("  \t mac\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.38")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			.append("  \t mozilla\t (").append(CommandLineMessages.getString("CommandLineLauncher.39")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			.append("  \t dni\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.40")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			.append("  \t pkcs12:p12file\t (").append(CommandLineMessages.getString("CommandLineLauncher.41")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			.append("  \t pkcs11:p11file\t (").append(CommandLineMessages.getString("CommandLineLauncher.47")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			.append("  ").append(PARAM_PASSWD).append(" password\t (").append(CommandLineMessages.getString("CommandLineLauncher.12")).append(")\n")  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  ").append(PARAM_ALIAS).append(" alias\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.16")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  ").append(PARAM_FILTER).append(" filter\t (").append(CommandLineMessages.getString("CommandLineLauncher.66")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  ").append(PARAM_XML).append("\t\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.18")).append(")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

		return sb.toString();
	}

	/** Construye la cadena de texto que explica la sintaxis para el uso del comando para la firma
	 * de lotes de documentos.
	 * @param op Comando.
	 * @param errorMessage Mensaje que explica el error cometido.
	 * @return Texto con el error de sintaxis y la explicaci&oacute;n de la sintaxis correcta. */
	private static String buildOperationBatchSignSyntaxError(final String op, final String errorMessage) {

		final String appName = DesktopUtil.getApplicationFilename();

		final StringBuilder sb = new StringBuilder();
		if (errorMessage != null) {
			sb.append(errorMessage).append("\n"); //$NON-NLS-1$
		}
		sb.append(CommandLineMessages.getString("CommandLineLauncher.7")) //$NON-NLS-1$
			.append(": ").append(appName).append(" ").append(op).append(" [").append(CommandLineMessages.getString("CommandLineLauncher.140")).append("...]\n\n")  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
			.append(CommandLineMessages.getString("CommandLineLauncher.115")).append(":\n") //$NON-NLS-1$ //$NON-NLS-2$
			.append("  ").append(PARAM_INPUT).append(" inputfile\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.62")).append(")\n")  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  ").append(PARAM_OUTPUT).append(" outputfile\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.63")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  ").append(PARAM_STORE).append("\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.31")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  \t auto\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.36")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			.append("  \t windows\t (").append(CommandLineMessages.getString("CommandLineLauncher.37")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			.append("  \t mac\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.38")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			.append("  \t mozilla\t (").append(CommandLineMessages.getString("CommandLineLauncher.39")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			.append("  \t dni\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.40")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			.append("  \t pkcs12:p12file\t (").append(CommandLineMessages.getString("CommandLineLauncher.41")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			.append("  \t pkcs11:p11file\t (").append(CommandLineMessages.getString("CommandLineLauncher.47")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			.append("  ").append(PARAM_PASSWD).append(" password\t (").append(CommandLineMessages.getString("CommandLineLauncher.12")).append(")\n")  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  ").append(PARAM_ALIAS).append(" alias\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.16")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  ").append(PARAM_FILTER).append(" filter\t (").append(CommandLineMessages.getString("CommandLineLauncher.66")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  ").append(PARAM_PREURL).append(" url\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.64")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  ").append(PARAM_POSTURL).append(" url\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.65")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			.append("  ").append(PARAM_XML).append("\t\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.18")).append(")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

		return sb.toString();
	}

	/** Construye la cadena de texto que explica la sintaxis para el uso del comando de
	 * verificaci&oacute;n de firmas por l&iacute;nea de comandos.
	 * @param op Comando.
	 * @param errorMessage Mensaje que explica el error cometido.
	 * @return Texto con el error de sintaxis y la explicaci&oacute;n de la sintaxis correcta. */
	private static String buildOperationVerifySyntaxError(final String op, final String errorMessage) {

		final String appName = DesktopUtil.getApplicationFilename();

		final StringBuilder sb = new StringBuilder();
		if (errorMessage != null) {
			sb.append(errorMessage).append("\n"); //$NON-NLS-1$
		}
		sb.append(CommandLineMessages.getString("CommandLineLauncher.7")) //$NON-NLS-1$
		.append(": ").append(appName).append(" ").append(op).append(" [").append(CommandLineMessages.getString("CommandLineLauncher.140")).append("...]\n\n")  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
		.append(CommandLineMessages.getString("CommandLineLauncher.115")).append(":\n") //$NON-NLS-1$ //$NON-NLS-2$
		.append("  ").append(PARAM_GUI).append(" \t\t (").append(CommandLineMessages.getString("CommandLineLauncher.23")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_INPUT).append(" inputfile\t (").append(CommandLineMessages.getString("CommandLineLauncher.13")).append(")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

		return sb.toString();
	}

	/** Construye la cadena de texto que explica la sintaxis para el uso del
	 * comando de listado de los alias de certificados.
	 * @param op Comando.
	 * @param errorMessage Mensaje que explica el error cometido.
	 * @return Texto con el error de sintaxis y la explicaci&oacute;n de la sintaxis correcta. */
	private static String buildOperationListSyntaxError(final String op, final String errorMessage) {

		final String appName = DesktopUtil.getApplicationFilename();

		final StringBuilder sb = new StringBuilder();
		if (errorMessage != null) {
			sb.append(errorMessage).append("\n"); //$NON-NLS-1$
		}
		sb.append(CommandLineMessages.getString("CommandLineLauncher.7")) //$NON-NLS-1$
		.append(": ").append(appName).append(" ").append(op).append(" [").append(CommandLineMessages.getString("CommandLineLauncher.140")).append("...]\n\n")  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
		.append(CommandLineMessages.getString("CommandLineLauncher.115")).append(":\n") //$NON-NLS-1$ //$NON-NLS-2$
		.append("  ").append(PARAM_STORE).append("\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.31")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  \t auto\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.36")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		.append("  \t windows\t (").append(CommandLineMessages.getString("CommandLineLauncher.37")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		.append("  \t mac\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.38")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		.append("  \t mozilla\t (").append(CommandLineMessages.getString("CommandLineLauncher.39")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		.append("  \t dni\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.40")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		.append("  \t pkcs12:p12file\t (").append(CommandLineMessages.getString("CommandLineLauncher.41")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		.append("  ").append(PARAM_PASSWD).append(" password\t (").append(CommandLineMessages.getString("CommandLineLauncher.12")).append(")\n")  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_XML).append("\t\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.18")).append(")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

		return sb.toString();
	}
}
/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.Console;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.URL;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableEntryException;
import java.security.cert.CertificateEncodingException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.keystores.CertificateContext;
import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOConfigurableContext;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.AOKeystoreAlternativeException;
import es.gob.afirma.keystores.CertificateFilter;
import es.gob.afirma.keystores.callbacks.CachePasswordCallback;
import es.gob.afirma.keystores.filters.CertFilterManager;
import es.gob.afirma.signers.batch.client.BatchSigner;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.odf.AOODFSigner;
import es.gob.afirma.signers.ooxml.AOOOXMLSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.pades.IncorrectPageException;
import es.gob.afirma.signers.pades.InvalidSignaturePositionException;
import es.gob.afirma.signers.pades.common.PdfExtraParams;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.signvalidation.SignValider;
import es.gob.afirma.signvalidation.SignValiderFactory;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.SignValidity.VALIDITY_ERROR;
import es.gob.afirma.standalone.plugins.AfirmaPlugin;
import es.gob.afirma.standalone.plugins.Permission;
import es.gob.afirma.standalone.plugins.PluginCommand;
import es.gob.afirma.standalone.plugins.PluginCommandAction;
import es.gob.afirma.standalone.plugins.PluginControlledException;
import es.gob.afirma.standalone.plugins.PluginInfo;
import es.gob.afirma.standalone.plugins.manager.PermissionChecker;
import es.gob.afirma.standalone.plugins.manager.PluginException;
import es.gob.afirma.standalone.plugins.manager.PluginLoader;
import es.gob.afirma.standalone.plugins.manager.PluginsManager;
import es.gob.afirma.standalone.ui.SignOperationConfig;
import es.gob.afirma.standalone.ui.SignOperationConfig.CryptoOperation;

/** Clase para la gesti&oacute;n de los par&aacute;metros proporcionados desde l&iacute;nea de comandos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class CommandLineLauncher {

	private CommandLineLauncher() {
		// No permitimos la instanciacion
	}

	private static final String PARAM_HELP    = "-help"; //$NON-NLS-1$

	private static final String EXTRA_PARAM_TARGET = "target"; //$NON-NLS-1$

	/** Clave con la que se configuran los filtros en el CertFilterManager. */
	private static final String KEY_FILTERS = "filters";  //$NON-NLS-1$

	private static final int STATUS_ERROR = -1;
	private static final int STATUS_SUCCESS = 0;

	private static final String STORE_AUTO = "auto"; //$NON-NLS-1$
	private static final String STORE_MAC  = "mac"; //$NON-NLS-1$
	private static final String STORE_WIN  = "windows"; //$NON-NLS-1$
	private static final String STORE_P12  = "pkcs12"; //$NON-NLS-1$
	private static final String STORE_NSS  = "mozilla"; //$NON-NLS-1$
	private static final String STORE_DNI  = "dni"; //$NON-NLS-1$
	private static final String STORE_P11  = "pkcs11"; //$NON-NLS-1$

    private static PluginsManager pluginsManager = null;

	static void processCommandLine(final String[] args) {

		final Console console = System.console();
		try (final PrintWriter pw = console != null ? console.writer() : new PrintWriter(System.out)) {

			// Comprobamos si hay que mostrar la sintaxis de la aplicacion
			if (args == null || args.length < 1 || PARAM_HELP.equalsIgnoreCase(args[0])) {
				closeApp(STATUS_SUCCESS, pw, buildGeneralSyntax(null));
				return;
			}

			// Identificamos el comando
			String response;
			final String argCommand = args[0].toLowerCase();
			final boolean needXmlResponse = checkXmlResponseNeeded(args);
			final CommandLineCommand command = CommandLineCommand.parse(argCommand);
			try {
				// Si es uno de los comandos reconocidos por la aplicacion, ejecutamos
				// la operacion
				if (command != null) {
					response = processCommand(command, args);
				}
				// Si no es es un comando reconocido por la aplicacion, comprobamos si esta
				// reconocido por alguno de los plugins
				else {
					final PluginCommand pluginCommand = getPluginCommand(args[0].toLowerCase());
					if (pluginCommand != null) {
						response = processPluginCommand(pluginCommand, args);
					}
					// Si no es un comando reconocido ni por la aplicacion ni por los plugins,
					// mostramos un error
					else {
						throw new UnsupportedOperationException(
								buildGeneralSyntax(
										CommandLineMessages.getString(
												"CommandLineLauncher.15", //$NON-NLS-1$
												args[0])));
					}
				}
			}
			catch (final CommandLineParameterException e) {
				if (e.isUsingGui()) {
					showErrorDialog(e.getMessage(), e);
				}

				final String msg = CommandLineParameters.buildSyntaxError(command, e.getMessage());
				closeApp(STATUS_ERROR, pw, msg);
				return;
			}
			catch (final CommandLineException e) {
				if (e.isUsingGui()) {
					showErrorDialog(e.getMessage(), e);
				}

				String msg;
				final Throwable cause = e.getCause();
				if (cause != null && cause instanceof AOKeystoreAlternativeException) {
					msg = CommandLineMessages.getString("CommandLineLauncher.49", cause.getMessage()); //$NON-NLS-1$
				} else if (cause != null) {
					msg = cause.getMessage();
				} else {
					msg = e.getMessage();
				}
				if (needXmlResponse) {
					msg = buildXmlResponse(false, msg, null);
				}
				closeApp(STATUS_ERROR, pw, msg);
				return;
			}
			catch (PluginControlledException | UnsupportedOperationException e) {
				String msg = e.getMessage();
				if (needXmlResponse) {
					msg = buildXmlResponse(false, msg, null);
				}
				closeApp(STATUS_ERROR, pw, msg);
				return;
			}
			catch (final Exception e) {
				String msg = CommandLineMessages.getString("CommandLineLauncher.50", e.getMessage()); //$NON-NLS-1$
				if (needXmlResponse) {
					msg = buildXmlResponse(false, msg, null);
				}
				closeApp(STATUS_ERROR, pw, msg);
				return;
			}

			// Imprimimos el resultado de la operacion
			printMessage(pw, response);

			if (response != null) {
				System.exit(0);
			}
		}
	}

	private static boolean checkXmlResponseNeeded(final String[] args) {
		boolean found = false;
		for (int i = 1; i < args.length && !found; i++) {
			if (CommandLineParameters.PARAM_XML.equalsIgnoreCase(args[i])) {
				found = true;
			}
		}
		return found;
	}

	private static String processCommand(final CommandLineCommand command, final String[] args)
			throws CommandLineException, UnsupportedOperationException {

		// Comprobamos si se debe mostrar la ayuda del comando
		if (args.length > 1 && PARAM_HELP.equalsIgnoreCase(args[1])) {
			return CommandLineParameters.buildSyntaxError(command, null);
		}

		// Cargamos los parametros
		final CommandLineParameters params = new CommandLineParameters(command, args);

		// Actuamos segun corresponda para cada comando
		String result = null;
		switch(command) {
		case LIST:
			result = listAliasesByCommandLine(params);
			break;
		case VERIFY:
			if (params.isGui()) {
				verifyByGui(params);
			}
			else {
				result = verifyByCommandLine(params);
			}
			break;
		case SIGN:
			if (params.isGui()) {
				signByGui(command, params);
			}
			else {
				result = signByCommandLine(command, params);
			}
			break;
		case COSIGN:
		case COUNTERSIGN:
			result = signByCommandLine(command, params);
			break;
		case BATCHSIGN:
			batchByCommandLine(params);
			break;
		default:
			throw new UnsupportedOperationException(
					CommandLineMessages.getString("CommandLineLauncher.54", command.getOp())); //$NON-NLS-1$
		}

		return result;
	}

	/**
	 * Busca el comando entre aquellos soportados por los plugins instalados.
	 * @return Informaci&oacute;n del comando soportado.
	 * @throws PluginException Cuando falla la carga de la informaci&oacute;n del plugin.
	 */
	private static PluginCommand[] getPluginCommands() throws PluginException {
		final List<PluginCommand> commands = new ArrayList<>();
		for (final AfirmaPlugin plugin : pluginsManager.getPluginsLoadedList()) {
			final PluginInfo info = plugin.getInfo();
			final PluginCommand[] pluginCommands = info.getCommands();
			if (pluginCommands != null) {
				for (final PluginCommand pluginCommand : pluginCommands) {
					commands.add(pluginCommand);
				}
			}
		}
		return commands.toArray(new PluginCommand[0]);
	}

	/**
	 * Busca el comando entre aquellos soportados por los plugins instalados.
	 * @param command Comando proporcionado a la aplicaci&oacute;n.
	 * @return Informaci&oacute;n del comando soportado.
	 * @throws PluginException Cuando falla la carga de la informaci&oacute;n del plugin.
	 */
	private static PluginCommand getPluginCommand(final String command) throws PluginException {
		for (final AfirmaPlugin plugin : pluginsManager.getPluginsLoadedList()) {
			final PluginInfo info = plugin.getInfo();
			if (PermissionChecker.check(info, Permission.COMMANDS)) {
				final PluginCommand[] pluginCommands = info.getCommands();
				if (pluginCommands != null) {
					for (final PluginCommand pluginCommand : pluginCommands) {
						if (pluginCommand.getCommand().equalsIgnoreCase(command)) {
							return pluginCommand;
						}
					}
				}
			}
		}
		return null;
	}

	/**
	 * Ejecuta la acci&oacute;n asociada a un comando de un plugin.
	 * @param pluginCommand Comando de un plugin.
	 * @param args Par&aacute;metros con los que se ha invocado a la aplicaci&oacute;n.
	 * @return Resultado de la operaci&oacute;n o null si no devolvi&oacute; resultado.
	 * @throws IllegalArgumentException Cuando los par&aacute;metros no eran v&aacute;lidos.
	 * @throws PluginControlledException Cuando se produjo un error durante la carga o
	 * ejecuci&oacute;n de la operaci&oacute;n.
	 */
	private static String processPluginCommand(final PluginCommand pluginCommand, final String[] args)
			throws IllegalArgumentException, PluginControlledException {

		final String commandClass = pluginCommand.getCommandActionClass();
		PluginCommandAction action;
		try {
			action = PluginLoader.getPluginCommandAction(commandClass);
		} catch (final PluginException e) {
			throw new PluginControlledException("No se pudo cargar la accion del plugin", e); //$NON-NLS-1$
		}

		return action.start(args);
	}

	/** Desactiva el log por consola.
	 * @param handlerName Nombre del manejador. */
	private static void deactivateConsoleLog(final String handlerName) {
		Logger.getLogger(handlerName).setLevel(Level.OFF);
	}

	/**
	 * Desactiva el log por consola de SLF4J cuando utiliza la
	 * implementaci&oacute;n simple.
	 */
	private static void deactivateSlf4JLogs() {
		System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "off"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Mostramos el panel de validaci&oacute;n de certificados y firmas.
 	 * @param params Par&aacute;metros de configuraci&oacute;n.
 	 * @throws CommandLineException Cuando falta algun par&aacute;metro necesario. */
 	private static void verifyByGui(final CommandLineParameters params) throws CommandLineException {

 		final File inputFile = params.getInputFile();
 		if (inputFile == null) {
 			throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.5"), true);  //$NON-NLS-1$
 		}
 		new VisorFirma(true, null).initialize(false, inputFile);
 	}

	/** Mostramos el panel de firmas. Se usara la configuraci&oacute;n de firma establecida
	 * en la interfaz de Autofirma.
	 * @param params Par&aacute;metros de configuraci&oacute;n.
	 * @throws CommandLineException Cuando falta algun par&aacute;metro necesario. */
	private static void signByGui(final CommandLineCommand command, final CommandLineParameters params) throws CommandLineException {

		final File inputFile = params.getInputFile();
		if (inputFile == null) {
			throw new CommandLineException(CommandLineMessages.getString("CommandLineLauncher.5"), true); //$NON-NLS-1$
		}

		SignOperationConfig signConfig;
		try {
			signConfig = loadSignConfig(command, params);
		} catch (final IOException e) {
			throw new CommandLineException(e.getMessage(), e);
		}

		final SimpleAfirma simpleAfirma = new SimpleAfirma();
		simpleAfirma.initGUI(inputFile, signConfig);
	}

	/**
	 * Obtiene la configuracion de firma a partir de los parametros proporcionados y los por
	 * defecto.
	 * @param params Par&aacute;metros de l&iacute;nea de comandos.
	 * @return Configuraci&oacute;n de firma.
	 * @throws CommandLineParameterException Cuando se indica un formato de firma no soportado.
	 * @throws IOException
	 */
	private static SignOperationConfig loadSignConfig(final CommandLineCommand command, final CommandLineParameters params)
			throws CommandLineParameterException, IOException {

		final SignOperationConfig signConfig = new SignOperationConfig();
		switch (command) {
		case SIGN:
			signConfig.setCryptoOperation(CryptoOperation.SIGN);
			break;
		case COSIGN:
			signConfig.setCryptoOperation(CryptoOperation.COSIGN);
			break;
		case COUNTERSIGN:
			signConfig.setCryptoOperation(CryptoOperation.COUNTERSIGN_LEAFS);
			break;
		default:
			signConfig.setCryptoOperation(CryptoOperation.SIGN);
			break;
		}

		final AOSigner signer;
		String format = params.getFormat();
		if (!CommandLineParameters.FORMAT_AUTO.equals(format)) {

			final byte[] data;
			try {
				data = loadFile(params.getInputFile());
			}
			catch(final Exception e) {
				throw new IOException(
					"No se ha podido leer el fichero de entrada: " + params.getInputFile().getAbsolutePath(), e //$NON-NLS-1$
				);
			}

			format = selectFormatByData(data);
			try {
				signer = getSigner(format);
			}
			catch (final Exception e) {
				throw new CommandLineParameterException(CommandLineMessages.getString("CommandLineLauncher.4", format), e); //$NON-NLS-1$
			}
			signConfig.setSigner(signer);
		}



		if (params.getExtraParams() != null) {
			final Properties extraParams = new Properties();
			for (final String prop : params.getExtraParams().split("\n")) { //$NON-NLS-1$
				final String propt = prop.trim();
				final int eq = propt.indexOf("="); //$NON-NLS-1$
				if (eq > 0 && !propt.startsWith("#")) { //$NON-NLS-1$
					final String key = propt.substring(0, eq).trim();
					final String value = propt.substring(eq + 1).trim();
					extraParams.setProperty(key, value);
				}
			}
			signConfig.setExtraParams(extraParams);
		}

		if (params.getAlgorithm() != null) {
			final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(params.getAlgorithm());
			signConfig.setDigestAlgorithm(digestAlgorithm);
		}

		return signConfig;
	}

	private static String batchByCommandLine(final CommandLineParameters params) throws CommandLineException {

		final File inputFile = params.getInputFile();
		if (inputFile == null) {
			throw new CommandLineParameterException(CommandLineMessages.getString("CommandLineLauncher.5")); //$NON-NLS-1$
		}

		String selectedAlias = params.getAlias();
		if (selectedAlias == null && params.getFilter() == null) {
			throw new CommandLineParameterException(CommandLineMessages.getString("CommandLineLauncher.17")); //$NON-NLS-1$
		}

		final File outputFile  = params.getOutputFile();
		if (outputFile == null && !params.isXml()) {
			throw new CommandLineParameterException(CommandLineMessages.getString("CommandLineLauncher.19")); //$NON-NLS-1$
		}

		final URL preUrl = params.getPreSignUrl();
		final URL postUrl = params.getPostSignUrl();
		if (preUrl == null || postUrl == null) {
			throw new CommandLineParameterException(
				CommandLineMessages.getString("CommandLineLauncher.60") //$NON-NLS-1$
			);
		}

		try {
			final AOKeyStoreManager ksm = getKsm(params.getStore(), params.getPassword());
			if (params.getFilter() != null) {
				selectedAlias = filterCertificates(ksm, params.getFilter());
			}
			final PrivateKeyEntry pke = ksm.getKeyEntry(selectedAlias);
			if (pke == null) {
				throw new CommandLineException(
					CommandLineMessages.getString("CommandLineLauncher.61", selectedAlias) //$NON-NLS-1$
				);
			}
			final byte[] inputXml;
			try (
				final InputStream fis = new FileInputStream(inputFile);
				final InputStream bis = new BufferedInputStream(fis);
			) {
				inputXml = AOUtil.getDataFromInputStream(bis);
			}
			final String xml;
			if (!Base64.isBase64(inputXml)) {
				xml = Base64.encode(inputXml, true);
			}
			else {
				xml = new String(inputXml).replace("+", "-").replace("/", "_"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			}
			final String res = BatchSigner.signXML(
				xml,
				preUrl.toString(),
				postUrl.toString(),
				pke.getCertificateChain(),
				pke.getPrivateKey(),
				buildProperties(params.getExtraParams())
			);

			try (
				final FileOutputStream fos = new FileOutputStream(outputFile);
				final BufferedOutputStream bos = new BufferedOutputStream(fos);
			) {
				fos.write(res.getBytes());
				fos.flush();
			}

			final String okMsg = CommandLineMessages.getString("CommandLineLauncher.22"); //$NON-NLS-1$
			if (params.isXml()) {
				if (params.getOutputFile() != null) {
					return buildXmlResponse(true, okMsg, null);
				}
				return buildXmlResponse(true, okMsg, res.getBytes());
			}
			return okMsg;

		}
		catch (IOException |
			   AOKeystoreAlternativeException |
			   KeyStoreException              |
			   NoSuchAlgorithmException       |
			   UnrecoverableEntryException    |
			   CertificateEncodingException   |
			   AOException e) {
					if (params.isXml()) {
						return buildXmlResponse(false, e.getMessage(), null);
					}
					return e.getMessage();
		}

	}

	/** Verifica por l&iacute;nea de comandos.
	 * @param params Par&aacute;metros de configuraci&oacute;n.
	 * @return Mensaje con el resultado de la operaci&oacute;n.
	 * @throws CommandLineException Cuando falta algun par&aacute;metro necesario
	 * o se produce un error en la operaci&oacute;n. */
	private static String verifyByCommandLine(final CommandLineParameters params)
			throws CommandLineException {

		if (params.getInputFile() == null) {
			throw new CommandLineParameterException(CommandLineMessages.getString("CommandLineLauncher.5")); //$NON-NLS-1$
		}

		// Leemos el fichero de entrada
		final byte[] sign;
		try {
			sign = loadFile(params.getInputFile());
		}
		catch(final Exception e) {
			throw new CommandLineException(
					"No se ha podido leer el fichero de entrada: " + params.getInputFile().getAbsolutePath(), e); //$NON-NLS-1$
		}

		List<SignValidity> validityList = new ArrayList<>();
		try {
			final SignValider sv = SignValiderFactory.getSignValider(sign);
			if (sv != null) {
				validityList = sv.validate(sign);
			}
			else if(DataAnalizerUtil.isSignedODF(sign)) {
				validityList.add(new SignValidity(SIGN_DETAIL_TYPE.UNKNOWN, VALIDITY_ERROR.ODF_UNKOWN_VALIDITY));
			}
			else if(DataAnalizerUtil.isSignedOOXML(sign)) {
				validityList.add(new SignValidity(SIGN_DETAIL_TYPE.UNKNOWN, VALIDITY_ERROR.OOXML_UNKOWN_VALIDITY));
			}

			if (validityList.size() == 0) {
				validityList.add(new SignValidity(SIGN_DETAIL_TYPE.KO, VALIDITY_ERROR.UNKOWN_SIGNATURE_FORMAT));
			}
		}
		catch (final Exception e) {
			throw new CommandLineException("Error en la validacion de la firma", e); //$NON-NLS-1$
		}

		final StringBuilder buffer = new StringBuilder();
		for (final SignValidity validity : validityList) {
			buffer.append(validity.toString()).append("\n"); //$NON-NLS-1$
		}

		return buffer.toString();
	}

	/** Firma por l&iacute;nea de comandos.
	 * @param command Comando ejecutado en l&iacute;nea de comandos.
	 * @param params Par&aacute;metros de configuraci&oacute;n.
	 * @return Mensaje con el resultado de la operaci&oacute;n.
	 * @throws CommandLineException Cuando falta algun par&aacute;metro necesario
	 * o se produce un error en la operaci&oacute;n. */
	private static String signByCommandLine(final CommandLineCommand command, final CommandLineParameters params)
			throws CommandLineException {

		if (params.getInputFile() == null) {
			throw new CommandLineParameterException(CommandLineMessages.getString("CommandLineLauncher.5")); //$NON-NLS-1$
		}

		if (params.getAlias() == null && params.getFilter() == null && !params.isCertGui()) {
			throw new CommandLineParameterException(CommandLineMessages.getString("CommandLineLauncher.17")); //$NON-NLS-1$
		}

		if (params.getOutputFile() == null && !params.isXml()) {
			throw new CommandLineParameterException(CommandLineMessages.getString("CommandLineLauncher.19")); //$NON-NLS-1$
		}

		byte[] res;
		try {
			AOKeyStoreManager ksm = getKsm(params.getStore(), params.getPassword());

			String selectedAlias;
			if (params.isCertGui()) {
				final Properties extraParams = buildProperties(params.getExtraParams());
				if (params.getFilter() != null && !params.getFilter().isEmpty()) {
					extraParams.put("filters", params.getFilter()); //$NON-NLS-1$
				}
				final CertFilterManager filterManager = new CertFilterManager(extraParams);
				final List<CertificateFilter> filters = filterManager.getFilters();

				final AOKeyStoreDialog dialog = new AOKeyStoreDialog(ksm, null,
						true, // Solo mostrar certificados con clave privada
						true, // Mostrar certificados caducados
						false, // Advertir si certicado caducado
						filters,
						false
						);
				dialog.allowOpenExternalStores(filterManager.isExternalStoresOpeningAllowed());
				dialog.show();

				// Obtenemos el almacen del certificado seleccionado (que puede no ser el mismo
		    	// que se indico originalmente por haberlo cambiado desde el dialogo de seleccion)
				// y de ahi sacamos la referencia a la clave
				final CertificateContext context = dialog.getSelectedCertificateContext();
				final KeyStoreManager selectedKsm = context.getKeyStoreManager();
				if (selectedKsm instanceof AOKeyStoreManager) {
					ksm = (AOKeyStoreManager) selectedKsm;
				}
				selectedAlias = context.getAlias();
			}
			else {
				selectedAlias = params.getAlias();
				if (params.getFilter() != null) {
					selectedAlias = filterCertificates(ksm, params.getFilter());
				}
			}

			res = sign(
				command,
				params.getFormat(),
				params.getAlgorithm(),
				params.getExtraParams(),
				params.getInputFile(),
				selectedAlias,
				ksm,
				params.getPassword()
			);
		}
		catch (IOException | AOException | AOKeystoreAlternativeException e) {
			throw new CommandLineException(e.getMessage(), e);
		}

		// Si se ha proporcionado un fichero de salida, se guarda el resultado de la firma en el.
		// La respuesta, si se indico que fuese XML, sera un XML con el texto descriptivo de la respuesta
		// y, si no se guardo la firma, el resultado de la firma. Si la respuesta no es XML simplemente
		// se devuelve el texto plano con el resultado.
		if (params.getOutputFile() != null) {

			try (final OutputStream fos = new FileOutputStream(params.getOutputFile());) {
				fos.write(res);
			}
			catch(final Exception e) {
				throw new CommandLineException(CommandLineMessages.getString(
					"CommandLineLauncher.21", //$NON-NLS-1$
					params.getOutputFile().getAbsolutePath())
				, e);
			}
		}

		final String okMsg = CommandLineMessages.getString("CommandLineLauncher.22"); //$NON-NLS-1$
		if (params.isXml()) {
			// Si se guardo, no es necesario mostrarla en la salida
			if (params.getOutputFile() != null) {
				return buildXmlResponse(true, okMsg, null);
			}
			return buildXmlResponse(true, okMsg, res);
		}
		return okMsg;
	}

	/** Filtra los certificados del almac&eacute;n y devuelve el alias del &uacute;nico certificado
	 * que pasa el filtro.
	 * @param ksm Gestor del almac&eacute;n en el que aplicar el filtro.
	 * @param filterConfig Configuraci&oacute;n del filtro de certificados.
	 * @return Alias del certificado seleccionado.
	 * @throws CommandLineException Cuando no se ha encontrado ning&uacute;n certificado que pase
	 *                              el filtro o cuando se encuentra m&aacute;s de uno. */
	private static String filterCertificates(final AOKeyStoreManager ksm,
			                                 final String filterConfig) throws CommandLineException {

		final Properties config = new Properties();
		config.setProperty(KEY_FILTERS, filterConfig);

		// Componemos el filtro (solo puede haber 1)
		final CertificateFilter filter = new CertFilterManager(config).getFilters().get(0);

		final String[] filteredAliases = filter.matches(ksm.getAliases(), ksm);

		if (filteredAliases == null || filteredAliases.length == 0) {
			throw new CommandLineException(
					CommandLineMessages.getString("CommandLineLauncher.52") //$NON-NLS-1$
			);
		}

		if (filteredAliases.length > 1) {
			throw new CommandLineException(
					CommandLineMessages.getString("CommandLineLauncher.53") //$NON-NLS-1$
			);
		}

		return filteredAliases[0];
	}

	private static byte[] sign(final CommandLineCommand command,
			                   final String fmt,
			                   final String algorithm,
			                   final String extraParams,
			                   final File inputFile,
			                   final String alias,
			                   final AOKeyStoreManager ksm,
			                   final String storePassword) throws CommandLineException, IOException, AOException {

		ksm.setEntryPasswordCallBack(
			new CachePasswordCallback(
				storePassword != null ? storePassword.toCharArray() : "dummy".toCharArray() //$NON-NLS-1$
			)
		);
		final PrivateKeyEntry ke;
		try {
			ke = ksm.getKeyEntry(alias);
		}
		catch (final Exception e) {
			throw new AOException("No se ha podido obtener la referencia a la clave privada: " + e, e); //$NON-NLS-1$
		}
		if (ke == null) {
			throw new AOException("No se hay ninguna entrada en el almacen con el alias indicado: " + alias); //$NON-NLS-1$
		}

		// Leemos el fichero de entrada
		final byte[] data;
		try {
			data = loadFile(inputFile);
		}
		catch(final Exception e) {
			throw new IOException(
				"No se ha podido leer el fichero de entrada: " + inputFile.getAbsolutePath(), e //$NON-NLS-1$
			);
		}

		Properties extraParamsProperties = null;

		// Si el formato es "auto", configuramos un formato valido para el tipo de fichero
		String format = fmt;
		if (CommandLineParameters.FORMAT_AUTO.equals(fmt)) {
			format = selectFormatByData(data);
		}

		extraParamsProperties = buildProperties(extraParams);

		// Instanciamos un firmador del tipo adecuado
		final AOSigner signer;
		try {
			signer = getSigner(format);
		}
		catch (final Exception e) {
			throw new CommandLineParameterException(CommandLineMessages.getString("CommandLineLauncher.4", format), e); //$NON-NLS-1$
		}

		// Si el firmador admite la configuracion de su contexto, rebajamos el modo de seguridad para permitir
		// que se puedan hacer cosas tales como cargar los recursos del firmador a partir de ficheros en lugar
		// de deber proveerlos directamente como parmetros
		if (signer instanceof AOConfigurableContext) {
			((AOConfigurableContext) signer).setSecureMode(false);
		}

		// Seleccionamos el algoritmo de firma
		final String keyType = ke.getPrivateKey().getAlgorithm();
		String signatureAlgorithm;
		try {
			signatureAlgorithm = AOSignConstants.composeSignatureAlgorithmName(algorithm, keyType);
		}
		catch (final Exception e) {
			throw new CommandLineParameterException(CommandLineMessages.getString("CommandLineLauncher.141", keyType), e); //$NON-NLS-1$
		}

		// Obtenemos el resultado de la operacion adecuada
		byte[] resBytes = null;
		try {
			if (command == CommandLineCommand.SIGN) {
				resBytes = signer.sign(
					data,
					signatureAlgorithm,
					ke.getPrivateKey(),
					ke.getCertificateChain(),
					extraParamsProperties
				);
			}
			else if (command == CommandLineCommand.COSIGN) {
				resBytes = signer.cosign(
					data, // Firma
					signatureAlgorithm,
					ke.getPrivateKey(),
					ke.getCertificateChain(),
					extraParamsProperties
				);
			}
			else if (command == CommandLineCommand.COUNTERSIGN) {
				CounterSignTarget csTarget = CounterSignTarget.LEAFS;
				if (extraParamsProperties != null && extraParamsProperties.containsKey(EXTRA_PARAM_TARGET) &&
						CounterSignTarget.TREE.name().equalsIgnoreCase(extraParamsProperties.getProperty(EXTRA_PARAM_TARGET))) {
					csTarget = CounterSignTarget.TREE;
				}

				resBytes = signer.countersign(
					data, // Firma
					signatureAlgorithm,
					csTarget,
					null,
					ke.getPrivateKey(),
					ke.getCertificateChain(),
					extraParamsProperties
				);
			}
			else {
				throw new CommandLineParameterException("Operacion no soportada: " + command.getOp()); //$NON-NLS-1$
			}
		}
		catch(InvalidSignaturePositionException | IncorrectPageException e) {
			// Si hay algun error de pagina no valida, se vuelve a firmar de manera invisible
			final String xParams = removeSignaturePageProperties(extraParams);
			resBytes = sign(command, fmt, signatureAlgorithm, xParams, inputFile, alias, ksm, storePassword);
		}
		catch(final Exception e) {
			throw new AOException("Error en la operacion de firma: " + e.getMessage(), e); //$NON-NLS-1$
		}

		return resBytes;
	}

	private static byte[] loadFile(final File dataFile) throws IOException {
		final byte[] data;
		try (final InputStream input = new FileInputStream(dataFile)) {
			data = AOUtil.getDataFromInputStream(input);
		}
		return data;
	}

	/**
	 * Analiza los datos y selecciona un formato de firma adecuado para el mismo.
	 * @param data Datos que se analizar&aacute;n.
	 * @return Formato de firma.
	 * @throws IOException Cuando no se puedan analizar los datos.
	 */
	private static String selectFormatByData(final byte[] data) throws IOException {
		String format;
		final String ext = new MimeHelper(data).getExtension();
		if ("pdf".equals(ext)) { //$NON-NLS-1$
			format = CommandLineParameters.FORMAT_PADES;
		}
		else if ("xml".equals(ext)) { //$NON-NLS-1$
			format = CommandLineParameters.FORMAT_XADES;
		}
		// Desactivamos la firma por defecto con OOXML y ODF
//		else if("docx".equals(ext) || "xlsx".equals(ext) || "pptx".equals(ext)) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
//		{
//			format = CommandLineParameters.FORMAT_OOXML;
//		}
//		else if("odt".equals(ext) || "ods".equals(ext) || "odp".equals(ext)) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
//		{
//			format = CommandLineParameters.FORMAT_ODF;
//		}
		else {
			format = CommandLineParameters.FORMAT_CADES;
		}
		return format;
	}

	private static AOSigner getSigner(final String format) {

		AOSigner signer;
		if (CommandLineParameters.FORMAT_CADES.equals(format)) {
			signer = new AOCAdESSigner();
		}
		else if (CommandLineParameters.FORMAT_XADES.equals(format)) {
			signer = new AOXAdESSigner();
		}
		else if (CommandLineParameters.FORMAT_PADES.equals(format)) {
			signer = new AOPDFSigner();
		}
		else if (CommandLineParameters.FORMAT_FACTURAE.equals(format)) {
			signer = new AOFacturaESigner();
		}
		else if (CommandLineParameters.FORMAT_OOXML.equals(format)) {
			signer = new AOOOXMLSigner();
		}
		else if (CommandLineParameters.FORMAT_ODF.equals(format)) {
			signer = new AOODFSigner();
		}
		else {
			throw new IllegalArgumentException("Formato de firma no soportado: " + format); //$NON-NLS-1$
		}
		return signer;
	}

	private static String listAliasesByCommandLine(final CommandLineParameters params) throws CommandLineException {

		String[] aliases;
		try {
			aliases = getKsm(params.getStore(), params.getPassword()).getAliases();
		}
		catch (IOException | AOKeystoreAlternativeException e) {
			throw new CommandLineException(e.getMessage(), e);
		}
		final StringBuilder sb = new StringBuilder();

		if (params.isXml()) {
			sb.append("<afirma><result>ok</result><response>"); //$NON-NLS-1$
			for (final String alias : aliases) {
				sb.append("<alias>").append(alias).append("</alias>"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			sb.append("</response></afirma>"); //$NON-NLS-1$
		}
		else {
			for (final String alias : aliases) {
				sb.append(alias).append('\n');
			}
		}

		return sb.toString();
	}

	private static AOKeyStoreManager getKsm(final String storeType,
			                                final String pwd) throws IOException,
	                                                                 CommandLineException,
	                                                                 AOKeystoreAlternativeException {
		final AOKeyStore store;
		String lib = null;
		if (STORE_AUTO.equals(storeType) || storeType == null) {
			if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				store = AOKeyStore.APPLE;
			}
			else if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
				store = AOKeyStore.WINDOWS;
			}
			else {
				store = AOKeyStore.SHARED_NSS;
			}
		}
		else if (STORE_MAC.equals(storeType)) {
			store = AOKeyStore.APPLE;
		}
		else if (STORE_WIN.equals(storeType)) {
			store = AOKeyStore.WINDOWS;
		}
		else if (STORE_NSS.equals(storeType)) {
			store = AOKeyStore.MOZ_UNI;
		}
		else if (STORE_DNI.equals(storeType) || "dnie".equals(storeType)) { //$NON-NLS-1$
			store = AOKeyStore.DNIEJAVA;
		}
		else if (storeType.startsWith(STORE_P12 + ":")) { //$NON-NLS-1$
			store = AOKeyStore.PKCS12;
			final String libName = storeType.replace(STORE_P12 + ":", ""); //$NON-NLS-1$ //$NON-NLS-2$
			final File cleanedLibFile = cleanupPathFile(libName);
			if (!cleanedLibFile.exists()) {
				throw new IOException("El fichero PKCS#12 indicado no existe: " + libName); //$NON-NLS-1$
			}
			if (!cleanedLibFile.canRead()) {
				throw new IOException("No se tienen permisos de lectura para el fichero PKCS#12 indicado: " + libName); //$NON-NLS-1$
			}
			lib = libName;
		}
		else if (storeType.startsWith(STORE_P11 + ":")) { //$NON-NLS-1$
			store = AOKeyStore.PKCS11;
			final String libName = storeType.replace(STORE_P11 + ":", ""); //$NON-NLS-1$ //$NON-NLS-2$
			final File cleanedLibFile = cleanupPathFile(libName);
			if (!cleanedLibFile.exists()) {
				throw new IOException("La biblioteca PKCS#11 indicada no existe: " + libName); //$NON-NLS-1$
			}
			if (!cleanedLibFile.canRead()) {
				throw new IOException("No se tienen permisos de lectura para la biblioteca PKCS#11 indicada: " + libName); //$NON-NLS-1$
			}
			lib = cleanedLibFile.getAbsolutePath();
		}
		else {
			throw new CommandLineParameterException(CommandLineMessages.getString("CommandLineLauncher.48", storeType)); //$NON-NLS-1$
		}

		return AOKeyStoreManagerFactory.getAOKeyStoreManager(
			store,
			lib,
			"CommandLine", //$NON-NLS-1$
			pwd != null ? new CachePasswordCallback(pwd.toCharArray()) : null,
			null
		);
	}

	/**
	 * Sanea una ruta de archivo.
	 * @param path Ruta de archivo.
	 * @return Ruta de archivo limpia.
	 */
	private static File cleanupPathFile(final String path) {
		final String cleanedpath = path.trim().replace("\"", "").replace("'", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		File cleanedFile;
		try {
			cleanedFile = new File(cleanedpath).getCanonicalFile();
		} catch (final Exception e) {
			cleanedFile = null;
		}
		return cleanedFile;
	}

	/** Construye la cadena de texto que explica la sintaxis para el uso de la aplicaci&oacute;n
	 * por l&iacute;nea de comandos.
	 * @param errorMessage Mensaje con el error de explica cometido.
	 * @return Texto con el error de sintaxis y la explicaci&oacute;n de la sintaxis correcta. */
	private static String buildGeneralSyntax(final String errorMessage) {
		final StringBuilder sb = new StringBuilder();
		if (errorMessage != null) {
			sb.append(errorMessage).append("\n"); //$NON-NLS-1$;
		}
		else {
			sb.append(CommandLineMessages.getString("CommandLineLauncher.34")).append("\n\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		final String appName = DesktopUtil.getApplicationFilename();

		sb.append(CommandLineMessages.getString("CommandLineLauncher.7")).append(": ").append(appName).append(" cmd [").append(CommandLineMessages.getString("CommandLineLauncher.140")).append("...]\n\n")  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
		.append(CommandLineMessages.getString("CommandLineLauncher.33")) .append(" cmd:\n\n") //$NON-NLS-1$ //$NON-NLS-2$
		.append("  ").append(CommandLineCommand.SIGN.getOp())			 .append("\t\t (").append(CommandLineMessages.getString("CommandLineLauncher.8")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(CommandLineCommand.COSIGN.getOp())		     .append("\t (")  .append(CommandLineMessages.getString("CommandLineLauncher.9")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(CommandLineCommand.COUNTERSIGN.getOp())	 .append("\t (")  .append(CommandLineMessages.getString("CommandLineLauncher.10")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(CommandLineCommand.LIST.getOp())			 .append("\t (")  .append(CommandLineMessages.getString("CommandLineLauncher.11")).append(")\n")  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(CommandLineCommand.VERIFY.getOp())		     .append("\t (")  .append(CommandLineMessages.getString("CommandLineLauncher.29")).append(")\n")  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(CommandLineCommand.BATCHSIGN.getOp())	     .append("\t (")  .append(CommandLineMessages.getString("CommandLineLauncher.69")).append(")\n\n");  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

		// Cargamos la informacion de los plugins instalados
		PluginCommand[] pluginCommands;
		try {
			pluginCommands = getPluginCommands();
		}
		catch (final Exception e) {
			sb.append(CommandLineMessages.getString("CommandLineLauncher.125")); //$NON-NLS-1$
			pluginCommands = null;
		}
		if (pluginCommands != null && pluginCommands.length > 0) {
			sb.append(CommandLineMessages.getString("CommandLineLauncher.126")).append(":\n\n"); //$NON-NLS-1$ //$NON-NLS-2$
			for (final PluginCommand command : pluginCommands) {
				sb.append("  ").append(command.getCommand()).append("\t (")  .append(command.getDescription()).append(")\n");  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
			}
			sb.append("\n"); //$NON-NLS-1$
		}

		// Indicacios para el uso de la ayuda
		sb.append(CommandLineMessages.getString("CommandLineLauncher.30", appName)) ; //$NON-NLS-1$

		return sb.toString();
	}

	/** Construye el resultado para una firma/multifirma. Si el resultado debe
	 * devolverse en XML se devolver&aacute; un XML con la forma:<br>
	 * {@code
	 * <afirma>
	 *   <result>true|false</result>
	 * 	 <response>
	 *     <msg>MENSAJE</msg>
	 *     <sign>FIRMA_BASE64</sign>
	 * 	 </response>
	 * </afirma>
	 * }
	 * @param ok Resultado de la operaci&oacute;n.
	 * @param msg Mensaje de respuesta.
	 * @param resBytes Firma generada o {@code null} si no es necesario imprimirla.
	 * @return XML resultado de la operaci&oacute;n. */
	private static String buildXmlResponse(final boolean ok, final String msg, final byte[] resBytes) {

		final StringBuilder sb = new StringBuilder();
		sb.append("<afirma><result>").append(ok).append("</result>"); //$NON-NLS-1$ //$NON-NLS-2$
		if (msg != null || resBytes != null) {
			sb.append("<response>"); //$NON-NLS-1$
			if (msg != null){
				sb.append("<msg>").append(msg).append("</msg>"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			if (resBytes != null) {
				sb.append("<sign>").append(Base64.encode(resBytes)).append("</sign>"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			sb.append("</response>"); //$NON-NLS-1$
		}
		sb.append("</afirma>"); //$NON-NLS-1$

		return sb.toString();
	}

	/**
	 * Cierra la aplicaci&oacute;n mostrando un &uacute;ltimo mensaje si se le proporcionan
	 * los recursos necesarios.
	 * @param status Estado de cierre de la aplicaci&oacute;n.
	 * @param pw Objeto para la impresi&oacute;n por consola.
	 * @param message Mensaje a mostrar.
	 */
	private static void closeApp(final int status, final PrintWriter pw, final String message) {
		printMessage(pw, message);
		System.exit(status);
	}

	/**
	 * Muestra un mensaje por consola si se le proporcionan los recursos necesarios.
	 * @param pw Objeto para la impresi&oacute;n por consola.
	 * @param message Mensaje a mostrar.
	 */
	private static void printMessage(final PrintWriter pw, final String message) {
		if (pw != null && message != null) {
			pw.write(message);
			pw.write("\n"); //$NON-NLS-1$
			pw.flush();
		}
	}

	public static void main(final String[] args) {

		// Desactivamos el Logger de consola para que no interfiera con los comandos
		deactivateConsoleLog("java.util.prefs"); //$NON-NLS-1$
		deactivateConsoleLog("es.gob.afirma"); //$NON-NLS-1$
		deactivateConsoleLog("es.gob.jmulticard"); //$NON-NLS-1$
		deactivateSlf4JLogs();

    	// Se define el look and feel por si se solicita mostrar interfaces graficas
    	LookAndFeelManager.applyLookAndFeel();

		if (pluginsManager == null) {
			pluginsManager = SimpleAfirma.getPluginsManager();
		}

		processCommandLine( args );
	}

	private static Properties buildProperties(final String propertiesParams) {
		final Properties properties = new Properties();
		if (propertiesParams != null) {
			final String params = propertiesParams.trim();

			// La division no funciona correctamente con split porque el caracter salto de linea se protege
			// al insertarse por consola, asi que lo hacemos manualmente.
			int beginIndex = 0;
			int endIndex;
			while ((endIndex = params.indexOf("\\n", beginIndex)) != -1) { //$NON-NLS-1$
				final String keyValue = params.substring(beginIndex, endIndex).trim();
				// Solo procesamos las lineas con contenido que no sean comentario
				if (keyValue.length() > 0 && keyValue.charAt(0) != '#') {
					properties.setProperty(
							keyValue.substring(0, keyValue.indexOf('=')),
							keyValue.substring(keyValue.indexOf('=') + 1)
							);
				}
				beginIndex = endIndex + "\\n".length();  //$NON-NLS-1$
			}
			properties.setProperty(
					params.substring(beginIndex, params.indexOf('=', beginIndex)).trim(),
					params.substring(params.indexOf('=', beginIndex) + 1).trim()
					);
		}
		return properties;
	}

	/**
	 * Elimina los parametros relacionados con la firma visible de los par&aacute;metros extra.
	 * @param propertiesParams Parametros de donde borrar.
	 * @return Devuelve las propiedades sin los par&aacute;metros.
	 */
	private static String removeSignaturePageProperties(final String propertiesParams) {
		String result = null;
		if (propertiesParams != null) {
			final String [] arrayParams = propertiesParams.replace("\\n", "\n").split("\\n");  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
			for (final String param : arrayParams) {
				if (param.indexOf(PdfExtraParams.SIGNATURE_PAGE) == -1
						&& param.indexOf(PdfExtraParams.SIGNATURE_PAGES) == -1
						&& param.indexOf(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTX) == -1
						&& param.indexOf(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_LOWER_LEFTY) == -1
						&& param.indexOf(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTX) == -1
						&& param.indexOf(PdfExtraParams.SIGNATURE_POSITION_ON_PAGE_UPPER_RIGHTY) == -1) {
							result += param + "\\n"; //$NON-NLS-1$
				}
			}
		}
		return result;
	}

	private static void showErrorDialog(final String message, final Exception t) {
		final String title = CommandLineMessages.getString("CommandLineLauncher.127"); //$NON-NLS-1$
		AOUIFactory.showErrorMessage(message, title, JOptionPane.ERROR_MESSAGE, t);
	}
}

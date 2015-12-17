package es.gob.afirma.standalone;

import java.io.Console;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.security.KeyStore.PrivateKeyEntry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.AOUnsupportedSignFormatException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.AOKeystoreAlternativeException;
import es.gob.afirma.keystores.callbacks.CachePasswordCallback;
import es.gob.afirma.keystores.filters.CertFilterManager;
import es.gob.afirma.keystores.filters.CertificateFilter;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xades.AOXAdESSigner;

/** Clase para la gesti&oacute;n de los par&aacute;metros proporcionados desde l&iacute;nea de comandos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class CommandLineLauncher {

	private CommandLineLauncher() {
		// No permitimos la instanciacion
	}

	private static final String PARAM_INPUT   = "-i"; //$NON-NLS-1$
	private static final String PARAM_OUTPUT  = "-o"; //$NON-NLS-1$
	private static final String PARAM_ALIAS   = "-alias"; //$NON-NLS-1$
	private static final String PARAM_FILTER  = "-filter"; //$NON-NLS-1$
	private static final String PARAM_STORE   = "-store"; //$NON-NLS-1$
	private static final String PARAM_FORMAT  = "-format"; //$NON-NLS-1$
	private static final String PARAM_PASSWD  = "-password"; //$NON-NLS-1$
	private static final String PARAM_XML     = "-xml"; //$NON-NLS-1$
	private static final String PARAM_ALGO    = "-algorithm"; //$NON-NLS-1$
	private static final String PARAM_CONFIG  = "-config"; //$NON-NLS-1$
	private static final String PARAM_GUI     = "-gui"; //$NON-NLS-1$

	private static final String STORE_AUTO = "auto"; //$NON-NLS-1$
	private static final String STORE_MAC  = "mac"; //$NON-NLS-1$
	private static final String STORE_WIN  = "windows"; //$NON-NLS-1$
	private static final String STORE_P12  = "pkcs12"; //$NON-NLS-1$
	private static final String STORE_NSS  = "mozilla"; //$NON-NLS-1$
	private static final String STORE_DNI  = "dni"; //$NON-NLS-1$
	private static final String STORE_P11  = "pkcs11"; //$NON-NLS-1$

	private static final String FORMAT_AUTO     = "auto"; //$NON-NLS-1$
	private static final String FORMAT_XADES    = "xades"; //$NON-NLS-1$
	private static final String FORMAT_PADES    = "pades"; //$NON-NLS-1$
	private static final String FORMAT_CADES    = "cades"; //$NON-NLS-1$
	private static final String FORMAT_FACTURAE = "facturae"; //$NON-NLS-1$

	private static final String COMMAND_LIST = "listaliases"; //$NON-NLS-1$
	private static final String COMMAND_SIGN = "sign"; //$NON-NLS-1$
	private static final String COMMAND_COSIGN = "cosign"; //$NON-NLS-1$
	private static final String COMMAND_COUNTERSIGN = "countersign"; //$NON-NLS-1$
	private static final String COMMAND_VERIFY = "verify"; //$NON-NLS-1$

	private static final String DEFAULT_SIGN_ALGORITHM = "SHA512withRSA"; //$NON-NLS-1$

	private static final String EXTRA_PARAM_TARGET = "target"; //$NON-NLS-1$

	/** Clave con la que se configuran los filtros en el CertFilterManager. */
	private static final String KEY_FILTERS = "filters";  //$NON-NLS-1$

	private static final int STATUS_ERROR = -1;
	private static final int STATUS_SUCCESS = 0;

	static void processCommandLine(final String[] args) {

		if (args == null || args.length < 1) {
			return;
		}

		// Desactivamos el Logger para que no interfiera con la consola
		//TODO: Descomentar para poner en produccion
		Logger.getLogger("es.gob.afirma").setLevel(Level.OFF); //$NON-NLS-1$

		final Console console = System.console();

		try (
			final PrintWriter pw = console != null ? console.writer() : null;
		) {

			final String command = args[0].toLowerCase();

			String store = null;
			String alias = null;
			String filter = null;
			File inputFile = null;
			File outputFile = null;
			String format = null;
			String password = null;
			String algorithm = null;
			String extraParams = null;
			boolean xml = false;
			boolean gui = false;

			try {
				for (int i = 1; i < args.length; i++) {

					if (PARAM_XML.equals(args[i])) {

						xml = true;
					}
					else if (PARAM_GUI.equals(args[i])) {

						gui = true;
					}
					else if (PARAM_STORE.equals(args[i])) {

						if (store != null) {
							closeApp(
								STATUS_ERROR,
								pw,
								buildSyntaxError(CommandLineMessages.getString("CommandLineLauncher.26", args[i])) //$NON-NLS-1$
							);
						}
						store = args[i+1];
						i++;
					}
					else if (PARAM_ALGO.equals(args[i])) {

						if (algorithm != null) {
							closeApp(
								STATUS_ERROR,
								pw,
								buildSyntaxError(CommandLineMessages.getString("CommandLineLauncher.26", args[i])) //$NON-NLS-1$
							);
						}
						algorithm = args[i+1];
						i++;
					}
					else if (PARAM_CONFIG.equals(args[i])) {

						if (extraParams != null) {
							closeApp(
								STATUS_ERROR,
								pw,
								buildSyntaxError(CommandLineMessages.getString("CommandLineLauncher.26", args[i])) //$NON-NLS-1$
							);
						}
						extraParams = args[i+1];
						i++;
					}
					else if (PARAM_PASSWD.equals(args[i])) {

						if (password != null) {
							closeApp(
								STATUS_ERROR,
								pw,
								buildSyntaxError(CommandLineMessages.getString("CommandLineLauncher.26", args[i])) //$NON-NLS-1$
							);
						}
						password = args[i+1];
						i++;
					}
					else if (PARAM_ALIAS.equals(args[i])) {

						if (alias != null) {
							closeApp(STATUS_ERROR, pw,
									buildSyntaxError(CommandLineMessages.getString("CommandLineLauncher.26", args[i]))); //$NON-NLS-1$
						}
						if (filter != null) {
							closeApp(STATUS_ERROR, pw,
									buildSyntaxError(CommandLineMessages.getString("CommandLineLauncher.28", args[i]))); //$NON-NLS-1$
						}
						alias = args[i+1];
						i++;
					}
					else if (PARAM_FILTER.equals(args[i])) {

						if (filter != null) {
							closeApp(STATUS_ERROR, pw,
									buildSyntaxError(CommandLineMessages.getString("CommandLineLauncher.26", args[i]))); //$NON-NLS-1$
						}
						if (alias != null) {
							closeApp(STATUS_ERROR, pw,
									buildSyntaxError(CommandLineMessages.getString("CommandLineLauncher.28", args[i]))); //$NON-NLS-1$
						}
						filter = args[i+1];
						i++;
					}
					else if (PARAM_INPUT.equals(args[i])) {

						if (inputFile != null) {
							closeApp(
								STATUS_ERROR,
								pw,
								buildSyntaxError(
									CommandLineMessages.getString("CommandLineLauncher.26", args[i]) //$NON-NLS-1$
								)
							);
						}

						inputFile = new File(args[i+1]);

						if (!inputFile.exists()) {
							closeApp(STATUS_ERROR, pw, CommandLineMessages.getString("CommandLineLauncher.0") + " " + args[i+1]); //$NON-NLS-1$ //$NON-NLS-2$
						}
						if (!inputFile.canRead()) {
							closeApp(STATUS_ERROR, pw, CommandLineMessages.getString("CommandLineLauncher.1") + " " + args[i+1]); //$NON-NLS-1$ //$NON-NLS-2$
						}
						if (!inputFile.isFile()) {
							closeApp(STATUS_ERROR, pw, CommandLineMessages.getString("CommandLineLauncher.2") + " " + args[i+1]); //$NON-NLS-1$ //$NON-NLS-2$
						}
						i++;
					}
					else if (PARAM_FORMAT.equals(args[i])) {

						if (format != null) {
							closeApp(STATUS_ERROR, pw,
									buildSyntaxError(CommandLineMessages.getString("CommandLineLauncher.26", args[i]))); //$NON-NLS-1$
						}

						format = args[i+1].toLowerCase();
						if (!format.equals(FORMAT_XADES) &&
							!format.equals(FORMAT_CADES) &&
							!format.equals(FORMAT_PADES) &&
							!format.equals(FORMAT_FACTURAE) &&
							!format.equals(FORMAT_AUTO)) {
								closeApp(STATUS_ERROR, pw, CommandLineMessages.getString("CommandLineLauncher.4", args[i+1])); //$NON-NLS-1$
						}
						i++;
					}
					else if (PARAM_OUTPUT.equals(args[i])) {

						if (outputFile != null) {
							closeApp(STATUS_ERROR, pw,
									buildSyntaxError(CommandLineMessages.getString("CommandLineLauncher.26", args[i]))); //$NON-NLS-1$
						}

						outputFile = new File(args[i+1]);
						final String parent = outputFile.getParent();
						if (parent != null && !new File(parent).canWrite()) {
							closeApp(STATUS_ERROR, pw, CommandLineMessages.getString("CommandLineLauncher.3", args[i+1])); //$NON-NLS-1$
						}
						i++;
					}
					else {
						closeApp(
							STATUS_ERROR,
							pw,
							CommandLineMessages.getString("CommandLineLauncher.25", args[i]) //$NON-NLS-1$
						);
					}
				}

				if (gui) {

					if (inputFile == null) {
						closeApp(
							STATUS_ERROR,
							pw,
							buildSyntaxError(CommandLineMessages.getString("CommandLineLauncher.5")) //$NON-NLS-1$
						);
					}

					if (COMMAND_SIGN.equals(command)) {

						final SimpleAfirma simpleAfirma = new SimpleAfirma();
						simpleAfirma.initialize(inputFile);
						simpleAfirma.loadFileToSign(inputFile);
					}
					else if (COMMAND_VERIFY.equals(command)) {
						new VisorFirma(true, null).initialize(false, inputFile);
					}
					else {
						closeApp(
							STATUS_ERROR,
							pw,
							buildSyntaxError(CommandLineMessages.getString("CommandLineLauncher.15", command)) //$NON-NLS-1$
						);
					}

					return;
				}

				if (store == null) {
					store = STORE_AUTO;
				}

				if (COMMAND_LIST.equals(command)) {

					final String aliases = listAliases(store, password, xml);

					closeApp(STATUS_SUCCESS, pw, aliases);
				}
				else if (!COMMAND_SIGN.equals(command) && !COMMAND_COSIGN.equals(command) && !COMMAND_COUNTERSIGN.equals(command)) {
					closeApp(
						STATUS_ERROR,
						pw,
						buildSyntaxError(CommandLineMessages.getString("CommandLineLauncher.15", command)) //$NON-NLS-1$
					);
				}

				if (format == null) {
					format = FORMAT_AUTO;
				}

				if (algorithm == null) {
					algorithm = DEFAULT_SIGN_ALGORITHM;
				}

				if (inputFile == null) {
					closeApp(STATUS_ERROR, pw, CommandLineMessages.getString("CommandLineLauncher.5")); //$NON-NLS-1$
				}

				if (alias == null && filter == null) {
					closeApp(STATUS_ERROR, pw, CommandLineMessages.getString("CommandLineLauncher.17")); //$NON-NLS-1$
				}

				if (outputFile == null && !xml) {
					closeApp(STATUS_ERROR, pw, CommandLineMessages.getString("CommandLineLauncher.19")); //$NON-NLS-1$
				}

				boolean failed = false;
				byte[] res = null;
				try {
					// Obtenemos el almacen
					final AOKeyStoreManager ksm;
					try {
						ksm = getKsm(store, password);
					}
					catch (final Exception e) {
						throw new CommandLineException("No se ha podido inicializar el almacen de claves: " + e, e); //$NON-NLS-1$
					}

					String selectedAlias = alias;
					if (filter != null) {
						selectedAlias = filterCertificates(ksm, filter);
					}

					res = buildSuccessSignMessage(
						sign(
							command,
							format,
							algorithm,
							extraParams,
							inputFile,
							selectedAlias,
							ksm,
							password
						),
						xml
					);
				}
				catch (final CommandLineException e) {
					res = buildErrorMessage(e.getMessage(), e, xml);
					failed = true;
				}

				if (outputFile != null) {

					try (
						final OutputStream fos = new FileOutputStream(outputFile);
					) {
						fos.write(res);
					}
					catch(final Exception e) {
						closeApp(
							STATUS_ERROR,
							pw,
							CommandLineMessages.getString("CommandLineLauncher.21", outputFile.getAbsolutePath()) //$NON-NLS-1$
						);
					}

				}

				// Devolvemos el mensaje segun la configuracion establecida

				// Si se ha solicitado que se obtenga el resultado en XML el resultado siempre se indicara que el proceso
				// termino correctamente y el desarrollador debera analizarlo para conocer el resultado de la operacion.
				// Si no se produjo error, también indicaremos que el proceso finalizo correctamente.
				// El mensaje a mostrar sera puramente informativo si el usuario solucito que la salida se redrigiese a un
				// fichero, mientras que si no configuro fichero de salida se devolvera la firma resultante
				if (xml || !failed) {
					if (outputFile != null) {
						closeApp(STATUS_SUCCESS, pw, CommandLineMessages.getString("CommandLineLauncher.22")); //$NON-NLS-1$
					}
					else {
						closeApp(STATUS_SUCCESS, pw, new String(res));
					}
				}
				// Si la operacion finalizo con errores y la salida no debe hacer en forma de XML, indicamos directamente el
				// resultado
				else {
					closeApp(STATUS_ERROR, pw, new String(res));
				}
			}
			catch(final ArrayIndexOutOfBoundsException e) {
				closeApp(STATUS_ERROR, pw, buildSyntaxError());
			}
		}

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
				"No se han encontrado certificados que se ajusten al filtro establecido" //$NON-NLS-1$
			);
		}

		if (filteredAliases.length > 1) {
			throw new CommandLineException(
				"Se ha encontrado mas de un certificado a partir del fitro establecido" //$NON-NLS-1$
			);
		}

		return filteredAliases[0];
	}

	private static byte[] sign(final String op,
			                   final String fmt,
			                   final String algorithm,
			                   final String extraParams,
			                   final File inputFile,
			                   final String alias,
			                   final AOKeyStoreManager ksm,
			                   final String storePassword) throws CommandLineException {

		final PrivateKeyEntry ke;
		ksm.setEntryPasswordCallBack(
			new CachePasswordCallback(
				storePassword != null ? storePassword.toCharArray() : "dummy".toCharArray() //$NON-NLS-1$
			)
		);
		try {
			ke = ksm.getKeyEntry(
				alias
			);
		}
		catch (final Exception e) {
			throw new CommandLineException("No se ha podido obtener la referencia a la clave privada", e); //$NON-NLS-1$
		}
		if (ke == null) {
			throw new CommandLineException("No se hay ninguna entrada en el almacen con el alias indicado: " + alias); //$NON-NLS-1$
		}

		// Leemos el fichero de entrada
		final byte[] data;
		try (
			final InputStream input = new FileInputStream(inputFile);
		) {
			data = AOUtil.getDataFromInputStream(input);
		}
		catch(final Exception e) {
			throw new CommandLineException(
				"No se ha podido leer el fichero de entrada: " + inputFile.getAbsolutePath(), e //$NON-NLS-1$
			);
		}

		// Si el formato es "auto", miramos si es XML o PDF para asignar XAdES o PAdES
		final String format;
		if (FORMAT_AUTO.equals(fmt)) {
			final String ext = new MimeHelper(data).getExtension();
			if ("pdf".equals(ext)) { //$NON-NLS-1$
				format = FORMAT_PADES;
			}
			else if ("xml".equals(ext)) { //$NON-NLS-1$
				format = FORMAT_XADES;
			}
			else {
				format = FORMAT_CADES;
			}
		}
		else {
			format = fmt;
		}

		Properties extraParamsProperties = null;
		if (extraParams != null) {
			try {
				final String params = extraParams.trim();
				extraParamsProperties = new Properties();

				// La division no funciona correctamente con split porque el caracter salto de linea se protege
				// al insertarse por consola, asi que lo hacemos manualmente.
				int beginIndex = 0;
				int endIndex;
				while ((endIndex = params.indexOf("\\n", beginIndex)) != -1) { //$NON-NLS-1$
					final String keyValue = params.substring(beginIndex, endIndex).trim();
					// Solo procesamos las lineas con contenido que no sean comentario
					if (keyValue.length() > 0 && keyValue.charAt(0) != '#') {
						extraParamsProperties.setProperty(
							keyValue.substring(0, keyValue.indexOf('=')),
							keyValue.substring(keyValue.indexOf('=') + 1)
						);
					}
					beginIndex = endIndex + "\\n".length();  //$NON-NLS-1$
				}
				extraParamsProperties.setProperty(
					params.substring(beginIndex, params.indexOf('=', beginIndex)),
					params.substring(params.indexOf('=', beginIndex) + 1)
				);
			}
			catch (final Exception e) {
				throw new CommandLineException(
					"Error en los parametros de configuracion de firma:\n" + extraParams, e //$NON-NLS-1$
				);
			}
		}

		// Instanciamos un firmador del tipo adecuado
		final AOSigner signer;
		if (FORMAT_CADES.equals(format)) {
			signer = new AOCAdESSigner();
		}
		else if (FORMAT_XADES.equals(format)) {
			signer = new AOXAdESSigner();
		}
		else if (FORMAT_PADES.equals(format)) {
			signer = new AOPDFSigner();
		}
		else if (FORMAT_FACTURAE.equals(format)) {
			signer = new AOFacturaESigner();
		}
		else {
			throw new CommandLineException(
				"No se reconoce o no esta soportado el formato de firma: " + format, //$NON-NLS-1$
				new AOUnsupportedSignFormatException("No se reconoce o no esta soportado el formato de firma:" + format) //$NON-NLS-1$
			);
		}

		// Obtenemos el resultado de la operacion adecuada
		final byte[] resBytes;
		try {
			if (COMMAND_SIGN.equals(op)) {
				resBytes = signer.sign(
					data,
					algorithm,
					ke.getPrivateKey(),
					ke.getCertificateChain(),
					extraParamsProperties
				);
			}
			else if (COMMAND_COSIGN.equals(op)) {
				resBytes = signer.cosign(
					data, // Firma
					algorithm,
					ke.getPrivateKey(),
					ke.getCertificateChain(),
					extraParamsProperties
				);
			}
			else if (COMMAND_COUNTERSIGN.equals(op)) {
				CounterSignTarget csTarget = CounterSignTarget.LEAFS;
				if (extraParamsProperties != null && extraParamsProperties.containsKey(EXTRA_PARAM_TARGET) &&
						CounterSignTarget.TREE.name().equalsIgnoreCase(extraParamsProperties.getProperty(EXTRA_PARAM_TARGET))) {
					csTarget = CounterSignTarget.TREE;
				}

				resBytes = signer.countersign(
					data, // Firma
					algorithm,
					csTarget,
					null,
					ke.getPrivateKey(),
					ke.getCertificateChain(),
					extraParamsProperties
				);
			}
			else {
				throw new CommandLineException("Operacion no soportada: " + op); //$NON-NLS-1$
			}
		}
		catch(final Exception e) {
			throw new CommandLineException("Error en la operacion de firma: " + e.getMessage(), e); //$NON-NLS-1$
		}

		return resBytes;
	}

	private static String listAliases(final String store, final String password, final boolean xml) {
		final String[] aliases;
		try {
			aliases = getKsm(store, password).getAliases();
		}
		catch (final Exception e) {
			return CommandLineMessages.getString("CommandLineLauncher.6") + e; //$NON-NLS-1$
		}
		final StringBuilder sb = new StringBuilder();
		if (xml) {
			sb.append("<afirma><result>ok</result><response>"); //$NON-NLS-1$
		}
		for (final String alias : aliases) {
			if (xml) {
				sb.append("<alias>"); //$NON-NLS-1$
			}
			sb.append(alias);
			if (xml) {
				sb.append("</alias>"); //$NON-NLS-1$
			}
			else {
				sb.append('\n');
			}
		}
		if (xml) {
			sb.append("</response></afirma>"); //$NON-NLS-1$
		}
		return sb.toString();
	}

	private static AOKeyStoreManager getKsm(final String storeType, final String pwd) throws AOKeystoreAlternativeException, IOException {
		if (storeType == null) {
			throw new IllegalArgumentException(
				"El tipo de almacen de claves no puede ser nulo" //$NON-NLS-1$
			);
		}
		final AOKeyStore store;
		String lib = null;
		if (STORE_AUTO.equals(storeType)) {
			if (Platform.OS.MACOSX.equals(Platform.getOS())) {
				store = AOKeyStore.APPLE;
			}
			else if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
				store = AOKeyStore.WINDOWS;
			}
			else {
				store = AOKeyStore.MOZ_UNI;
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
			if (!new File(libName).exists()) {
				throw new IllegalArgumentException("El fichero PKCS#12 indicado no existe: " + libName); //$NON-NLS-1$
			}
			if (!new File(libName).canRead()) {
				throw new IllegalArgumentException("No se tienen permisos de lectura para el fichero PKCS#12 indicado: " + libName); //$NON-NLS-1$
			}
			lib = libName;
		}
		else if (storeType.startsWith(STORE_P11 + ":")) { //$NON-NLS-1$
			store = AOKeyStore.PKCS11;
			final String libName = storeType.replace(STORE_P11 + ":", ""); //$NON-NLS-1$ //$NON-NLS-2$
			if (!new File(libName).exists()) {
				throw new IllegalArgumentException("La biblioteca PKCS#11 indicada no existe: " + libName); //$NON-NLS-1$
			}
			if (!new File(libName).canRead()) {
				throw new IllegalArgumentException("No se tienen permisos de lectura para la biblioteca PKCS#11 indicada: " + libName); //$NON-NLS-1$
			}
			lib = libName;
		}
		else {
			throw new IllegalArgumentException("Tipo de almacen desconocido: " + storeType); //$NON-NLS-1$
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
	 * Construye la cadena de texto que explica la sintaxis para el uso de la aplicaci&oacute;n
	 * por l&iacute;nea de comandos.
	 * @return Texto con la explicaci&oacute;n de la sintaxis correcta.
	 */
	private static String buildSyntaxError() {
		return buildSyntaxError(null);
	}

	/**
	 * Construye la cadena de texto que explica la sintaxis para el uso de la aplicaci&oacute;n
	 * por l&iacute;nea de comandos.
	 * @param errorMessage Mensaje con el error de explica cometido.
	 * @return Texto con el error de sintaxis y la explicaci&oacute;n de la sintaxis correcta.
	 */
	private static String buildSyntaxError(final String errorMessage) {
		final StringBuilder sb = new StringBuilder();
		if (errorMessage != null) {
			sb.append(errorMessage);
		}
		sb.append("\n") //$NON-NLS-1$
		.append(CommandLineMessages.getString("CommandLineLauncher.7")).append(": SimpleAfirma cmd [options...]\n")  //$NON-NLS-1$//$NON-NLS-2$
		.append("cmd\n") //$NON-NLS-1$
		.append("  sign        (").append(CommandLineMessages.getString("CommandLineLauncher.8")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		.append("  cosign      (").append(CommandLineMessages.getString("CommandLineLauncher.9")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		.append("  countersign (").append(CommandLineMessages.getString("CommandLineLauncher.10")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		.append("  ").append(COMMAND_LIST).append(" (").append(CommandLineMessages.getString("CommandLineLauncher.11")).append(")\n")  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("options\n") //$NON-NLS-1$
		.append("  ").append(PARAM_STORE).append(" auto|windows|mac|mozilla|dni|pkcs12:p12file\n") //$NON-NLS-1$ //$NON-NLS-2$
		.append("  ").append(PARAM_PASSWD).append(" storepassword (").append(CommandLineMessages.getString("CommandLineLauncher.12")).append(")\n")  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_FORMAT).append(" auto|xades|cades|pades|facturae\n")  //$NON-NLS-1$//$NON-NLS-2$
		.append("  ").append(PARAM_CONFIG).append(" extraParams (").append(CommandLineMessages.getString("CommandLineLauncher.27")).append(")\n")  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_INPUT).append(" inputfile (").append(CommandLineMessages.getString("CommandLineLauncher.13")).append(")\n")  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_OUTPUT).append(" outputfile (").append(CommandLineMessages.getString("CommandLineLauncher.14")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_ALIAS).append(" alias (").append(CommandLineMessages.getString("CommandLineLauncher.16")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_ALGO).append(" algo (").append(CommandLineMessages.getString("CommandLineLauncher.20")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_XML).append(" (").append(CommandLineMessages.getString("CommandLineLauncher.18")).append(")\n") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		.append("  ").append(PARAM_GUI).append(" (").append(CommandLineMessages.getString("CommandLineLauncher.23")).append(")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

		return sb.toString();
	}

	/**
	 * Procesa de error de la operaci&oacute;n. Si se indica que debe presentarse como
	 * XML se devolver&aacute; un XML con la forma:
	 * <pre>
	 * &lt;afirma&gt;
	 *   &lt;result&gt;ko&lt;/result&gt;
	 *   &lt;response&gt;
	 *       &lt;description&gt;DESCRIPCION_ERROR&lt;/description&gt;
	 *   &lt;/response&gt;
	 * &lt;/afirma&gt;
	 * </pre>
	 * Si el resultado no debe ser un XML se lanzar&aacute; una excepci&oacute;n de tipo
	 * @param message Mensaje de error.
	 * @param e Excepci&oacute;n que provoc&oacute; el error.
	 * @param xml Indica si debe construirse un XML con el detalle del error.
	 * @return XML con el resultado de la operaci&oacute;n.
	 */
	private static byte[] buildErrorMessage(final String message, final CommandLineException e, final boolean xml) {

		final StringBuilder sb = new StringBuilder();
		if (xml) {
			sb.append("<afirma><result>ko</result><response><description>"); //$NON-NLS-1$
		}
		sb.append(message);
		if (e != null && e.getCause() != null) {
			sb.append(": "); //$NON-NLS-1$
			sb.append(e.getCause().toString());
		}

		if (xml) {
			sb.append("</description></response></afirma>"); //$NON-NLS-1$
		}
		else {
			sb.append('\n');
		}
		return sb.toString().getBytes();
	}

	/**
	 * Construye el resultado para una firma/multifirma realizada correctamente. Si el resultado debe
	 * devolverse en XML se devolver&aacute; un XML con la forma:<br>
	 * {@code
	 * <afirma>
	 *   <result>ok</result>
	 * 	 <response>
	 *     <sign>FIRMA_BASE64</sign>
	 * 	 </response>
	 * </afirma>
	 * }
	 * Si no se debe devolver el resultado en XML se devolver&aacute; directamente la firma generada.
	 * @param resBytes Resultado de la operaci&oacute;n.
	 * @param xml Indica si el resultado debe devolverse en XML o no.
	 * @return Resultado de la operaci&oacute;n.
	 */
	private static byte[] buildSuccessSignMessage(final byte[] resBytes, final boolean xml) {

		final StringBuilder sb = new StringBuilder();
		if (xml) {
			sb.append("<afirma><result>ok</result><response><sign>"); //$NON-NLS-1$");
			sb.append(Base64.encode(resBytes));
			sb.append("</sign></response></afirma>"); //$NON-NLS-1$
			return sb.toString().getBytes();
		}
		return resBytes;
	}

	/**
	 * Cierra la aplicaci&oacute;n mostrando un &uacute;ltimo mensaje si se le proporcionan
	 * los recursos necesarios.
	 * @param status Estado de cierre de la aplicaci&oacute;n.
	 * @param pw Objeto para la impresi&oacute;n por consola.
	 * @param message Mensaje a mostrar.
	 */
	private static void closeApp(final int status, final PrintWriter pw, final String message) {
		if (pw != null) {
			if (message != null) {
				pw.write(message);
			}
		}
		System.exit(status);
	}

	public static void main(final String[] args) {
		processCommandLine(args);
	}

}

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
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.MimeHelper;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.keystores.main.callbacks.CachePasswordCallback;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.main.common.AOKeystoreAlternativeException;
import es.gob.afirma.signers.cades.AOCAdESSigner;
import es.gob.afirma.signers.pades.AOPDFSigner;
import es.gob.afirma.signers.xades.AOXAdESSigner;

/** Clase para la gesti&oacute;n de los par&aacute;metros proporcionados desde l&iacute;nea de comandos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class CommandLineLauncher {

	private static final String PARAM_INPUT  = "-i"; //$NON-NLS-1$
	private static final String PARAM_OUTPUT = "-o"; //$NON-NLS-1$
	private static final String PARAM_ALIAS  = "-alias"; //$NON-NLS-1$
	private static final String PARAM_STORE  = "-store"; //$NON-NLS-1$
	private static final String PARAM_FORMAT = "-format"; //$NON-NLS-1$
	private static final String PARAM_PASSWD = "-password"; //$NON-NLS-1$
	private static final String PARAM_XML    = "-xml"; //$NON-NLS-1$
	private static final String PARAM_ALGO   = "-algorithm"; //$NON-NLS-1$

	private static final String STORE_AUTO = "auto"; //$NON-NLS-1$
	private static final String STORE_MAC  = "mac"; //$NON-NLS-1$
	private static final String STORE_WIN  = "windows"; //$NON-NLS-1$
	private static final String STORE_P12  = "pkcs12"; //$NON-NLS-1$
	private static final String STORE_NSS  = "mozilla"; //$NON-NLS-1$
	private static final String STORE_DNI  = "dni"; //$NON-NLS-1$

	private static final String FORMAT_AUTO     = "auto"; //$NON-NLS-1$
	private static final String FORMAT_XADES    = "xades"; //$NON-NLS-1$
	private static final String FORMAT_PADES    = "pades"; //$NON-NLS-1$
	private static final String FORMAT_CADES    = "cades"; //$NON-NLS-1$
	private static final String FORMAT_FACTURAE = "facturae"; //$NON-NLS-1$

	private static final String COMMAND_LIST = "listaliases"; //$NON-NLS-1$
	private static final String COMMAND_SIGN = "sign"; //$NON-NLS-1$
	private static final String COMMAND_COSIGN = "cosign"; //$NON-NLS-1$
	private static final String COMMAND_COUNTERSIGN = "countersign"; //$NON-NLS-1$

	private static final String DEFAULT_SIGN_ALGORITHM = "SHA512withRSA"; //$NON-NLS-1$


	static void processCommandLine(final String[] args) {

		// Desactivamos el Logger para que no interfiera con la consola
		Logger.getLogger("es.gob.afirma").setLevel(Level.OFF); //$NON-NLS-1$

		final Console c = System.console();
		if (c == null) {
			return;
		}
		final PrintWriter pw = c.writer();
		if (args == null || args.length < 1) {
			// Esta cadena no se puede cambiar
			pw.write("Firma facil con @firma. 2013 Gobierno de Espana\n"); //$NON-NLS-1$
			syntaxError(pw);
		}

		final String command = args[0].toLowerCase();

		String store = null;
		String alias = null;
		File inputFile = null;
		File outputFile = null;
		String format = null;
		String password = null;
		String algorithm = null;
		boolean xml = false;
		try {
			for (int i=1; i<args.length; i++) {
				if (PARAM_XML.equals(args[i])) {
					xml = true;
				}
				else if (PARAM_STORE.equals(args[i])) {
					if (store != null) {
						syntaxError(pw);
					}
					store = args[i+1];
					i++;
				}
				else if (PARAM_ALGO.equals(args[i])) {
					if (algorithm != null) {
						syntaxError(pw);
					}
					algorithm = args[i+1];
					i++;
				}
				else if (PARAM_PASSWD.equals(args[i])) {
					if (password != null) {
						syntaxError(pw);
					}
					password = args[i+1];
					i++;
				}
				else if (PARAM_ALIAS.equals(args[i])) {
					if (alias != null) {
						syntaxError(pw);
					}
					alias = args[i+1];
					i++;
				}
				else if (PARAM_INPUT.equals(args[i])) {
					if (inputFile != null) {
						syntaxError(pw);
					}
					inputFile = new File(args[i+1]);
					if (!inputFile.exists()) {
						pw.write(CommandLineMessages.getString("CommandLineLauncher.0") + " " + args[i+1]);  //$NON-NLS-1$//$NON-NLS-2$
						pw.flush();
						pw.close();
						System.exit(-1);
					}
					if (!inputFile.canRead()) {
						pw.write(CommandLineMessages.getString("CommandLineLauncher.1") + " " + args[i+1]);  //$NON-NLS-1$//$NON-NLS-2$
						pw.flush();
						pw.close();
						System.exit(-1);
					}
					if (!inputFile.isFile()) {
						pw.write(CommandLineMessages.getString("CommandLineLauncher.2") + " " + args[i+1]);  //$NON-NLS-1$//$NON-NLS-2$
						pw.flush();
						pw.close();
						System.exit(-1);
					}
					i++;
				}
				else if (PARAM_FORMAT.equals(args[i])) {
					format = args[i+1].toLowerCase();
					if (!format.equals(FORMAT_XADES) &&
						!format.equals(FORMAT_CADES) &&
						!format.equals(FORMAT_PADES) &&
						!format.equals(FORMAT_FACTURAE) &&
						!format.equals(FORMAT_AUTO)) {
							pw.write(CommandLineMessages.getString("CommandLineLauncher.4") + args[i+1]); //$NON-NLS-1$
							pw.flush();
							pw.close();
							System.exit(-1);
					}
					i++;
				}
				else if (PARAM_OUTPUT.equals(args[i])) {
					if (outputFile != null) {
						syntaxError(pw);
					}
					outputFile = new File(args[i+1]);
					final String parent = outputFile.getParent();
					if (parent != null && !new File(parent).canWrite()) {
						pw.write(CommandLineMessages.getString("CommandLineLauncher.3") + " " + args[i+1]);  //$NON-NLS-1$//$NON-NLS-2$
						pw.flush();
						pw.close();
						System.exit(-1);
					}
					i++;
				}
				else {
					syntaxError(pw);
				}
			}
			if (store == null) {
				store = STORE_AUTO;
			}
			if (COMMAND_LIST.equals(command)) {
				pw.write(listAliases(store, password, xml));
				pw.flush();
				pw.close();
				System.exit(0);
			}
			else if (!COMMAND_SIGN.equals(command) && !COMMAND_COSIGN.equals(command) && !COMMAND_COUNTERSIGN.equals(command)) {
				pw.write(CommandLineMessages.getString("CommandLineLauncher.15") + " " + command); //$NON-NLS-1$ //$NON-NLS-2$
				pw.flush();
				pw.close();
				System.exit(-1);
			}

			if (format == null) {
				format = FORMAT_AUTO;
			}

			if (algorithm == null) {
				algorithm = DEFAULT_SIGN_ALGORITHM;
			}

			if (inputFile == null) {
				pw.write(CommandLineMessages.getString("CommandLineLauncher.5")); //$NON-NLS-1$
				pw.flush();
				pw.close();
				System.exit(-1);
			}

			if (alias == null) {
				pw.write(CommandLineMessages.getString("CommandLineLauncher.17")); //$NON-NLS-1$
				pw.flush();
				pw.close();
				System.exit(-1);
			}

			if (outputFile == null && !xml) {
				pw.write(CommandLineMessages.getString("CommandLineLauncher.19")); //$NON-NLS-1$
				pw.flush();
				pw.close();
				System.exit(-1);
			}

			final byte[] res = sign(command, format, algorithm, inputFile, alias, store, password, xml);

			if (outputFile != null) {
				try {
					final OutputStream fos = new FileOutputStream(outputFile);
					fos.write(res);
					fos.flush();
					fos.close();
				}
				catch(final Exception e) {
					pw.write(CommandLineMessages.getString("CommandLineLauncher.21") + " '" + outputFile.getAbsolutePath() + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					pw.flush();
					pw.close();
					System.exit(-1);
				}
				pw.write(CommandLineMessages.getString("CommandLineLauncher.22")); //$NON-NLS-1$
				pw.flush();
				pw.close();
				System.exit(0);
			}

			pw.write(new String(res));
			pw.flush();
			pw.close();
			System.exit(0);
		}
		catch(final ArrayIndexOutOfBoundsException e) {
			syntaxError(pw);
		}

		pw.flush();
		pw.close();
	}

	private static byte[] sign(final String op,
			                 final String fmt,
			                 final String algorithm,
			                 final File inputFile,
			                 final String alias,
			                 final String store,
			                 final String storePassword,
			                 final boolean xml) {

		final StringBuilder sb = new StringBuilder();

		// Almacen
		final AOKeyStoreManager ksm;
		try {
			ksm = getKsm(store, storePassword);
		}
		catch (final Exception e) {
			if (xml) {
				sb.append("<afirma><result>ko</result><response><description>"); //$NON-NLS-1$
			}
			sb.append("No se ha podido inicializar el almacen de claves: "); //$NON-NLS-1$
			sb.append(e.toString());
			if (xml) {
				sb.append("</description></response></afirma>"); //$NON-NLS-1$
			}
			else {
				sb.append('\n');
			}
			return sb.toString().getBytes();
		}
		final PrivateKeyEntry ke;
		try {
			ke = ksm.getKeyEntry(
				alias,
				new CachePasswordCallback(storePassword != null ? storePassword.toCharArray() : "".toCharArray()) //$NON-NLS-1$
			);
		}
		catch (final Exception e) {
			if (xml) {
				sb.append("<afirma><result>ko</result><response><description>"); //$NON-NLS-1$
			}
			sb.append("No se ha podido obtener la referencia a la clave privada: "); //$NON-NLS-1$
			sb.append(e.toString());
			if (xml) {
				sb.append("</description></response></afirma>"); //$NON-NLS-1$
			}
			else {
				sb.append('\n');
			}
			return sb.toString().getBytes();
		}
		if (ke == null) {
			if (xml) {
				sb.append("<afirma><result>ko</result><response><description>"); //$NON-NLS-1$
			}
			sb.append("No se hay ninguna entrada en el almacen con el alias indicado: "); //$NON-NLS-1$
			sb.append(alias);
			if (xml) {
				sb.append("</description></response></afirma>"); //$NON-NLS-1$
			}
			else {
				sb.append('\n');
			}
			return sb.toString().getBytes();
		}

		// Leemos el fichero de entrada
		final byte[] data;
		try {
			final InputStream input = new FileInputStream(inputFile);
			data = AOUtil.getDataFromInputStream(input);
			input.close();
		}
		catch(final Exception e) {
			if (xml) {
				sb.append("<afirma><result>ko</result><response><description>"); //$NON-NLS-1$
			}
			sb.append("No se ha podido leer el fichero de entrada: "); //$NON-NLS-1$
			sb.append(alias);
			if (xml) {
				sb.append("</description></response></afirma>"); //$NON-NLS-1$
			}
			else {
				sb.append('\n');
			}
			return sb.toString().getBytes();
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

		// Instanciamos un firmador del tipo adecuado
		final AOSigner signer;
		if (FORMAT_CADES.equals(format)) {
			signer = new AOCAdESSigner();
		}
		else if (FORMAT_XADES.equals(format)) {
			signer = new AOXAdESSigner();
		}
		else /*if (FORMAT_PADES.equals(format))*/ {
			signer = new AOPDFSigner();
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
					null//extraParams
				);
			}
			else {
				if (xml) {
					sb.append("<afirma><result>ko</result><response><description>"); //$NON-NLS-1$
				}
				sb.append("Operacion no soportada: "); //$NON-NLS-1$
				sb.append(op);
				if (xml) {
					sb.append("</description></response></afirma>"); //$NON-NLS-1$
				}
				else {
					sb.append('\n');
				}
				return sb.toString().getBytes();
			}
		}
		catch(final Exception e) {
			if (xml) {
				sb.append("<afirma><result>ko</result><response><description>"); //$NON-NLS-1$
			}
			sb.append("Error en la operacion de firma: "); //$NON-NLS-1$
			sb.append(e.toString());
			if (xml) {
				sb.append("</description></response></afirma>"); //$NON-NLS-1$
			}
			else {
				sb.append('\n');
			}
			return sb.toString().getBytes();
		}

		if (xml) {
			sb.append("<afirma><result>ok</result><response><sign>"); //$NON-NLS-1$");
			sb.append(Base64.encode(resBytes));
			sb.append("</sign></afirma>"); //$NON-NLS-1$
			return sb.toString().getBytes();
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
			throw new IllegalArgumentException("El tipo de almacen de claves no puede ser nulo"); //$NON-NLS-1$
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

	private static void syntaxError(final PrintWriter pw) {
		pw.write(
			CommandLineMessages.getString("CommandLineLauncher.7") + ": SimpleAfirma cmd [options...]\n" +  //$NON-NLS-1$//$NON-NLS-2$
			"cmd\n" + //$NON-NLS-1$
			"  sign        (" + CommandLineMessages.getString("CommandLineLauncher.8") + ")\n" + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			"  cosign      (" + CommandLineMessages.getString("CommandLineLauncher.9") + ")\n" + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			"  countersign (" + CommandLineMessages.getString("CommandLineLauncher.10") + ")\n" + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			"  " + COMMAND_LIST + " (" + CommandLineMessages.getString("CommandLineLauncher.11") + ")\n" +  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			"options\n" + //$NON-NLS-1$
			"  " + PARAM_STORE  + " auto|windows|mac|mozilla|dni|pkcs12:p12file\n" + //$NON-NLS-1$ //$NON-NLS-2$
			"  " + PARAM_PASSWD + " storepassword (" + CommandLineMessages.getString("CommandLineLauncher.12") + ")\n" +  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			"  " + PARAM_FORMAT + " auto|xades|cades|pades|facturae\n" +  //$NON-NLS-1$//$NON-NLS-2$
			"  " + PARAM_INPUT  + " inputfile (" + CommandLineMessages.getString("CommandLineLauncher.13") + ")\n" +  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			"  " + PARAM_OUTPUT + " outputfile (" + CommandLineMessages.getString("CommandLineLauncher.14") + ")\n" + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			"  " + PARAM_ALIAS  + " alias (" + CommandLineMessages.getString("CommandLineLauncher.16") + ")\n" + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			"  " + PARAM_ALGO   + " algo (" + CommandLineMessages.getString("CommandLineLauncher.20") + ")\n" + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			"  " + PARAM_XML    + " (" + CommandLineMessages.getString("CommandLineLauncher.18") + ")" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		);
		pw.flush();
		pw.close();
		System.exit(-1);
	}

	public static void main(final String[] args) {
		processCommandLine(args);
	}

}

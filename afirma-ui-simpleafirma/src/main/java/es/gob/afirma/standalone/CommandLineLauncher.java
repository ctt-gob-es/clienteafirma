package es.gob.afirma.standalone;

import java.io.Console;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.main.callbacks.CachePasswordCallback;
import es.gob.afirma.keystores.main.common.AOKeyStore;
import es.gob.afirma.keystores.main.common.AOKeyStoreManager;
import es.gob.afirma.keystores.main.common.AOKeyStoreManagerFactory;
import es.gob.afirma.keystores.main.common.AOKeystoreAlternativeException;

/** Clase para la gesti&oacute;n de los par&aacute;metros proporcionados desde l&iacute;nea de comandos.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
final class CommandLineLauncher {

	private static final String PARAM_INPUT  = "-i"; //$NON-NLS-1$
	private static final String PARAM_OUTPUT = "-o"; //$NON-NLS-1$
	private static final String PARAM_ALIAS  = "-alias"; //$NON-NLS-1$
	private static final String PARAM_STORE  = "-store"; //$NON-NLS-1$
	private static final String PARAM_FORMAT = "-format"; //$NON-NLS-1$
	private static final String PARAM_PASSWD = "-password"; //$NON-NLS-1$

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

		for (final String a : args) {
			System.out.println(a);
		}

		final String command = args[0].toLowerCase();

		String store = null;
		String alias = null;
		File inputFile = null;
		File outputFile = null;
		String format = null;
		String password = null;
		try {
			for (int i=1; i<args.length; i++) {
				if (PARAM_STORE.equals(args[i])) {
					if (store != null) {
						syntaxError(pw);
					}
					store = args[i+1];
					i++;
				}
				if (PARAM_PASSWD.equals(args[i])) {
					if (password != null) {
						syntaxError(pw);
					}
					password = args[i+1];
					i++;
				}
				if (PARAM_ALIAS.equals(args[i])) {
					if (alias != null) {
						syntaxError(pw);
					}
					alias = args[i+1];
					i++;
				}
				if (PARAM_INPUT.equals(args[i])) {
					if (inputFile != null) {
						syntaxError(pw);
					}
					inputFile = new File(args[i+1]);
					if (!inputFile.exists()) {
						pw.write(Messages.getString("CommandLineLauncher.0") + " " + args[i+1]);  //$NON-NLS-1$//$NON-NLS-2$
						pw.flush();
						pw.close();
						System.exit(-1);
					}
					if (!inputFile.canRead()) {
						pw.write(Messages.getString("CommandLineLauncher.1") + " " + args[i+1]);  //$NON-NLS-1$//$NON-NLS-2$
						pw.flush();
						pw.close();
						System.exit(-1);
					}
					if (!inputFile.isFile()) {
						pw.write(Messages.getString("CommandLineLauncher.2") + " " + args[i+1]);  //$NON-NLS-1$//$NON-NLS-2$
						pw.flush();
						pw.close();
						System.exit(-1);
					}
					i++;
				}
				if (PARAM_OUTPUT.equals(args[i])) {
					if (outputFile != null) {
						syntaxError(pw);
					}
					outputFile = new File(args[i+1]);
					final String parent = outputFile.getParent();
					if (parent != null && !new File(parent).canWrite()) {
						pw.write(Messages.getString("CommandLineLauncher.3") + " " + args[i+1]);  //$NON-NLS-1$//$NON-NLS-2$
						pw.flush();
						pw.close();
						System.exit(-1);
					}
					i++;
				}
				if (PARAM_FORMAT.equals(args[i])) {
					format = args[i+1].toLowerCase();
					if (!format.equals(FORMAT_XADES) &&
						!format.equals(FORMAT_CADES) &&
						!format.equals(FORMAT_PADES) &&
						!format.equals(FORMAT_FACTURAE) &&
						!format.equals(FORMAT_AUTO)) {
							pw.write(Messages.getString("CommandLineLauncher.4") + args[i+1]); //$NON-NLS-1$
							pw.flush();
							pw.close();
							System.exit(-1);
					}
					i++;
				}
			}
			if (store == null) {
				store = STORE_AUTO;
			}
			if (COMMAND_LIST.equals(command)) {
				pw.write(listAliases(store, password));
				pw.flush();
				pw.close();
				System.exit(0);
			}
			if (format == null) {
				format = FORMAT_AUTO;
			}
			if (inputFile == null) {
				pw.write(Messages.getString("CommandLineLauncher.5")); //$NON-NLS-1$
				pw.flush();
				pw.close();
				System.exit(-1);
			}
		}
		catch(final ArrayIndexOutOfBoundsException e) {
			syntaxError(pw);
		}

		pw.flush();
		pw.close();
	}

	private static String listAliases(final String store, final String password) {
		final String[] aliases;
		try {
			aliases = getKsm(store, password).getAliases();
		}
		catch (final Exception e) {
			return Messages.getString("CommandLineLauncher.6") + e; //$NON-NLS-1$
		}
		final StringBuilder sb = new StringBuilder();
		for (final String alias : aliases) {
			sb.append(alias);
			sb.append('\n');
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
			Messages.getString("CommandLineLauncher.7") + ": SimpleAfirma cmd [options...]\n" +  //$NON-NLS-1$//$NON-NLS-2$
			"cmd\n" + //$NON-NLS-1$
			"  sign        (" + Messages.getString("CommandLineLauncher.8") + ")\n" + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			"  cosign      (" + Messages.getString("CommandLineLauncher.9") + ")\n" + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			"  countersign (" + Messages.getString("CommandLineLauncher.10") + ")\n" + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			"  " + COMMAND_LIST + " (" + Messages.getString("CommandLineLauncher.11") + ")\n" +  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			"options\n" + //$NON-NLS-1$
			"  " + PARAM_STORE  + " auto|windows|mac|mozilla|dni|pkcs12:p12file\n" + //$NON-NLS-1$ //$NON-NLS-2$
			"  " + PARAM_PASSWD + " storepassword (" + Messages.getString("CommandLineLauncher.12") + ")\n" +  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			"  " + PARAM_FORMAT + " auto|xades|cades|pades|facturae\n" +  //$NON-NLS-1$//$NON-NLS-2$
			"  " + PARAM_INPUT  + " inputfile (" + Messages.getString("CommandLineLauncher.13") + ")\n" +  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			"  " + PARAM_OUTPUT + " outputfile (" + Messages.getString("CommandLineLauncher.14") + ")\n" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		);
		pw.flush();
		pw.close();
		System.exit(-1);
	}

	public static void main(final String[] args) {
		processCommandLine(args);
	}

}

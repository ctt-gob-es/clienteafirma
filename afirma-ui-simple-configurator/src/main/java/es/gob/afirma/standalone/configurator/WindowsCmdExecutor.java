package es.gob.afirma.standalone.configurator;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Collections;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import es.gob.afirma.standalone.configurator.common.ConfiguratorUtil;

/**
 * Clase para la ejecuci&oacute;n de scripts de Windows.
 */
public class WindowsCmdExecutor {

	private static final String ES_GOB_AFIRMA = "es.gob.afirma"; //$NON-NLS-1$

	/**
	 * Copia a una ruta temporal un scripts interno del m&oacute;dulo.
	 * @param resourcePath Ruta interna del script.
	 * @return Ruta del fichero guardado.
	 * @throws IOException Cuando se produce un error al guardar.
	 */
	public static Path copyCmdFromResources(final String resourcePath) throws IOException {

		final Path tempFile;
		try (final InputStream inputStream = WindowsCmdExecutor.class
				.getClassLoader()
				.getResourceAsStream(resourcePath)) {

			if (inputStream == null) {
				throw new FileNotFoundException("No se encontro el recurso: " + resourcePath); //$NON-NLS-1$
			}

			// Creamos una copia local del script usando la misma extension que el fichero original
			// porque, por ejemplo, PowerShell no ejecutara el script si la extension no es valida
			final String scriptName = new File(resourcePath).getName();
			final int extDotIdx = scriptName.lastIndexOf('.');
			final String ext = extDotIdx == -1 ? ".cmd" : scriptName.substring(extDotIdx); //$NON-NLS-1$

			tempFile = Files.createTempFile("script-", ext); //$NON-NLS-1$
			Files.copy(inputStream, tempFile, StandardCopyOption.REPLACE_EXISTING);
		}

		return tempFile;
	}

	/**
	 * Ejecuta un script p&aacute;sandole los par&aacute;metros proporcionados.
	 * @param cmdPath Ruta del script.
	 * @param params Listado de par&aacute;metros.
	 * @throws IOException Cuando ocurre un error durante la ejecuci&oacute;n.
	 * @throws InterruptedException Cuando se interrumpe la ejecuci&oacute;n de script.
	 */
	public static void executePathCmd(final Path cmdPath, final String... params)
			throws IOException, InterruptedException {
		executePathCmd(cmdPath, 0, params);
	}

	/**
	 * Ejecuta un script p&aacute;sandole los par&aacute;metros proporcionados.
	 * @param cmdPath Ruta del script.
	 * @param params Listado de par&aacute;metros.
	 * @throws IOException Cuando ocurre un error durante la ejecuci&oacute;n.
	 * @throws InterruptedException Cuando se interrumpe la ejecuci&oacute;n de script.
	 * @throws IllegalThreadStateException Si el proceso tard&oacute; m&aacute;s del tiempo establecido
	 * y se deja de esperar.
	 */
	public static void executePathCmd(final Path cmdPath, final long seconds, final String... params)
			throws IOException, InterruptedException, IllegalThreadStateException {

		final File workingDir = ConfiguratorUtil.getApplicationDirectory();

		final ArrayList<String> processParams = new ArrayList<>();

		processParams.add("cmd.exe"); //$NON-NLS-1$
		processParams.add("/c"); //$NON-NLS-1$
		processParams.add(cmdPath.toAbsolutePath().toString());
		if (params != null) {
			Collections.addAll(processParams, params);
		}

		final ProcessBuilder pb = new ProcessBuilder(processParams.toArray(new String[0]));

		pb.directory(workingDir);
		pb.redirectErrorStream(true);

		final Process p = pb.start();

		Logger.getLogger(ES_GOB_AFIRMA).info("Iniciamos la espera de la ejecucion del script " + cmdPath.toAbsolutePath().toString()); //$NON-NLS-1$

		int exitCode;
		if (seconds <= 0) {
			exitCode = p.waitFor();
		}
		else {
			if (!p.waitFor(seconds, TimeUnit.SECONDS)) {
				Logger.getLogger(ES_GOB_AFIRMA).info("El script tardo demasiado y se dejo de esperar"); //$NON-NLS-1$
			}
			try {
				exitCode = p.exitValue();
			}
			catch (final Exception e) {
				p.destroyForcibly();
				exitCode = 1;
			}
		}

		if (exitCode != 0) {
			throw new IOException("El CMD termino con error. ExitCode=" + exitCode); //$NON-NLS-1$
		}
	}


	/**
	 * Ejecuta un script PowerShell p&aacute;sandole los par&aacute;metros proporcionados.
	 * @param psPath Ruta del script.
	 * @param params Listado de par&aacute;metros.
	 * @throws IOException Cuando ocurre un error durante la ejecuci&oacute;n.
	 * @throws InterruptedException Cuando se interrumpe la ejecuci&oacute;n de script.
	 * @throws IllegalThreadStateException Si el proceso tard&oacute; m&aacute;s del tiempo establecido
	 * y se deja de esperar.
	 */
	public static void executePathPs(final Path psPath, final long seconds, final String... params)
			throws IOException, InterruptedException, IllegalThreadStateException {

		final File workingDir = ConfiguratorUtil.getApplicationDirectory();

		final ArrayList<String> processParams = new ArrayList<>();
		processParams.add("powershell.exe"); //$NON-NLS-1$
		processParams.add("-ExecutionPolicy"); //$NON-NLS-1$
		processParams.add("ByPass"); //$NON-NLS-1$
		processParams.add("-File"); //$NON-NLS-1$

		processParams.add(psPath.toFile().getCanonicalPath());
		if (params != null) {
			Collections.addAll(processParams, params);
		}

		final ProcessBuilder pb = new ProcessBuilder(processParams.toArray(new String[0]));

		pb.directory(workingDir);
		pb.redirectErrorStream(true);

		final Process p = pb.start();

		Logger.getLogger(ES_GOB_AFIRMA).info("Iniciamos la espera de la ejecucion del script " + psPath.toAbsolutePath().toString()); //$NON-NLS-1$

		int exitCode;
		if (seconds <= 0) {
			exitCode = p.waitFor();
		}
		else {
			if (!p.waitFor(seconds, TimeUnit.SECONDS)) {
				Logger.getLogger(ES_GOB_AFIRMA).info("El script tardo demasiado y se dejo de esperar"); //$NON-NLS-1$
			}
			try {
				exitCode = p.exitValue();
			}
			catch (final Exception e) {
				p.destroyForcibly();
				exitCode = 1;
			}
		}

		if (exitCode != 0) {
			throw new IOException("El script termino con error. ExitCode=" + exitCode); //$NON-NLS-1$
		}
	}
}

package es.gob.afirma.keystores.mozilla.apple;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.LoggerUtil;

/**
 * Clase para la ejecuci&oacute;n de scripts de consola.
 */
public class ShellScript {

	private static final Charset DEFAULT_CHARSET = StandardCharsets.UTF_8;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final String script;

	private final File scriptFile;

	private boolean deleteFile = false;

	/**
	 * Crea el objeto para la ejecuci&oacute;n de un script.
	 * @param scriptText Texto del script.
	 */
	public ShellScript(final String scriptText) {
		if (scriptText == null) {
			throw new IllegalArgumentException("El script de entrada no puede ser nulo"); //$NON-NLS-1$
		}
		this.script = scriptText;
		this.scriptFile = null;
	}

	/**
	 * Crea el objeto para la ejecuci&oacute;n de un script almacenado en un fichero.
	 * @param scriptFile Fichero con el script.
	 * @param deleteFile {@code true} si se debe eliminar el fichero tras la ejecuci&oacute;n,
	 * {@code false} en caso contrario.
	 */
	public ShellScript(final File scriptFile, final boolean deleteFile) {
		if (scriptFile == null) {
			throw new IllegalArgumentException("El fichero script de entrada no puede ser nulo"); //$NON-NLS-1$
		}
		this.script = null;
		this.scriptFile = scriptFile;
		this.deleteFile = deleteFile;
	}

	/**
	 * Ejecuta el script.
	 * @return Texto devuelto por el script o {@code null} si no se devolvi&oacute; nada.
	 * @throws IOException Cuando falla la ejecuci&oacute;n del script.
	 * @throws InterruptedException Cuando finaliza inesperadamente la ejecuci&oacute;n del script.
	 */
	public String run() throws IOException, InterruptedException {
		return run(false);
	}

	/**
	 * Ejecuta el script con permisos de administrador.
	 * @return Texto devuelto por el script o {@code null} si no se devolvi&oacute; nada.
	 * @throws IOException Cuando falla la ejecuci&oacute;n del script.
	 * @throws InterruptedException Cuando finaliza inesperadamente la ejecuci&oacute;n del script.
	 */
	public String runAsAdministrator() throws IOException, InterruptedException {
		return run(true);
	}

	/**
	 * Ejecuta el script.
	 * @param asAdmin {@code true} si el script debe ejecutarse como administrador,
	 * {@code false} en caso contrario.
	 * @return Texto devuelto por el script o {@code null} si no se devolvi&oacute; nada.
	 * @throws IOException Cuando falla la ejecuci&oacute;n del script.
	 * @throws InterruptedException Cuando finaliza inesperadamente la ejecuci&oacute;n del script.
	 */
	private String run(final boolean asAdmin) throws IOException, InterruptedException {

		final List<String> params = new ArrayList<>();
		File scriptToRun = this.scriptFile;
		File tempScript = null;

		if (scriptToRun == null) {
			tempScript = File.createTempFile("afirma-shell-", ".sh"); //$NON-NLS-1$ //$NON-NLS-2$
			tempScript.deleteOnExit();
			Files.write(tempScript.toPath(), this.script.getBytes(DEFAULT_CHARSET));
			if (!tempScript.setExecutable(true, true)) {
				LOGGER.fine("No se pudo marcar el script temporal como ejecutable"); //$NON-NLS-1$
			}
			scriptToRun = tempScript;
		}

		final String canonical = scriptToRun.getCanonicalPath();
		if (canonical.indexOf('"') >= 0 || canonical.indexOf('$') >= 0 || canonical.indexOf('`') >= 0) {
			throw new IOException("Ruta de script no permitida"); //$NON-NLS-1$
		}

		if (!asAdmin) {
			params.add("/bin/bash"); //$NON-NLS-1$
			params.add(canonical);
		}
		else {
			params.add("/usr/bin/osascript"); //$NON-NLS-1$
			params.add("-e"); //$NON-NLS-1$
			final String escaped = canonical.replace("\\", "\\\\").replace("\"", "\\\""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
			params.add("do shell script \"/bin/bash " + escaped + "\" with administrator privileges"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		LOGGER.fine("Ejecutando script: " + canonical + (asAdmin ? " (admin)" : "")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

		final ProcessBuilder processBuilder = new ProcessBuilder(params);
		final Process process = processBuilder.start();
		final int processResult = process.waitFor();
		if (processResult != 0) {
			byte[] errorOutput = null;
			try (final InputStream errorStream = process.getErrorStream()) {
				errorOutput = AOUtil.getDataFromInputStream(errorStream);
			}
			LOGGER.warning("El script finalizo con un error y la salida : " + ( //$NON-NLS-1$
					errorOutput != null ? LoggerUtil.getTrimBytes(errorOutput) : "")); //$NON-NLS-1$
			throw new IOException("La ejecucion del script devolvio el codigo de finalizacion: " + processResult); //$NON-NLS-1$
		}

		byte[] output = null;
		try (final InputStream standardStream = process.getInputStream()) {
			output = AOUtil.getDataFromInputStream(standardStream);
		}

		if (this.deleteFile && this.scriptFile != null) {
			Files.delete(this.scriptFile.toPath());
		}
		if (tempScript != null) {
			Files.deleteIfExists(tempScript.toPath());
		}

		return output != null ? new String(output, DEFAULT_CHARSET) : null;
	}
}

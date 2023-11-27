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
		params.add("/usr/bin/osascript"); //$NON-NLS-1$
		params.add("-e"); //$NON-NLS-1$


		String scriptText;
		if (this.scriptFile != null) {
			scriptText = this.scriptFile.getAbsolutePath();
		} else {
			scriptText = this.script;
		}
		scriptText = "do shell script \"" + scriptText + "\""; //$NON-NLS-1$ //$NON-NLS-2$
		if (asAdmin) {
			scriptText += " with administrator privileges"; //$NON-NLS-1$
		}
		params.add(scriptText);

		LOGGER.fine("Ejecutando shell script: " + scriptText); //$NON-NLS-1$

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

		return output != null ? new String(output, DEFAULT_CHARSET) : null;
	}
}

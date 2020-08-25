package es.gob.afirma.standalone.so.macos;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;

/**
 * Clase para la ejecuci&oacute;n de Applet scripts.
 */
public class AppleScript {

	private static final Charset DEFAULT_CHARSET = StandardCharsets.UTF_8;

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final String script;

	public AppleScript(final String paramString) {
		if (paramString == null) {
			throw new IllegalArgumentException("El script de entrada no puede ser nulo"); //$NON-NLS-1$
		}
		this.script = paramString;
	}

	/**
	 * Ejecuta el script.
	 * @return Texto devuelto por el script o {@code null} si no se devolvi&oacute; nada.
	 * @throws IOException Cuando falla la ejecuci&oacute;n del script.
	 * @throws InterruptedException Cuando finaliza inesperadamente la ejecuci&oacute;n del script.
	 */
	public String run() throws IOException, InterruptedException {

		final List<String> params = new ArrayList<>();
		params.add("/usr/bin/osascript"); //$NON-NLS-1$
		params.add("-e"); //$NON-NLS-1$
		params.add(this.script);

		LOGGER.fine("Ejecutando apple script: " + this.script); //$NON-NLS-1$

		final ProcessBuilder processBuilder = new ProcessBuilder(params);
		final Process process = processBuilder.start();
		final int exitValue = process.waitFor();

		if (exitValue != 0) {
			byte[] errorOutput;
			try (final InputStream errorStream = process.getErrorStream()) {
				errorOutput = AOUtil.getDataFromInputStream(errorStream);
			}
			LOGGER.warning("Salida de error: " + (errorOutput != null ? new String(errorOutput) : "")); //$NON-NLS-1$ //$NON-NLS-2$
			throw new IOException("La ejecucion del script devolvio el codigo de finalizacion: " + exitValue); //$NON-NLS-1$
		}

		byte[] output;
		try (final InputStream inputStream = process.getInputStream()) {
			output = AOUtil.getDataFromInputStream(inputStream);
		}
		return output != null ? new String(output, DEFAULT_CHARSET) : null;
	}
}
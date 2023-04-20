/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.server.webstart;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Servicio para la obtenci&oacute;n de un fichero de despliegue JNLP
 * al que se le proporciona un argumento proporcionado en la llamada.
 */
public class AutoFirmaJnlpService extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static final String COMPLETE_INSTALATION_TEMPLATE = "autofirma_complete.jnlp"; //$NON-NLS-1$
	private static final String PROTOCOL_CONSUMER_TEMPLATE = "autofirma.jnlp"; //$NON-NLS-1$

	private static final String PARAM_COMPLETE = "cp"; //$NON-NLS-1$
	private static final String PARAM_ARGUMENT = "arg"; //$NON-NLS-1$
	private static final String PARAM_OS_NAME = "os"; //$NON-NLS-1$

	private static final String OS_WINDOWS = "windows"; //$NON-NLS-1$
	private static final String OS_LINUX = "linux"; //$NON-NLS-1$
	private static final String OS_MAC = "mac"; //$NON-NLS-1$

	private static final String TEMPLATE_REPLACE_OS_JAR_REFERENCE = "%OSJAR%"; //$NON-NLS-1$
	private static final String TEMPLATE_REPLACE_OS_NAME = "%OSNAME%"; //$NON-NLS-1$
	private static final String TEMPLATE_REPLACE_CODEBASE = "%CODEBASE%"; //$NON-NLS-1$
	private static final String TEMPLATE_REPLACE_ARGUMENT = "%ARGUMENT%"; //$NON-NLS-1$

	private static final String ARGUMENT_PREFIX = "afirma://"; //$NON-NLS-1$

	private static final String TEMPLATE_NODE_REFERENCE = "<jar href=\"%CODEBASE%%OSNAME%.jar\"/>"; //$NON-NLS-1$

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	@Override
	protected void service(final HttpServletRequest request, final HttpServletResponse response) throws ServletException, IOException {

		final boolean complete = Boolean.parseBoolean(request.getParameter(PARAM_COMPLETE));
		final String arg = request.getParameter(PARAM_ARGUMENT);
		final String osName = request.getParameter(PARAM_OS_NAME);
		final String codebase = ConfigManager.getCodeBase();

		// Cargamos la plantilla del JNLP
		String template = loadJnlpTemplate(complete);

		// Agregamos al JNLP los recursos propios del sistema operativo que nos hayan indicado
		if (OS_WINDOWS.equalsIgnoreCase(osName) ||
				OS_LINUX.equalsIgnoreCase(osName) ||
				OS_MAC.equalsIgnoreCase(osName)) {
			template = template.replace(TEMPLATE_REPLACE_OS_JAR_REFERENCE,
					TEMPLATE_NODE_REFERENCE.replace(TEMPLATE_REPLACE_OS_NAME, osName));
		} else {
			template = template.replace(TEMPLATE_REPLACE_OS_JAR_REFERENCE, ""); //$NON-NLS-1$
		}

		// Devolvemos el JNLP
		response.addHeader("Content-Type", "application/x-java-jnlp-file"); //$NON-NLS-1$ //$NON-NLS-2$
		response.getWriter().append(
				template
				.replace(TEMPLATE_REPLACE_CODEBASE, codebase)
				.replace(TEMPLATE_REPLACE_ARGUMENT, arg != null && !arg.isEmpty() && arg.startsWith(ARGUMENT_PREFIX) ?
						"<argument>" + new String(Base64.decode(arg, true)) + "</argument>" : "")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		response.flushBuffer();
	}

	/**
	 * Carga la plantilla del JNLP en la que sobreescribir el codebase del JAR WebStart
	 * y el argumento a proporcionarle.
	 * @return Plantilla JNLP.
	 * @throws IOException Cuando ocurre un error en la lectura.
	 */
	private static String loadJnlpTemplate(final boolean complete) throws IOException {

		final String template = complete ? COMPLETE_INSTALATION_TEMPLATE : PROTOCOL_CONSUMER_TEMPLATE;

		final InputStream templateIs = AutoFirmaJnlpService.class.getClassLoader().getResourceAsStream(template);
		final byte[] content = readInputStream(templateIs);
		templateIs.close();

		return new String(content);
	}

	/**
	 * Lee el contenido de un flujo de datos.
	 * @param is Flujo de datos de entrada.
	 * @return Contenido del flujo de datos.
	 * @throws IOException Cuando ocurre un error en la lectura.
	 */
	private static byte[] readInputStream(final InputStream is) throws IOException {

		int n;
		final byte[] buffer = new byte[2048];
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		while ((n = is.read(buffer)) > 0) {
			baos.write(buffer, 0, n);
		}
		return baos.toByteArray();
	}
}

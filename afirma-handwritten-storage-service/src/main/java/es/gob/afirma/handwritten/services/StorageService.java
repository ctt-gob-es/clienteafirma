package es.gob.afirma.handwritten.services;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import es.gob.afirma.core.misc.Base64;

/**
 * Servlet implementation class StorageService
 */
public final class StorageService extends HttpServlet {

	private static final long serialVersionUID = -3272368448371213403L;

	/** Log para registrar las acciones del servicio. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

	/** Nombre del par&aacute;metro con los datos. */
	private static final String PARAMETER_NAME_DATA = "data"; //$NON-NLS-1$

	@Override
	protected void service(final HttpServletRequest request, final HttpServletResponse response) throws ServletException, IOException {

		LOGGER.info("Se recibe una peticion de almacenamiento"); //$NON-NLS-1$

		final String dataB64 = request.getParameter(PARAMETER_NAME_DATA);

		response.setHeader("Access-Control-Allow-Origin", "*"); //$NON-NLS-1$ //$NON-NLS-2$
		response.setContentType("text/plain"); //$NON-NLS-1$
		response.setCharacterEncoding("utf-8"); //$NON-NLS-1$

		final PrintWriter out = response.getWriter();

		if (dataB64 == null) {
			LOGGER.warning("No se han obtenido los datos en servidor"); //$NON-NLS-1$
			out.println("No se han obtenido los datos en servidor"); //$NON-NLS-1$
			out.close();
			return;
		}

		File dataFile = File.createTempFile("handwritten-", ".pdf"); //$NON-NLS-1$ //$NON-NLS-2$
		FileOutputStream fos = new FileOutputStream(dataFile);
		fos.write(Base64.decode(dataB64, Base64.URL_SAFE));
		fos.close();
		LOGGER.info("Los datos recibidos se han guardado en: " + dataFile.getAbsolutePath()); //$NON-NLS-1$

		out.write("OK"); //$NON-NLS-1$

		out.close();
	}

}

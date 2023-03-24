package es.gob.afirma.triphase.server;

import java.io.IOException;
import java.io.InputStream;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Servicio para la consulta de la versi&oacute;n.
 */
public class VersionService extends HttpServlet {

	/** Serial Id. */
	private static final long serialVersionUID = 4540594536500777002L;

	private static final String VERSION_RESOURCE = "/version"; //$NON-NLS-1$
	private static final String CHARSET = "utf-8"; //$NON-NLS-1$

	@Override
	protected void doGet(final HttpServletRequest req, final HttpServletResponse resp) throws ServletException, IOException {

		final byte[] version = new byte[64];
		try (InputStream is = getClass().getResourceAsStream(VERSION_RESOURCE)) {
			is.read(version);
		}

		resp.getWriter().print(new String(version, CHARSET).trim());
		resp.getWriter().flush();
	}
}

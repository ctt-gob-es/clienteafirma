package es.gob.afirma.miniapplet.report;

import java.io.IOException;
import java.util.logging.Logger;

import javax.servlet.ServletOutputStream;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Servlet implementation class ExtractApplet
 */
@WebServlet("/ExtractDataServlet")
public class ExtractDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /**
     * @see HttpServlet#HttpServlet()
     */
    public ExtractDataServlet() {
        super();
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	@Override
	protected void doGet(final HttpServletRequest request, final HttpServletResponse response) {

		byte[] data = ExtractReport.getReport();
		if(data == null) {
			LOGGER.severe("Error, nos se han podido extraer los datos de la base de datos."); //$NON-NLS-1$
			return;
		}

		response.setContentType("text/csv"); //$NON-NLS-1$
		response.setHeader("Content-Disposition", "attachment; filename=data.csv"); //$NON-NLS-1$ //$NON-NLS-2$
		response.setContentLength(data.length);
		ServletOutputStream out;
		try {
			out = response.getOutputStream();
			out.write(data);
		} catch (IOException e) {
			LOGGER.severe("Error escribiendo los datos en el fichero CSV: " + e); //$NON-NLS-1$
		}

	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	@Override
	protected void doPost(final HttpServletRequest request, final HttpServletResponse response)  {

	}

}

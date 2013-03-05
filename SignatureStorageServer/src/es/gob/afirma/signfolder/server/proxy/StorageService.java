package es.gob.afirma.signfolder.server.proxy;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Properties;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/** Servicio de almacenamiento temporal de firmas. &Uacute;til para servir de intermediario en comunicaci&oacute;n
 * entre JavaScript y <i>Apps</i> m&oacute;viles nativas.
 * @author Tom&aacute;s Garc&iacute;a-;er&aacute;s */
@WebServlet(description = "Servicio de almacenamiento temporal de firmas", urlPatterns = { "/StorageService" })
public final class StorageService extends HttpServlet {

	private static final long serialVersionUID = -3272368448371213403L;

	/** Fichero de configuraci&oacute;n. */
	private static final String CONFIG_FILE = "/configuration.properties"; //$NON-NLS-1$

	/** Log para registrar las acciones del servicio. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");  //$NON-NLS-1$

	/** Nombre del par&aacute;metro con la operaci&oacute;n realizada. */
	private static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro con el identificador del fichero temporal. */
	private static final String PARAMETER_NAME_ID = "id"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro con la versi&oacute;n de la sintaxis de petici&oacute; utilizada. */
	private static final String PARAMETER_NAME_SYNTAX_VERSION = "v"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro con los datos a firmar. */
	private static final String PARAMETER_NAME_DATA = "dat"; //$NON-NLS-1$

	private static final String OPERATION_STORE = "put"; //$NON-NLS-1$
	private static final String SUCCESS = "OK"; //$NON-NLS-1$

	@Override
	public void init() throws ServletException {
		super.init();

		final String WARNING_CONFIG_MSG = "Se utilizara el directorio temporal para el guardado de los ficheros de intercambio, lo cual puede constituir un problema de seguridad. Configure un directorio con permisos controlados para mayor seguridad."; //$NON-NLS-1$
		final Properties p = new Properties();
		try {
			final InputStream is = this.getServletContext().getResourceAsStream(CONFIG_FILE);
			p.load(is);
			is.close();
		} catch (final IOException e) {
			Logger.getLogger("es.gob.afirma").severe("No se ha podido cargar el fichero con la configuracion del servicio: " + e.toString()); //$NON-NLS-1$ //$NON-NLS-2$
			Logger.getLogger("es.gob.afirma").warning(WARNING_CONFIG_MSG); //$NON-NLS-1$
			return;
		}

		if (!p.containsKey(StorageConfig.TMP_DIR_KEY)) {
			Logger.getLogger("es.gob.afirma").warning("No se ha encontrado un valor definido para el directorio de intercambio de ficheros."); //$NON-NLS-1$ //$NON-NLS-2$
			Logger.getLogger("es.gob.afirma").warning(WARNING_CONFIG_MSG); //$NON-NLS-1$
		}
	}

	@Override
	protected void service(final HttpServletRequest request, final HttpServletResponse response) throws ServletException, IOException {

		LOGGER.info("Se recibe una peticion de almacenamiento"); //$NON-NLS-1$

		final String operation = request.getParameter(PARAMETER_NAME_OPERATION);
		final String syntaxVersion = request.getParameter(PARAMETER_NAME_SYNTAX_VERSION);
		response.setContentType("text/plain"); //$NON-NLS-1$
		response.setCharacterEncoding("utf-8"); //$NON-NLS-1$

		final PrintWriter out = response.getWriter();
		if (operation == null) {
			out.println(ErrorManager.genError(ErrorManager.ERROR_MISSING_OPERATION_NAME, null));
			return;
		}
		if (syntaxVersion == null) {
			out.println(ErrorManager.genError(ErrorManager.ERROR_MISSING_SYNTAX_VERSION, null));
			return;
		}

		final StorageConfig config;
		try {
			config = new StorageConfig(this.getServletContext());
			config.load(CONFIG_FILE);
		} catch (final IOException e) {
			LOGGER.severe(ErrorManager.genError(ErrorManager.ERROR_CONFIGURATION_FILE_PROBLEM, null));
			out.println(ErrorManager.genError(ErrorManager.ERROR_CONFIGURATION_FILE_PROBLEM, null));
			return;
		}

		if (OPERATION_STORE.equalsIgnoreCase(operation)) {
			storeSign(out, request, config);
		} else {
			out.println(ErrorManager.genError(ErrorManager.ERROR_UNSUPPORTED_OPERATION_NAME, null));
		}
		out.close();
	}

	/**
	 * Almacena una firma en servidor.
	 * @param response Respuesta a la petici&oacute;n.
	 * @param request Petici&oacute;n.
	 * @throws IOException Cuando ocurre un error al general la respuesta.
	 */
	private static void storeSign(final PrintWriter out, final HttpServletRequest request, final StorageConfig config) throws IOException {

		LOGGER.info("Solicitud de guardado"); //$NON-NLS-1$

		final String id = request.getParameter(PARAMETER_NAME_ID);
		if (id == null) {
			LOGGER.severe(ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA_ID, null));
			out.println(ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA_ID, null));
			return;
		}

		LOGGER.info("Se solicita guardar un fichero con el identificador: " + id); //$NON-NLS-1$

		// Si no se indican los datos, se transmite el error en texto plano a traves del fichero generado
		String dataText = request.getParameter(PARAMETER_NAME_DATA);
		if (dataText == null) {
			LOGGER.severe(ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA, null));
			dataText = ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA, null);
		}

		final byte[] data = dataText.getBytes();

		if (!config.getTempDir().exists()) {
			config.getTempDir().mkdirs();
		}

		final File outFile = new File(config.getTempDir(), id);
		try {
			final OutputStream fos = new FileOutputStream(outFile);
			fos.write(data);
			fos.flush();
			fos.close();
		} catch (final IOException e) {
			LOGGER.severe(ErrorManager.genError(ErrorManager.ERROR_COMMUNICATING_WITH_WEB, null));
			out.println(ErrorManager.genError(ErrorManager.ERROR_COMMUNICATING_WITH_WEB, null));
			return;
		}

		LOGGER.info("Se guardo correctamente el fichero: " + outFile.getAbsolutePath()); //$NON-NLS-1$

		out.print(SUCCESS);
	}
}
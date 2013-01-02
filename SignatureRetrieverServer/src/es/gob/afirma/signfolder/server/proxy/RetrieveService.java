package es.gob.afirma.signfolder.server.proxy;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/** Servicio de almacenamiento temporal de firmas. &Uacute;til para servir de intermediario en comunicaci&oacute;n
 * entre JavaScript y <i>Apps</i> m&oacute;viles nativas.
 * @author Tom&aacute;s Garc&iacute;a-;er&aacute;s */
@WebServlet(description = "Servicio para la recuperacion de firmas", urlPatterns = { "/RetrieverService" })
public final class RetrieveService extends HttpServlet {

	private static final long serialVersionUID = -3272368448371213403L;

	/** Fichero de configuraci&oacute;n. */
	private static final String CONFIG_FILE = "/configuration.properties"; //$NON-NLS-1$

	/** Juego de carateres UTF-8. */
	private static final String UTF8 = "utf-8"; //$NON-NLS-1$

	private static final String TMP_DIR = System.getProperty("java.io.tmpdir") + File.separator + "afirma"; //$NON-NLS-1$ //$NON-NLS-2$

	private static final String ERROR_PREFIX = "err-"; //$NON-NLS-1$

	private static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_DATA = "dat"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_ID = "id"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_CIPHER_KEY = "key"; //$NON-NLS-1$

	/** Parametro para indicar la versiond el protocolo utilizado */
	private static final String PARAMETER_NAME_VERSION = "v"; //$NON-NLS-1$

	private static final String OPERATION_RETRIEVE = "get"; //$NON-NLS-1$

	private static final String SUCCESS = "OK"; //$NON-NLS-1$

	/** Sem&aacute;foro para evitar procesos de limpieza demasiado asiduos. */
	private static final Object removeProcessBlocker = new Integer(0);

	private static long lastRemoveDate = System.currentTimeMillis();

	@Override
	protected void service(final HttpServletRequest request, final HttpServletResponse response) throws ServletException, IOException {
		final String operation = request.getParameter(PARAMETER_NAME_OPERATION);
		response.setContentType("text/plain"); //$NON-NLS-1$
		response.setCharacterEncoding("iso-8859-15"); //$NON-NLS-1$

		try (final PrintWriter out = response.getWriter()) {
			if (operation == null) {
				out.println(ErrorManager.genError(ErrorManager.ERROR_MISSING_OPERATION_NAME, null));
				return;
			}

			System.out.println("Cargamos el fichero de propiedades");

			final RetrieveConfig config = new RetrieveConfig(this.getServletContext());
			config.load(CONFIG_FILE);

			System.out.println("Hemos cargado el fichero de propiedades");

			System.out.println("Directorio temporal: " + config.getTempDir());

			switch(operation) {
			case OPERATION_RETRIEVE:
				retrieveSign(out, request, config);
				return;
			default:
				out.println(ErrorManager.genError(ErrorManager.ERROR_UNSUPPORTED_OPERATION_NAME, null));
			}
		}
	}

	/**
	 * Recupera la firma del servidor.
	 * @param response Respuesta a la petici&oacute;n.
	 * @param request Petici&oacute;n.
	 * @throws IOException Cuando ocurre un error al general la respuesta.
	 */
	private static void retrieveSign(final PrintWriter out, final HttpServletRequest request, final RetrieveConfig config) throws IOException {

		Logger.getLogger("es.gob.afirma").info("Se va a recuperar la respuesta.");
		final StringBuilder sb = new StringBuilder("Parametros:\n");
		for (final String key : request.getParameterMap().keySet().toArray(new String[request.getParameterMap().size()])) {
			sb.append(key).append(": ").append(request.getParameter(key)).append("\n");
		}
		Logger.getLogger("es.gob.afirma").info(sb.toString());

		final String id = request.getParameter(PARAMETER_NAME_ID);
		if (id == null) {
			out.println(ErrorManager.genError(ErrorManager.ERROR_MISSING_DATA_ID, null));
			return;
		}
		final File inFile = new File(config.getTempDir(), request.getRemoteAddr().replace(":", "_") + "-" + id); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

		Logger.getLogger("es.gob.afirma").info("Recuperamos de: " + inFile.getAbsolutePath());

//		try(final InputStream fis = new FileInputStream(inFile);) {
//			byte[] data = AOUtil.getDataFromInputStream(fis);
//
//			Logger.getLogger("es.gob.afirma").info("Datos cifrados recogidos (mostrados en Base64): " + Base64.encode(data));
//			Logger.getLogger("es.gob.afirma").info("Devolvemos en hexadecimal el Base64 del mensaje cifrado: " + AOUtil.hexify(data, false));
//			out.println(AOUtil.hexify(data, false));
//
//			fis.close();
//			data = null;
//		}
//		catch (final FileNotFoundException e) {
//			out.println(ErrorManager.genError(ErrorManager.ERROR_INVALID_DATA_ID, null));
//			return;
//		}
//		catch (final IOException e) {
//			out.println(ErrorManager.genError(ErrorManager.ERROR_INVALID_DATA, null));
//		}
//
//		inFile.delete();



//		// Cuando se sobrepase el tiempo minimo de espera entre procesos de limpieza, se realizara un
//		// proceso de limpieza. Se tendra en cuenta que durante este tiempo es posible que se soliciten
//		// nuevos procesos. Se registran los tiempos de inicio y fin del proceso de limpieza
//		//TODO: Revisar
//		if ((System.currentTimeMillis() - lastRemoveDate) > config.getRemoveProcessInterval()) {
//			synchronized (removeProcessBlocker) {
//				if (System.currentTimeMillis() - lastRemoveDate > config.getRemoveProcessInterval()) {
//					lastRemoveDate = System.currentTimeMillis();
//					removeExpiredFiles(config);
//					lastRemoveDate = System.currentTimeMillis();
//				}
//			}
//		}

		// No hacemos distincion si el archivo no existe, no es un fichero, no puede leerse o ha caducado
		// para evitar que un atacante conozca su situacion. Lo borramos despuest de usarlo
		if (!inFile.exists() || !inFile.isFile() || !inFile.canRead()) {
			out.println(ErrorManager.genError(ErrorManager.ERROR_INVALID_DATA_ID, null));
			if (inFile.exists() && inFile.isFile()) {
				inFile.delete();
			}
			return;
		}

		try (final InputStream fis = new FileInputStream(inFile)) {
			out.println(new String(getDataFromInputStream(fis)));
		}
		catch (final IOException e) {
			out.println(ErrorManager.genError(ErrorManager.ERROR_INVALID_DATA, null));
			return;
		}
		inFile.delete();
	}

	/**
	 * Elimina del directorio temporal todos los ficheros que hayan sobrepasado el tiempo m&aacute;ximo
	 * de vida para los ficheros.
	 * @param config Opciones de configuraci&oacute;n de la operaci&oacute;n.
	 */
	private static void removeExpiredFiles(final RetrieveConfig config) {
		for (final File file : config.getTempDir().listFiles()) {
			if (System.currentTimeMillis() - file.lastModified() > config.getExpirationTime()) {
				file.delete();
			}
		}
	}

	private static final int BUFFER_SIZE = 4096;

	/** Lee un flujo de datos de entrada y los recupera en forma de array de
     * bytes. Este m&eacute;todo consume pero no cierra el flujo de datos de
     * entrada.
     * @param input
     *        Flujo de donde se toman los datos.
     * @return Los datos obtenidos del flujo.
     * @throws IOException
     *         Cuando ocurre un problema durante la lectura */
    private static byte[] getDataFromInputStream(final InputStream input) throws IOException {
        if (input == null) {
            return new byte[0];
        }
        int nBytes = 0;
        final byte[] buffer = new byte[BUFFER_SIZE];
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        while ((nBytes = input.read(buffer)) != -1) {
            baos.write(buffer, 0, nBytes);
        }
        return baos.toByteArray();
    }
}
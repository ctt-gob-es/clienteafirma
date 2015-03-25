package es.gob.afirma.webtests;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/** Guarda los resultados de test QUnit en el servidor.
 *  */
@WebServlet("/TestStorer")
public class TestStorer extends HttpServlet {
	private static final long serialVersionUID = 1L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$


    /** Constructor por defecto
     * @see HttpServlet#HttpServlet()
     */
    public TestStorer() {
        super();
    }

	/** M&eacute;todo para recibir los datos a trav&eacute;s del par&aecute;metro json
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	@Override
	protected void doPost(final HttpServletRequest request, final HttpServletResponse response) throws IOException {
		LOGGER.info(request.getParameter("json")); //$NON-NLS-1$
		JSONObject obj;
		List<StoreBean> sbList = new ArrayList<StoreBean>();

		try {
			JSONArray jsonarray = new JSONArray(request.getParameter("json")); //$NON-NLS-1$
			for(int i = 0; i < jsonarray.length(); i++) {
				obj = jsonarray.getJSONObject(i);


				StoreBean sb =
					new StoreBean(
						((Integer)obj.get("id")).intValue(), //$NON-NLS-1$
						(String) obj.get("testName"), //$NON-NLS-1$
						Boolean.parseBoolean(obj.get("result").toString()), //$NON-NLS-1$
						(String) obj.get("data"), //$NON-NLS-1$
						(String) obj.get("userAgent"), //$NON-NLS-1$
						System.currentTimeMillis(),
						getElementString("miniapplet_version", obj), //$NON-NLS-1$
						getElementString("browser", obj), //$NON-NLS-1$
						getElementString("browser_version", obj), //$NON-NLS-1$
						getElementString("os", obj), //$NON-NLS-1$
						getElementString("os_version", obj), //$NON-NLS-1$
						getElementString("java_version", obj), //$NON-NLS-1$
						getElementString("java_arch", obj) //$NON-NLS-1$
					);
				sbList.add(sb);
			}

		} catch (JSONException e) {
			LOGGER.severe("Error accediendo a los datos del JSON. " + e); //$NON-NLS-1$
			response.getWriter().println("Error accediendo a los datos del JSON. " + e); //$NON-NLS-1$
		}


		String content = ""; //$NON-NLS-1$
		for (int i = 0; i < sbList.size(); i++) {
			content = content + sbList.get(i).toHTML();
		}

		response.getWriter()
			.println(
				"<!DOCTYPE html><html>" + //$NON-NLS-1$
					"<head></head>" + //$NON-NLS-1$
					"<body>" +  content + "</body>" + //$NON-NLS-1$ //$NON-NLS-2$
				"</html>" //$NON-NLS-1$
			);

		LOGGER.info("Guardamos los datos en la BD"); //$NON-NLS-1$
		boolean res = SaveDataDB.storedData(sbList);
		if(res) {
			LOGGER.info("Datos almacenados correctamente"); //$NON-NLS-1$
		}
		else {
			LOGGER.severe("Error guardando los datos en la BD."); //$NON-NLS-1$
		}
	}

	private static String getElementString(final String elementTypem, final JSONObject jsonObject){
		String res;
		try{
			res = (String) jsonObject.get(elementTypem);
		}
		catch(Exception e) {
			res = ""; //$NON-NLS-1$
		}
		return res;
	}

}

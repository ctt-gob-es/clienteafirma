package es.gob.afirma.signers.batch.json;


import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONTokener;

import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseData.TriSign;

/**
 * Clase encargada de parsear datos de tipo JSON.
 * @author Jose.Montero
 *
 */
public class TriphaseDataParser {

	/**
	 * Carga datos de firma trif&aacute;sica.
	 * @param json Estructura de datos con la informacion de firma trif&aacute;sica.
	 * @return Informaci&oacute;n de firma trif&aacute;sica.
	 */
	public static TriphaseData parseFromJSON(final byte[] json) {

		final JSONObject jsonObject = new JSONObject(new JSONTokener(new ByteArrayInputStream(json)));
		return parseFromJSON(jsonObject);
	}

	/**
	 * Carga datos de firma trif&aacute;sica.
	 * @param jsonObject Objeto JSON con los datos a transformar.
	 * @return Informaci&oacute;n de firma trif&aacute;sica.
	 */
	public static TriphaseData parseFromJSON(final JSONObject jsonObject) {
		JSONArray signsArray = null;
		if (jsonObject.has("signs")) { //$NON-NLS-1$
			signsArray = jsonObject.getJSONArray("signs"); //$NON-NLS-1$
		}

		String format = null;
		if (jsonObject.has("format")) { //$NON-NLS-1$
			format = jsonObject.getString("format"); //$NON-NLS-1$
		}

		final List<TriSign> triSigns = new ArrayList<>();

		if (signsArray != null) {
			for (int i = 0 ; i < signsArray.length() ; i++) {
				final JSONObject sign = signsArray.getJSONObject(i);
				final JSONArray signInfo = sign.getJSONArray("signinfo"); //$NON-NLS-1$

				for (int j = 0; j < signInfo.length(); j++) {
					final String id = signInfo.getJSONObject(j).getString("id"); //$NON-NLS-1$
					final JSONObject params = signInfo.getJSONObject(j).getJSONObject("params"); //$NON-NLS-1$

					triSigns.add(new TriSign(parseParamsJSON(params), id));
				}
			}
		} else {
			final JSONArray signInfoArray = jsonObject.getJSONArray("signinfo"); //$NON-NLS-1$
			for (int i = 0 ; i < signInfoArray.length() ; i++) {
				final String id = signInfoArray.getJSONObject(i).getString("id"); //$NON-NLS-1$
				final JSONObject params = signInfoArray.getJSONObject(i).getJSONObject("params"); //$NON-NLS-1$
				triSigns.add(new TriSign(parseParamsJSON(params),id));
			}
		}
		return new TriphaseData(triSigns,format);
	}

	/**
	 * Mapea los par&aacute;metros de las firmas.
	 * @param params par&aacute;metros a parsear.
	 * @return par&aacute;metros mapeados.
	 */
	private static Map<String, String> parseParamsJSON(final JSONObject params){

		final Map<String, String> paramsResult = new ConcurrentHashMap<>();

		final Map<String, Object> paramToMap = params.toMap();
		for (final String key : paramToMap.keySet()) {
			final String value = (String) paramToMap.get(key);
			paramsResult.put(key, value);
		}

		return paramsResult;
	}

	/**
	 * Genera un JSON con la descripci&oacute;n del mensaje trif&aacute;sico.
	 * @param td objeto con los datos a generar.
	 * @return JSON con la descripci&oacute;n.
	 * */
	public static JSONObject triphaseDataToJson(final TriphaseData td) {

		final JSONObject jsonObject = new JSONObject();
		jsonObject.put("format", td.getFormat()); //$NON-NLS-1$

		final JSONArray signInfos = new JSONArray();

		final Iterator<TriSign> firmasIt = td.getTriSigns().iterator();
		while (firmasIt.hasNext()) {
			final TriSign signConfig = firmasIt.next();

			final JSONObject signInfo = new JSONObject();

			// Agrefamos el identificador
			if (signConfig.getId() != null) {
				signInfo.put("id", signConfig.getId()); //$NON-NLS-1$
			}

			// Agregamos los parametros de la firma trifasica
			final JSONObject params = new JSONObject();
			final Iterator<String> firmaIt = signConfig.getDict().keySet().iterator();
			while (firmaIt.hasNext()) {
				final String p = firmaIt.next();
				params.put(p, signConfig.getProperty(p));
			}
			signInfo.put("params", params); //$NON-NLS-1$

			signInfos.put(signInfo);
		}

		jsonObject.put("signinfo", signInfos); //$NON-NLS-1$

		return jsonObject;
	}

}

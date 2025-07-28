package es.gob.afirma.signers.batch.client;


import java.io.ByteArrayInputStream;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;

import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONTokener;

import es.gob.afirma.core.misc.protocol.ParameterException;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseData.TriSign;

/**
 * Clase encargada de parsear datos de tipo JSON.
 * @author Jose.Montero
 *
 */
public class TriphaseDataParser {

	static final String DEFAULT_URL_ENCODING = StandardCharsets.UTF_8.name();

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
				final JSONArray signInfos = sign.getJSONArray("signinfo"); //$NON-NLS-1$

				for (int j = 0; j < signInfos.length(); j++) {
					final JSONObject signInfo = signInfos.getJSONObject(j);
					final String id = signInfo.getString("id"); //$NON-NLS-1$
					final String signId = signInfo.optString("signid", null); //$NON-NLS-1$
					final JSONObject params = signInfo.getJSONObject("params"); //$NON-NLS-1$

					triSigns.add(new TriSign(parseParamsJSON(params), id, signId));
				}
			}
		} else {
			final JSONArray signInfoArray = jsonObject.getJSONArray("signinfo"); //$NON-NLS-1$
			for (int i = 0 ; i < signInfoArray.length() ; i++) {
				final JSONObject signInfo = signInfoArray.getJSONObject(i);
				final String id = signInfo.getString("id"); //$NON-NLS-1$
				final String signId = signInfo.optString("signid", null); //$NON-NLS-1$
				final JSONObject params = signInfo.getJSONObject("params"); //$NON-NLS-1$
				triSigns.add(new TriSign(parseParamsJSON(params), id, signId));
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
	public static String triphaseDataToJsonString(final TriphaseData td) {

		final JSONArray signInfos = new JSONArray();
		final Iterator<TriSign> firmasIt = td.getTriSigns().iterator();
		while (firmasIt.hasNext()) {

			final JSONObject signInfo = new JSONObject();

			// Agregamos el identificador de la firma concreta (en las contrafirmas una firma puede tener varias firmas)
			final TriSign signConfig = firmasIt.next();
			if (signConfig.getId() != null) {
				signInfo.put("id", signConfig.getId()); //$NON-NLS-1$
			}
			// Agregamos el identificador de la firma concreta (en las contrafirmas una firma puede tener varias firmas)
			if (signConfig.getSignatureId() != null) {
				signInfo.put("signid", signConfig.getSignatureId()); //$NON-NLS-1$
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

		final JSONObject tdObject = new JSONObject();
		if (td.getFormat() != null) {
			tdObject.put("format", td.getFormat()); //$NON-NLS-1$
		}
		tdObject.put("signinfo", signInfos); //$NON-NLS-1$

		return tdObject.toString();
	}

	/** Analiza un JSON de entrada para obtener la lista de par&aacute;metros asociados
	 * @param json JSON con el listado de par&aacute;metros.
	 * @return Devuelve una tabla <i>hash</i> con cada par&aacute;metro asociado a un valor
	 * @throws ParameterException Cuando el JSON de entrada no es v&acute;lido. */
	public static Map<String, String> parseParamsListJson(final byte[] json) throws ParameterException {

		final Map<String, String> params = new HashMap<>();
		final JSONArray elems;

		final JSONObject jsonDoc = new JSONObject(new JSONTokener(new ByteArrayInputStream(json)));
		elems = jsonDoc.getJSONArray("params"); //$NON-NLS-1$

		for (int i = 0; i < elems.length(); i++) {
			final JSONObject element = elems.getJSONObject(i);
			if (!element.has("k") || !element.has("v")) { //$NON-NLS-1$ //$NON-NLS-2$
				throw new ParameterException("El JSON no tiene la forma esperada"); //$NON-NLS-1$
			}
			final String key = element.getString("k"); //$NON-NLS-1$
			final String value = element.getString("v"); //$NON-NLS-1$
			try {
				params.put(key, URLDecoder.decode(value, DEFAULT_URL_ENCODING));
			}
			catch (final UnsupportedEncodingException e) {
				Logger.getLogger("es.gob.afirma").warning("Codificacion no soportada para la URL (" + DEFAULT_URL_ENCODING + "): " + e);  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
				params.put(key, value);
			}
		}
		return params;
	}

}

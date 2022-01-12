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
	 * Transforma un array de bytes en datos trif&aacute;s.
	 * @param json datos a transformar.
	 * @return datos transformados y estructurados en objeto trif&aacute;sico.
	 */
	public static TriphaseData parseFromJSON(final byte[] json) {

		final JSONObject jsonObject = new JSONObject(new JSONTokener(new ByteArrayInputStream(json)));
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

				final String id = signInfo.getJSONObject(0).getString("Id"); //$NON-NLS-1$
				final JSONArray params = signInfo.getJSONObject(0).getJSONArray("params"); //$NON-NLS-1$

				triSigns.add(new TriSign(parseParamsJSON(params),id));
			}
		} else {
			final JSONArray signInfoArray = jsonObject.getJSONArray("signinfo"); //$NON-NLS-1$
			for (int i = 0 ; i < signInfoArray.length() ; i++) {
				final String id = signInfoArray.getJSONObject(i).getString("Id"); //$NON-NLS-1$
				final JSONArray params = signInfoArray.getJSONObject(i).getJSONArray("params"); //$NON-NLS-1$
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
	private static Map<String, String> parseParamsJSON(final JSONArray params){

		final Map<String, String> paramsResult = new ConcurrentHashMap<>();

		for (int i = 0; i < params.length() ; i++) {
			final JSONObject param = params.getJSONObject(i);
			final Map<String, Object> paramToMap = param.toMap();
			for (final String key : paramToMap.keySet()) {
			    final String value = (String) paramToMap.get(key);
			    paramsResult.put(key, value);
			}
		}

		return paramsResult;
	}

	/**
	 * Genera un JSON con la descripci&oacute;n del mensaje trif&aacute;sico.
	 * @param td objeto con los datos a generar.
	 * @return JSON con la descripci&oacute;n.
	 * */
	public static String triphaseDataToJsonString(final TriphaseData td) {

		final StringBuilder builder = new StringBuilder();
		builder.append("{\n"); //$NON-NLS-1$
		if (td.getFormat() != null) {
			builder.append(" \"format\":\""); //$NON-NLS-1$
			builder.append(td.getFormat());
			builder.append("\","); //$NON-NLS-1$
		}
		builder.append("\n\"signinfo\":["); //$NON-NLS-1$
		final Iterator<TriSign> firmasIt = td.getTriSigns().iterator();
		while (firmasIt.hasNext()) {
			final TriSign signConfig = firmasIt.next();
			builder.append("{\n"); //$NON-NLS-1$

			if (signConfig.getId() != null) {
				builder.append(" \"Id\":\""); //$NON-NLS-1$
				builder.append(signConfig.getId());
				builder.append("\""); //$NON-NLS-1$
			}

			builder.append(",\n\"params\":\n[{\n"); //$NON-NLS-1$
			final Iterator<String> firmaIt = signConfig.getDict().keySet().iterator();
			while (firmaIt.hasNext()) {
				final String p = firmaIt.next();
				builder.append("\n\"") //$NON-NLS-1$
					.append(p)
						.append("\":\"") //$NON-NLS-1$
							.append(signConfig.getProperty(p))
								.append("\"\n"); //$NON-NLS-1$
				if(firmaIt.hasNext()) {
					builder.append(","); //$NON-NLS-1$
				}
			}
			builder.append("  }]\n"); //$NON-NLS-1$
			if(firmasIt.hasNext()) {
				builder.append("},\n"); //$NON-NLS-1$
			}
		}
		builder.append(" }\n]\n}"); //$NON-NLS-1$
		return builder.toString();
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

package es.gob.afirma.signers.batch.client;


import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.protocol.ParameterException;
import es.gob.afirma.core.misc.protocol.UrlParametersToSign;

/**
 * Clase encargada de parsear datos de tipo JSON para la firma de lotes monof&aacute;sica.
 * @author Jose.Montero
 */
public class LocalDataParser {

	static final String DEFAULT_URL_ENCODING = StandardCharsets.UTF_8.name();

	/**
	 * Transforma un objeto json en datos procesados para llamar a la firma monof&aacute;sica por lotes.
	 * @param json datos a transformar.
	 * @return datos transformados y estructurados en datos para firma de lotes monof&aacute;sica.
	 * @throws ParameterException error al indicar alguno de los par&aacute;metros
	 * @throws IOException error de entrada o salida al decodificar a base 64
	 * @throws JSONException error al parsear JSON
	 */
	public static List<UrlParametersToSign> parseJSONToUrlParamsToSign(final JSONObject json) throws ParameterException, JSONException, IOException {

		final List<UrlParametersToSign> result = new ArrayList<UrlParametersToSign>();

		final JSONArray singleSignsArray = json.getJSONArray("singlesigns"); //$NON-NLS-1$
		if (singleSignsArray != null) {
			for (int i = 0 ; i < singleSignsArray.length() ; i++) {
				final UrlParametersToSign urlParams = new UrlParametersToSign();
				final Map<String, String> params = new HashMap<String, String>();
				params.put("id", singleSignsArray.getJSONObject(i).getString("id"));
				urlParams.setData(Base64.decode(singleSignsArray.getJSONObject(i).getString("datareference")));
				params.put("op", singleSignsArray.getJSONObject(i).has("suboperation") ?
									singleSignsArray.getJSONObject(i).getString("suboperation")
									: json.getString("suboperation")
				);
				params.put("format", singleSignsArray.getJSONObject(i).has("format") ?
						singleSignsArray.getJSONObject(i).getString("format")
						: json.getString("format")
				);
				params.put("algorithm", json.getString("algorithm"));
				params.put("properties", singleSignsArray.getJSONObject(i).has("extraParams") ?
						singleSignsArray.getJSONObject(i).getString("extraParams")
						: json.has("extraparams") ? json.getString("extraparams") : null
						);
				urlParams.setSignParameters(params);
				result.add(urlParams);
			}
		}
		return result;
	}

}

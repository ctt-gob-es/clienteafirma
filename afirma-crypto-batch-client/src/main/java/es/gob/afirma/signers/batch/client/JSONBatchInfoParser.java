package es.gob.afirma.signers.batch.client;

import java.io.ByteArrayInputStream;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

public class JSONBatchInfoParser {

	private static final String JSELEMENT_SIGNS = "signs"; //$NON-NLS-1$
	private static final String JSELEMENT_ID = "id"; //$NON-NLS-1$
	private static final String JSELEMENT_RESULT = "result"; //$NON-NLS-1$
	private static final String JSELEMENT_DESCRIPTION = "description"; //$NON-NLS-1$

	/**
	 * Parsea un objeto JSON para obtener la informaci&oacute;n del lote.
	 * @param json JSON con la informacion del lote.
	 * @return Objeto con la informaci&oacute;n del lote.
	 * @throws JSONException Cuando el JSON no esta bien formado.
	 */
	static BatchInfo parse(final byte[] json) throws JSONException {

		JSONObject jsonObject = null;
		try {
			jsonObject = new JSONObject(new JSONTokener(new ByteArrayInputStream(json)));
		} catch (final JSONException e){
			throw new JSONException("El JSON del lote de firmas no esta formado correctamente", e); //$NON-NLS-1$
		}

		return new JSONBatchInfo(jsonObject);
	}

	/**
	 * Construye un JSON con un resultado vac&iacute;o de firma del lote.
	 * @return JSON con el resultado.
	 */
	static JSONObject buildEmptyResult() {
		final JSONObject mainObject = new JSONObject();
		mainObject.put(JSELEMENT_SIGNS, new JSONArray());

		return mainObject;
	}

	/**
	 * Construye un JSON con el resultado de firma del lote con los resultados individuales
	 * proporcionados.
	 * @param results Resultados individuales.
	 * @return JSON con el resultado.
	 */
	static JSONObject buildResult(final List<BatchDataResult> results) {

		final JSONArray signsObject = new JSONArray();
		for (final BatchDataResult result : results) {
			final JSONObject resultObject = new JSONObject();
			resultObject.put(JSELEMENT_ID, result.getId());
			resultObject.put(JSELEMENT_RESULT, result.getResult().name());
			if (result.getDescription() != null) {
				resultObject.put(JSELEMENT_DESCRIPTION, result.getDescription());
			}
			signsObject.put(resultObject);
		}

		final JSONObject mainObject = new JSONObject();
		mainObject.put(JSELEMENT_SIGNS, signsObject);

		return mainObject;
	}
}

package es.gob.afirma.signers.batch.client;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.batch.client.BatchDataResult.Result;

public class JSONPreSignBatchParser {

	private static final String JSELEM_TD = "td"; //$NON-NLS-1$
	private static final String JSELEM_RESULTS = "results"; //$NON-NLS-1$
	private static final String JSELEM_ID = "id"; //$NON-NLS-1$
	private static final String JSELEM_RESULT = "result"; //$NON-NLS-1$
	private static final String JSELEM_DESCRIPTION = "description"; //$NON-NLS-1$

	public static PresignBatch parseFromJSON(final byte[] preSignJson) throws JSONException {

		JSONObject jsonObject = null;
		try {
			jsonObject = new JSONObject(new JSONTokener(new ByteArrayInputStream(preSignJson)));
		} catch (final JSONException e){
			throw new JSONException("El JSON de prefirma del lote no esta formado correctamente", e); //$NON-NLS-1$
		}

		TriphaseData td = null;
		if (jsonObject.has(JSELEM_TD)) {
			td = TriphaseDataParser.parseFromJSON(jsonObject.getJSONObject(JSELEM_TD));
		}

		List<BatchDataResult> results = null;
		if (jsonObject.has(JSELEM_RESULTS)) {
			results = parseBatchDataResults(jsonObject.getJSONArray(JSELEM_RESULTS));
		}
		return new PresignBatch(td, results);
	}

	private static List<BatchDataResult> parseBatchDataResults(final JSONArray jsonResults) {

		if (jsonResults.isEmpty()) {
			return null;
		}

		final List<BatchDataResult> results = new ArrayList<>();
		for (int i = 0; i < jsonResults.length(); i++) {
			final JSONObject dataResult = jsonResults.getJSONObject(i);

			if (!dataResult.has(JSELEM_ID) || !dataResult.has(JSELEM_RESULT)) {
				throw new JSONException("Se obtuvo un resultado parcial sin identificador o resultado"); //$NON-NLS-1$
			}

			Result result;
			try {
				result = Result.valueOf(dataResult.getString(JSELEM_RESULT));
			}
			catch (final Exception e) {
				throw new JSONException("Estado de firma trifasica desconocido", e); //$NON-NLS-1$
			}

			final String id = dataResult.getString(JSELEM_ID);

			String description = null;
			if (dataResult.has(JSELEM_DESCRIPTION)) {
				description = dataResult.getString(JSELEM_DESCRIPTION);
			}

			results.add(new BatchDataResult(id, result, description));
		}

		return results;
	}
}

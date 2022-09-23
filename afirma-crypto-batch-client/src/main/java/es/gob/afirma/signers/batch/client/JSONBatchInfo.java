package es.gob.afirma.signers.batch.client;

import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;

public class JSONBatchInfo implements BatchInfo {

	private static final String JSELEM_SINGLESIGNS = "singlesigns"; //$NON-NLS-1$
	private static final String JSELEM_DATAREFERENCE = "datareference"; //$NON-NLS-1$
	private static final String JSELEM_FORMAT = "format"; //$NON-NLS-1$
	private static final String JSELEM_SUBOPERATION = "suboperation"; //$NON-NLS-1$
	private static final String JSELEM_EXTRAPARAMS = "extraparams"; //$NON-NLS-1$
	private static final String JSELEM_ID = "id"; //$NON-NLS-1$
	private static final String JSELEM_RESULT = "result"; //$NON-NLS-1$
	private static final String JSELEM_DESCRIPTION = "description"; //$NON-NLS-1$

	private final JSONObject batch;

	public JSONBatchInfo(final JSONObject batch) {
		this.batch = batch;
	}

	@Override
	public void updateResults(final List<BatchDataResult> results) {

		final JSONArray singleSigns = this.batch.getJSONArray(JSELEM_SINGLESIGNS);

		for (int i = 0; i < singleSigns.length(); i++) {
			final JSONObject singleSign = singleSigns.getJSONObject(i);
			final String signId = singleSign.getString(JSELEM_ID);

			boolean updated = false;
			for (int j = 0; j < results.size() && !updated; j++) {
				final BatchDataResult result = results.get(j);
				if (result.getId().equals(signId)) {
					// Eliminamos la referencia a los datos y cualquier configuracion particular
					singleSign.remove(JSELEM_DATAREFERENCE);
					singleSign.remove(JSELEM_FORMAT);
					singleSign.remove(JSELEM_SUBOPERATION);
					singleSign.remove(JSELEM_EXTRAPARAMS);
					// Agregamos el resultado parcial
					singleSign.put(JSELEM_RESULT, result.getResult().name());
					if (result.getDescription() != null) {
						singleSign.put(JSELEM_DESCRIPTION, result.getDescription());
					}
					updated = true;
				}
			}
		}
	}

	@Override
	public String getInfoString() {
		return this.batch.toString();
	}
}

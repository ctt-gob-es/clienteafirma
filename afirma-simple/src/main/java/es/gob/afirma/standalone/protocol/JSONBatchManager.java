package es.gob.afirma.standalone.protocol;


import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.protocol.ParameterException;
import es.gob.afirma.standalone.protocol.LocalBatchSigner.LocalSingleBatchResult;
import es.gob.afirma.standalone.protocol.SingleSignOperation.Operation;

/**
 * Clase encargada de parsear datos de tipo JSON para la firma de lotes monof&aacute;sica.
 * @author Jose.Montero
 */
public class JSONBatchManager {

	/* Elementos de las peticiones */
	private static final String ELEM_STOPONERROR = "stoponerror"; //$NON-NLS-1$
	private static final String ELEM_SINGLESIGNS = "singlesigns"; //$NON-NLS-1$
	private static final String ELEM_ID = "id"; //$NON-NLS-1$
	private static final String ELEM_SUBOPERATION = "suboperation"; //$NON-NLS-1$
	private static final String ELEM_DATAREFERENCE = "datareference"; //$NON-NLS-1$
	private static final String ELEM_FORMAT = "format"; //$NON-NLS-1$
	private static final String ELEM_ALGORITHM = "algorithm"; //$NON-NLS-1$
	private static final String ELEM_EXTRAPARAMS = "extraparams"; //$NON-NLS-1$

	/* Elementos de las respuestas */
	private static final String RESP_ELEM_MAIN = "signs"; //$NON-NLS-1$
	private static final String RESP_ELEM_ID = "id"; //$NON-NLS-1$
	private static final String RESP_ELEM_RESULT = "result"; //$NON-NLS-1$
	private static final String RESP_ELEM_DESCRIPTION = "description"; //$NON-NLS-1$
	private static final String RESP_ELEM_SIGNATURE = "signature"; //$NON-NLS-1$

	public static BatchSignOperation parseBatchConfig(final byte[] batchConfig) throws ParameterException {

		final JSONTokener tokener = new JSONTokener(new ByteArrayInputStream(batchConfig));

		JSONObject jsonObject = null;
		try {
			jsonObject = new JSONObject(tokener);
		}
		catch (final JSONException e){
			throw new JSONException(
					"El JSON de definicion de lote de firmas no esta formado correctamente", e //$NON-NLS-1$
		 		);
		}

		BatchSignOperation config;
		try {
			config = JSONBatchManager.parseBatchConfig(jsonObject);
		}
		catch (final Exception e) {
			throw new ParameterException("La declaracion del lote suministrada no es correcta", e); //$NON-NLS-1$
		}

		return config;
	}

	/**
	 * Transforma un objeto json en datos procesados para llamar a la firma monof&aacute;sica por lotes.
	 * @param json datos a transformar.
	 * @return datos transformados y estructurados en datos para firma de lotes monof&aacute;sica.
	 * @throws ParameterException error al indicar alguno de los par&aacute;metros
	 * @throws IOException error de entrada o salida al decodificar a base 64
	 * @throws JSONException error al parsear JSON
	 */
	private static BatchSignOperation parseBatchConfig(final JSONObject json) throws ParameterException, JSONException, IOException {

		// Identificamos que hacer en caso de error
		final boolean stopOnError = json.has(ELEM_STOPONERROR) && json.getBoolean(ELEM_STOPONERROR);

		// Identificamos la operacion de firma general del lote (por defecto "firmar")
		final Operation subOp = json.has(ELEM_SUBOPERATION)
				? SingleSignOperation.Operation.getOperation(json.getString(ELEM_SUBOPERATION))
						: SingleSignOperation.Operation.SIGN;

		// Identificamos el formato de firma general del lote
		String format;
		if (!json.has(ELEM_FORMAT)) {
			throw new ParameterException("El lote no declaraba el formato de firma"); //$NON-NLS-1$
		}
		format = json.getString(ELEM_FORMAT);

		// Identificamos el algorimo de firma general del lote
		String algorithm;
		if (!json.has(ELEM_ALGORITHM)) {
			throw new ParameterException("El lote no declaraba el algoritmo de firma"); //$NON-NLS-1$
		}
		algorithm = json.getString(ELEM_ALGORITHM);

		// Identificamos los extraParams del lote
		final String extraParamsParameter = json.has(ELEM_EXTRAPARAMS)
					? json.getString(ELEM_EXTRAPARAMS) : null;
		final Properties extraParams = extraParamsParameter != null
				? expanExtraParams(extraParamsParameter) : null;


		final List<SingleSignOperation> singleOperations = new ArrayList<>();

		final JSONArray singleSignsArray = json.getJSONArray(ELEM_SINGLESIGNS);
		if (singleSignsArray != null) {
			for (int i = 0 ; i < singleSignsArray.length() ; i++) {

				final JSONObject jsonSinleSign = singleSignsArray.getJSONObject(i);

				final SingleSignOperation singleOperation = new SingleSignOperation();

				if (!jsonSinleSign.has(ELEM_ID)) {
					throw new ParameterException("No se ha incluido el identificador de un documento del lote"); //$NON-NLS-1$
				}
				singleOperation.setDocId(jsonSinleSign.getString(ELEM_ID));

				if (!jsonSinleSign.has(ELEM_DATAREFERENCE)) {
					throw new ParameterException("No se ha incluido la referencia de un documento del lote"); //$NON-NLS-1$
				}
				singleOperation.setData(Base64.decode(jsonSinleSign.getString(ELEM_DATAREFERENCE)));

				singleOperation.setAlgorithm(algorithm);

				if (jsonSinleSign.has(ELEM_SUBOPERATION)) {
					singleOperation.setCryptoOperation(
							SingleSignOperation.Operation.getOperation(
									jsonSinleSign.getString(ELEM_SUBOPERATION)));
				}
				else {
					singleOperation.setCryptoOperation(subOp);
				}

				if (jsonSinleSign.has(ELEM_FORMAT)) {
					singleOperation.setFormat(jsonSinleSign.getString(ELEM_FORMAT));
				}
				else {
					singleOperation.setFormat(format);
				}

				if (jsonSinleSign.has(ELEM_EXTRAPARAMS)) {
					singleOperation.setExtraParams(expanExtraParams(jsonSinleSign.getString(ELEM_EXTRAPARAMS)));
				}
				else {
					final Properties extraParamsCopy = new Properties();
					if (extraParams != null) {
						extraParamsCopy.putAll(extraParams);
					}
					singleOperation.setExtraParams(extraParamsCopy);
				}

				singleOperations.add(singleOperation);
			}
		}
		return new BatchSignOperation(stopOnError, singleOperations);
	}

	/**
	 * Convierte una cadena de propiedades en un properties.
	 * @param params Cadena con las propiedades.
	 * @return Properties.
	 * @throws IOException Cuando ocurre un error en la carga.
	 */
	private static Properties expanExtraParams(final String params) throws IOException {

		if (params == null || params.isEmpty()) {
			return null;
		}

		final String cleanParams = new String(Base64.decode(params), StandardCharsets.UTF_8)
				.trim().replace("\\n", "\n"); //$NON-NLS-1$ //$NON-NLS-2$

		final Properties extraParams = new Properties();
		extraParams.load(new ByteArrayInputStream(cleanParams.getBytes(StandardCharsets.UTF_8)));

		return extraParams;
	}

	public static String buildBatchResultJson(final List<LocalSingleBatchResult> results) {

		final JSONArray signs = new JSONArray();

		for (final LocalSingleBatchResult singleResult : results) {

			final JSONObject element = new JSONObject();
			element.put(RESP_ELEM_ID, singleResult.getDocId());
			element.put(RESP_ELEM_RESULT, singleResult.getResult());
			if (singleResult.getDescription() != null) {
				element.put(RESP_ELEM_DESCRIPTION, singleResult.getDescription());
			}
			if (singleResult.getSignature() != null) {
				element.put(RESP_ELEM_SIGNATURE, Base64.encode(singleResult.getSignature()));
			}
			signs.put(element);
		}

		final JSONObject mainElement = new JSONObject();
		mainElement.put(RESP_ELEM_MAIN, signs);

		return mainElement.toString();
	}


}

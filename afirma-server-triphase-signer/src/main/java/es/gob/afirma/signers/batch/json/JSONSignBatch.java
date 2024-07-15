/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.batch.json;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Logger;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.batch.BatchException;
import es.gob.afirma.signers.batch.ProcessResult;
import es.gob.afirma.signers.batch.ProcessResult.Result;
import es.gob.afirma.signers.batch.SingleSignConstants;
import es.gob.afirma.signers.batch.TempStore;
import es.gob.afirma.signers.batch.TempStoreFactory;
import es.gob.afirma.triphase.server.ConfigManager;
import es.gob.afirma.triphase.server.cache.DocumentCacheManager;
import es.gob.afirma.triphase.server.document.BatchDocumentManager;
import es.gob.afirma.triphase.server.document.DocumentManager;

/** Lote de firmas electr&oacute;nicas */
public abstract class JSONSignBatch {

	private static final String JSON_ELEMENT_ID = "id"; //$NON-NLS-1$
	private static final String JSON_ELEMENT_DATAREFERENCE = "datareference"; //$NON-NLS-1$
	private static final String JSON_ELEMENT_FORMAT = "format"; //$NON-NLS-1$
	private static final String JSON_ELEMENT_ALGORITHM = "algorithm"; //$NON-NLS-1$
	private static final String JSON_ELEMENT_SINGLESIGNS = "singlesigns"; //$NON-NLS-1$
	private static final String JSON_ELEMENT_SUBOPERATION = "suboperation"; //$NON-NLS-1$
	private static final String JSON_ELEMENT_STOPONERROR = "stoponerror"; //$NON-NLS-1$
	private static final String JSON_ELEMENT_EXTRAPARAMS = "extraparams"; //$NON-NLS-1$

	private static final String JSELEM_TD = "td"; //$NON-NLS-1$
	private static final String JSELEM_RESULTS = "results"; //$NON-NLS-1$
	private static final String JSELEM_ID = "id"; //$NON-NLS-1$
	private static final String JSELEM_RESULT = "result"; //$NON-NLS-1$
	private static final String JSELEM_DESCRIPTION = "description"; //$NON-NLS-1$
	private static final String JSELEM_FORMAT = "format"; //$NON-NLS-1$
	private static final String JSELEM_SIGNS = "signs"; //$NON-NLS-1$

	private static final String EXTRAPARAM_HEADLESS = "headless"; //$NON-NLS-1$

	protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Lista de firmas a procesar. */
	protected final List<JSONSingleSign> signs;

	protected SingleSignConstants.DigestAlgorithm algorithm = null;

	protected String id;

	protected String extraParams;

	protected SingleSignConstants.SignSubOperation subOperation = null;

	protected SingleSignConstants.SignFormat format = null;

	protected DocumentManager documentManager = null;

	protected DocumentCacheManager docCacheManager = null;

	/** Indica si se debe parar al encontrar un error o por el contrario se debe continuar con el proceso. */
	protected boolean stopOnError = false;

	/**
	 * Ejecuta el preproceso de firma por lote.
	 * @param certChain Cadena de certificados del firmante.
	 * @return Resultados parciales y datos trif&aacute;sicos de pre-firma del lote.
	 * @throws BatchException Cuando hay errores irrecuperables en el preproceso.
	 */
	public abstract JSONObject doPreBatch(final X509Certificate[] certChain) throws BatchException;

	/**
	 * Ejecuta el postproceso de firma por lote.
	 * @param certChain Cadena de certificados del firmante.
	 * @param td Datos trif&aacute;sicos del preproceso.
	 *           Debe contener los datos de todas y cada una de las firmas del lote.
	 * @return Registro del resultado general del proceso por lote, en un JSON (<a href="../doc-files/resultlog-scheme.html">descripci&oacute;n
	 *         del formato</a>).
	 * @throws BatchException Cuando hay errores irrecuperables en el postproceso.
	 */
	public abstract String doPostBatch(final X509Certificate[] certChain,
                                       final TriphaseData td) throws BatchException;

	/**
	 * Crea un lote de firmas a partir de su definici&oacute;n JSON.
	 * @param json JSON de definici&oacute;n de lote de firmas (<a href="./doc-files/batch-scheme.html">descripci&oacute;n
	 *            del formato</a>).
	 * @throws IOException Si hay problemas en el tratamiento de datos en el an&aacute;lisis del JSON.
	 * @throws SecurityException Si se sobrepasa alguna de las limitaciones establecidas para el lote
	 * (n&uacute;mero de documentos, tama&ntilde;o de las referencias, tama&ntilde;o de documento, etc.)
	 */
	protected JSONSignBatch(final byte[] json) throws IOException, SecurityException {

		if (json == null || json.length < 1) {
			throw new IllegalArgumentException(
				"El JSON de definicion de lote de firmas no puede ser nulo ni vacio" //$NON-NLS-1$
			);
		}

		// Se comprueba que el JSON definido no supere el tamano maximo permitido por la opcion configurada
		final long maxReqSize = ConfigManager.getBatchMaxRequestSize();
		if (maxReqSize > 0 && json.length > maxReqSize) {
			throw new SecurityException(
					"El JSON de definicion de lote supera el tamano permitido: " + maxReqSize); //$NON-NLS-1$
		}

		JSONObject jsonObject = null;
		try {
			jsonObject = new JSONObject(new JSONTokener(new ByteArrayInputStream(json)));
		} catch (final JSONException e){
			LOGGER.severe("Error al parsear JSON: " + e); //$NON-NLS-1$
			throw new JSONException(
					"El JSON de definicion de lote de firmas no esta formado correctamente", e //$NON-NLS-1$
				);
		}

		this.id = UUID.randomUUID().toString();

		this.stopOnError = jsonObject.has(JSON_ELEMENT_STOPONERROR) ?
				jsonObject.getBoolean(JSON_ELEMENT_STOPONERROR) : false;

		if (jsonObject.has(JSON_ELEMENT_ALGORITHM)) {
			this.algorithm = SingleSignConstants.DigestAlgorithm.getAlgorithm(
								jsonObject.getString(JSON_ELEMENT_ALGORITHM)
								);
		} else {
			this.algorithm = null;
		}

		if (jsonObject.has(JSON_ELEMENT_FORMAT)) {
			this.format = SingleSignConstants.SignFormat.getFormat(
							jsonObject.getString(JSON_ELEMENT_FORMAT)
							);
		} else {
			this.format = null;
		}

		if (jsonObject.has(JSON_ELEMENT_SUBOPERATION)) {
			this.subOperation = SingleSignConstants.SignSubOperation.getSubOperation(
									jsonObject.getString(JSON_ELEMENT_SUBOPERATION)
								);
		} else {
			this.subOperation = null;
		}

		if (jsonObject.has(JSON_ELEMENT_EXTRAPARAMS)) {
			this.extraParams = jsonObject.getString(JSON_ELEMENT_EXTRAPARAMS);
		} else {
			this.extraParams = null;
		}

		try {
			final String className = ConfigManager.getDocManagerClassName();
			final Class<?> docManagerClass = Class.forName(className, false, getClass().getClassLoader());

			if (BatchDocumentManager.class.isAssignableFrom(docManagerClass)) {
				this.documentManager = (BatchDocumentManager) docManagerClass.newInstance();
				((BatchDocumentManager) this.documentManager).init(ConfigManager.getConfig());
			} else {
				try {

				final Constructor<?> constructor = docManagerClass.getConstructor(Properties.class);
				this.documentManager = (DocumentManager) constructor.newInstance(ConfigManager.getConfig());

				} catch (final Exception e) {
					LOGGER.severe("El DocumentManager utilizado no dispone de un constructor con Properties, "+ //$NON-NLS-1$
									"se utilizara un constructor vacio para instanciarlo"); //$NON-NLS-1$
					this.documentManager = (DocumentManager) docManagerClass.newInstance();
				}
			}

		} catch (final Exception e) {
			throw new IllegalArgumentException("Error al instanciar la clase utilizada para el documentManager", e); //$NON-NLS-1$
		}

		if (Boolean.parseBoolean(ConfigManager.isCacheEnabled())) {
			this.docCacheManager = DocumentCacheFactory.newDocumentCacheManager();
		}

		this.signs = fillSingleSigns(jsonObject);
	}


	private List<JSONSingleSign> fillSingleSigns(final JSONObject jsonObject) throws SecurityException {
		final ArrayList<JSONSingleSign> singleSignsList = new ArrayList<>();
		final JSONArray singleSignsArray = jsonObject.getJSONArray(JSON_ELEMENT_SINGLESIGNS);

		if (singleSignsArray != null) {

			// Comprobamos si la propiedad batch.maxDocuments esta configurada y si permite el numero de documentos
			final long maxDocuments = ConfigManager.getBatchMaxDocuments();
			if (maxDocuments > 0 && singleSignsArray.length() > maxDocuments) {
				throw new SecurityException(
						"El lote incluye mas documentos de los permitidos. Numero de documentos del lote: " //$NON-NLS-1$
						+ singleSignsArray.length());
			}

			for (int i = 0 ; i < singleSignsArray.length() ; i++){

				final JSONObject jsonSingleSign = singleSignsArray.getJSONObject(i);
				final JSONSingleSign singleSign = new JSONSingleSign(jsonSingleSign.getString(JSON_ELEMENT_ID));

				// Cada nodo debe terner una referencia a los datos o el resultado de la operacion
				if (!jsonSingleSign.has(JSON_ELEMENT_DATAREFERENCE) && !jsonSingleSign.has(JSELEM_RESULT)) {
					throw new JSONException("La declaracion del lote no es valida. Todas las firmas deben declarar el atributo " //$NON-NLS-1$
							+ JSON_ELEMENT_DATAREFERENCE + " o " + JSELEM_RESULT); //$NON-NLS-1$
				}

				// Si tiene la referencia a los datos es que la firma aun no se ha completado
				// y tomamos los datos necesarios para hacerlo
				if (jsonSingleSign.has(JSON_ELEMENT_DATAREFERENCE)) {

					final String dataReference = jsonSingleSign.getString(JSON_ELEMENT_DATAREFERENCE);

					// Comprobamos si la propiedad batch.maxReferenceSize esta configurada y si permite el numero de documentos
					final long maxRefSize = ConfigManager.getBatchMaxReferenceSize();
					if (maxRefSize > 0 && dataReference != null && dataReference.length() > maxRefSize) {
						throw new SecurityException(
								"El tamano de la referencia supera el limite permitido. Tamano de la referencia: " //$NON-NLS-1$
								+ dataReference.length());
					}

					singleSign.setDataRef(dataReference);

					singleSign.setFormat(jsonSingleSign.has(JSON_ELEMENT_FORMAT)
							? SingleSignConstants.SignFormat.getFormat(jsonSingleSign.getString(JSON_ELEMENT_FORMAT))
									: this.format);

					singleSign.setSubOperation(jsonSingleSign.has(JSON_ELEMENT_SUBOPERATION)
							? SingleSignConstants.SignSubOperation.getSubOperation(jsonSingleSign.getString(JSON_ELEMENT_SUBOPERATION))
									: this.subOperation);

					singleSign.setDocumentManager(this.documentManager);

					try {
						Properties signExtraParams;
						if (jsonSingleSign.has(JSON_ELEMENT_EXTRAPARAMS)) {
							signExtraParams = AOUtil.base642Properties(jsonSingleSign.getString(JSON_ELEMENT_EXTRAPARAMS));
						} else {
							signExtraParams = AOUtil.base642Properties(this.extraParams);
						}
						signExtraParams.setProperty(EXTRAPARAM_HEADLESS, Boolean.TRUE.toString());
						singleSign.setExtraParams(signExtraParams);
					} catch (final Exception e) {
						throw new JSONException(
								"El objeto JSON no esta correctamente formado"); //$NON-NLS-1$
					}
				}
				// Si no esta la referencia a los datos, es que ya se ha obtenido un resultado
				else {
					final String result = jsonSingleSign.getString(JSELEM_RESULT);
					String description = null;
					if (jsonSingleSign.has(JSELEM_DESCRIPTION)) {
						description = jsonSingleSign.getString(JSELEM_DESCRIPTION);
					}
					final ProcessResult processResult = new ProcessResult(Result.valueOf(result), description);
					singleSign.setProcessResult(processResult);
				}

				singleSignsList.add(singleSign);
			}
		}

		return singleSignsList;
	}


	protected static JSONObject buildSignResult(final String id, final Result result, final Throwable error) {
		final JSONObject jsonResult = new JSONObject();
		jsonResult.put(JSELEM_ID, id);
		jsonResult.put(JSELEM_RESULT, result.name());
		if (error != null) {
			jsonResult.put(JSELEM_DESCRIPTION, error.getMessage());
		}
		return jsonResult;
	}

	protected static JSONObject buildPreBatch(final String format, final JSONArray trisigns, final JSONArray errors) {
		final JSONObject preBatch = new JSONObject();
		if (trisigns != null && !trisigns.isEmpty()) {
			final JSONObject triphaseInfo = new JSONObject();
			triphaseInfo.put(JSELEM_FORMAT, format);
			triphaseInfo.put(JSELEM_SIGNS, trisigns);
			preBatch.put(JSELEM_TD, triphaseInfo);
		}
		if (errors != null && !errors.isEmpty()) {
			preBatch.put(JSELEM_RESULTS, errors);
		}

		return preBatch;
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder(
			"{\n\"stoponerror\":\"" //$NON-NLS-1$
		);
		sb.append(Boolean.toString(this.stopOnError));
		sb.append("\",\n\"format\":\""); //$NON-NLS-1$
		sb.append(this.format);
		sb.append("\",\n\"algorithm\":\""); //$NON-NLS-1$
		sb.append(this.algorithm);
		sb.append(",\n\"Id\":\""); //$NON-NLS-1$
		sb.append(this.id);
		sb.append("\",\n"); //$NON-NLS-1$
		sb.append("\n\"singlesigns\":[\n"); //$NON-NLS-1$
		for (int i = 0 ; i < this.signs.size() ; i++) {
			sb.append(this.signs.get(i).toString());
			if (this.signs.size()-1 != i) {
				sb.append(',');
			}
			sb.append('\n');
		}
		sb.append("]\n"); //$NON-NLS-1$
		sb.append("}\n"); //$NON-NLS-1$
		return sb.toString();
	}

	/**
	 * Indica si el proceso por lote debe detenerse cuando se encuentre un error.
	 * @param soe <code>true</code> si el proceso por lote debe detenerse cuando se encuentre un error,
	 *            <code>false</code> si se debe continuar con el siguiente elemento del lote cuando se
	 *            produzca un error.
	 */
	public void setStopOnError(final boolean soe) {
		this.stopOnError = soe;
	}

	/**
	 * Obtiene el <i>log</i> con el resultado del proceso del lote.
	 * @return <i>Log</i> en formato JSON con el resultado del proceso del lote.
	 * */
	protected String getResultLog() {
		// Iniciamos el log de retorno
		final StringBuilder ret = new StringBuilder("{\"signs\":["); //$NON-NLS-1$
		for (int i = 0; i < this.signs.size() ; i++) {
			ret.append(printProcessResult(this.signs.get(i).getProcessResult()));
			if (this.signs.size() - 1 != i) {
				ret.append(","); //$NON-NLS-1$
			}
		}
		ret.append("]}"); //$NON-NLS-1$
		return ret.toString();
	}

	public static String printProcessResult(final ProcessResult result) {
		String jsonText = "{\"id\":\"" + scapeText(result.getId()) + "\", \"result\":\"" + result.getResult() + "\""; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		if (result.getDescription() != null) {
			jsonText += ", \"description\":\"" + scapeText(result.getDescription()) + "\"";	 //$NON-NLS-1$ //$NON-NLS-2$
		}
		jsonText += "}"; //$NON-NLS-1$
		return jsonText;
	}

	private static String scapeText(final String text) {
		return text == null ? null :
			text.replace("\\", "\\\\").replace("\"", "\\\""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
	}

	/** Borra todos los ficheros temporales usados en el proceso del lote. */
	protected void deleteAllTemps() {
		final TempStore ts = TempStoreFactory.getTempStore();
		for (final JSONSingleSign ss : this.signs) {
			ts.delete(ss, getId());
		}
	}

	public String getExtraParams() {
		return this.extraParams;
	}

	public void setExtraParams(final String extraParams) {
		this.extraParams = extraParams;
	}

	String getId() {
		return this.id;
	}

	void setId(final String i) {
		if (i != null) {
			this.id = i;
		}
	}

	/**
	 * Obtiene el algoritmo de firma.
	 * @return Algoritmo de firma.
	 * */
	public SingleSignConstants.DigestAlgorithm getSignAlgorithm() {
		return this.algorithm;
	}

}

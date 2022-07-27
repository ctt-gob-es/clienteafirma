/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.batch.json;

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

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.batch.BatchException;
import es.gob.afirma.signers.batch.ProcessResult;
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

	private static final String EXTRAPARAM_HEADLESS = "headless"; //$NON-NLS-1$

	protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Lista de firmas a procesar. */
	protected final List<JSONSingleSign> signs;

	protected SingleSignConstants.SignAlgorithm algorithm = null;

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
	 * @return Datos trif&aacute;sicos de pre-firma del lote.
	 * @throws BatchException Cuando hay errores irrecuperables en el preproceso.
	 */
	public abstract String doPreBatch(final X509Certificate[] certChain) throws BatchException;

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
	 * @throws IOException Si hay problemas en el tratamiento de datoso en el an&aacute;lisis del JSON.
	 */
	protected JSONSignBatch(final byte[] json) throws IOException {

		if (json == null || json.length < 1) {
			throw new IllegalArgumentException(
				"El JSON de definicion de lote de firmas no puede ser nulo ni vacio" //$NON-NLS-1$
			);
		}

		JSONObject jsonObject = null;
		final String convertedJson = new String(json);
		try {
			jsonObject = new JSONObject(convertedJson);
		}catch (final JSONException e){
			LOGGER.severe("Error al parsear JSON: " + e); //$NON-NLS-1$
			throw new JSONException(
					"El JSON de definicion de lote de firmas no esta formado correctamente", e //$NON-NLS-1$
				);
		}

		this.id = UUID.randomUUID().toString();

		this.stopOnError = jsonObject.has(JSON_ELEMENT_STOPONERROR) ?
				jsonObject.getBoolean(JSON_ELEMENT_STOPONERROR) : false;

		if (jsonObject.has(JSON_ELEMENT_ALGORITHM)) {
			this.algorithm = SingleSignConstants.SignAlgorithm.getAlgorithm(
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

			final Class<?> docCacheManagerClass;
			String docCacheManagerClassName;
			docCacheManagerClassName = ConfigManager.getDocCacheManagerClassName();

			try {
				docCacheManagerClass = Class.forName(docCacheManagerClassName);
			}
			catch (final ClassNotFoundException e) {
				throw new RuntimeException(
						"La clase DocumentCacheManager indicada no existe ("  //$NON-NLS-1$
						+ docCacheManagerClassName +  ")", e //$NON-NLS-1$
						);
			}

			try {
				final Constructor<?> docCacheManagerConstructor = docCacheManagerClass.getConstructor(Properties.class);
				this.docCacheManager = (DocumentCacheManager) docCacheManagerConstructor.newInstance(ConfigManager.getConfig());
			}
			catch (final Exception e) {
				try {
					this.docCacheManager = (DocumentCacheManager) docCacheManagerClass.getConstructor().newInstance();
				}
				catch (final Exception e2) {
					throw new RuntimeException(
							"No se ha podido inicializar el DocumentCacheManager. Debe tener un constructor vacio o que reciba un Properties: " + e2, e //$NON-NLS-1$
							);
				}
			}

			LOGGER.info("Se usara el siguiente 'DocumentCacheManager' para firma trifasica: " + this.docCacheManager.getClass().getName()); //$NON-NLS-1$
		}

		this.signs = fillSingleSigns(jsonObject);
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

	private List<JSONSingleSign> fillSingleSigns(final JSONObject jsonObject) {
		final ArrayList<JSONSingleSign> singleSignsList = new ArrayList<>();
		final JSONArray singleSignsArray = jsonObject.getJSONArray(JSON_ELEMENT_SINGLESIGNS);

		if (singleSignsArray != null) {
			for (int i = 0 ; i < singleSignsArray.length() ; i++){

				final JSONObject jsonSingleSign = singleSignsArray.getJSONObject(i);
				final JSONSingleSign singleSign = new JSONSingleSign(jsonSingleSign.getString(JSON_ELEMENT_ID));

				singleSign.setDataRef(jsonSingleSign.getString(JSON_ELEMENT_DATAREFERENCE));

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
							"El objeto JSON no está correctamente formado"); //$NON-NLS-1$
				}

				singleSignsList.add(singleSign);
			}
		}

		return singleSignsList;
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
	public SingleSignConstants.SignAlgorithm getSignAlgorithm() {
		return this.algorithm;
	}

}

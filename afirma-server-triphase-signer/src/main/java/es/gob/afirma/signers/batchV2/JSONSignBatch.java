/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.batchV2;

import java.io.IOException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.logging.Logger;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.triphase.server.document.DocumentManager;
import es.gob.afirma.triphase.server.document.DocumentManagerBase;

/** Lote de firmas electr&oacute;nicas */
public abstract class JSONSignBatch {

	private static final String JSON_ELEMENT_ID = "id"; //$NON-NLS-1$
	private static final String JSON_ELEMENT_DATAREFERENCE = "datareference"; //$NON-NLS-1$
	private static final String JSON_ELEMENT_CONCURRENTTIMEOUT = "concurrenttimeout"; //$NON-NLS-1$
	private static final String JSON_ELEMENT_FORMAT = "format"; //$NON-NLS-1$
	private static final String JSON_ELEMENT_ALGORITHM = "algorithm"; //$NON-NLS-1$
	private static final String JSON_ELEMENT_SINGLESIGNS = "singlesigns"; //$NON-NLS-1$
	private static final String JSON_ELEMENT_SUBOPERATION = "suboperation"; //$NON-NLS-1$
	private static final String JSON_ELEMENT_STOPONERROR = "stoponerror"; //$NON-NLS-1$
	private static final String JSON_ELEMENT_EXTRAPARAMS = "extraparams"; //$NON-NLS-1$

	protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Lista de firmas a procesar. */
	protected final List<JSONSingleSign> signs;

	protected JSONSingleSignConstants.SignAlgorithm algorithm = null;

	protected String id;

	protected String extraParams;

	protected long concurrentTimeout = Long.MAX_VALUE;

	protected JSONSingleSignConstants.SignSubOperation subOperation = null;

	protected JSONSingleSignConstants.SignFormat format = null;

	protected DocumentManager documentManager = null;

	/** Indica si se debe parar al encontrar un error o por el contrario se debe continuar con el proceso. */
	protected boolean stopOnError = false;

	/**
	 * Ejecuta el preproceso de firma por lote.
	 * @param certChain Cadena de certificados del firmante.
	 * @return Datos trif&aacute;sicos de pre-firma del lote.
	 * @throws JSONBatchException Si hay errores irrecuperables en el proceso.
	 */
	public abstract String doPreBatch(final X509Certificate[] certChain) throws JSONBatchException;

	/**
	 * Ejecuta el postproceso de firma por lote.
	 * @param certChain Cadena de certificados del firmante.
	 * @param td Datos trif&aacute;sicos del preproceso.
	 *           Debe contener los datos de todas y cada una de las firmas del lote.
	 * @return Registro del resultado general del proceso por lote, en un JSON (<a href="../doc-files/resultlog-scheme.html">descripci&oacute;n
	 *         del formato</a>).
	 * @throws JSONBatchException Si hay errores irrecuperables en el postproceso.
	 */
	public abstract String doPostBatch(final X509Certificate[] certChain,
                                       final TriphaseData td) throws JSONBatchException;

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
		}catch (final JSONException jsonEx){
			LOGGER.severe("Error al parsear JSON"); //$NON-NLS-1$
			throw new JSONException(
					"El JSON de definicion de lote de firmas no esta formado correctamente" //$NON-NLS-1$
				);
		}

		this.id = jsonObject.has(JSON_ELEMENT_ID) ?
				jsonObject.getString(JSON_ELEMENT_ID) : UUID.randomUUID().toString();

		this.concurrentTimeout = jsonObject.getLong(JSON_ELEMENT_CONCURRENTTIMEOUT);
		this.stopOnError = jsonObject.getBoolean(JSON_ELEMENT_STOPONERROR);

		if (jsonObject.has(JSON_ELEMENT_ALGORITHM)) {
			this.algorithm = JSONSingleSignConstants.SignAlgorithm.getAlgorithm(
								jsonObject.getString(JSON_ELEMENT_ALGORITHM)
								);
		} else {
			this.algorithm = null;
		}

		if (jsonObject.has(JSON_ELEMENT_FORMAT)) {
			this.format = JSONSingleSignConstants.SignFormat.getFormat(
							jsonObject.getString(JSON_ELEMENT_FORMAT)
							);
		} else {
			this.format = null;
		}

		if (jsonObject.has(JSON_ELEMENT_SUBOPERATION)) {
			this.subOperation = JSONSingleSignConstants.SignSubOperation.getSubOperation(
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
			final String className = JSONBatchConfigManager.getSaverFile();
			final Class<?> docManagerClass = Class.forName(className, false, getClass().getClassLoader());

			if (DocumentManagerBase.class.isAssignableFrom(docManagerClass)) {
				this.documentManager = (DocumentManagerBase) docManagerClass.newInstance();
				((DocumentManagerBase) this.documentManager).init(JSONBatchConfigManager.getConfig());
			} else {
				this.documentManager = (DocumentManager) docManagerClass.newInstance();
			}

		} catch (final Exception e) {
			throw new IllegalArgumentException("Error al instanciar la clase utilizada para el signSaver"); //$NON-NLS-1$
		}

		this.signs = fillSingleSigns(jsonObject);
	}

	protected JSONSignBatch(final List<JSONSingleSign> signatures,
			            final JSONSingleSignConstants.SignAlgorithm algo,
			            final boolean soe) {

		if (signatures == null) {
			throw new IllegalArgumentException(
				"La lista de firmas del lote no puede ser nula" //$NON-NLS-1$
			);
		}
		if (algo == null) {
			throw new IllegalArgumentException(
				"El algoritmo de firma no puede ser nulo" //$NON-NLS-1$
			);
		}
		this.signs = signatures;
		this.stopOnError = soe;
		this.algorithm = null;
		this.id = UUID.randomUUID().toString();
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
		sb.append("\",\n\"concurrenttimeout\":"); //$NON-NLS-1$
		sb.append(this.concurrentTimeout);
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
		final StringBuilder ret = new StringBuilder("{ \n \"signs\":[\n"); //$NON-NLS-1$
		for (int i = 0; i < this.signs.size() ; i++) {
			ret.append(this.signs.get(i).getProcessResult().toString());
			if(this.signs.size()-1 != i) {
				ret.append(","); //$NON-NLS-1$
			}
			ret.append("\n"); //$NON-NLS-1$
		}
		ret.append("\n]\n}"); //$NON-NLS-1$
		return ret.toString();
	}

	/** Borra todos los ficheros temporales usados en el proceso del lote. */
	protected void deleteAllTemps() {
		final JSONTempStore ts = JSONTempStoreFactory.getTempStore();
		for (final JSONSingleSign ss : this.signs) {
			ts.delete(ss, getId());
		}
	}

	private List<JSONSingleSign> fillSingleSigns(final JSONObject jsonObject) {
		final ArrayList<JSONSingleSign> singleSignsList = new ArrayList<JSONSingleSign>();
		final JSONArray jArray = jsonObject.getJSONArray(JSON_ELEMENT_SINGLESIGNS);

		if (jArray != null) {
			for (int i=0;i<jArray.length();i++){

				final JSONSingleSign singleSign = new JSONSingleSign(jArray.getJSONObject(i).getString(JSON_ELEMENT_ID));

				singleSign.setReference(jArray.getJSONObject(i).getString(JSON_ELEMENT_DATAREFERENCE));

				singleSign.setFormat(jArray.getJSONObject(i).has(JSON_ELEMENT_FORMAT)
						? JSONSingleSignConstants.SignFormat.getFormat(
								jsonObject.getString(JSON_ELEMENT_FORMAT)
								)
								: this.format);

				singleSign.setSubOperation(jArray.getJSONObject(i).has(JSON_ELEMENT_SUBOPERATION)
						? JSONSingleSignConstants.SignSubOperation.getSubOperation(
								jsonObject.getString(JSON_ELEMENT_SUBOPERATION)
								)
								: this.subOperation);

				singleSign.setDocumentManager(this.documentManager);

				try {
					singleSign.setExtraParams(jArray.getJSONObject(i).has(JSON_ELEMENT_EXTRAPARAMS)
							? AOUtil.base642Properties(JSON_ELEMENT_EXTRAPARAMS) : null);
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
	public JSONSingleSignConstants.SignAlgorithm getSignAlgorithm() {
		return this.algorithm;
	}

}

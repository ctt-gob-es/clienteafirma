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
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.Properties;
import java.util.concurrent.Callable;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.batch.ProcessResult;
import es.gob.afirma.signers.batch.ProcessResult.Result;
import es.gob.afirma.signers.batch.SingleSignConstants;
import es.gob.afirma.signers.batch.SingleSignConstants.SignFormat;
import es.gob.afirma.signers.batch.SingleSignConstants.SignSubOperation;
import es.gob.afirma.signers.batch.TempStore;
import es.gob.afirma.signers.batch.xml.SingleSign;
import es.gob.afirma.triphase.server.cache.DocumentCacheManager;
import es.gob.afirma.triphase.server.document.DocumentManager;

/** Firma electr&oacute;nica &uacute;nica dentro de un lote. */
public final class JSONSingleSign extends SingleSign {

	private static final String JSON_ATTRIBUTE_ID = "Id"; //$NON-NLS-1$

	private static final String JSON_ELEMENT_DATAREFERENCE = "datareference"; //$NON-NLS-1$
	private static final String JSON_ELEMENT_FORMAT = "format"; //$NON-NLS-1$
	private static final String JSON_ELEMENT_SUBOPERATION = "suboperation"; //$NON-NLS-1$
	private static final String JSON_ELEMENT_EXTRAPARAMS = "extraparams"; //$NON-NLS-1$

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private DocumentManager documentManager;

	/** Crea una definici&oacute;n de tarea de firma electr&oacute;nica &uacute;nica.
	 * @param id Identificador de la firma. */
	JSONSingleSign(final String id) {
		this.id = id;
	}

	/** Crea una definici&oacute;n de tarea de firma electr&oacute;nica &uacute;nica.
	 * @param id Identificador de la firma.
	 * @param dataSrc Datos a firmar.
	 * @param fmt Formato de firma.
	 * @param subOp Tipo de firma a realizar.
	 * @param xParams Opciones adicionales de la firma.
	 * @param ss Objeto para guardar la firma una vez completada. */
	public JSONSingleSign(final String id,
			          final String dataSrc,
			          final SignFormat fmt,
			          final SignSubOperation subOp,
			          final Properties xParams,
			          final DocumentManager ss) {

		if (dataSrc == null) {
			throw new IllegalArgumentException(
				"El origen de los datos a firmar no puede ser nulo" //$NON-NLS-1$
			);
		}

		if (fmt == null) {
			throw new IllegalArgumentException(
				"El formato de firma no puede ser nulo" //$NON-NLS-1$
			);
		}

		if (ss == null) {
			throw new IllegalArgumentException(
				"El objeto de guardado de firma no puede ser nulo" //$NON-NLS-1$
			);
		}

		this.dataRef = dataSrc;
		this.format = fmt;

		this.id = id;

		this.subOperation = subOp;
		this.documentManager = ss;
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder("{\n"); //$NON-NLS-1$
		sb.append(":\""); //$NON-NLS-1$
		sb.append(JSON_ATTRIBUTE_ID);
		sb.append(":\""); //$NON-NLS-1$
		sb.append(getId());
		sb.append("\" , \n\""); //$NON-NLS-1$
		sb.append(JSON_ELEMENT_DATAREFERENCE);
		sb.append("\":\""); //$NON-NLS-1$
		sb.append(this.dataRef);
		sb.append("\",\n"); //$NON-NLS-1$
		sb.append("\""); //$NON-NLS-1$
		sb.append(JSON_ELEMENT_FORMAT);
		sb.append("\":\""); //$NON-NLS-1$
		sb.append(getSignFormat().toString());
		sb.append("\",\n"); //$NON-NLS-1$
		sb.append("\""); //$NON-NLS-1$
		sb.append(JSON_ELEMENT_SUBOPERATION);
		sb.append("\":\""); //$NON-NLS-1$
		sb.append(getSubOperation().toString());
		sb.append("\",\n"); //$NON-NLS-1$
		sb.append("\""); //$NON-NLS-1$
		sb.append(JSON_ELEMENT_EXTRAPARAMS);
		sb.append("\":\""); //$NON-NLS-1$
		try {
			sb.append(AOUtil.properties2Base64(getExtraParams()));
		}
		catch (final IOException e) {
			LOGGER.severe(
				"Error convirtiendo los parametros adicionales de la firma '" + getId() + "' a Base64: " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
		}
		sb.append("\",\n"); //$NON-NLS-1$
		sb.append("\""); //$NON-NLS-1$
		sb.append("\n}\n"); //$NON-NLS-1$

		return sb.toString();
	}

	/** Realiza el proceso de prefirma, incluyendo la descarga u obtenci&oacute;n de datos.
	 * @param certChain Cadena de certificados del firmante.
	 * @param algorithm Algoritmo de firma.
	 * @param docManager Gestor de documentos con el que procesar el lote.
	 * @param docCacheManager Gestor para el guardado de datos en cach&eacute;.
	 * @return Objeto JSON con los datos trif&aacute;sicos.
	 * @throws AOException Si hay problemas en la propia firma electr&oacute;nica.
	 * @throws IOException Si hay problemas en la obtenci&oacute;n, tratamiento o gradado de datos. */
	TriphaseData doPreProcess(final X509Certificate[] certChain,
			            final SingleSignConstants.DigestAlgorithm algorithm,
			            final DocumentManager docManager,
			            final DocumentCacheManager docCacheManager) throws IOException,
			                                                                      AOException {
		return JSONSingleSignPreProcessor.doPreProcess(this, certChain, algorithm, docManager, docCacheManager);
	}

	/** Obtiene la tarea de preproceso de firma para ser ejecutada en paralelo.
	 * @param certChain Cadena de certificados del firmante.
	 * @param algorithm Algoritmo de firma.
	 * @param docManager Gestor de documentos con el que procesar el lote.
	 * @param docCacheManager Gestor para el guardado de datos en cach&eacute;.
	 * @return Tarea de preproceso de firma para ser ejecutada en paralelo. */
	Callable<PreprocessResult> getPreProcessCallable(final X509Certificate[] certChain,
                                                  final SingleSignConstants.DigestAlgorithm algorithm,
                                                  final DocumentManager docManager,
                                                  final DocumentCacheManager docCacheManager) {
		return new PreProcessCallable(this, certChain, algorithm, docManager, docCacheManager);
	}

	/** Realiza el proceso de postfirma, incluyendo la subida o guardado de datos.
	 * @param certChain Cadena de certificados del firmante.
	 * @param td Datos trif&aacute;sicos relativos <b>&uacute;nicamente</b> a esta firma.
	 *           Debe serializarse como un JSON con esta forma (ejemplo):
	 *<pre>
	 * {
	 * "format":"PAdES",
	 * "signs":
	 * [{
	 *	"signinfo":[{
 	 *		"Id":"7725374e-728d-4a33-9db9-3a4efea4cead",
	 *		"params":
	 *			[{
	 *			"PRE":"PGGYzMC1iOTub3JnL1RSLzIwMDEvUkVDLXh",
	 *			"ENCODING":"UTF-8",
	 *			"NEED_PRE":"true",
	 *			"BASE":"PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGl"}]}]},
	 *{
	 *	"signinfo":[{
 	 *		"Id":"93d1531c-cd32-4c8e-8cc8-1f1cfe66f64a",
	 *		"params":
	 *			[{
	 *			"PRE":"MYIBAzAYBgkqhkiG9w0BCQMxCwYJKoZIhv",
	 *			"NEED_PRE":"true",
	 *			"TIME":"1621423575727",
	 *			"PID":"Wzw5MjBjODdmYmE4ZTEyZTM0YjU2OWUzOW"}]}]}]
	 * }
	 * </pre>
	 * @param algorithm Algoritmo de firma.
	 * @param batchId Identificador del lote de firma.
	 * @param docManager Gestor de documentos con el que procesar el lote.
	 * @param docCacheManager Gestor para la carga de datos desde cach&eacute;.
	 * @throws AOException Si hay problemas en la propia firma electr&oacute;nica.
	 * @throws IOException Si hay problemas en la obtenci&oacute;n, tratamiento o gradado de datos.
	 * @throws NoSuchAlgorithmException Si no se soporta alg&uacute;n algoritmo necesario. */
	void doPostProcess(final X509Certificate[] certChain,
			                  final TriphaseData td,
			                  final SingleSignConstants.DigestAlgorithm algorithm,
			                  final String batchId,
			                  final DocumentManager docManager,
			                  final DocumentCacheManager docCacheManager) throws IOException,
			                                               AOException,
			                                               NoSuchAlgorithmException {
		JSONSingleSignPostProcessor.doPostProcess(
			this, certChain, td, algorithm, batchId, docManager, docCacheManager
		);
	}

	/** Obtiene la tarea de postproceso de firma para ser ejecutada en paralelo.
	 * @param certChain Cadena de certificados del firmante.
	 * @param td Datos trif&aacute;sicos relativos <b>&uacute;nicamente</b> a esta firma.
	 *           Debe serializarse como un JSON con esta forma (ejemplo):
	 *           <pre>
	 *	{
 	 *	"signs":[
	 *		{	"id":"CADES-001",
 	 *			"result":"DONE_AND_SAVED",
 	 *			"description":""
	 *		},
	 *		{	"id":"XADES-002",
 	 *			"result":"DONE_AND_SAVED",
 	 *			"description":""
	 *		},
	 *		{	"id":"PADES-003",
 	 *			"result":"DONE_AND_SAVED",
 	 *			"description":""
	 *		}
	 *	]
	 * }
	 *           </pre>
	 * @param algorithm Algoritmo de firma.
	 * @param batchId Identificador del lote de firma.
	 * @param docManager Gestor de documentos con el que procesar el lote.
	 * @param docCacheManager Gestor para la carga de datos desde cach&eacute;.
	 * @return Tarea de postproceso de firma para ser ejecutada en paralelo. */
	Callable<ResultSingleSign> getPostProcessCallable(final X509Certificate[] certChain,
			                                                          final TriphaseData td,
			                                                          final SingleSignConstants.DigestAlgorithm algorithm,
			                                                          final String batchId,
			                                                          final DocumentManager docManager,
			                                                          final DocumentCacheManager docCacheManager) {
		return new PostProcessCallable(this, certChain, td, algorithm, batchId, docManager, docCacheManager);
	}

	Callable<ResultSingleSign> getSaveCallableJSON(final TempStore ts, final X509Certificate[] certChain, final String batchId) {

		return new JSONSaveCallable(this, this.documentManager, ts, certChain, batchId);
	}

	public void setDocumentManager(final DocumentManager documentManager) {
		this.documentManager = documentManager;
	}

//	static class CallableResult {
//
//		private final String signId;
//		private final Exception exception;
//
//		CallableResult(final String id) {
//			this.signId = id;
//			this.exception = null;
//		}
//
//		CallableResult(final String id, final Exception e) {
//			this.signId = id;
//			this.exception = e;
//		}
//
//		boolean isOk() {
//			return this.exception == null;
//		}
//
//		Exception getError() {
//			return this.exception;
//		}
//
//		String getSignatureId() {
//			return this.signId;
//		}
//	}

	static class PreProcessCallable implements Callable<PreprocessResult> {
		private final JSONSingleSign ss;
		private final X509Certificate[] certChain;
		private final SingleSignConstants.DigestAlgorithm algorithm;
		private final DocumentManager documentManager;
		private final DocumentCacheManager docCacheManager;

		public PreProcessCallable(final JSONSingleSign ss, final X509Certificate[] certChain,
                final SingleSignConstants.DigestAlgorithm algorithm,
                final DocumentManager docManager,
                final DocumentCacheManager docCacheManager) {
			this.ss = ss;
			this.certChain = certChain;
			this.algorithm = algorithm;
			this.documentManager = docManager;
			this.docCacheManager = docCacheManager;
		}

		@Override
		public PreprocessResult call() throws Exception {

			PreprocessResult result;
			try {
				final TriphaseData presignature = JSONSingleSignPreProcessor.doPreProcess(this.ss, this.certChain,
					this.algorithm, this.documentManager,
					this.docCacheManager);
				result = new PreprocessResult(presignature);
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING, "Error en la pretfirma del documento: " + this.ss.getId(), e); //$NON-NLS-1$
				final ProcessResult errorResult = new ProcessResult(Result.ERROR_PRE, e.getMessage());
				errorResult.setId(this.ss.getId());
				final ResultSingleSign singleResult = new ResultSingleSign(this.ss.getId(), false, errorResult);
				result = new PreprocessResult(singleResult);

			}

			return result;
		}


	}

	static class PostProcessCallable implements Callable<ResultSingleSign> {

		private final JSONSingleSign ss;
		private final X509Certificate[] certChain;
		private final TriphaseData td;
		private final SingleSignConstants.DigestAlgorithm algorithm;
		private final String batchId;
		private final DocumentManager documentManager;
		private final DocumentCacheManager docCacheManager;

		public PostProcessCallable(final JSONSingleSign ss, final X509Certificate[] certChain,
                final TriphaseData td, final SingleSignConstants.DigestAlgorithm algorithm,
                final String batchId, final DocumentManager docManager,
                final DocumentCacheManager docCacheManager) {
			this.ss = ss;
			this.certChain = certChain;
			this.td = td;
			this.algorithm = algorithm;
			this.batchId = batchId;
			this.documentManager = docManager;
			this.docCacheManager = docCacheManager;
		}

		@Override
		public ResultSingleSign call() {
			try {
				JSONSingleSignPostProcessor.doPostProcess(this.ss, this.certChain, this.td,
														this.algorithm, this.batchId, this.documentManager,
														this.docCacheManager);
			}
			catch(final Exception e) {
				LOGGER.log(Level.WARNING, "Error en la postfirma del documento: " + this.ss.getId(), e); //$NON-NLS-1$
				final ProcessResult result = new ProcessResult(Result.ERROR_POST, e.getMessage());
				return new ResultSingleSign(this.ss.getId(), false, result);
			}
			return new ResultSingleSign(this.ss.getId(), true, ProcessResult.PROCESS_RESULT_OK_UNSAVED);
		}
	}

	static class JSONSaveCallable implements Callable<ResultSingleSign> {

		private final JSONSingleSign ss;
		private final DocumentManager documentManager;
		private final X509Certificate[] certChain;
		private final TempStore ts;
		private final String batchId;

		public JSONSaveCallable(final JSONSingleSign ss, final DocumentManager documentManager, final TempStore ts, final X509Certificate[] certChain, final String batchId) {
			this.ss = ss;
			this.documentManager = documentManager;
			this.ts = ts;
			this.certChain = certChain;
			this.batchId = batchId;
		}

		@Override
		public ResultSingleSign call() {
			try {
				final byte[] dataToSave = this.ts.retrieve(this.ss, this.batchId);
				final Properties singleSignProps = this.ss.getExtraParams();
				singleSignProps.put("format", this.ss.getSignFormat().toString()); //$NON-NLS-1$
				this.documentManager.storeDocument(this.ss.getDataRef(), this.certChain, dataToSave, singleSignProps);
			}
			catch(final Exception e) {
				LOGGER.log(Level.WARNING, "No se puede almacenar la firma del documento: " + this.ss.getId(), e); //$NON-NLS-1$
				final ProcessResult result = new ProcessResult(Result.DONE_BUT_ERROR_SAVING, "Error al almacenar la firma del documento"); //$NON-NLS-1$
				return new ResultSingleSign(this.ss.getId(), false, result);
			}
			return new ResultSingleSign(this.ss.getId(), true, ProcessResult.PROCESS_RESULT_DONE_SAVED);
		}
	}
}

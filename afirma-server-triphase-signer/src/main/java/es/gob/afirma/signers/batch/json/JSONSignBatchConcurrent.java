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
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

import org.json.JSONArray;
import org.json.JSONObject;

import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.batch.BatchException;
import es.gob.afirma.signers.batch.ProcessResult;
import es.gob.afirma.signers.batch.ProcessResult.Result;
import es.gob.afirma.signers.batch.TempStore;
import es.gob.afirma.signers.batch.TempStoreFactory;
import es.gob.afirma.triphase.server.ConfigManager;
import es.gob.afirma.triphase.server.document.BatchDocumentManager;

/** Lote de firmas electr&oacute;nicas que se ejecuta en paralelo. */
public final class JSONSignBatchConcurrent extends JSONSignBatch {


	private final long concurrentTimeout;
	private final int concurrentMaxSigns;

	/**
	 * Crea un lote de firmas que se ejecuta de forma concurrente por cada una de sus firmas.
	 * @param json JSON de definici&oacute;n de lote.
	 * @throws IOException Si hay problemas en la creaci&oacute;n del lote.
	 * @throws SecurityException Si se sobrepasa alguna de las limitaciones establecidas para el lote
	 * (n&uacute;mero de documentos, tama&ntilde;o de las referencias, tama&ntilde;o de documento, etc.)
	 */
	public JSONSignBatchConcurrent(final byte[] json) throws IOException, SecurityException {
		super(json);

		this.concurrentTimeout = ConfigManager.getConcurrentTimeout();
		this.concurrentMaxSigns = ConfigManager.getConcurrentMaxSigns();
	}

	@Override
	public JSONObject doPreBatch(final X509Certificate[] certChain) throws BatchException {

		final ExecutorService executorService = Executors.newFixedThreadPool(this.concurrentMaxSigns);
		final Collection<Callable<PreprocessResult>> callables = new ArrayList<>(this.signs.size());

		for (final JSONSingleSign ss : this.signs) {
			final Callable<PreprocessResult> callable = ss.getPreProcessCallable(certChain, this.algorithm, this.documentManager,
					this.docCacheManager);
			callables.add(callable);
		}

		final List<Future<PreprocessResult>> results;
		try {
			results = executorService.invokeAll(callables, this.concurrentTimeout, TimeUnit.SECONDS);
		}
		catch (final InterruptedException e) {
			stopExecution(executorService);
			throw new BatchException(
				"Error en el preproceso en paralelo del lote de firma", e); //$NON-NLS-1$
		}

		boolean needStop = false;

		final Map<String, TriphaseData> trisigns = new HashMap<>();
		final List<ResultSingleSign> errors = new ArrayList<>();

		for (int i = 0 ; i < results.size() ; i++) {
			final Future<PreprocessResult> futureResult = results.get(i);

			// Si por algun motivo se ha pedido detener la operacion, se
			// detiene y no continuamos
			if (needStop) {
				futureResult.cancel(true);
				continue;
			}

			try {
				final PreprocessResult preprocessResult = futureResult.get(this.concurrentTimeout, TimeUnit.SECONDS);

				// Si durante la ejecucion se cancelo el proceso, no continuamos.
				if (futureResult.isCancelled()) {
					continue;
				}

				// Registramos el resultado como exito o error segun corresponda
				if (preprocessResult.getPresign() != null) {
					final TriphaseData triphaseData = preprocessResult.getPresign();
					trisigns.put(this.signs.get(i).getId(), triphaseData);
				}
				else {
					final ResultSingleSign singleResult = preprocessResult.getSignResult();
					errors.add(singleResult);
					if (this.stopOnError) {
						needStop = true;
						stopExecution(executorService);
					}
				}
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING, "Error no esperado en la prefirma de una de las firmas del lote", e); //$NON-NLS-1$
				final ProcessResult errorResult = new ProcessResult(Result.ERROR_PRE, null);
				final ResultSingleSign singleResult = new ResultSingleSign(this.signs.get(i).getId(), false, errorResult);
				errors.add(singleResult);
				if (this.stopOnError) {
					needStop = true;
					stopExecution(executorService);
					break;
				}
			}
		}

		executorService.shutdown();

		// Si se detecto un error que obligo a detener el proceso, se
		// identifica a que firma pertenecia, se establece que todas
		// las demas se omiten y se eliminan los resultados obtenidos
		if (needStop) {
			// Como el proceso se detiene ante el primer error, este
			// sera el de la firma fallida
			final String failedSignId = errors.get(0).getId();
			final ProcessResult skippedResult = new ProcessResult(Result.SKIPPED, null);
			for (final JSONSingleSign ssign : this.signs) {
				if (!failedSignId.equals(ssign.getId())) {
					errors.add(new ResultSingleSign(ssign.getId(), false, skippedResult));
				}
			}
			trisigns.clear();
		}

		return buildPreBatchJson(trisigns, errors);
	}

	private JSONObject buildPreBatchJson(final Map<String, TriphaseData> trisigns, final List<ResultSingleSign> errors) {

		// Listado de prefirmas y errores
		final JSONArray trisignsJson = new JSONArray();
		final JSONArray errorsJson = new JSONArray();

		// Establecemos los errores y exitos de todas las firmas
		for (final ResultSingleSign error : errors) {
			errorsJson.put(buildJSONSingleResut(error));
		}

		for (String signatureId : trisigns.keySet().toArray(new String[0])) {
			TriphaseData trisign = trisigns.get(signatureId);
			trisignsJson.put(TriphaseDataParser.triphaseDataToJson(trisign));
		}

		return buildPreBatch(this.format.toString(), trisignsJson, errorsJson);
	}

	@Override
	public String doPostBatch(final X509Certificate[] certChain,
                              final TriphaseData td) throws BatchException {

		if (td == null) {
			throw new IllegalArgumentException(
				"Los datos de sesion trifasica no pueden ser nulos" //$NON-NLS-1$
			);
		}

		final ExecutorService executorService = Executors.newFixedThreadPool(this.concurrentMaxSigns);
		final Collection<Callable<ResultSingleSign>> callables = new ArrayList<>(this.signs.size());

		// Indica si se debe detener la ejecucion de las operaciones
		boolean needStop = false;

		for (final JSONSingleSign ss : this.signs) {

			// Si la firma ya esta registrada como finalizada, es que fallo previamente y no se
			// postfirmara
			if (ss.getProcessResult().isFinished()) {
				continue;
			}

			// Si no se encuentran firmas con ese identificador, es que fallaron en la prefirma.
			// Esto no deberia ocurrir ya que deberia haber un estado de error senalandolo antes
			if (td.getTriSigns(ss.getId()) == null) {
				if (this.stopOnError) {
					LOGGER.warning("Se detecto un error previo en la firma, se ignoraran el resto de elementos"); //$NON-NLS-1$
					needStop = true;
				}
				else {
					LOGGER.warning("Se detecto un error previo en la firma, se continua con el resto de elementos"); //$NON-NLS-1$
				}
				ss.setProcessResult(new ProcessResult(ProcessResult.Result.ERROR_PRE, "Error en la prefirma")); //$NON-NLS-1$
				skipTheOthersSigns(ss.getId());
				break;
			}

			final Callable<ResultSingleSign> callable = ss.getPostProcessCallable(
				certChain, td, this.algorithm, getId(), this.documentManager, this.docCacheManager
			);
			callables.add(callable);
		}

		// Si se encontro algun error y no se permitian, devolvemos inmediantamente el resultado
		if (needStop) {
			return getResultLog();
		}

		final List<Future<ResultSingleSign>> results;
		try {
			results = executorService.invokeAll(callables, this.concurrentTimeout, TimeUnit.SECONDS);
		}
		catch (final InterruptedException e) {
			stopExecution(executorService);
			throw new BatchException(
				"Error en el postproceso en paralelo del lote de firma: " + e, //$NON-NLS-1$
				e
			);
		}


		for (int i = 0; i < results.size(); i++) {

			final Future<ResultSingleSign> futureResult = results.get(i);

			// Si por algun motivo se ha pedido detener la operacion, se
			// detiene y no continuamos
			if (needStop) {
				futureResult.cancel(true);
				continue;
			}


			final ResultSingleSign signatureResult;
			try {
				signatureResult = futureResult.get(this.concurrentTimeout, TimeUnit.SECONDS);
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING, "Error en una postfirma en paralelo del lote de firma", e); //$NON-NLS-1$

				if (this.stopOnError) {
					LOGGER.severe("Se interrumpe la postfirma de todos los elementos del lote al detectar un error"); //$NON-NLS-1$
					needStop = true;
					stopExecution(executorService);
					skipTheOthersSigns(this.signs.get(i).getId());
				}
				continue;
			}

			// Si durante la ejecucion se cancelo el proceso, no continuamos.
			if (futureResult.isCancelled()) {
				continue;
			}

			final JSONSingleSign singleSign = getSingleSignById(signatureResult.getId());
			if (singleSign == null) {
				LOGGER.warning("No se encontro en el lote una firma de la que se obtuvo resultado: " + signatureResult.getId()); //$NON-NLS-1$
				continue;
			}


			// Si todo fue bien
			if (signatureResult.isCorrect()) {
				singleSign.setProcessResult(ProcessResult.PROCESS_RESULT_OK_UNSAVED);
			}
			// Si hay error
			else {
				singleSign.setProcessResult(
						new ProcessResult(
								ProcessResult.Result.ERROR_POST,
								signatureResult.getResult().getDescription()));

				if (this.stopOnError) {
					LOGGER.severe(
							"Error en una de las firmas del lote (" + signatureResult.getId() + "), se parara el proceso: " + signatureResult.getResult().getDescription() //$NON-NLS-1$ //$NON-NLS-2$
							);
					needStop = true;
					stopExecution(executorService);
					skipTheOthersSigns(signatureResult.getId());
				}
				else {
					LOGGER.log(Level.WARNING,
							"Error en una de las firmas del lote (" + signatureResult.getId() + "), se continua con el siguiente elemento: " + signatureResult.getResult().getDescription()); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
		}

		// En este punto las firmas estan en almacenamiento temporal

		// Si hubo errores y se indico parar en caso de error, no hacemos los guardados de datos,
		// borramos los temporales y enviamos el log
		if (needStop) {
			deleteAllTemps();
			return getResultLog();
		}

		// En otro caso procedemos a la subida de datos
		final TempStore ts = TempStoreFactory.getTempStore();

		final Collection<Callable<ResultSingleSign>> saveCallables = new ArrayList<>(this.signs.size());
		for (final JSONSingleSign ss : this.signs) {

			// Si la firma ya esta registrada como finalizada, es que fallo previamente y no se
			// almacenara. Tambien se abortara el guardado si no se tiene resultado de esa firma,
			// aunque esto no deberia ocurrir ya que deberia haber tenido un estado finalizado
			if (ss.getProcessResult().isFinished() || td.getTriSigns(ss.getId()) == null) {
				continue;
			}

			final Callable<ResultSingleSign> callable = ss.getSaveCallableJSON(
				ts, certChain, getId()
			);
			saveCallables.add(callable);
		}

		final List<Future<ResultSingleSign>> saveResults;
		try {
			saveResults = executorService.invokeAll(saveCallables, this.concurrentTimeout, TimeUnit.SECONDS);
		}
		catch (final InterruptedException e) {
			stopExecution(executorService);
			throw new BatchException(
				"Error en el guardado en paralelo del lote de firma: " + e, //$NON-NLS-1$
				e
			);
		}

		for (int i = 0; i < saveResults.size(); i++) {

			final Future<ResultSingleSign> futureResult = saveResults.get(i);

			if (needStop) {
				futureResult.cancel(true);
				continue;
			}


			final ResultSingleSign result;
			try {
				result = futureResult.get(this.concurrentTimeout, TimeUnit.SECONDS);
			}
			catch(final Exception e) {
				LOGGER.log(Level.WARNING, "Error en el guardado en paralelo de una firma del lote", e); //$NON-NLS-1$
				if (this.stopOnError) {
					LOGGER.severe("Se interrumpe el guardado del lote al detectar un error"); //$NON-NLS-1$
					needStop = true;
					stopExecution(executorService);
					skipTheOthersSigns(this.signs.get(i).getId());
				}
				continue;
			}

			if (result.isCorrect()) {
				getSingleSignById(result.getId()).setProcessResult(
						ProcessResult.PROCESS_RESULT_DONE_SAVED
				);
			}
			else {
				getSingleSignById(result.getId()).setProcessResult(
						new ProcessResult(
								ProcessResult.Result.DONE_BUT_ERROR_SAVING,
								result.getResult().getDescription()));
				LOGGER.warning("Error en lel guardado en paralelo del lote de firma"); //$NON-NLS-1$
				if (this.stopOnError) {
					LOGGER.severe("Error en el guardadoSe interrumpe el guardado del lote al detectar un error"); //$NON-NLS-1$
					needStop = true;
					stopExecution(executorService);
					skipTheOthersSigns(this.signs.get(i).getId());
				}
			}
		}

		// Cerramos el servicio de ejecucion concurrente
		executorService.shutdown();

		// Borramos temporales
		deleteAllTemps();

		// Tenemos los datos subidos, ahora hay que, si hubo error, deshacer
		// los que se subiesen antes del error si se indico parar en error
		if (needStop) {
			for (final JSONSingleSign ss : this.signs) {
				if (ss.getProcessResult().wasSaved()) {

					if (BatchDocumentManager.class.isAssignableFrom(this.documentManager.getClass())) {
						final Properties singleSignProps = new Properties();
						singleSignProps.put("format", ss.getSignFormat().toString()); //$NON-NLS-1$
						try {
							((BatchDocumentManager) this.documentManager).rollback(ss.getDataRef(), certChain, singleSignProps);
						} catch (final IOException e) {
							LOGGER.severe(
									"No se pudo deshacer el guardado de una firma (" + ss.getId() + ") despues de la cancelacion del lote: " + e //$NON-NLS-1$ //$NON-NLS-2$
									);
						}
					}

					ss.setProcessResult(ProcessResult.PROCESS_RESULT_ROLLBACKED);
				}
			}
		}

		return getResultLog();
	}

	private void skipTheOthersSigns(final String signId) {
		for (final JSONSingleSign ss : this.signs) {
			if (!ss.getId().equals(signId)) {
				ss.setProcessResult(ProcessResult.PROCESS_RESULT_SKIPPED);
			}
		}
	}

	private JSONSingleSign getSingleSignById(final String singleSignId) {
		for (final JSONSingleSign ss: this.signs) {
			if (ss.getId().equals(singleSignId)) {
				return ss;
			}
		}
		return null;
	}

	private static JSONObject buildJSONSingleResut(final ResultSingleSign singleResult) {
		final JSONObject jsonResult = new JSONObject();
		jsonResult.put("id", singleResult.getId()); //$NON-NLS-1$
		final ProcessResult processResult = singleResult.getResult();
		jsonResult.put("result", processResult.getResult().name()); //$NON-NLS-1$
		if (processResult.getDescription() != null) {
			jsonResult.put("description", processResult.getDescription()); //$NON-NLS-1$
		}
		return jsonResult;
	}

	/**
	 * Detiene la ejecuci&oacute;n de las operaciones concurrentes.
	 * @param executorService Servicio de ejecuci&oacute;n.
	 */
	private static void stopExecution(final ExecutorService executorService) {
		executorService.shutdown();
		try {
		    if (!executorService.awaitTermination(200, TimeUnit.MILLISECONDS)) {
		        executorService.shutdownNow();
		    }
		}
		catch (final InterruptedException ex) {
			LOGGER.warning(
				"Error intentando hacer una parada controlada del firmador concurrente de lotes de firma: " + ex //$NON-NLS-1$
			);
		    executorService.shutdownNow();
		}
	}
}

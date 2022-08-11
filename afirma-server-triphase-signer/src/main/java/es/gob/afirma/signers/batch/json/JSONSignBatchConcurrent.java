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
import java.util.List;
import java.util.Properties;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.batch.BatchException;
import es.gob.afirma.signers.batch.ProcessResult;
import es.gob.afirma.signers.batch.TempStore;
import es.gob.afirma.signers.batch.TempStoreFactory;
import es.gob.afirma.signers.batch.json.JSONSingleSign.CallableResult;
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
	 * (n&ueacute;mero de documentos, tama&ntilde;o de las referencias, tama&ntilde;o de documento, etc.)
	 */
	public JSONSignBatchConcurrent(final byte[] json) throws IOException, SecurityException {
		super(json);

		this.concurrentTimeout = ConfigManager.getConcurrentTimeout();
		this.concurrentMaxSigns = ConfigManager.getConcurrentMaxSigns();
	}

	@Override
	public String doPreBatch(final X509Certificate[] certChain) throws BatchException {

		final ExecutorService executorService = Executors.newFixedThreadPool(this.concurrentMaxSigns);
		final Collection<Callable<String>> callables = new ArrayList<>(this.signs.size());

		for (final JSONSingleSign ss : this.signs) {
			final Callable<String> callable = ss.getPreProcessCallable(certChain, this.algorithm, this.documentManager,
																	this.docCacheManager);
			callables.add(callable);
		}

		final List<Future<String>> results;
		try {
			results = executorService.invokeAll(callables, this.concurrentTimeout, TimeUnit.SECONDS);
		}
		catch (final InterruptedException e) {
			stopExecution(executorService);
			throw new BatchException(
				"Error en el preproceso en paralelo del lote de firma: " + e, //$NON-NLS-1$
				e
			);
		}

		final StringBuilder sb = new StringBuilder("{"); //$NON-NLS-1$
		sb.append("\"format\":\"" + this.format + "\",");  //$NON-NLS-1$//$NON-NLS-2$
		sb.append("\"signs\": ["); //$NON-NLS-1$

		for (int i = 0 ; i < results.size() ; i++) {
			final String tmp;
			try {
				tmp = results.get(i).get(this.concurrentTimeout, TimeUnit.SECONDS);
			}
			catch (final Exception e) {
				if (this.stopOnError) {
					stopExecution(executorService);
					throw new BatchException(
						"Error en la prefirma de una de las firmas del lote, se parara el proceso", e //$NON-NLS-1$
					);
				}
				LOGGER.log(Level.WARNING,
					"Error en la prefirma de una de las firmas del lote, se continua con el siguiente elemento: " + e, //$NON-NLS-1$
					e
				);
				continue;
			}
			sb.append(tmp);
			if (results.size() -1 != i) {
				sb.append(","); //$NON-NLS-1$
			}
		}

		sb.append("]}"); //$NON-NLS-1$

		executorService.shutdown();

		return sb.toString();
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
		final Collection<Callable<CallableResult>> callables = new ArrayList<>(this.signs.size());

		boolean ignoreRemaining = false;
		boolean error = false;

		for (final JSONSingleSign ss : this.signs) {

			// Si no se encuentran firmas con ese identificador, es que fallaron en la prefirma
			if (td.getTriSigns(ss.getId()) == null) {
				error = true;
				if (this.stopOnError) {
					LOGGER.warning("Se detecto un error previo en la firma, se ignoraran el resto de elementos"); //$NON-NLS-1$
					ignoreRemaining = true;
				}
				else {
					LOGGER.warning("Se detecto un error previo en la firma, se continua con el resto de elementos"); //$NON-NLS-1$
				}
				ss.setProcessResult(new ProcessResult(ProcessResult.Result.ERROR_PRE, "Error en la prefirma")); //$NON-NLS-1$
				continue;
			}

			final Callable<CallableResult> callable = ss.getPostProcessCallable(
				certChain, td, this.algorithm, getId(), this.documentManager, this.docCacheManager
			);
			callables.add(callable);
		}

		final List<Future<CallableResult>> results;
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

		for (final Future<CallableResult> f : results) {

			final CallableResult signatureResult;
			try {
				signatureResult = f.get(this.concurrentTimeout, TimeUnit.SECONDS);
			}
			catch (final Exception e) {
				LOGGER.log(Level.WARNING, "Error en la postfirma en paralelo del lote de firma", e); //$NON-NLS-1$
				error = true;
				if (this.stopOnError) {
					LOGGER.severe("Se interrumpe la postfirma de todos los elementos del lote al detectar un error"); //$NON-NLS-1$
					stopExecution(executorService);
					break;
				}
				continue;
			}

			final JSONSingleSign singleSign = getSingleSignById(signatureResult.getSignatureId());
			if (!ignoreRemaining) {

				// Si hay error
				if (!signatureResult.isOk()) {
					error = true;
					if (singleSign != null) {
						singleSign.setProcessResult(
								new ProcessResult(ProcessResult.Result.ERROR_POST, signatureResult.getError().toString())
								);
					}

					if (this.stopOnError) {
						LOGGER.severe(
								"Error en una de las firmas del lote (" + signatureResult.getSignatureId() + "), se parara el proceso: " + signatureResult.getError() //$NON-NLS-1$ //$NON-NLS-2$
								);
						ignoreRemaining = true;
					}
					else {
						LOGGER.log(Level.WARNING,
								"Error en una de las firmas del lote (" + signatureResult.getSignatureId() + "), se continua con el siguiente elemento: " + signatureResult.getError(), //$NON-NLS-1$ //$NON-NLS-2$
								signatureResult.getError());
					}
				}
				// Si todo fue bien
				else {
					if (singleSign != null) {
						singleSign.setProcessResult(
							ProcessResult.PROCESS_RESULT_OK_UNSAVED);
					}
				}
			}
			// Cuando se indica que se pare en caso de error, se marcan las firmas que no se han
			// llegado a procesar
			else {
				if (singleSign != null) {
					singleSign.setProcessResult(
							ProcessResult.PROCESS_RESULT_SKIPPED
							);
				}
			}

		}

		// En este punto las firmas estan en almacenamiento temporal

		// Si hubo errores y se indico parar en caso de error, no hacemos los guardados de datos,
		// borramos los temporales y enviamos el log
		if (error && this.stopOnError) {
			deleteAllTemps();
			return getResultLog();
		}

		// En otro caso procedemos a la subida de datos

		final TempStore ts = TempStoreFactory.getTempStore();

		final Collection<Callable<CallableResult>> saveCallables = new ArrayList<>(this.signs.size());
		for (final JSONSingleSign ss : this.signs) {

			// Si no se encuentra la firma, es que fallo en una operacion anterior
			// y no debe intentar guardarse
			if (td.getTriSigns(ss.getId()) == null) {
				continue;
			}

			final Callable<CallableResult> callable = ss.getSaveCallableJSON(
				ts, certChain, getId()
			);
			saveCallables.add(callable);
		}

		final List<Future<CallableResult>> saveResults;
		try {
			saveResults = executorService.invokeAll(saveCallables, this.concurrentTimeout, TimeUnit.SECONDS);
		}
		catch (final InterruptedException e) {
			stopExecution(executorService);
			throw new BatchException(
				"Error en el preproceso en paralelo del lote de firma: " + e, //$NON-NLS-1$
				e
			);
		}

		for (final Future<CallableResult> f : saveResults) {
			final CallableResult result;
			try {
				result = f.get(this.concurrentTimeout, TimeUnit.SECONDS);
			}
			catch(final Exception e) {
				LOGGER.log(Level.WARNING, "Error en la recogida del resultado del guardado en paralelo del lote de firma", e); //$NON-NLS-1$
				error = true;
				if (this.stopOnError) {
					LOGGER.severe("Se interrumpe el guardado del lote al detectar un error"); //$NON-NLS-1$
					stopExecution(executorService);
					break;
				}
				continue;
			}

			if (result.isOk()) {
				getSingleSignById(result.getSignatureId()).setProcessResult(
						ProcessResult.PROCESS_RESULT_DONE_SAVED
				);
			}
			else {
				error = true;
				getSingleSignById(result.getSignatureId()).setProcessResult(
					new ProcessResult(
							ProcessResult.Result.DONE_BUT_ERROR_SAVING,
						result.getError().toString()
					)
				);
				if (this.stopOnError) {
					stopExecution(executorService);
					break;
				}
			}
		}

		// Cerramos el servicio de ejecucion concurrente
		executorService.shutdown();

		// Borramos temporales
		deleteAllTemps();

		// Tenemos los datos subidos, ahora hay que, si hubo error, deshacer
		// los que se subiesen antes del error si se indico parar en error
		if (error && this.stopOnError) {
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

	private JSONSingleSign getSingleSignById(final String singleSignId) {
		for (final JSONSingleSign ss: this.signs) {
			if (ss.getId().equals(singleSignId)) {
				return ss;
			}
		}
		return null;
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

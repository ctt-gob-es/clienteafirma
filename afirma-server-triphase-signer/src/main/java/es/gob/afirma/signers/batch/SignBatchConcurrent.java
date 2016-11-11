package es.gob.afirma.signers.batch;

import java.io.IOException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.batch.SingleSign.CallableResult;
import es.gob.afirma.signers.batch.SingleSign.ProcessResult;
import es.gob.afirma.signers.batch.SingleSign.ProcessResult.Result;
import es.gob.afirma.signers.batch.SingleSignConstants.SignAlgorithm;

/** Lote de firmas electr&oacute;nicas que se ejecuta en paralelo.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class SignBatchConcurrent extends SignBatch {

	/** Crea un lote de firmas que se ejecuta de forma concurrente por cada una de sus firmas.
	 * @param xml XML de definici&oacute;n de lote.
	 * @throws IOException Si hay problemas en la creaci&oacute;n del lote. */
	public SignBatchConcurrent(final byte[] xml) throws IOException {
		super(xml);
	}

	/** Crea un lote de firmas para ejecuci&oacute;n paralela.
	 * @param signs Firmas del lote.
	 * @param algo ALgoritmo de firma para todo el lote.
	 * @param soe <code>true</code> si se debe parar el proceso al encontrar un error,
	 *            <code>false</code> si se deben intentar las firmas restantes del lote aun
	 *            cuando una previa ha resultado en error. */
	public SignBatchConcurrent(final List<SingleSign> signs,
			                   final SignAlgorithm algo,
			                   final boolean soe) {
		super(signs, algo, soe);
	}

	@Override
	public String doPreBatch(final X509Certificate[] certChain) throws BatchException {

		final ExecutorService executorService = Executors.newFixedThreadPool(BatchConfigManager.getMaxCurrentSigns());
		final Collection<Callable<String>> callables = new ArrayList<Callable<String>>(this.signs.size());

		for (final SingleSign ss : this.signs) {
			final Callable<String> callable = ss.getPreProcessCallable(certChain, this.algorithm);
			callables.add(callable);
		}

		final List<Future<String>> results;
		try {
			results = executorService.invokeAll(callables, this.concurrentTimeout, TimeUnit.SECONDS);
		}
		catch (final InterruptedException e) {
			throw new BatchException(
				"Error en el preproceso en paralelo del lote de firma: " + e, //$NON-NLS-1$
				e
			);
		}

		final StringBuilder sb = new StringBuilder("<xml>\n <firmas>"); //$NON-NLS-1$

		for (final Future<String> f : results) {
			String tmp;
			try {
				tmp = f.get();
			}
			catch (final Exception e) {
				if (this.stopOnError) {
					throw new BatchException(
						"Error en una de las firmas del lote, se parara el proceso: " + e, e //$NON-NLS-1$
					);
				}
				LOGGER.log(Level.SEVERE,
					"Error en una de las firmas del lote, se continua con el siguiente elemento: " + e, //$NON-NLS-1$
					e
				);
				continue;
			}
			sb.append(tmp);
		}

		sb.append("</firmas>\n</xml>"); //$NON-NLS-1$

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

		final ExecutorService executorService = Executors.newFixedThreadPool(BatchConfigManager.getMaxCurrentSigns());
		final Collection<Callable<CallableResult>> callables = new ArrayList<Callable<CallableResult>>(this.signs.size());

		for (final SingleSign ss : this.signs) {
			final Callable<CallableResult> callable = ss.getPostProcessCallable(
				certChain, td, this.algorithm, getId()
			);
			callables.add(callable);
		}

		final List<Future<CallableResult>> results;
		try {
			results = executorService.invokeAll(callables, this.concurrentTimeout, TimeUnit.SECONDS);
		}
		catch (final InterruptedException e) {
			throw new BatchException(
				"Error en el postproceso en paralelo del lote de firma: " + e, //$NON-NLS-1$
				e
			);
		}

		boolean ignoreRemaining = false;
		boolean error = false;

		for (final Future<CallableResult> f : results) {

			final CallableResult tmp;
			try {
				tmp = f.get();
			}
			catch (final Exception e) {
				// Este caso no debe darse nunca, porque el call() del Callable no lanza excepciones
				throw new IllegalStateException(
					"Error en la recogida del resultados del postproceso en paralelo del lote de firma: " + e, //$NON-NLS-1$
					e
				);
			}

			if (!ignoreRemaining) {

				// Si hay error
				if (!tmp.isOk()) {

					error = true;

					getSingleSignById(tmp.getSignatureId()).setProcessResult(
						new ProcessResult(Result.ERROR_POST, tmp.getError().toString())
					);

					if (this.stopOnError) {
						LOGGER.severe(
								"Error en una de las firmas del lote (" + tmp.getSignatureId() + "), se parara el proceso: " + tmp.getError() //$NON-NLS-1$ //$NON-NLS-2$
								);
						ignoreRemaining = true;
					}
					else {
						LOGGER.log(Level.WARNING,
								"Error en una de las firmas del lote (" + tmp.getSignatureId() + "), se continua con el siguiente elemento: " + tmp.getError() //$NON-NLS-1$ //$NON-NLS-2$
								, tmp.getError());
					}
				}
				// Si todo fue bien
				else {
					getSingleSignById(tmp.getSignatureId()).setProcessResult(
						ProcessResult.PROCESS_RESULT_OK_UNSAVED
					);
				}
			}
			// Cuando se indica que se pare en error se marcan las firmas que no se han
			// llegado a procesar
			else {
				getSingleSignById(tmp.getSignatureId()).setProcessResult(
					ProcessResult.PROCESS_RESULT_SKIPPED
				);
			}

		}

		// En este punto las firmas estan en almacenamiento temporal

		// Si hubo errores y se indico parar en error no hacemos los guardados de datos, borramos los temporales
		// y enviamos el log
		if (error && this.stopOnError) {
			deleteAllTemps();
			return getResultLog();
		}

		// En otro caso procedemos a la subida de datos

		final TempStore ts = TempStoreFactory.getTempStore();

		final Collection<Callable<CallableResult>> saveCallables = new ArrayList<Callable<CallableResult>>(this.signs.size());
		for (final SingleSign ss : this.signs) {
			final Callable<CallableResult> callable = ss.getSaveCallable(
				ts, getId()
			);
			saveCallables.add(callable);
		}

		final List<Future<CallableResult>> saveResults;
		try {
			saveResults = executorService.invokeAll(saveCallables, this.concurrentTimeout, TimeUnit.SECONDS);
		}
		catch (final InterruptedException e) {
			throw new BatchException(
				"Error en el preproceso en paralelo del lote de firma: " + e, //$NON-NLS-1$
				e
			);
		}

		for (final Future<CallableResult> f : saveResults) {
			final CallableResult result;
			try {
				result = f.get();
			}
			catch(final Exception e) {
				// Este caso no debe darse nunca, porque el call() del Callable no lanza excepciones
				throw new IllegalStateException(
					"Error en la recogida del resultados del guardado en paralelo del lote de firma: " + e, //$NON-NLS-1$
					e
				);
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
					break;
				}
			}
		}

		deleteAllTemps();

		// Tenemos los datos subidos, ahora hay que, si hubo error, deshacer
		// los que se subiesen antes del error si se indico parar en error
		if (error && this.stopOnError) {
			for (final SingleSign ss : this.signs) {
				if (ss.getProcessResult().wasSaved()) {
					ss.rollbackSave();
					ss.setProcessResult(ProcessResult.PROCESS_RESULT_ROLLBACKED);
				}
			}
		}

		return getResultLog();
	}

	private SingleSign getSingleSignById(final String id) {
		for (final SingleSign ss: this.signs) {
			if (ss.getId().equals(id)) {
				return ss;
			}
		}
		return null;
	}
}

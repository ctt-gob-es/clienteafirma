package es.gob.afirma.signers.batch;

import java.io.IOException;
import java.security.cert.X509Certificate;
import java.util.List;
import java.util.logging.Level;

import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.batch.SingleSign.ProcessResult;
import es.gob.afirma.signers.batch.SingleSign.ProcessResult.Result;
import es.gob.afirma.signers.batch.SingleSignConstants.SignAlgorithm;

/** Lote de firmas electr&oacute;nicas que se ejecuta secuencialmente.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class SignBatchSerial extends SignBatch {

	/** Crea un lote de firmas que se ejecuta secuencialmente.
	 * @param xml XML de definici&oacute;n de lote.
	 * @throws IOException Si hay problemas en la creaci&oacute;n del lote. */
	public SignBatchSerial(final byte[] xml) throws IOException {
		super(xml);
	}

	/** Crea un lote de firmas para ejecuci&oacute;n secuencial.
	 * @param signs Firmas del lote.
	 * @param algo ALgoritmo de firma para todo el lote.
	 * @param soe <code>true</code> si se debe parar el proceso al encontrar un error,
	 *            <code>false</code> si se deben intentar las firmas restantes del lote aun
	 *            cuando una previa ha resultado en error. */
	public SignBatchSerial(final List<SingleSign> signs,
                           final SignAlgorithm algo,
                           final boolean soe) {
		super(signs, algo, soe);
	}

	@Override
	public String doPreBatch(final X509Certificate[] certChain) throws BatchException {

		final StringBuilder sb = new StringBuilder("<xml>\n <firmas>"); //$NON-NLS-1$
		for (final SingleSign ss : this.signs) {
			String tmp;
			try {
				tmp = ss.doPreProcess(certChain, this.algorithm);
			}
			catch(final Exception e) {
				if (this.stopOnError) {
					throw new BatchException(
						"Error en una de las firmas del lote (" + ss.getId() + "), se parara el proceso: " + e, e //$NON-NLS-1$ //$NON-NLS-2$
					);
				}
				LOGGER.log(Level.WARNING,
						"Error en una de las firmas del lote (" + ss.getId() + "), se continua con el siguiente elemento: " + e //$NON-NLS-1$ //$NON-NLS-2$
						, e);
				continue;
			}
			sb.append(tmp);
		}
		sb.append("</firmas>\n</xml>"); //$NON-NLS-1$
		return sb.toString();
	}

	@Override
	public String doPostBatch(final X509Certificate[] certChain,
			                  final TriphaseData td) {

		if (td == null) {
			throw new IllegalArgumentException("Los datos de sesion trifasica no pueden ser nulos"); //$NON-NLS-1$
		}

		boolean ignoreRemaining = false;
		boolean error = false;

		for (final SingleSign ss : this.signs) {

			if (!ignoreRemaining) {

				try {
					ss.doPostProcess(
						certChain,
						td,
						this.algorithm,
						getId()
					);
				}
				catch (final Exception e) {

					error = true;

					ss.setProcessResult(new ProcessResult(Result.ERROR_POST, e.toString()));

					if (this.stopOnError) {
						LOGGER.log(
								Level.SEVERE,
							"Error en una de las firmas del lote (" + ss.getId() + "), se parara el proceso: " + e, //$NON-NLS-1$ //$NON-NLS-2$
							e
						);
						ignoreRemaining = true;
					}
					LOGGER.severe(
						"Error en una de las firmas del lote (" + ss.getId() + "), se continua con el siguiente elemento: " + e //$NON-NLS-1$ //$NON-NLS-2$
					);
					continue;
				}

				ss.setProcessResult(ProcessResult.PROCESS_RESULT_OK_UNSAVED);
			}
			// Cuando se indica que se pare en error se marcan las firmas que no se han
			// llegado a procesar
			else {
				ss.setProcessResult(ProcessResult.PROCESS_RESULT_SKIPPED);
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
		error = false;
		for (final SingleSign ss : this.signs) {
			try {
				ss.save(ts.retrieve(ss, getId()));
				ss.setProcessResult(ProcessResult.PROCESS_RESULT_DONE_SAVED);
			}
			catch (final IOException e) {
				LOGGER.warning("Error en el guardado de la firma: " + e); //$NON-NLS-1$
				error = true;
				ss.setProcessResult(
					new ProcessResult(
						ProcessResult.Result.DONE_BUT_ERROR_SAVING,
						e.toString()
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

}

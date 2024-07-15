/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.batch.xml;

import java.io.IOException;
import java.security.cert.X509Certificate;
import java.util.List;
import java.util.logging.Level;

import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.batch.BatchException;
import es.gob.afirma.signers.batch.ProcessResult;
import es.gob.afirma.signers.batch.SingleSignConstants.DigestAlgorithm;
import es.gob.afirma.signers.batch.TempStore;
import es.gob.afirma.signers.batch.TempStoreFactory;

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
                           final DigestAlgorithm algo,
                           final boolean soe) {
		super(signs, algo, soe);
	}

	@Override
	public String doPreBatch(final X509Certificate[] certChain) throws BatchException {

		boolean ignoreRemaining = false;
		final StringBuilder sb = new StringBuilder("<xml>\n <firmas>"); //$NON-NLS-1$
		for (final SingleSign ss : this.signs) {
			if (ignoreRemaining) {
				ss.setProcessResult(ProcessResult.PROCESS_RESULT_SKIPPED);
				continue;
			}
			final String tmp;
			try {
				tmp = ss.doPreProcess(certChain, this.algorithm);
			}
			catch(final Exception e) {
				ss.setProcessResult(new ProcessResult(ProcessResult.Result.ERROR_PRE, e.toString()));
				if (this.stopOnError) {
					ignoreRemaining = true;
					LOGGER.log(Level.WARNING,
							"Error en una de las firmas del lote (" + ss.getId() + "), se ignoraran el resto de elementos: " + e //$NON-NLS-1$ //$NON-NLS-2$
							, e);
				}
				else {
					LOGGER.log(Level.WARNING,
							"Error en una de las firmas del lote (" + ss.getId() + "), se continua con el siguiente elemento: " + e //$NON-NLS-1$ //$NON-NLS-2$
							, e);
				}
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

			// Si se ha detectado un error y no deben procesarse el resto de firmas, se marcan como tal
			if (ignoreRemaining) {
				ss.setProcessResult(ProcessResult.PROCESS_RESULT_SKIPPED);
				continue;
			}

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

				ss.setProcessResult(new ProcessResult(ProcessResult.Result.ERROR_POST, e.toString()));

				if (this.stopOnError) {
					LOGGER.log(
						Level.SEVERE,
						"Error en una de las firmas del lote (" + ss.getId() + "), se parara el proceso", //$NON-NLS-1$ //$NON-NLS-2$
						e
					);
					ignoreRemaining = true;
				}
				else {
					LOGGER.log(
							Level.SEVERE,
							"Error en una de las firmas del lote (" + ss.getId() + "), se continua con el siguiente elemento", //$NON-NLS-1$ //$NON-NLS-2$
							e
					);
				}
				continue;
			}

			ss.setProcessResult(ProcessResult.PROCESS_RESULT_OK_UNSAVED);
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
		error = false;
		for (final SingleSign ss : this.signs) {
			if (ss.getProcessResult() != null &&
					ss.getProcessResult().getResult() != ProcessResult.Result.DONE_BUT_NOT_SAVED_YET) {
				continue;
			}

			try {
				ss.save(
					ts.retrieve(
						ss,
						getId()
					)
				);
				ss.setProcessResult(ProcessResult.PROCESS_RESULT_DONE_SAVED);
			}
			catch (final IOException e) {
				LOGGER.log(Level.SEVERE, "Error en el guardado de la firma", e); //$NON-NLS-1$
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

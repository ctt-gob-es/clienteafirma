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
import java.util.Properties;
import java.util.logging.Level;

import org.json.JSONArray;
import org.json.JSONObject;

import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.batch.BatchException;
import es.gob.afirma.signers.batch.ProcessResult;
import es.gob.afirma.signers.batch.ProcessResult.Result;
import es.gob.afirma.triphase.server.document.BatchDocumentManager;

/** Lote de firmas electr&oacute;nicas que se ejecuta secuencialmente. */
public final class JSONSignBatchSerial extends JSONSignBatch {

	/**
	 * Crea un lote de firmas que se ejecuta secuencialmente.
	 * @param json JSON de definici&oacute;n de lote.
	 * @throws IOException Si hay problemas en la creaci&oacute;n del lote.
	 * @throws SecurityException Si se sobrepasa alguna de las limitaciones establecidas para el lote
	 * (n&uacute;mero de documentos, tama&ntilde;o de las referencias, tama&ntilde;o de documento, etc.)
	 * */
	public JSONSignBatchSerial(final byte[] json) throws IOException, SecurityException {
		super(json);
	}

	@Override
	public JSONObject doPreBatch(final X509Certificate[] certChain) throws BatchException {

		final JSONArray errors = new JSONArray();
		final JSONArray trisigns = new JSONArray();

		boolean ignoreRemaining = false;
		for (int i = 0 ; i < this.signs.size() ; i++) {
			final JSONSingleSign ss = this.signs.get(i);
			if (ignoreRemaining) {
				errors.put(buildSignResult(ss.getId(), Result.SKIPPED, null));
				continue;
			}

			try {
				final TriphaseData td = ss.doPreProcess(certChain, this.algorithm, this.documentManager, this.docCacheManager);
				trisigns.put(TriphaseDataParser.triphaseDataToJson(td));
			}
			catch(final Exception e) {
				errors.put(buildSignResult(ss.getId(), Result.ERROR_PRE, e));

				if (this.stopOnError) {
					ignoreRemaining = true;
					LOGGER.log(Level.WARNING,
							"Error en una de las firmas del lote (" + ss.getId() + "), se ignoraran el resto de elementos", e); //$NON-NLS-1$ //$NON-NLS-2$

				}
				else {
					LOGGER.log(Level.WARNING,
							"Error en una de las firmas del lote (" + ss.getId() + "), se continua con el siguiente elemento", e); //$NON-NLS-1$ //$NON-NLS-2$
				}
				continue;
			}
		}

		return buildPreBatch(this.format.toString(), trisigns, errors);
	}

	@Override
	public String doPostBatch(final X509Certificate[] certChain,
			                  final TriphaseData td) {

		if (td == null) {
			throw new IllegalArgumentException("Los datos de sesion trifasica no pueden ser nulos"); //$NON-NLS-1$
		}

		boolean ignoreRemaining = false;
		boolean error = false;

		for (final JSONSingleSign ss : this.signs) {

			// Si se ha detectado un error y no deben procesarse el resto de firmas, se marcan como tal
			if (ignoreRemaining) {
				ss.setProcessResult(ProcessResult.PROCESS_RESULT_SKIPPED);
				continue;
			}

			// Si la firma ya esta registrada como finalizada, es que fallo previamente y no se
			// postfirmara
			if (ss.getProcessResult().isFinished()) {
				error = true;
				if (this.stopOnError) {
					LOGGER.warning("Se detecto un error previo en la firma, se ignoraran el resto de elementos"); //$NON-NLS-1$
					ignoreRemaining = true;
				}
				continue;
			}

			// Postfirmamos la firma
			try {
				ss.doPostProcess(
					certChain,
					td,
					this.algorithm,
					getId(),
					this.documentManager,
					this.docCacheManager
				);
			}
			catch (final Exception e) {

				error = true;

				final ProcessResult.Result resultado;
				if (e instanceof AOSaveDataException) {
					resultado = ProcessResult.Result.DONE_BUT_ERROR_SAVING;
				} else {
					resultado = ProcessResult.Result.ERROR_POST;
				}

				ss.setProcessResult(new ProcessResult(resultado, e.getMessage()));

				if (this.stopOnError) {
					LOGGER.log(
							Level.SEVERE,
							"Error en una de las firmas del lote (" + ss.getId() + "), se parara el proceso: " + e, //$NON-NLS-1$ //$NON-NLS-2$
							e
							);
					ignoreRemaining = true;
				}
				else {
					LOGGER.severe(
							"Error en una de las firmas del lote (" + ss.getId() + "), se continua con el siguiente elemento: " + e //$NON-NLS-1$ //$NON-NLS-2$
							);
				}
				continue;
			}

			ss.setProcessResult(ProcessResult.PROCESS_RESULT_DONE_SAVED);
		}

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

		deleteAllTemps();

		return getResultLog();

	}
}

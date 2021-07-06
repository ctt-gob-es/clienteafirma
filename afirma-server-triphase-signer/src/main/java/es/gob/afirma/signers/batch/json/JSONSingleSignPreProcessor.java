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
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.ExtraParamsProcessor;
import es.gob.afirma.core.signers.ExtraParamsProcessor.IncompatiblePolicyException;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.signers.batch.SingleSignConstants;
import es.gob.afirma.signers.batch.TriPhaseHelper;
import es.gob.afirma.triphase.server.ConfigManager;
import es.gob.afirma.triphase.server.cache.DocumentCacheManager;
import es.gob.afirma.triphase.server.document.DocumentManager;
import es.gob.afirma.triphase.signer.processors.TriPhasePreProcessor;

final class JSONSingleSignPreProcessor {

	private static final String EXTRA_PARAM_CHECK_SIGNATURES = "checkSignatures"; //$NON-NLS-1$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Propiedad dedicada a almacenar el identificador del archivo a escribir o leer de cach&eacute;. */
	private static final String TRIPHASE_PROP_CACHE_ID = "CACHE_ID"; //$NON-NLS-1$

	private JSONSingleSignPreProcessor() {
		// No instanciable
	}

	/** Realiza el proceso de prefirma, incluyendo la descarga u obtenci&oacute;n de datos.
	 * @param sSign Firma sobre la que hay que hacer el preproceso.
	 * @param certChain Cadena de certificados del firmante.
	 * @param algorithm Algoritmo de firma.
	 * @param docManager Gestor de documentos con el que procesar el lote.
	 * @param docCacheManager Gestor para el guardado de datos en cach&eacute;.
	 * @return Nodo <code>firma</code> del JSON de datos trif&aacute;sicos (sin ninguna etiqueta
	 *         antes ni despu&eacute;s).
	 * @throws AOException Si hay problemas en la propia firma electr&oacute;nica.
	 * @throws IOException Si hay problemas en la obtenci&oacute;n, tratamiento o gradado de datos. */
	static String doPreProcess(final JSONSingleSign sSign,
			                   final X509Certificate[] certChain,
			                   final SingleSignConstants.SignAlgorithm algorithm,
			                   final DocumentManager docManager,
			                   final DocumentCacheManager docCacheManager) throws IOException,
			                                                                             AOException {
		final TriphaseData td = getPreSign(sSign, certChain, algorithm, docManager, docCacheManager);
		return TriphaseDataParser.triphaseDataToJsonString(td);
	}

	private static TriphaseData getPreSign(final JSONSingleSign sSign,
			                               final X509Certificate[] certChain,
			                               final SingleSignConstants.SignAlgorithm algorithm,
			                               final DocumentManager docManager,
			                               final DocumentCacheManager docCacheManager) throws IOException,
			                                                                                         AOException {
		if (certChain == null || certChain.length < 1) {
			throw new IllegalArgumentException(
				"La cadena de certificados del firmante no puede ser nula ni vacia" //$NON-NLS-1$
			);
		}

		// Instanciamos el preprocesador adecuado
		final TriPhasePreProcessor prep = TriPhaseHelper.getTriPhasePreProcessor(sSign);

		final byte[] docBytes = docManager.getDocument(sSign.getDataRef(), certChain, sSign.getExtraParams());

		Properties extraParams;
		try {
			extraParams = ExtraParamsProcessor.expandProperties(sSign.getExtraParams(), docBytes, sSign.getSignFormat().name());
		}
		catch (final IncompatiblePolicyException e) {
			LOGGER.log(
					Level.WARNING, "No se ha podido expandir la politica de firma. Se realizara una firma basica: " + e, e); //$NON-NLS-1$
			extraParams = sSign.getExtraParams();
		}

		// Eliminamos configuraciones que no deseemos que se utilicen externamente
		extraParams.remove("profile"); //TODO: Deshacer cuando se permita la generacion de firmas baseline //$NON-NLS-1$

		// Comprobamos si se ha pedido validar las firmas antes de agregarles una nueva
        final boolean checkSignatures = Boolean.parseBoolean(extraParams.getProperty(EXTRA_PARAM_CHECK_SIGNATURES));

        TriphaseData td;

		switch(sSign.getSubOperation()) {
			case SIGN:
				td = prep.preProcessPreSign(
						docBytes,
						algorithm.toString(),
						certChain,
						extraParams,
						checkSignatures
					);
				break;
			case COSIGN:
				td = prep.preProcessPreCoSign(
						docBytes,
						algorithm.toString(),
						certChain,
						extraParams,
						checkSignatures
					);
				break;
			case COUNTERSIGN:
				final CounterSignTarget target = CounterSignTarget.getTarget(
					extraParams.getProperty("target", CounterSignTarget.LEAFS.name()) //$NON-NLS-1$
				);
				if (!target.equals(CounterSignTarget.LEAFS) && !target.equals(CounterSignTarget.TREE)) {
					throw new IllegalArgumentException(
						"Objetivo de contrafirma no soportado en proceso por lotes: " + target //$NON-NLS-1$
					);
				}
				td = prep.preProcessPreCounterSign(
						docBytes,
						algorithm.toString(),
						certChain,
						extraParams,
						target,
						checkSignatures
					);
				break;
			default:
				throw new UnsupportedOperationException(
					"Operacion no soportada: " + sSign.getSubOperation() //$NON-NLS-1$
				);
		}

		// Agregamos los codigos de verificacion para posteriormente poder comprobar
		// que el PKCS#1 recibido se genero con el certificado de firma
		try {
			TriPhaseHelper.addVerificationCodes(td, certChain[0]);
		} catch (final Exception e) {
			throw new AOException("No se pudo agregar le codigo de verificacion de firmas", e); //$NON-NLS-1$
		}

		if (Boolean.parseBoolean(ConfigManager.isCacheEnabled())) {
			saveToCache(docCacheManager, td, docBytes);
		}

		return td;
	}

	/**
	 * A&ntilde;ade el fichero en cach&eacute; para m&aacute;s tarde leerlo
	 * @param docCacheManager Gestor para el guardado de datos en cach&eacute;.
	 * @param triphaseData datos de la prefirma
	 * @param docBytes datos a almacenar en la cach&eacute;
	 */
	private static void saveToCache(final DocumentCacheManager docCacheManager, final TriphaseData triphaseData, final byte [] docBytes) {
		try {
			final String cacheId = docCacheManager.storeDocumentToCache(docBytes);
			triphaseData.getTriSigns().get(0).addProperty(TRIPHASE_PROP_CACHE_ID, cacheId);
		} catch (final IOException e) {
			LOGGER.log(Level.WARNING, "Error en la escritura del fichero en cache", e); //$NON-NLS-1$
		}
	}
}

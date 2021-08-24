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
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.ExtraParamsProcessor;
import es.gob.afirma.core.signers.ExtraParamsProcessor.IncompatiblePolicyException;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseData.TriSign;
import es.gob.afirma.signers.batch.SingleSignConstants;
import es.gob.afirma.signers.batch.SingleSignConstants.SignSubOperation;
import es.gob.afirma.signers.batch.TempStoreFactory;
import es.gob.afirma.signers.batch.TriPhaseHelper;
import es.gob.afirma.signers.batch.xml.LegacyFunctions;
import es.gob.afirma.triphase.server.ConfigManager;
import es.gob.afirma.triphase.server.cache.DocumentCacheManager;
import es.gob.afirma.triphase.server.document.DocumentManager;
import es.gob.afirma.triphase.signer.processors.TriPhasePreProcessor;

final class JSONSingleSignPostProcessor {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Propiedad dedicada a almacenar el identificador del archivo a escribir o leer de cach&eacute;. */
	private static final String TRIPHASE_PROP_CACHE_ID = "CACHE_ID"; //$NON-NLS-1$

	private JSONSingleSignPostProcessor() {
		// No instanciable
	}

	/** Realiza el proceso de postfirma, incluyendo la subida o guardado de datos.
	 * @param sSign Firma sobre la que hay que hacer el postproceso.
	 * @param certChain Cadena de certificados del firmante.
	 * @param tdata Datos trif&aacute;sicos relativos <b>&uacute;nicamente</b> a esta firma.
	 *           Debe serializarse como un XML con esta forma (ejemplo):
	 *           <pre>
	 *{
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
	 *}
	 *           </pre>
	 * @param algorithm Algoritmo de firma.
	 * @param batchId Identificador del lote de firma.
	 * @param docManager Gestor de documentos con el que procesar el lote.
	 * @param docCacheManager Gestor para la carga de datos desde cach&eacute;.
	 * @throws AOException Si hay problemas en la propia firma electr&oacute;nica.
	 * @throws IOException Si hay problemas en la obtenci&oacute;n, tratamiento o gradado de datos.
	 * @throws NoSuchAlgorithmException Si no se soporta alg&uacute;n algoritmo necesario. */
	public static void doPostProcess(final JSONSingleSign sSign,
			                  final X509Certificate[] certChain,
			                  final TriphaseData tdata,
			                  final SingleSignConstants.SignAlgorithm algorithm,
			                  final String batchId,
			                  final DocumentManager docManager,
			                  final DocumentCacheManager docCacheManager) throws IOException,
			                                                                            AOException,
			                                                                            NoSuchAlgorithmException {
		if (certChain == null || certChain.length < 1) {
			throw new IllegalArgumentException(
				"La cadena de certificados del firmante no puede ser nula ni vacia" //$NON-NLS-1$
			);
		}

		final TriphaseData td = cleanTriphaseData(tdata, sSign.getId());

		try {
			TriPhaseHelper.checkSignaturesIntegrity(td, certChain[0]);
		}
		catch (final Exception e) {
			throw new AOException("Error en la verificacion de los PKCS#1 de las firmas recibidas", e); //$NON-NLS-1$
		}

		// Instanciamos el preprocesador adecuado
		final TriPhasePreProcessor prep = TriPhaseHelper.getTriPhasePreProcessor(sSign);

		byte[] docBytes = null;

		if (Boolean.parseBoolean(ConfigManager.isCacheEnabled())) {
			LOGGER.info("Recuperamos el documento de cache"); //$NON-NLS-1$
			final TriSign preSign = td.getTriSigns().get(0);
			if (preSign.getProperty(TRIPHASE_PROP_CACHE_ID) != null) {
				final String cacheId = preSign.getProperty(TRIPHASE_PROP_CACHE_ID);
				try {
					docBytes = docCacheManager.getDocumentFromCache(cacheId);
				}
				catch (final Exception e) {
					LOGGER.log(Level.WARNING, "No se pudo obtener un documento de la cache", e); //$NON-NLS-1$
					docBytes = null;
				}
			}
		}

		if (docBytes == null) {
			docBytes = docManager.getDocument(sSign.getDataRef(), certChain, sSign.getExtraParams());
		}

		Properties extraParams;
		try {
			extraParams = ExtraParamsProcessor.expandProperties(sSign.getExtraParams(), docBytes, sSign.getSignFormat().name());
		}
		catch (final IncompatiblePolicyException e) {
			LOGGER.log(
					Level.WARNING, "No se ha podido expandir la politica de firma. Se realizara una firma basica: " + e, e); //$NON-NLS-1$
			extraParams = sSign.getExtraParams();
		}

		//TODO: Deshacer cuando se permita la generacion de firmas baseline
		extraParams.remove("profile"); //$NON-NLS-1$

		// XXX: Codigo de soporte de firmas XAdES explicitas (Eliminar cuando se
		// abandone el soporte de XAdES explicitas)
		if (sSign.getSubOperation() == SignSubOperation.SIGN
				&& LegacyFunctions.isXadesExplicitConfigurated(sSign.getSignFormat().name(), extraParams)) {
			LOGGER.warning(
				"Se ha pedido una firma XAdES explicita, este formato dejara de soportarse en proximas versiones" //$NON-NLS-1$
			);
			try {
				docBytes = MessageDigest.getInstance("SHA1").digest(docBytes); //$NON-NLS-1$
				extraParams.setProperty("mimeType", "hash/sha1"); //$NON-NLS-1$ //$NON-NLS-2$
			} catch (final Exception e) {
				LOGGER.warning("Error al generar la huella digital de los datos para firmar como 'XAdES explicit', " //$NON-NLS-1$
					+ "se realizara una firma XAdES corriente: " + e); //$NON-NLS-1$
			}
		}

		final byte[] signedDoc;
		switch(sSign.getSubOperation()) {
			case SIGN:
				signedDoc = prep.preProcessPostSign(
					docBytes,
					algorithm.toString(),
					certChain,
					extraParams,
					td
				);
				break;
			case COSIGN:
				signedDoc = prep.preProcessPostCoSign(
					docBytes,
					algorithm.toString(),
					certChain,
					extraParams,
					td
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
				signedDoc = prep.preProcessPostCounterSign(
					docBytes,
					algorithm.toString(),
					certChain,
					extraParams,
					td,
					target
				);
				break;
			default:
				throw new UnsupportedOperationException(
					"Operacion no soportada: " + sSign.getSubOperation() //$NON-NLS-1$
				);
		}

		// Se almacenara el documento con la configuracion indicada en el DocumentManager
		if(ConfigManager.isConcurrentModeEnable()) {
			TempStoreFactory.getTempStore().store(signedDoc, sSign, batchId);
		} else {
			final Properties singleSignProps = sSign.getExtraParams();
			singleSignProps.put("format", sSign.getSignFormat().toString()); //$NON-NLS-1$
			docManager.storeDocument(sSign.getDataRef(), certChain, signedDoc, singleSignProps);
		}

	}

	/** Elimina los datos de sesi&oacute;n que no est&eacute;n relacionados con la firma actual.
	 * @param td Datos de sesi&oacute;n a limpiar.
	 * @param signId Identificador de la firma actual.
	 * @return Datos de sesi&oacute;n que contienen &uacute;nicamente informaci&oacute;n relacionada
	 *         con la firma actual. */
	private static TriphaseData cleanTriphaseData(final TriphaseData td, final String signId) {
		if (td == null) {
			throw new IllegalArgumentException("Los datos trifasicos no pueden ser nulos"); //$NON-NLS-1$
		}
		final List<TriSign> tmpTs = td.getTriSigns(signId);
		if (tmpTs == null) {
			throw new IllegalArgumentException(
				"Los datos trifasicos proporcionados no contienen una firma con ID=" + signId //$NON-NLS-1$
			);
		}

		return new TriphaseData(tmpTs);
	}

}

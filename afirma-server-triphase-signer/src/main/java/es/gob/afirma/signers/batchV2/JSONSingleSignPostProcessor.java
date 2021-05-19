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
import es.gob.afirma.triphase.server.document.DocumentManager;
import es.gob.afirma.triphase.signer.processors.TriPhasePreProcessor;

final class JSONSingleSignPostProcessor {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

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
	 * @throws AOException Si hay problemas en la propia firma electr&oacute;nica.
	 * @throws IOException Si hay problemas en la obtenci&oacute;n, tratamiento o gradado de datos.
	 * @throws NoSuchAlgorithmException Si no se soporta alg&uacute;n algoritmo necesario. */
	static void doPostProcess(final JSONSingleSign sSign,
			                  final X509Certificate[] certChain,
			                  final TriphaseData tdata,
			                  final JSONSingleSignConstants.SignAlgorithm algorithm,
			                  final String batchId,
			                  final DocumentManager docManager) throws IOException,
			                                                                            AOException,
			                                                                            NoSuchAlgorithmException {
		if (certChain == null || certChain.length < 1) {
			throw new IllegalArgumentException(
				"La cadena de certificados del firmante no puede ser nula ni vacia" //$NON-NLS-1$
			);
		}

		final TriphaseData td = cleanTriphaseData(tdata, sSign.getId());

		try {
			JSONTriPhaseHelper.checkSignaturesIntegrity(td, certChain[0]);
		}
		catch (final Exception e) {
			throw new AOException("Error en la verificacion de los PKCS#1 de las firmas recibidas", e); //$NON-NLS-1$
		}

		// Instanciamos el preprocesador adecuado
		final TriPhasePreProcessor prep = JSONSingleSignConstants.getTriPhasePreProcessor(sSign);

		final byte[] docBytes = docManager.getDocument(sSign.getReference(), certChain, null);

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
		if(JSONBatchConfigManager.isConcurrentMode()) {
			JSONTempStoreFactory.getTempStore().store(signedDoc, sSign, batchId);
		} else {
			docManager.storeDocument(sSign.getId(), certChain, signedDoc, extraParams);
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

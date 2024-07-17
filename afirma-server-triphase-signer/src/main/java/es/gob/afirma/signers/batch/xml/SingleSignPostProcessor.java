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
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.ExtraParamsProcessor;
import es.gob.afirma.core.signers.ExtraParamsProcessor.IncompatiblePolicyException;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseData.TriSign;
import es.gob.afirma.signers.batch.LegacyFunctions;
import es.gob.afirma.signers.batch.SingleSignConstants;
import es.gob.afirma.signers.batch.SingleSignConstants.SignSubOperation;
import es.gob.afirma.signers.batch.TempStoreFactory;
import es.gob.afirma.signers.batch.TriPhaseHelper;
import es.gob.afirma.triphase.signer.processors.TriPhasePreProcessor;

final class SingleSignPostProcessor {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private SingleSignPostProcessor() {
		// No instanciable
	}

	/** Realiza el proceso de postfirma, incluyendo la subida o guardado de datos.
	 * @param sSign Firma sobre la que hay que hacer el postproceso.
	 * @param certChain Cadena de certificados del firmante.
	 * @param tdata Datos trif&aacute;sicos relativos <b>&uacute;nicamente</b> a esta firma.
	 *           Debe serializarse como un XML con esta forma (ejemplo):
	 *           <pre>
	 *            &lt;xml&gt;
	 *             &lt;firmas&gt;
	 *              &lt;firma Id="53820fb4-336a-47ee-b7ba-f32f58e5cfd6"&gt;
	 *               &lt;param n="PRE"&gt;MYICXDAYBgk[...]GvykA=&lt;/param&gt;
	 *               &lt;param n="PK1"&gt;dC2dIILB9HV[...]xT1bY=&lt;/param&gt;
	 *               &lt;param n="NEED_PRE"&gt;true&lt;/param&gt;
	 *              &lt;/firma&gt;
	 *             &lt;/firmas&gt;
	 *            &lt;/xml&gt;
	 *           </pre>
	 * @param algorithm Algoritmo de firma.
	 * @param batchId Identificador del lote de firma.
	 * @throws AOException Si hay problemas en la propia firma electr&oacute;nica.
	 * @throws IOException Si hay problemas en la obtenci&oacute;n, tratamiento o gradado de datos.
	 * @throws NoSuchAlgorithmException Si no se soporta alg&uacute;n algoritmo necesario. */
	static void doPostProcess(final SingleSign sSign,
			                  final X509Certificate[] certChain,
			                  final TriphaseData tdata,
			                  final SingleSignConstants.DigestAlgorithm algorithm,
			                  final String batchId) throws IOException,
			                                                                            AOException,
			                                                                            NoSuchAlgorithmException {
		if (certChain == null || certChain.length < 1) {
			throw new IllegalArgumentException(
				"La cadena de certificados del firmante no puede ser nula ni vacia" //$NON-NLS-1$
			);
		}

		final TriphaseData td = cleanTriphaseData(tdata, sSign.getId());

		// Instanciamos el preprocesador adecuado
		final TriPhasePreProcessor prep = TriPhaseHelper.getTriPhasePreProcessor(sSign);

		byte[] docBytes = sSign.getData(true);

		Properties extraParams;
		try {
			extraParams = ExtraParamsProcessor.expandProperties(sSign.getExtraParams(), docBytes, sSign.getSignFormat().name());
		}
		catch (final IncompatiblePolicyException e) {
			LOGGER.log(
					Level.WARNING, "No se ha podido expandir la politica de firma. Se realizara una firma basica", e); //$NON-NLS-1$
			extraParams = sSign.getExtraParams();
		}


		//TODO: Deshacer cuando se permita la generacion de firmas baseline
		extraParams.remove("profile");

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

		//TODO: Dado que las cofirmas y las contrafirmas no se han realizado sobre los datos aqui
		// disponibles sino de los extraidos de la firma, indicamos que en las firmas se valide el
		// PKCS#1. En las cofirmas y contrafirmas solo se comprobara la integridad. Habria que buscar
		// un modo de mejorar esto
		final boolean needVerifyPkcs1 = sSign.getSubOperation() == SignSubOperation.SIGN;
		try {
			TriPhaseHelper.checkSignaturesIntegrity(td, docBytes, certChain[0], algorithm, needVerifyPkcs1);
		}
		catch (final Exception e) {
			throw new AOException("Error en la verificacion de los PKCS#1 de las firmas recibidas", e); //$NON-NLS-1$
		}

		// Identificamos el algoritmo de firma apropiado la clave del certificado seleccionado
        final String signAlgorithm = AOSignConstants.composeSignatureAlgorithmName(algorithm.getName(), certChain[0].getPublicKey().getAlgorithm());

		final byte[] signedDoc;
		switch(sSign.getSubOperation()) {
			case SIGN:
				signedDoc = prep.preProcessPostSign(
					docBytes,
					signAlgorithm,
					certChain,
					extraParams,
					td.toString().getBytes()
				);
				break;
			case COSIGN:
				signedDoc = prep.preProcessPostCoSign(
					docBytes,
					signAlgorithm,
					certChain,
					extraParams,
					td.toString().getBytes()
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
					signAlgorithm,
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

		// Guardamos el resultado en almacenamiento temporal
		TempStoreFactory.getTempStore().store(signedDoc, sSign, batchId);
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

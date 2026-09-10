/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.server;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.security.cert.X509Certificate;
import java.util.Map;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.triphase.server.batch.BatchServerUtil;
import es.gob.afirma.triphase.server.batch.json.JSONSignBatch;
import es.gob.afirma.triphase.server.batch.json.JSONSignBatchConcurrent;
import es.gob.afirma.triphase.server.batch.json.JSONSignBatchSerial;

/**
 * Realiza la tercera (y &uacute;ltima) fase de un proceso de firma por lote.
 * Servlet implementation class BatchPostsigner
 */
public final class JSONBatchPostsignerHandler {

	private static final String BATCH_JSON_PARAM = "json"; //$NON-NLS-1$
	private static final String BATCH_CRT_PARAM = "certs"; //$NON-NLS-1$
	private static final String BATCH_TRI_PARAM = "tridata"; //$NON-NLS-1$

	private static final Charset DEFAULT_CHARSET = StandardCharsets.UTF_8;

	/**
	 * Realiza la tercera y &uacute;ltima fase de un proceso de firma por lote.
	 * Debe recibir la definici&oacute;n JSON (<a href="../doc-files/batch-scheme.html">descripci&oacute;n
	 * del formato</a>) del lote (exactamente la misma enviada para la primera fase)
	 * en un JSON pero convertido a Base64 (puede ser en formato <i>URL Safe</i>) y la cadena de
	 * certificados del firmante (exactamente la misma que la enviada en la primera fase),
	 * convertidos a Base64 (puede ser <i>URL Safe</i>) y separados por punto y coma (<code>;</code>).<br>
	 * Devuelve un JSON de resumen de resultado (<a href="../doc-files/resultlog-scheme.html">descripci&oacute;n
	 * del formato</a>)
	 * @see HttpServlet#service(HttpServletRequest request, HttpServletResponse response)
	 * */
	public static byte[] processRequest(final Map<String, String> parameters) throws ServletResponseException {

		final String json = parameters.get(BATCH_JSON_PARAM);
		if (json == null) {
			throw new BadRequestException(
				"No se ha recibido una definicion de lote en el parametro " + BATCH_JSON_PARAM //$NON-NLS-1$
			);
		}

		final JSONSignBatch batch;
		try {
			final byte[] jsonBatch = Base64.decode(json, true);
			batch = ConfigManager.isConcurrentModeEnable() ?
					new JSONSignBatchConcurrent(jsonBatch) :
						new JSONSignBatchSerial(jsonBatch);
		}
		catch(final Exception e) {
			throw new BadRequestException(
				"La definicion de lote es invalida", e //$NON-NLS-1$
			);
		}

		final String certListUrlSafeBase64 = parameters.get(BATCH_CRT_PARAM);
		if (certListUrlSafeBase64 == null) {
			throw new BadRequestException(
				"No se ha recibido la cadena de certificados del firmante en el parametro " + BATCH_CRT_PARAM //$NON-NLS-1$
			);
		}

		final X509Certificate[] certs;
		try {
			certs = BatchServerUtil.getCertificates(certListUrlSafeBase64);
		}
		catch (final Exception e) {
			throw new BadRequestException(
				"La cadena de certificados del firmante es invalida", e //$NON-NLS-1$
			);
		}

		final String triphaseDataAsUrlSafeBase64 = parameters.get(BATCH_TRI_PARAM);
		if (triphaseDataAsUrlSafeBase64 == null) {
			throw new BadRequestException(
				"No se ha recibido el resultado de las firmas cliente en el parametro " + BATCH_TRI_PARAM //$NON-NLS-1$
			);
		}

		final TriphaseData td;
		try {
			td = BatchServerUtil.getTriphaseDataFromJSON(triphaseDataAsUrlSafeBase64.getBytes(DEFAULT_CHARSET));
		}
		catch(final Exception e) {
			throw new BadRequestException(
				"El JSON de firmas cliente es invalido", e //$NON-NLS-1$
			);
		}

		final String ret;
		try {
			ret = batch.doPostBatch(certs, td);
		}
		catch (final Exception e) {
			throw new ServletResponseException(
				"Error en el postproceso del lote", e //$NON-NLS-1$
			);
		}
		
		return ret.getBytes(StandardCharsets.UTF_8);
	}

}

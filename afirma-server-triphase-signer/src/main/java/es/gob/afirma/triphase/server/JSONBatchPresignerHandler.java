/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.server;

import java.nio.charset.StandardCharsets;
import java.security.cert.X509Certificate;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.json.JSONObject;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.signers.xml.XmlDSigProviderHelper;
import es.gob.afirma.triphase.server.batch.BatchServerUtil;
import es.gob.afirma.triphase.server.batch.json.JSONSignBatch;
import es.gob.afirma.triphase.server.batch.json.JSONSignBatchConcurrent;
import es.gob.afirma.triphase.server.batch.json.JSONSignBatchSerial;

/** Realiza la primera fase de un proceso de firma por lote. */
public final class JSONBatchPresignerHandler {

	private static final Logger LOGGER = Logger.getLogger(ConfigManager.LOGGER_NAME);

	private static final String BATCH_JSON_PARAM = "json"; //$NON-NLS-1$
	private static final String BATCH_CRT_PARAM = "certs"; //$NON-NLS-1$

	
	
	static {
		// Indicamos si se debe instalar el proveedor de firma XML de Apache
		XmlDSigProviderHelper.configureXmlDSigProvider();
	}

	/**
	 * Realiza la primera fase de un proceso de firma por lote.
	 * Debe recibir la definici&oacute;n del lote en un JSON (<a href="../doc-files/batch-scheme.html">descripci&oacute;n
	 * del formato</a>) convertido completamente
	 * en Base64 y la cadena de certificados del firmante, convertidos a Base64 (puede ser
	 * <i>URL Safe</i>) y separados por punto y coma (<code>;</code>).
	 * Devuelve los bytes de un JSON de sesi&oacute;n trif&aacute;sica codificado en UTF-8.
	 * @param parameters Par&aacute;metros de la petici&oacute;n.
	 * @return Bytes del JSON de la respuesta. 
	 */
	public static byte[] processRequest(final Map<String, String> parameters) throws ServletResponseException {

		final String json = parameters.get(BATCH_JSON_PARAM);
		if (json == null) {
			LOGGER.severe("No se ha recibido una definicion de lote en el parametro " + BATCH_JSON_PARAM); //$NON-NLS-1$
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
		catch(final SecurityException e) {
			LOGGER.severe("Se sobrepaso alguno de los limites de seguridad establecidos en servidor para los lotes: " + e); //$NON-NLS-1$
			throw new BadRequestException(
				"El lote de firma no cumple con los requisitos establecidos en servidor", e //$NON-NLS-1$
			);
		}
		catch(final Exception e) {
			LOGGER.severe("La definicion de lote es invalida: " + e); //$NON-NLS-1$
			throw new BadRequestException(
				"La definicion de lote es invalida", e //$NON-NLS-1$
			);
		}

		final String certListUrlSafeBase64 = parameters.get(BATCH_CRT_PARAM);
		if (certListUrlSafeBase64 == null) {
			LOGGER.severe("No se ha recibido la cadena de certificados del firmante en el parametro " + BATCH_CRT_PARAM); //$NON-NLS-1$
			throw new BadRequestException(
				"No se ha recibido la cadena de certificados del firmante en el parametro " + BATCH_CRT_PARAM //$NON-NLS-1$
			);
		}

		final X509Certificate[] certs;
		try {
			certs = BatchServerUtil.getCertificates(certListUrlSafeBase64);
		}
		catch (final Exception e) {
			LOGGER.severe("La cadena de certificados del firmante es invalida: " + e); //$NON-NLS-1$
			throw new BadRequestException(
				"La cadena de certificados del firmante es invalida",  e //$NON-NLS-1$
			);
		}

		final JSONObject jsonPreBatch;
		try {
			jsonPreBatch = batch.doPreBatch(certs);
		}
		catch(final Exception e) {
			LOGGER.log(Level.SEVERE, "Error en el preproceso del lote", e); //$NON-NLS-1$
			throw new ServletResponseException(
				"Error en el preproceso del lote", e //$NON-NLS-1$
			);
		}

		return jsonPreBatch.toString().getBytes(StandardCharsets.UTF_8);
	}
}

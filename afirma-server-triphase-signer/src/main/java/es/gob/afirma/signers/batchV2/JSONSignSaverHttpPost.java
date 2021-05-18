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
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.http.UrlHttpMethod;
import es.gob.afirma.triphase.server.document.DocumentManagerBase;

/** Guarda firmas envi&aacute;ndolas a un servicio HTTP POST. */
public final class JSONSignSaverHttpPost extends DocumentManagerBase {

	private static final String PROP_POST_URL = "PostUrl"; //$NON-NLS-1$
	private static final String PROP_POST_PARAM_NAME = "PostParamName"; //$NON-NLS-1$

	private String url;
	private String param;

	/** Construye un objeto de guardado de firmas mediante un servicio HTTP POST.
	 * El servicio recibir&aacute; los datos en Base64 dentro del par&aacute;metro indicado.
	 * @param postUrl URL del servicio HTTP POST.
	 * @param postParam Nombre del par&aacute;metro POST en el que se enviar&aacute;n los datos. */
	public JSONSignSaverHttpPost(final String postUrl, final String postParam) {
		if (postUrl == null) {
			throw new IllegalArgumentException(
				"La URL para hacer el POST no puede ser nula" //$NON-NLS-1$
			);
		}
		if (postParam == null) {
			throw new IllegalArgumentException(
				"El nombre del parametro para hacer el POST no puede ser nulo" //$NON-NLS-1$
			);
		}
		this.url = postUrl;
		this.param = postParam;
	}

	/** Constructor vac&iacute;o. */
	public JSONSignSaverHttpPost() {
		// Vacio
	}

	@Override
	public void init(final Properties config) {
		if (config == null) {
			throw new IllegalArgumentException(
				"La configuracion no puede ser nula" //$NON-NLS-1$
			);
		}
		final String postUrl = config.getProperty(PROP_POST_URL);
		if (postUrl == null) {
			throw new IllegalArgumentException(
				"Es obligarorio que la configuracion incluya un valor para la propiedad " + PROP_POST_URL //$NON-NLS-1$
			);
		}
		this.url = postUrl;

		final String postParam = config.getProperty(PROP_POST_PARAM_NAME);
		if (postParam == null) {
			throw new IllegalArgumentException(
				"Es obligarorio que la configuracion incluya un valor para la propiedad " + PROP_POST_PARAM_NAME //$NON-NLS-1$
			);
		}
		this.param = postParam;
	}

	@Override
	public byte[] getDocument(final String id, final X509Certificate[] certChain, final Properties config) throws IOException {

		byte [] res = null;

		if (!this.url.contains("?")) { //$NON-NLS-1$
			this.url += "?"; //$NON-NLS-1$
		}

		final String targetUrl = this.url + "&" + id;//$NON-NLS-1$

		try {
			res = UrlHttpManagerFactory.getInstalledManager().readUrl(targetUrl, UrlHttpMethod.POST);
		} catch (final IOException e) {
			throw new IOException(
					"Error al intentar recuperar documento mediante URL"  //$NON-NLS-1$
				);
		}

		return res;
	}

	@Override
	public String storeDocument(final String id, final X509Certificate[] certChain, final byte[] data, final Properties config) throws IOException {
		if (!this.url.contains("?")) { //$NON-NLS-1$
			this.url += "?"; //$NON-NLS-1$
		}

		final String targetUrl = this.url + "&" + this.param + "=" + Base64.encode(data, true); //$NON-NLS-1$ //$NON-NLS-2$

		try {
			UrlHttpManagerFactory.getInstalledManager().readUrl(targetUrl, UrlHttpMethod.POST);
		} catch (final IOException e) {
			throw new IOException(
					"Error al intentar almacenar documento mediante URL"  //$NON-NLS-1$
				);
		}

		return targetUrl;
	}

}

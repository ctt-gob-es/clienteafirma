/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.misc.protocol;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/** Clase de utilidad para el an&aacute;lisis sint&aacute;ctico de URL.
 * @author Alberto Mart&iacute;nez */
public final class ProtocolInvocationUriParser {

	private ProtocolInvocationUriParser() {
		// Constructor privado. No se permite instancias
	}

	/** Comprueba que est&eacute;n disponibles en una URI todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de firma.
	 * @param uri URL de llamada.
	 * @return Par&aacute;metros.
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSign getParametersToSign(final String uri) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToSign(parserUri(uri));
	}

	/** Comprueba que est&eacute;n disponibles en un XML todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de firma.
	 * @param xml XML de entrada
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSign getParametersToSign(final byte[] xml) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToSign(ProtocolInvocationUriParserUtil.parseXml(xml));
	}

	/** Comprueba que est&eacute;n disponibles en una URI todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de firma/multifirma y guardado de datos.
	 * @param uri URL de llamada.
	 * @return Par&aacute;metros.
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSignAndSave getParametersToSignAndSave(final String uri) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToSignAndSave(parserUri(uri));
	}

	/** Comprueba que est&eacute;n disponibles en un XML todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de firma/multifirma y guardado de datos.
	 * @param xml XML de entrada
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSignAndSave getParametersToSignAndSave(final byte[] xml) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToSignAndSave(ProtocolInvocationUriParserUtil.parseXml(xml));
	}

	/** Comprueba que est&eacute;n disponibles en una URI todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de selecci&oacute;n de certificados.
	 * @param uri URL de llamada.
	 * @return Par&aacute;metros.
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSelectCert getParametersToSelectCert(final String uri) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToSelectCert(parserUri(uri));
	}

	/** Comprueba que est&eacute;n disponibles en un XML todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de selecci&oacute;n de certificados.
	 * @param xml XML de entrada
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSelectCert getParametersToSelectCert(final byte[] xml) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToSelectCert(ProtocolInvocationUriParserUtil.parseXml(xml));
	}

	/** Recupera todos los par&aacute;metros necesarios para la configuraci&oacute;n de una
	 * operaci&oacute;n de guardado de datos en el dispositivo.Si falta alg&uacute;n par&aacute;metro o
	 * es err&oacute;neo se lanzar&aacute; una excepci&oacute;n.
	 * @param xml XML con los par&aacute;metros
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSave getParametersToSave(final byte[] xml) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToSave(ProtocolInvocationUriParserUtil.parseXml(xml));
	}

	/** Recupera de una URI todos los par&aacute;metros necesarios para la configuraci&oacute;n de una
	 * operaci&oacute;n de guardado de datos en el dispositivo.Si falta alg&uacute;n par&aacute;metro o
	 * es err&oacute;neo se lanzar&aacute; una excepci&oacute;n.
	 * @param uri Url de llamada
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSave getParametersToSave(final String uri) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToSave(parserUri(uri));
	}


	/** Comprueba que est&eacute;n disponibles en una URI todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de firma por lotes predefinidos.
	 * @param uri URL de llamada.
	 * @return Par&aacute;metros.
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersForBatch getParametersForBatch(final String uri) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersForBatch(parserUri(uri));
	}

	/** Comprueba que est&eacute;n disponibles en un XML todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de firma por lotes definidos en XML.
	 * @param xml XML de entrada
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersForBatch getParametersForBatch(final byte[] xml) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersForBatch(ProtocolInvocationUriParserUtil.parseXml(xml));
	}

	/** Recupera de una URI todos los par&aacute;metros necesarios para la configuraci&oacute;n de una
	 * operaci&oacute;n de carga de datos.Si falta alg&uacute;n par&aacute;metro o
	 * es err&oacute;neo se lanzar&aacute; una excepci&oacute;n.
	 * @param uri Url de llamada
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToLoad getParametersToLoad(final String uri) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToLoad(parserUri(uri));
	}

	/** Recupera de una URI todos los par&aacute;metros necesarios para la configuraci&oacute;n de una
	 * operaci&oacute;n de carga de datos.Si falta alg&uacute;n par&aacute;metro o
	 * es err&oacute;neo se lanzar&aacute; una excepci&oacute;n.
	  @param xml XML de entrada
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToLoad getParametersToLoad(final byte[] xml) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToLoad(ProtocolInvocationUriParserUtil.parseXml(xml));
	}

	/** Recupera de una URI todos los par&aacute;metros necesarios para la configuraci&oacute;n de una
	 * operaci&oacute;n de carga de datos.Si falta alg&uacute;n par&aacute;metro o
	 * es err&oacute;neo se lanzar&aacute; una excepci&oacute;n.
	 * @param uri Url de llamada
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToGetCurrentLog getParametersToGetCurrentLog(final String uri) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToGetCurrentLog(parserUri(uri));
	}

	/** Recupera de una URI todos los par&aacute;metros necesarios para la configuraci&oacute;n de una
	 * operaci&oacute;n de carga de datos.Si falta alg&uacute;n par&aacute;metro o
	 * es err&oacute;neo se lanzar&aacute; una excepci&oacute;n.
	  @param xml XML de entrada
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToGetCurrentLog getParametersToGetCurrentLog(final byte[] xml) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToGetCurrentLog(ProtocolInvocationUriParserUtil.parseXml(xml));
	}

	/** Analiza la URL de entrada para obtener la lista de par&aacute;metros asociados.
	 * @param uri URL de llamada.
	 * @return Devuelve una tabla <i>hash</i> con cada par&aacute;metro asociado a un valor. */
	private static Map<String, String> parserUri(final String uri) {
		final Map<String, String> params = new HashMap<>();
		final String[] parameters = uri.substring(uri.indexOf('?') + 1).split("&"); //$NON-NLS-1$
		for (final String param : parameters) {
			if (param.indexOf('=') > 0) {
				try {
					params.put(
						param.substring(0, param.indexOf('=')),
						param.indexOf('=') == param.length() - 1 ?
							"" : //$NON-NLS-1$
								URLDecoder.decode(param.substring(param.indexOf('=') + 1), ProtocolInvocationUriParserUtil.DEFAULT_URL_ENCODING)
					);
				}
				catch (final UnsupportedEncodingException e) {
					params.put(
						param.substring(0, param.indexOf('=')),
						param.indexOf('=') == param.length() - 1 ? "" : param.substring(param.indexOf('=') + 1) //$NON-NLS-1$
					);
				}
			}
		}

		// Agregamos como codigo de operacion el nombre de host de la URL

		Logger.getLogger("es.gob.afirma").info("URI recibida: " + uri); //$NON-NLS-1$ //$NON-NLS-2$

		String path = uri.substring(uri.indexOf("://") + "://".length(), uri.indexOf('?') != -1 ? uri.indexOf('?') : uri.length()); //$NON-NLS-1$ //$NON-NLS-2$
		if (path.endsWith("/")) { //$NON-NLS-1$
			path = path.substring(0, path.length() - 1);
		}
		params.put(ProtocolConstants.OPERATION_PARAM, path.substring(path.lastIndexOf('/') + 1));

		return params;
	}
}
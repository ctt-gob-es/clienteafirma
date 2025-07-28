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
		return getParametersToSign(uri, false);
	}

	/** Comprueba que est&eacute;n disponibles en una URI todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de firma.
	 * @param uri URL de llamada.
	 * @param servicesRequired Indica si entre los par&aacute;metros es obligatorio que
	 * est&eacute;n los servicios de comunicaci%oacute;n.
	 * @return Par&aacute;metros.
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSign getParametersToSign(final String uri,
			final boolean servicesRequired) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToSign(
				parserUri(uri), servicesRequired);
	}

	/** Comprueba que est&eacute;n disponibles en un XML todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de firma.
	 * @param xml XML de entrada
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSign getParametersToSign(final byte[] xml) throws ParameterException {
		return getParametersToSign(xml, false);
	}

	/** Comprueba que est&eacute;n disponibles en un XML todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de firma.
	 * @param xml XML de entrada
	 * @param servicesRequired Indica si entre los par&aacute;metros es obligatorio que
	 * est&eacute;n los servicios de comunicaci%oacute;n.
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSign getParametersToSign(final byte[] xml,
			final boolean servicesRequired) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToSign(
				ProtocolInvocationUriParserUtil.parseXml(xml), servicesRequired);
	}

	/** Comprueba que est&eacute;n disponibles en una URI todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de firma/multifirma y guardado de datos.
	 * @param uri URL de llamada.
	 * @return Par&aacute;metros.
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSignAndSave getParametersToSignAndSave(final String uri) throws ParameterException {
		return getParametersToSignAndSave(uri, false);
	}

	/** Comprueba que est&eacute;n disponibles en una URI todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de firma/multifirma y guardado de datos.
	 * @param uri URL de llamada.
	 * @param servicesRequired Indica si entre los par&aacute;metros es obligatorio que
	 * est&eacute;n los servicios de comunicaci%oacute;n.
	 * @return Par&aacute;metros.
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSignAndSave getParametersToSignAndSave(final String uri,
			final boolean servicesRequired) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToSignAndSave(
				parserUri(uri), servicesRequired);
	}

	/** Comprueba que est&eacute;n disponibles en un XML todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de firma/multifirma y guardado de datos.
	 * @param xml XML de entrada
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSignAndSave getParametersToSignAndSave(final byte[] xml) throws ParameterException {
		return getParametersToSignAndSave(xml, false);
	}

	/** Comprueba que est&eacute;n disponibles en un XML todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de firma/multifirma y guardado de datos.
	 * @param xml XML de entrada
	 * @param servicesRequired Indica si entre los par&aacute;metros es obligatorio que
	 * est&eacute;n los servicios de comunicaci%oacute;n.
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSignAndSave getParametersToSignAndSave(final byte[] xml,
			final boolean servicesRequired) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToSignAndSave(
				ProtocolInvocationUriParserUtil.parseXml(xml), servicesRequired);
	}

	/** Comprueba que est&eacute;n disponibles en una URI todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de selecci&oacute;n de certificados.
	 * @param uri URL de llamada.
	 * @return Par&aacute;metros.
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSelectCert getParametersToSelectCert(final String uri) throws ParameterException {
		return getParametersToSelectCert(uri, false);
	}

	/** Comprueba que est&eacute;n disponibles en una URI todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de selecci&oacute;n de certificados.
	 * @param uri URL de llamada.
	 * @param servicesRequired Indica si entre los par&aacute;metros es obligatorio que
	 * est&eacute;n los servicios de comunicaci%oacute;n.
	 * @return Par&aacute;metros.
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSelectCert getParametersToSelectCert(final String uri,
			final boolean servicesRequired) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToSelectCert(
				parserUri(uri), servicesRequired);
	}

	/** Comprueba que est&eacute;n disponibles en un XML todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de selecci&oacute;n de certificados.
	 * @param xml XML de entrada
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSelectCert getParametersToSelectCert(final byte[] xml) throws ParameterException {
		return getParametersToSelectCert(xml, false);
	}

	/** Comprueba que est&eacute;n disponibles en un XML todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de selecci&oacute;n de certificados.
	 * @param xml XML de entrada
	 * @param servicesRequired Indica si entre los par&aacute;metros es obligatorio que
	 * est&eacute;n los servicios de comunicaci%oacute;n.
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSelectCert getParametersToSelectCert(final byte[] xml,
			final boolean servicesRequired) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToSelectCert(
				ProtocolInvocationUriParserUtil.parseXml(xml), servicesRequired);
	}

	/** Recupera todos los par&aacute;metros necesarios para la configuraci&oacute;n de una
	 * operaci&oacute;n de guardado de datos en el dispositivo.Si falta alg&uacute;n par&aacute;metro o
	 * es err&oacute;neo se lanzar&aacute; una excepci&oacute;n.
	 * @param xml XML con los par&aacute;metros
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSave getParametersToSave(final byte[] xml) throws ParameterException {
		return getParametersToSave(xml, false);
	}

	/** Recupera todos los par&aacute;metros necesarios para la configuraci&oacute;n de una
	 * operaci&oacute;n de guardado de datos en el dispositivo.Si falta alg&uacute;n par&aacute;metro o
	 * es err&oacute;neo se lanzar&aacute; una excepci&oacute;n.
	 * @param xml XML con los par&aacute;metros
	 * @param servicesRequired Indica si entre los par&aacute;metros es obligatorio que
	 * est&eacute;n los servicios de comunicaci%oacute;n.
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSave getParametersToSave(final byte[] xml,
			final boolean servicesRequired) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToSave(
				ProtocolInvocationUriParserUtil.parseXml(xml), servicesRequired);
	}

	/** Recupera de una URI todos los par&aacute;metros necesarios para la configuraci&oacute;n de una
	 * operaci&oacute;n de guardado de datos en el dispositivo.Si falta alg&uacute;n par&aacute;metro o
	 * es err&oacute;neo se lanzar&aacute; una excepci&oacute;n.
	 * @param uri Url de llamada
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSave getParametersToSave(final String uri) throws ParameterException {
		return getParametersToSave(uri, false);
	}

	/** Recupera de una URI todos los par&aacute;metros necesarios para la configuraci&oacute;n de una
	 * operaci&oacute;n de guardado de datos en el dispositivo.Si falta alg&uacute;n par&aacute;metro o
	 * es err&oacute;neo se lanzar&aacute; una excepci&oacute;n.
	 * @param uri Url de llamada
	 * @param servicesRequired Indica si entre los par&aacute;metros es obligatorio que
	 * est&eacute;n los servicios de comunicaci%oacute;n.
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToSave getParametersToSave(final String uri,
			final boolean servicesRequired) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToSave(parserUri(uri), servicesRequired);
	}

	/** Recupera todos los par&aacute;metros necesarios para la configuraci&oacute;n de una
	 * operaci&oacute;n de guardado de datos en el dispositivo.Si falta alg&uacute;n par&aacute;metro o
	 * es err&oacute;neo se lanzar&aacute; una excepci&oacute;n.
	 * @param xml XML con los par&aacute;metros
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersForBatch getParametersToBatch(final byte[] xml) throws ParameterException {
		return getParametersToBatch(xml, false);
	}

	/** Recupera todos los par&aacute;metros necesarios para la configuraci&oacute;n de una
	 * operaci&oacute;n de guardado de datos en el dispositivo.Si falta alg&uacute;n par&aacute;metro o
	 * es err&oacute;neo se lanzar&aacute; una excepci&oacute;n.
	 * @param xml XML con los par&aacute;metros
	 * @param servicesRequired Indica si entre los par&aacute;metros es obligatorio que
	 * est&eacute;n los servicios de comunicaci%oacute;n.
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersForBatch getParametersToBatch(final byte[] xml,
			final boolean servicesRequired) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToBatch(
				ProtocolInvocationUriParserUtil.parseXml(xml), servicesRequired);
	}

	/** Comprueba que est&eacute;n disponibles en una URI todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de firma por lotes predefinidos.
	 * @param uri URL de llamada.
	 * @return Par&aacute;metros.
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersForBatch getParametersToBatch(final String uri) throws ParameterException {
		return getParametersToBatch(uri, false);
	}

	/** Comprueba que est&eacute;n disponibles en una URI todos los parametros disponibles en la
	 * entrada de datos para la operaci&oacute;n de firma por lotes predefinidos.
	 * @param uri URL de llamada.
	 * @param servicesRequired Indica si entre los par&aacute;metros es obligatorio que
	 * est&eacute;n los servicios de comunicaci%oacute;n.
	 * @return Par&aacute;metros.
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersForBatch getParametersToBatch(final String uri,
			final boolean servicesRequired) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToBatch(parserUri(uri), servicesRequired);
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
	 * @param xml XML de entrada
	 * @return Par&aacute;metros
	 * @throws ParameterException Si alg&uacute;n par&aacute;metro proporcionado es incorrecto. */
	public static UrlParametersToLoad getParametersToLoad(final byte[] xml) throws ParameterException {
		return ProtocolInvocationUriParserUtil.getParametersToLoad(ProtocolInvocationUriParserUtil.parseXml(xml));
	}

	/** Analiza la URL de entrada para obtener la lista de par&aacute;metros asociados.
	 * @param uri URL de llamada.
	 * @return Devuelve una tabla <i>hash</i> con cada par&aacute;metro asociado a un valor. */
	private static Map<String, String> parserUri(final String uri) {
		final Map<String, String> params = new HashMap<>();
		final String[] parameters = uri.substring(uri.indexOf('?') + 1).split("&"); //$NON-NLS-1$
		for (final String param : parameters) {
			final int eqPos = param.indexOf('=');
			if (eqPos > 0) {
				try {
					params.put(
						param.substring(0, eqPos),
						eqPos == param.length() - 1 ?
							"" : //$NON-NLS-1$
							URLDecoder.decode(param.substring(eqPos + 1), ProtocolInvocationUriParserUtil.DEFAULT_URL_ENCODING)
					);
				}
				catch (final UnsupportedEncodingException e) {
					params.put(
						param.substring(0, eqPos),
						eqPos == param.length() - 1 ? "" : param.substring(eqPos + 1) //$NON-NLS-1$
					);
				}
			}
		}


		Logger.getLogger("es.gob.afirma").info("URI recibida: " + (uri.length() <= 300 ? uri : uri.substring(0, 300) + "...")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

		String path = uri.substring(uri.indexOf("://") + "://".length(), uri.indexOf('?') != -1 ? uri.indexOf('?') : uri.length()); //$NON-NLS-1$ //$NON-NLS-2$
		if (path.endsWith("/")) { //$NON-NLS-1$
			path = path.substring(0, path.length() - 1);
		}
		// El codigo de operacion se habra recibido como la parte de host de la URL
		params.put(ProtocolConstants.OPERATION_PARAM, path.indexOf("/") == -1 ? path : path.substring(0, path.indexOf("/"))); //$NON-NLS-1$ //$NON-NLS-2$

		return params;
	}
}
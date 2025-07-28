/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.signers;

import java.io.IOException;
import java.net.URL;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.LoggerUtil;
import es.gob.afirma.core.misc.http.SSLErrorProcessor;
import es.gob.afirma.core.misc.http.UrlHttpManager;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.misc.http.UrlHttpMethod;
import es.gob.afirma.core.signers.TriphaseData.TriSign;
import es.gob.afirma.core.util.tree.AOTreeModel;

/** Manejador para la emulaci&oacute;n de la realizaci&oacute;n de firmas trif&aacute;sicas PKCS#1.
 * Las firmas PKCS#1 se realizan por completo en cliente, no tiene prefirma ni postfirma. Este
 * manejador permite emular la gestion de firmas trif&aacute;de PKCS#1 lo que nos permite que los
 * datos a firmar se obtengan desde el servidor trifasico en lugar del cliente. */
public class AOPkcs1TriPhaseSigner implements AOSigner {

	/** <i>Logger</i> para la clase y sus derivadas. */
	protected static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Nombre de la propiedad de URL del servidor de firma trif&aacute;sica. */
	private static final String PROPERTY_NAME_SIGN_SERVER_URL = "serverUrl"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n de prefirma en servidor. */
	private static final String OPERATION_PRESIGN = "pre"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n de postfirma en servidor. */
	private static final String OPERATION_POSTSIGN = "post"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n criptogr&aacute;fica de firma. */
	private static final String CRYPTO_OPERATION_SIGN = "sign"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro que identifica la operaci&oacute;n trif&aacute;sica
	 * en la URL del servidor de firma. */
	private static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro que identifica la operaci&oacute;n criptogr&aacute;fica
	 * en la URL del servidor de firma. */
	private static final String PARAMETER_NAME_CRYPTO_OPERATION = "cop"; //$NON-NLS-1$

	private static final String HTTP_CGI = "?"; //$NON-NLS-1$
	private static final String HTTP_EQUALS = "="; //$NON-NLS-1$
	private static final String HTTP_AND = "&"; //$NON-NLS-1$

	// Parametros que necesitamos para la URL de las llamadas al servidor de firma
	private static final String PARAMETER_NAME_DOCID = "doc"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_ALGORITHM = "algo"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_FORMAT = "format"; //$NON-NLS-1$
		private static final String PARAMETER_NAME_CERT = "cert"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_SESSION_DATA = "session"; //$NON-NLS-1$


	/** Nombre de la propiedad para almac&eacute;n de prefirmas. */
	private static final String PROPERTY_NAME_PRESIGN = "PRE"; //$NON-NLS-1$

	/** Nombre de la propiedad para almac&eacute;n de firmas PKCS#1. */
	private static final String PROPERTY_NAME_PKCS1_SIGN = "PK1"; //$NON-NLS-1$

	// Nombres de las propiedades intercambiadas con el servidor como Properties

	/** Indicador de finalizaci&oacute;n correcta de proceso. */
	private static final String SUCCESS = "OK"; //$NON-NLS-1$

	private final String signFormat;

	/** Construye un manejador de firmas trif&aacute;sicas para un determinado formato.
	 * @param format Formato de firma. */
	protected AOPkcs1TriPhaseSigner(final String format) {
		this.signFormat = format;
	}

	/** Construye un manejador de firmas trif&aacute;sicas para firmas PKCS#1. */
	public AOPkcs1TriPhaseSigner() {
		this(AOSignConstants.SIGN_FORMAT_PKCS1);
	}

	@Override
	public byte[] sign(final byte[] data,
			           final String algorithm,
			           final PrivateKey key,
			           final Certificate[] certChain,
			           final Properties xParams) throws AOException {
		return triPhaseOperation(
			this.signFormat,
			CRYPTO_OPERATION_SIGN,
			data,
			algorithm,
			key,
			certChain,
			xParams
		);
	}

	/** {@inheritDoc} */
	@Override
	public final byte[] getData(final byte[] sign) {
		throw new UnsupportedOperationException("No se soporta en firma trifasica"); //$NON-NLS-1$
	}
	
	/** {@inheritDoc} */
	@Override
	public byte[] getData(final byte[] sign, final Properties params) throws AOInvalidFormatException {
		throw new UnsupportedOperationException("No se soporta en firma trifasica"); //$NON-NLS-1$
	}

	@Override
	public byte[] cosign(final byte[] data,
			             final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties xParams) throws AOException {
		throw new UnsupportedOperationException("No se soporta la multifirma de firmas NONE"); //$NON-NLS-1$
	}

	@Override
	public byte[] cosign(final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties xParams) throws AOException {
		throw new UnsupportedOperationException("No se soporta la multifirma de firmas NONE"); //$NON-NLS-1$
	}

	@Override
	public byte[] countersign(final byte[] sign,
			final String algorithm,
			final CounterSignTarget targetType,
			final Object[] targets,
			final PrivateKey key,
			final Certificate[] certChain,
			final Properties xParams) throws AOException {

		throw new UnsupportedOperationException("No se soporta la multifirma de firmas NONE"); //$NON-NLS-1$
	}

	/** {@inheritDoc} */
	@Override
	public final AOTreeModel getSignersStructure(final byte[] sign,
			                                     final boolean asSimpleSignInfo) {
		throw new UnsupportedOperationException("No se soporta en firma trifasica"); //$NON-NLS-1$
	}
	
	/** {@inheritDoc} */
	@Override
	public AOTreeModel getSignersStructure(final byte[] sign, final Properties params, final boolean asSimpleSignInfo) {
		throw new UnsupportedOperationException("No se soporta en firma trifasica"); //$NON-NLS-1$
	}
	
	/** {@inheritDoc} */
	@Override
	public boolean isSign(final byte[] signData, final Properties params) {
		return isSign(signData);
	}

	/** {@inheritDoc} */
	@Override
	public boolean isSign(final byte[] sign) {
		return false;
	}

	/** {@inheritDoc} */
	@Override
	public boolean isValidDataFile(final byte[] data) {
		if (data == null) {
			LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
			return false;
		}
		return true;
	}

	/** {@inheritDoc} */
	@Override
	public String getSignedName(final String originalName, final String inText) {
		return originalName + (inText != null ? inText : "") + ".p1"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** {@inheritDoc} */
	@Override
	public AOSignInfo getSignInfo(final byte[] sign) {
		throw new UnsupportedOperationException("No se soporta en firma trifasica"); //$NON-NLS-1$
	}
	
	/** {@inheritDoc} */
	@Override
	public AOSignInfo getSignInfo(final byte[] data, final Properties params) throws AOException {
		throw new UnsupportedOperationException("No se soporta en firma trifasica"); //$NON-NLS-1$
	}

	/** Ejecuta una operaci&oacute;n de firma/multifirma en 3 fases, en donde la primera y
	 * la &uacute;ltima realmente no har&aacute;n m&aacute;s que recuperar el documento a
	 * firmar y almacenar la firma, respectivamente.
	 * @param format Formato de firma ("NONE" o "NONEtri" en este caso).
	 * @param cryptoOperation Tipo de operaci&oacute;n (solo se admite la firma).
	 * @param data Datos o firma sobre la que operar
	 * @param algorithm Algoritmo de firma
	 * @param key Clave privada del certificado de firma.
	 * @param certChain Cadena de certificaci&oacute;n.
	 * @param extraParams Par&aacute;metros para la configuraci&oacute;n de la operaci&oacute;n.
	 * S&oacute;lo se utiliza el {@code serverUrl}.
	 * @return Resultado de la operaci&oacute;n de firma.
	 * @throws AOException Cuando se produce un error durante la operaci&oacute;n. */
	protected static byte[] triPhaseOperation(final String format,
			                                final String cryptoOperation,
			                                final byte[] data,
			                                final String algorithm,
			                                final PrivateKey key,
			                                final Certificate[] certChain,
			                                final Properties extraParams) throws AOException {
		if (extraParams == null) {
			throw new IllegalArgumentException("Se necesitan parametros adicionales"); //$NON-NLS-1$
		}
		if (key == null) {
			throw new IllegalArgumentException("Es necesario proporcionar la clave privada de firma"); //$NON-NLS-1$
		}
		if (certChain == null || certChain.length == 0) {
			throw new IllegalArgumentException("Es necesario proporcionar el certificado de firma"); //$NON-NLS-1$
		}
		if (data == null) {
			throw new IllegalArgumentException("No se ha proporcionado el identificador de documento a firmar"); //$NON-NLS-1$
		}

		// Comprobamos la direccion del servidor
		final URL signServerUrl;
		try {
			signServerUrl = new URL(extraParams.getProperty(PROPERTY_NAME_SIGN_SERVER_URL));
		}
		catch (final Exception e) {
			throw new IllegalArgumentException(
				"No se ha proporcionado una URL valida para el servidor de firma: " + extraParams.getProperty(PROPERTY_NAME_SIGN_SERVER_URL), e //$NON-NLS-1$
			);
		}

		// Decodificamos el identificador del documento
		final String documentId = Base64.encode(data, true);

		final UrlHttpManager urlManager = UrlHttpManagerFactory.getInstalledManager();

		// Preparamos el parametro de cadena de certificados
		final String cerChainParamContent;
		try {
			cerChainParamContent = Base64.encode(certChain[0].getEncoded(), true);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error decodificando la cadena de certificados: " + e, e); //$NON-NLS-1$
		}

		final SSLErrorProcessor errorProcessor = new SSLErrorProcessor(extraParams);

		// ---------
		// PREFIRMA
		// ---------

		// Empezamos la prefirma
		byte[] preSignResult;
		try {
			// Llamamos a una URL pasando como parametros los datos necesarios para
			// configurar la operacion:
			//  - Operacion trifasica (prefirma o postfirma)
			//  - Operacion criptografica (firma, cofirma o contrafirma)
			//  - Formato de firma
			//  - Algoritmo de firma a utilizar
			//  - Certificado de firma
			//  - Parametros extra de configuracion
			//  - Datos o identificador del documento a firmar
			final StringBuffer urlBuffer = new StringBuffer();
			urlBuffer.append(signServerUrl).append(HTTP_CGI).
			append(PARAMETER_NAME_OPERATION).append(HTTP_EQUALS).append(OPERATION_PRESIGN).append(HTTP_AND).
			append(PARAMETER_NAME_CRYPTO_OPERATION).append(HTTP_EQUALS).append(cryptoOperation).append(HTTP_AND).
			append(PARAMETER_NAME_FORMAT).append(HTTP_EQUALS).append(format).append(HTTP_AND).
			append(PARAMETER_NAME_ALGORITHM).append(HTTP_EQUALS).append(algorithm).append(HTTP_AND).
			append(PARAMETER_NAME_CERT).append(HTTP_EQUALS).append(cerChainParamContent).append(HTTP_AND).
			append(PARAMETER_NAME_DOCID).append(HTTP_EQUALS).append(documentId);

			final String postUrl = urlBuffer.toString();

			if (LOGGER.isLoggable(Level.INFO)) {
				LOGGER.info("Se llamara por POST a la siguiente URL:\n" + LoggerUtil.getTrimStr(postUrl));  //$NON-NLS-1$
			}

			preSignResult = urlManager.readUrl(postUrl, UrlHttpMethod.POST, errorProcessor);

			urlBuffer.setLength(0);
		}
		catch (final IOException e) {
			if (errorProcessor.isCancelled()) {
				LOGGER.info("El usuario no permite la importacion del certificado SSL de confianza del servicio de firma trifasica: " //$NON-NLS-1$
						+ LoggerUtil.getTrimStr(signServerUrl.toString()));
			}
			throw new AOException("Error en la llamada de prefirma al servidor: " + e, e); //$NON-NLS-1$
		}

		// ----------
		// FIRMA
		// ----------

		// Convertimos la respuesta del servidor en un Objeto de sesion
		final TriphaseData triphaseData;
		try {
			triphaseData = TriphaseData.parser(
				Base64.decode(preSignResult, 0, preSignResult.length, true)
			);
		}
		catch (final Exception e) {
			LOGGER.severe("Error al analizar la prefirma enviada por el servidor: " + e); //$NON-NLS-1$
			throw new AOException("Error al analizar la prefirma enviada por el servidor: " + e, e); //$NON-NLS-1$
		}

		final TriSign tri = triphaseData.getSign(0);

		byte[] pkcs1;
		try {
			pkcs1 = new AOPkcs1Signer().sign(
				Base64.decode(tri.getProperty(PROPERTY_NAME_PRESIGN)),
				algorithm,
				key,
				certChain,
				extraParams
			);
		}
		catch (final IOException e) {
			LOGGER.severe("Error al decodificar la prefirma de los datos: " + e); //$NON-NLS-1$
			throw new AOException("Error al decodificar la prefirma de los datos", e); //$NON-NLS-1$
		}
		tri.addProperty(PROPERTY_NAME_PKCS1_SIGN, Base64.encode(pkcs1));

		final String preResultAsBase64 = Base64.encode(
			triphaseData.toString().getBytes(),
			true
		);

		// ---------
		// POSTFIRMA
		// ---------

		final byte[] triSignFinalResult;
		try {
			final StringBuffer urlBuffer = new StringBuffer();
			urlBuffer.append(signServerUrl).append(HTTP_CGI).
			append(PARAMETER_NAME_OPERATION).append(HTTP_EQUALS).append(OPERATION_POSTSIGN)    .append(HTTP_AND).
			append(PARAMETER_NAME_CRYPTO_OPERATION).append(HTTP_EQUALS).append(cryptoOperation).append(HTTP_AND).
			append(PARAMETER_NAME_FORMAT).append(HTTP_EQUALS).append(format)                   .append(HTTP_AND).
			append(PARAMETER_NAME_ALGORITHM).append(HTTP_EQUALS).append(algorithm)             .append(HTTP_AND).
			append(PARAMETER_NAME_CERT).append(HTTP_EQUALS).append(cerChainParamContent)       .append(HTTP_AND).
			append(PARAMETER_NAME_DOCID).append(HTTP_EQUALS).append(documentId).append(HTTP_AND).
			append(PARAMETER_NAME_SESSION_DATA).append(HTTP_EQUALS).append(preResultAsBase64);

			triSignFinalResult = urlManager.readUrl(urlBuffer.toString(), UrlHttpMethod.POST);
		}
		catch (final IOException e) {
			throw new AOException("Error en la llamada de postfirma al servidor: " + e, e); //$NON-NLS-1$
		}

		// Analizamos la respuesta del servidor
		final String stringTrimmedResult = new String(triSignFinalResult).trim();
		if (!stringTrimmedResult.startsWith(SUCCESS)) {
			throw new AOException("La firma trifasica no ha finalizado correctamente: " + stringTrimmedResult); //$NON-NLS-1$
		}

		// Los datos no se devuelven, se quedan en el servidor
		try {
			return Base64.decode(stringTrimmedResult.substring((SUCCESS + " NEWID=").length()), true); //$NON-NLS-1$
		}
		catch (final IOException e) {
			LOGGER.warning("El resultado de NEWID del servidor no estaba en Base64: " + e); //$NON-NLS-1$
			throw new AOException("El resultado devuelto por el servidor no es correcto: " + e, e); //$NON-NLS-1$
		}
	}

}

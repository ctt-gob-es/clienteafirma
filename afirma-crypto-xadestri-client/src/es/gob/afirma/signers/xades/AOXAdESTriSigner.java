/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.xades;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.CertificateEncodingException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.UrlHttpManagerImpl;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.util.tree.AOTreeModel;


/**
 * Manejador de firma XAdES trif&aacute;sicas. Mediante este manejador un usuario puede firmar un documento remoto
 * indicando el identificador del documento. Este manejador requiere de un servicio remoto que genere la estructura
 * de firma en servidor. La operación criptogr&aacute;fica de firma se realiza en el PC o dispositivo del usuario,
 * por lo que la clave privada de su certificado nunca sale de este.<br/>
 * El resultado de las operaciones criptogr&aacute;ficas no es el resultado generado sino el identificador con el que
 * el resultado se ha guardado en el servidor remoto (gestor documental, sistema de ficheros,...).
 * La l&oacute;gica de resoluci&oacute;n del identificador de entrada, recuperaci&oacute;n de los datos y el guardado
 * del resultado recae el un manejador conectado al servicio de firma.
 * Como alternativa, a indicar los datos mediante un identificador, un usuario puede introducir directamente los datos
 * (prevaleciendo estos sobre el identificador) de tal forma que estos viajan en cada una de las operaciones con el
 * servidor. El resultado ser&aacute; an&aacute;logo al anterior, recuperandose &uacute;nicamente el identificador
 * remoto asignado al resultado.
 */
public final class AOXAdESTriSigner implements AOSigner {

	static final Logger LOGGER = Logger.getLogger("es.agob.afirma"); //$NON-NLS-1$

	/** Nombre de la propiedad de URL del servidor de firma trif&aacute;sica. */
	private static final String PROPERTY_NAME_SIGN_SERVER_URL = "serverUrl"; //$NON-NLS-1$

	/** Identificador del documento a firmar, por el cual se obtiene desde el servidor documental. */
	private static final String PROPERTY_NAME_DOCUMENT_ID = "documentId"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n de prefirma en servidor. */
	private static final String OPERATION_PRESIGN = "pre"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n de postfirma en servidor. */
	private static final String OPERATION_POSTSIGN = "post"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n criptogr&aacute;fica de firma. */
	private static final String CRYPTO_OPERATION_SIGN = "sign"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n criptogr&aacute;fica de cofirma. */
	private static final String CRYPTO_OPERATION_COSIGN = "cosign"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro que identifica la operaci&oacute;n trif&aacute;sica en la URL del servidor de firma. */
	private static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro que identifica la operaci&oacute;n criptogr&aacute;fica en la URL del servidor de firma. */
	private static final String PARAMETER_NAME_CRYPTO_OPERATION = "cop"; //$NON-NLS-1$

	private static final String HTTP_CGI = "?"; //$NON-NLS-1$
	private static final String HTTP_EQUALS = "="; //$NON-NLS-1$
	private static final String HTTP_AND = "&"; //$NON-NLS-1$

	// Parametros que necesitamos para la URL de las llamadas al servidor de firma
	private static final String PARAMETER_NAME_DATA = "dat"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_DOCID = "doc"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_ALGORITHM = "algo"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_FORMAT = "format"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_CERT = "cert"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_EXTRA_PARAM = "params"; //$NON-NLS-1$

	private static final String XADES_FORMAT = "XAdES"; //$NON-NLS-1$

	// Nombres de las propiedades intercambiadas con el servidor como Properties

	/** Prefirma. */
	private static final String PROPERTY_NAME_PRESIGN = "PRE"; //$NON-NLS-1$

	/** Nombre de la propiedad de los sesi&oacute;n necesarios para completar la firma. */
	private static final String PROPERTY_NAME_SESSION_DATA = "SESSION"; //$NON-NLS-1$

	/** Firma PKCS#1. */
	private static final String PROPERTY_NAME_PKCS1_SIGN = "PK1"; //$NON-NLS-1$

	/** Indicador de finalizaci&oacute;n correcta de proceso. */
	private static final String SUCCESS = "OK"; //$NON-NLS-1$


	@Override
	public byte[] sign(final byte[] data,
			final String algorithm,
			final PrivateKeyEntry keyEntry,
			final Properties xParams) throws AOException {
		return triPhaseOperation(CRYPTO_OPERATION_SIGN, data, algorithm, keyEntry, xParams);
	}

	/** {@inheritDoc} */
	@Override
	public byte[] getData(final byte[] sign) throws AOInvalidFormatException {
		throw new UnsupportedOperationException("No se soporta en firma trifasica"); //$NON-NLS-1$
	}

	@Override
	public byte[] cosign(final byte[] data,
			final byte[] sign,
			final String algorithm,
			final PrivateKeyEntry keyEntry,
			final Properties xParams) throws AOException {

		return triPhaseOperation(CRYPTO_OPERATION_COSIGN, sign, algorithm, keyEntry, xParams);
	}

	@Override
	public byte[] cosign(final byte[] sign,
			final String algorithm,
			final PrivateKeyEntry keyEntry,
			final Properties xParams) throws AOException {
		return cosign(null, sign, algorithm, keyEntry, xParams);
	}

	@Override
	public byte[] countersign(final byte[] sign,
			final String algorithm,
			final CounterSignTarget targetType,
			final Object[] targets,
			final PrivateKeyEntry keyEntry,
			final Properties xParams) throws AOException {
		throw new UnsupportedOperationException("No se soporta en firma trifasica"); //$NON-NLS-1$
	}

	/** {@inheritDoc} */
	@Override
	public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) throws AOInvalidFormatException {
		throw new UnsupportedOperationException("No se soporta en firma trifasica"); //$NON-NLS-1$
	}

	/** {@inheritDoc} */
	@Override
	public boolean isSign(final byte[] sign) {
		throw new UnsupportedOperationException("No se soporta en firma trifasica"); //$NON-NLS-1$
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
		return originalName + (inText != null ? inText : "") + ".xsig"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** {@inheritDoc} */
	@Override
	public AOSignInfo getSignInfo(final byte[] sign) throws AOException {
		throw new UnsupportedOperationException("No se soporta en firma trifasica"); //$NON-NLS-1$
	}

	/**
	 * Ejecuta una operaci&oacute;n de firma/cofirma en 3 fases.
	 * @param cryptoOperation Tipo de operaci&oacute: "sign" (firma) o "cosign" (cofirma)
	 * @param data Datos o firma sobre la que operar
	 * @param algorithm Algoritmo de firma
	 * @param keyEntry Referencia a la clave del certificado de firma.
	 * @param xParams Par&aacute;metros para la configuraci&oacute;n de la operaci&oacute;n.
	 * @return
	 * @throws AOException
	 */
	private static byte[] triPhaseOperation(final String cryptoOperation,
			final byte[] data,
			final String algorithm,
			final PrivateKeyEntry keyEntry,
			final Properties xParams) throws AOException {

		if (!CRYPTO_OPERATION_SIGN.equals(cryptoOperation) && !CRYPTO_OPERATION_COSIGN.equals(cryptoOperation)) {
			throw new IllegalArgumentException("Operacion criptografica no soportada: " + cryptoOperation); //$NON-NLS-1$
		}

		if (xParams == null) {
			throw new IllegalArgumentException("Se necesitan parametros adicionales"); //$NON-NLS-1$
		}
		if (keyEntry == null) {
			throw new IllegalArgumentException("Es necesario proporcionar una entrada a la clave privada de firma"); //$NON-NLS-1$
		}

		if (data == null && !xParams.containsKey(PROPERTY_NAME_DOCUMENT_ID)) {
			throw new IllegalArgumentException("No se han proporcionado datos de entrada ni el identificador de documento a firmar"); //$NON-NLS-1$
		}

		// Creamos una copia de los parametros
		final Properties configParams = new Properties();
		for (final String key : xParams.keySet().toArray(new String[xParams.size()])) {
			configParams.setProperty(key, xParams.getProperty(key));
		}

		// Comprobamos la direccion del servidor
		final URL signServerUrl;
		try {
			signServerUrl = new URL(configParams.getProperty(PROPERTY_NAME_SIGN_SERVER_URL));
		}
		catch (final MalformedURLException e) {
			throw new IllegalArgumentException("No se ha proporcionado la URL del servidor de firma: " + configParams.getProperty(PROPERTY_NAME_SIGN_SERVER_URL), e); //$NON-NLS-1$
		}
		configParams.remove(PROPERTY_NAME_SIGN_SERVER_URL);

		String documentId = null;
		if (configParams.containsKey(PROPERTY_NAME_DOCUMENT_ID)) {
			if (data == null) {
				documentId = configParams.getProperty(PROPERTY_NAME_DOCUMENT_ID);
			}
			configParams.remove(PROPERTY_NAME_DOCUMENT_ID);
		}

		final String algoUri = SIGN_ALGOS_URI.get(algorithm);
		if (algoUri == null) {
			throw new UnsupportedOperationException(
					"Los formatos de firma XML no soportan el algoritmo de firma '" + algorithm + "'"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		// ---------
		// PREFIRMA
		// ---------

		// Empezamos la prefirma
		final byte[] preSignResult;
		try {
			// Llamamos a una URL pasando como parametros los datos necesarios para
			// configurar la operacion:
			//  - Operacion trifasica (prefirma o postfirma)
			//  - Formato de firma
			//  - Algoritmo de firma a utilizar
			//  - Certificado de firma
			//  - Parametros extra de configuracion
			//  - Datos o identificador del documento a firmar
			final StringBuffer urlBuffer = new StringBuffer();
			urlBuffer.append(signServerUrl).append(HTTP_CGI).
			append(PARAMETER_NAME_OPERATION).append(HTTP_EQUALS).append(OPERATION_PRESIGN).append(HTTP_AND).
			append(PARAMETER_NAME_CRYPTO_OPERATION).append(HTTP_EQUALS).append(cryptoOperation).append(HTTP_AND).
			append(PARAMETER_NAME_FORMAT).append(HTTP_EQUALS).append(XADES_FORMAT).append(HTTP_AND).
			append(PARAMETER_NAME_ALGORITHM).append(HTTP_EQUALS).append(algorithm).append(HTTP_AND).
			append(PARAMETER_NAME_CERT).append(HTTP_EQUALS).append(Base64.encodeBytes(keyEntry.getCertificate().getEncoded(), Base64.URL_SAFE));

			if (configParams.size() > 0) {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_EXTRA_PARAM).append(HTTP_EQUALS).
				append(properties2Base64(configParams));
			}

			if (data != null) {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_DATA).append(HTTP_EQUALS).
				append(Base64.encodeBytes(data, Base64.URL_SAFE));
			} else {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_DOCID).append(HTTP_EQUALS).
				append(documentId);
			}

			preSignResult = UrlHttpManagerImpl.readUrlByPost(urlBuffer.toString());
			urlBuffer.setLength(0);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error decodificando el certificado del firmante: " + e, e); //$NON-NLS-1$
		}
		catch (final IOException e) {
			throw new AOException("Error en la llamada de prefirma al servidor: " + e, e); //$NON-NLS-1$
		}

		// Convertimos la respuesta del servidor en un Properties
		final Properties preSign;
		try {
			preSign = base642Properties(new String(preSignResult));
		}
		catch (final IOException e) {
			throw new AOException("La respuesta del servidor no es valida: " + new String(preSignResult), e); //$NON-NLS-1$
		}

		// Sacamos los SignedAttributes de CAdES para firmarlos
		final String base64PreSign = preSign.getProperty(PROPERTY_NAME_PRESIGN);
		if (base64PreSign == null) {
			throw new AOException("El servidor no ha devuelto una prefirma : " + new String(preSignResult)); //$NON-NLS-1$
		}

		final byte[] coreData;
		try {
			coreData = Base64.decode(base64PreSign);
		}
		catch (final IOException e) {
			throw new AOException("Error decodificando el core de datos a firmar: " + e, e); //$NON-NLS-1$
		}

		// -------------------------------------------
		// FIRMA (Este bloque se hace en dispositivo)
		// -------------------------------------------

		final byte[] pkcs1sign = new AOPkcs1Signer().sign(
				coreData,
				algorithm,
				keyEntry,
				null // No hay parametros en PKCS#1
				);
		// Creamos la peticion de postfirma
		configParams.put(PROPERTY_NAME_PKCS1_SIGN, Base64.encode(pkcs1sign));
		if (preSign.containsKey(PROPERTY_NAME_SESSION_DATA)) {
			configParams.put(PROPERTY_NAME_SESSION_DATA, preSign.getProperty(PROPERTY_NAME_SESSION_DATA));
		}

		// ---------
		// POSTFIRMA
		// ---------

		final byte[] triSignFinalResult;
		try {
			final StringBuffer urlBuffer = new StringBuffer();
			urlBuffer.append(signServerUrl).append(HTTP_CGI).
			append(PARAMETER_NAME_OPERATION).append(HTTP_EQUALS).append(OPERATION_POSTSIGN).append(HTTP_AND).
			append(PARAMETER_NAME_CRYPTO_OPERATION).append(HTTP_EQUALS).append(cryptoOperation).append(HTTP_AND).
			append(PARAMETER_NAME_FORMAT).append(HTTP_EQUALS).append(XADES_FORMAT).append(HTTP_AND).
			append(PARAMETER_NAME_ALGORITHM).append(HTTP_EQUALS).append(algorithm).append(HTTP_AND).
			append(PARAMETER_NAME_CERT).append(HTTP_EQUALS).append(Base64.encodeBytes(keyEntry.getCertificate().getEncoded(), Base64.URL_SAFE)).
			append(HTTP_AND).append(PARAMETER_NAME_EXTRA_PARAM).append(HTTP_EQUALS).append(properties2Base64(configParams));

			if (data != null) {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_DATA).append(HTTP_EQUALS).
				append(Base64.encodeBytes(data, Base64.URL_SAFE));
			} else {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_DOCID).append(HTTP_EQUALS).
				append(documentId);
			}

			triSignFinalResult = UrlHttpManagerImpl.readUrlByPost(urlBuffer.toString());
			urlBuffer.setLength(0);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error decodificando el certificado del firmante: " + e, e); //$NON-NLS-1$
		}
		catch (final IOException e) {
			throw new AOException("Error en la llamada de postfirma al servidor: " + e, e); //$NON-NLS-1$
		}

		// Analizamos la respuesta del servidor
		final String stringTrimmedResult = new String(triSignFinalResult).trim();
		if (!stringTrimmedResult.startsWith(SUCCESS)) {
			throw new AOException("La firma trifasica no ha finalizado correctamente: " + new String(triSignFinalResult)); //$NON-NLS-1$
		}

		// Los datos no se devuelven, se quedan en el servidor
		try {
			return Base64.decode(stringTrimmedResult.replace(SUCCESS + " NEWID=", "")); //$NON-NLS-1$ //$NON-NLS-2$
		}
		catch (final IOException e) {
			LOGGER.warning("El resultado de NEWID del servidor no estaba en Base64: " + e); //$NON-NLS-1$
			return stringTrimmedResult.getBytes();
		}
	}

	private static String properties2Base64(final Properties p) throws IOException {
		final StringBuilder builder = new StringBuilder();
		for (final String key : p.keySet().toArray(new String[p.size()])) {
			builder.append(key).append("=").append(p.getProperty(key)).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return Base64.encodeBytes(builder.toString().getBytes(), Base64.URL_SAFE);
	}

	private static Properties base642Properties(final String base64) throws IOException {
		final Properties p = new Properties();
		p.load(new ByteArrayInputStream(Base64.decode(base64, Base64.URL_SAFE)));
		return p;
	}

	private static final String URL_SHA1_RSA    = "http://www.w3.org/2000/09/xmldsig#rsa-sha1"; //$NON-NLS-1$
	private static final String URL_MD5_RSA     = "http://www.w3.org/2001/04/xmldsig-more#rsa-md5"; //$NON-NLS-1$
	private static final String URL_SHA256_RSA  = "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256"; //$NON-NLS-1$
	private static final String URL_SHA384_RSA  = "http://www.w3.org/2001/04/xmldsig-more#rsa-sha384"; //$NON-NLS-1$
	private static final String URL_SHA512_RSA  = "http://www.w3.org/2001/04/xmldsig-more#rsa-sha512"; //$NON-NLS-1$

	/** URIs de los algoritmos de firma */
	public static final Map<String, String> SIGN_ALGOS_URI = new HashMap<String, String>() {
		private static final long serialVersionUID = 1897588397257599853L;
		{
			put(AOSignConstants.SIGN_ALGORITHM_SHA1WITHRSA, URL_SHA1_RSA);
			// Introducimos variantes para hacerlo mas robusto
			put("RSA", URL_SHA1_RSA); //$NON-NLS-1$
			put("SHA-1withRSA", URL_SHA1_RSA); //$NON-NLS-1$
			put("SHA1withRSAEncryption", URL_SHA1_RSA); //$NON-NLS-1$
			put("SHA-1withRSAEncryption", URL_SHA1_RSA); //$NON-NLS-1$
			put("SHAwithRSAEncryption", URL_SHA1_RSA); //$NON-NLS-1$
			put("SHAwithRSA", URL_SHA1_RSA); //$NON-NLS-1$

			put(AOSignConstants.SIGN_ALGORITHM_MD5WITHRSA, URL_MD5_RSA);

			put(AOSignConstants.SIGN_ALGORITHM_SHA256WITHRSA, URL_SHA256_RSA);
			// Introducimos variantes para hacerlo mas robusto
			put("SHA-256withRSA", URL_SHA256_RSA); //$NON-NLS-1$
			put("SHA256withRSAEncryption", URL_SHA256_RSA); //$NON-NLS-1$
			put("SHA-256withRSAEncryption", URL_SHA256_RSA); //$NON-NLS-1$

			put(AOSignConstants.SIGN_ALGORITHM_SHA384WITHRSA, URL_SHA384_RSA);
			// Introducimos variantes para hacerlo mas robusto
			put("SHA-384withRSA", URL_SHA384_RSA); //$NON-NLS-1$
			put("SHA384withRSAEncryption", URL_SHA384_RSA); //$NON-NLS-1$
			put("SHA-384withRSAEncryption", URL_SHA384_RSA); //$NON-NLS-1$

			put(AOSignConstants.SIGN_ALGORITHM_SHA512WITHRSA, URL_SHA512_RSA);
			// Introducimos variantes para hacerlo mas robusto
			put("SHA-512withRSA", URL_SHA512_RSA); //$NON-NLS-1$
			put("SHA512withRSAEncryption", URL_SHA512_RSA); //$NON-NLS-1$
			put("SHA-512withRSAEncryption", URL_SHA512_RSA); //$NON-NLS-1$

		}
	};
}

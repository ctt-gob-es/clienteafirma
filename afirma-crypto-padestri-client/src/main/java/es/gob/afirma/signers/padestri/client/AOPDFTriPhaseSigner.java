/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.padestri.client;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URL;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.util.Locale;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.http.UrlHttpManager;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.util.tree.AOTreeModel;

/** Firmador PAdES en tres fases.
 * Las firmas que genera no se etiquetan como ETSI, sino como "Adobe PKCS#7 Detached".
 * @author Tom&acute;s Garc&iacute;a-Mer&aacute;s */
public final class AOPDFTriPhaseSigner implements AOSigner {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Nombre de la propiedad de URL del servidor de firma trif&aacute;sica. */
	private static final String PROPERTY_NAME_SIGN_SERVER_URL = "serverUrl"; //$NON-NLS-1$

	/** Identificador de la operacion de prefirma en servidor. */
	private static final String OPERATION_PRESIGN = "pre"; //$NON-NLS-1$

	/** Identificador de la operacion de postfirma en servidor. */
	private static final String OPERATION_POSTSIGN = "post"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro de c&oacute;digo de operaci&oacute;n en la URL de llamada al servidor de firma. */
	private static final String PARAMETER_NAME_OPERATION = "op"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n criptogr&aacute;fica de firma. */
	private static final String CRYPTO_OPERATION_SIGN = "sign"; //$NON-NLS-1$

	/** Nombre del par&aacute;metro que identifica la operaci&oacute;n criptogr&aacute;fica en la URL del servidor de firma. */
	private static final String PARAMETER_NAME_CRYPTO_OPERATION = "cop"; //$NON-NLS-1$

	private static final String HTTP_CGI = "?"; //$NON-NLS-1$
	private static final String HTTP_EQUALS = "="; //$NON-NLS-1$
	private static final String HTTP_AND = "&"; //$NON-NLS-1$

	// Parametros que necesitamos para la URL de las llamadas al servidor de firma
	private static final String PARAMETER_NAME_DOCID = "doc"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_ALGORITHM = "algo"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_FORMAT = "format"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_CERT = "cert"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_EXTRA_PARAM = "params"; //$NON-NLS-1$
	private static final String PARAMETER_NAME_SESSION_DATA = "session"; //$NON-NLS-1$

	private static final String PADES_FORMAT = "pades"; //$NON-NLS-1$

	// Nombres de las propiedades intercambiadas con el servidor como Properties

	/** Prefijo para las propiedades que almacenan prefirmas. */
	private static final String PROPERTY_NAME_PRESIGN = "PRE"; //$NON-NLS-1$

	/** Firma PKCS#1. */
	private static final String PROPERTY_NAME_PKCS1_SIGN = "PK1"; //$NON-NLS-1$

	/** Indica si la postfirma requiere la prefirma. */
	private static final String PROPERTY_NAME_NEED_PRE = "NEED_PRE"; //$NON-NLS-1$

	private static final String PDF_FILE_HEADER = "%PDF-"; //$NON-NLS-1$
	private static final String PDF_FILE_SUFFIX = ".pdf"; //$NON-NLS-1$

	/** Indicador de finalizaci&oacute;n correcta de proceso. */
	private static final String SUCCESS = "OK"; //$NON-NLS-1$

	@Override
	public byte[] sign(final byte[] data,
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
			throw new IllegalArgumentException("No se ha proporcionado una URL valida para el servidor de firma: " + extraParams.getProperty(PROPERTY_NAME_SIGN_SERVER_URL), e); //$NON-NLS-1$
		}

		// Decodificamos el identificador del documento
		final String documentId = Base64.encode(data, true);

		final UrlHttpManager urlManager = UrlHttpManagerFactory.getInstalledManager();

		// ---------
		// PREFIRMA
		// ---------

		// Empezamos la prefirma
		final byte[] preSignResult;
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
			append(PARAMETER_NAME_CRYPTO_OPERATION).append(HTTP_EQUALS).append(CRYPTO_OPERATION_SIGN).append(HTTP_AND).
			append(PARAMETER_NAME_FORMAT).append(HTTP_EQUALS).append(PADES_FORMAT).append(HTTP_AND).
			append(PARAMETER_NAME_ALGORITHM).append(HTTP_EQUALS).append(algorithm).append(HTTP_AND).
			append(PARAMETER_NAME_CERT).append(HTTP_EQUALS).append(Base64.encode(certChain[0].getEncoded(), true)).append(HTTP_AND).
			append(PARAMETER_NAME_DOCID).append(HTTP_EQUALS).append(documentId);

			if (extraParams.size() > 0) {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_EXTRA_PARAM).append(HTTP_EQUALS).
				append(properties2Base64(extraParams));
			}

			preSignResult = urlManager.readUrlByPost(urlBuffer.toString());
			urlBuffer.setLength(0);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error decodificando el certificado del firmante: " + e, e); //$NON-NLS-1$
		}
		catch (final IOException e) {
			throw new AOException("Error en la llamada de prefirma al servidor: " + e, e); //$NON-NLS-1$
		}


		// ----------
		// FIRMA
		// ----------

		// Convertimos la respuesta del servidor en un Properties
		final TriphaseData triphaseData;
		try {
			triphaseData = TriphaseData.parser(Base64.decode(preSignResult, 0, preSignResult.length, true));
		}
		catch (final Exception e) {
			LOGGER.severe("Error al analizar la prefirma enviada por el servidor: " + e); //$NON-NLS-1$
			throw new AOException("Error al analizar la prefirma enviada por el servidor", e); //$NON-NLS-1$
		}

		// Comprobamos que se incluyan las prefirmas en los datos recibidos
		if (triphaseData.getSignsCount() < 1) {
			throw new AOException("No se han recibido prefirmas que firmar");  //$NON-NLS-1$
		}

		// Es posible que se ejecute mas de una firma como resultado de haber proporcionado varios
		// identificadores de datos o en una operacion de contrafirma.
		for (int i = 0; i < triphaseData.getSignsCount(); i++) {

			final TriphaseData.TriSign signConfig = triphaseData.getSign(i);
			final String base64PreSign = signConfig.getProperty(PROPERTY_NAME_PRESIGN);
			if (base64PreSign == null) {
				throw new AOException("El servidor no ha devuelto la prefirma numero " + i + ": " + new String(preSignResult)); //$NON-NLS-1$ //$NON-NLS-2$
			}

			final byte[] preSign;
			try {
				preSign = Base64.decode(base64PreSign);
			}
			catch (final IOException e) {
				throw new AOException("Error decodificando la prefirma: " + e, e); //$NON-NLS-1$
			}

			final byte[] pkcs1sign = new AOPkcs1Signer().sign(
					preSign,
					algorithm,
					key,
					certChain,
					null // No hay parametros en PKCS#1
					);

			// Configuramos la peticion de postfirma indicando las firmas PKCS#1 generadas
			signConfig.addProperty(PROPERTY_NAME_PKCS1_SIGN, Base64.encode(pkcs1sign));

			// Si no es necesaria la prefirma para completar la postfirma, la eliminamos
			if (signConfig.getProperty(PROPERTY_NAME_NEED_PRE) != null &&
					!Boolean.parseBoolean(signConfig.getProperty(PROPERTY_NAME_NEED_PRE))) {
				signConfig.deleteProperty(PROPERTY_NAME_PRESIGN);
			}
		}

		final String preResultAsBase64 = Base64.encode(triphaseData.toString().getBytes(), true);

		// ---------
		// POSTFIRMA
		// ---------

		final byte[] triSignFinalResult;
		try {
			final StringBuffer urlBuffer = new StringBuffer();
			urlBuffer.append(signServerUrl).append(HTTP_CGI).
			append(PARAMETER_NAME_OPERATION).append(HTTP_EQUALS).append(OPERATION_POSTSIGN).append(HTTP_AND).
			append(PARAMETER_NAME_CRYPTO_OPERATION).append(HTTP_EQUALS).append(CRYPTO_OPERATION_SIGN).append(HTTP_AND).
			append(PARAMETER_NAME_FORMAT).append(HTTP_EQUALS).append(PADES_FORMAT).append(HTTP_AND).
			append(PARAMETER_NAME_ALGORITHM).append(HTTP_EQUALS).append(algorithm).append(HTTP_AND).
			append(PARAMETER_NAME_CERT).append(HTTP_EQUALS).append(Base64.encode(certChain[0].getEncoded(), true)).
			append(HTTP_AND).append(PARAMETER_NAME_DOCID).append(HTTP_EQUALS).append(documentId).
			append(HTTP_AND).append(PARAMETER_NAME_SESSION_DATA).append(HTTP_EQUALS).append(preResultAsBase64);

			if (extraParams.size() > 0) {
				urlBuffer.append(HTTP_AND).append(PARAMETER_NAME_EXTRA_PARAM).append(HTTP_EQUALS).
				append(properties2Base64(extraParams));
			}

			triSignFinalResult = urlManager.readUrlByPost(urlBuffer.toString());
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
			return Base64.decode(stringTrimmedResult.substring((SUCCESS + " NEWID=").length()), true); //$NON-NLS-1$
		}
		catch (final IOException e) {
			LOGGER.warning("El resultado de NEWID del servidor no estaba en Base64: " + e); //$NON-NLS-1$
			throw new AOException("El resultado devuelto por el servidor no es correcto", e); //$NON-NLS-1$
		}
	}

	private static String properties2Base64(final Properties p) throws IOException {
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		p.store(baos, ""); //$NON-NLS-1$
		return Base64.encode(baos.toByteArray(), true);
	}

	@Override
	public byte[] cosign(final byte[] data,
			final byte[] sign,
			final String algorithm,
			final PrivateKey key,
			final Certificate[] certChain,
			final Properties extraParams) throws AOException {
		return sign(sign, algorithm, key, certChain, extraParams);
	}

	@Override
	public byte[] cosign(final byte[] sign,
			final String algorithm,
			final PrivateKey key,
			final Certificate[] certChain,
			final Properties extraParams) throws AOException {
		return sign(sign, algorithm, key, certChain, extraParams);
	}

	@Override
	public byte[] countersign(final byte[] sign,
			final String algorithm,
			final CounterSignTarget targetType,
			final Object[] targets,
			final PrivateKey key,
			final Certificate[] certChain,
			final Properties extraParams) throws AOException {
		throw new UnsupportedOperationException("No se soportan contrafirmas en PAdES"); //$NON-NLS-1$
	}

	@Override
	public AOTreeModel getSignersStructure(final byte[] sign,
			final boolean asSimpleSignInfo) {
		throw new UnsupportedOperationException("No soportado para firmas trifasicas"); //$NON-NLS-1$
	}

	@Override
	public boolean isSign(final byte[] data) {
		return false;
	}

	@Override
	public boolean isValidDataFile(final byte[] data) {
		if (data == null) {
			LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
			return false;
		}
		return isPdfFile(data);
	}

	@Override
	public String getSignedName(final String originalName, final String inText) {
		final String inTextInt = inText != null ? inText : ""; //$NON-NLS-1$
		if (originalName == null) {
			return "signed.pdf"; //$NON-NLS-1$
		}
		if (originalName.toLowerCase(Locale.ENGLISH).endsWith(PDF_FILE_SUFFIX)) {
			return originalName.substring(0, originalName.length() - PDF_FILE_SUFFIX.length()) + inTextInt + PDF_FILE_SUFFIX;
		}
		return originalName + inTextInt + PDF_FILE_SUFFIX;
	}

	@Override
	public byte[] getData(final byte[] sign) throws AOException {
		// Si no es una firma PDF valida, lanzamos una excepcion
		if (!isSign(sign)) {
			throw new AOInvalidFormatException("El documento introducido no contiene una firma valida"); //$NON-NLS-1$
		}
		return sign;
	}

	@Override
	public AOSignInfo getSignInfo(final byte[] data) throws AOException {
		if (data == null) {
			throw new IllegalArgumentException("No se han introducido datos para analizar"); //$NON-NLS-1$
		}

		if (!isSign(data)) {
			throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un objeto de firma"); //$NON-NLS-1$
		}

		return new AOSignInfo(AOSignConstants.SIGN_FORMAT_PDF);
		// Aqui podria venir el analisis de la firma buscando alguno de los
		// otros datos de relevancia que se almacenan en el objeto AOSignInfo
	}

	private static boolean isPdfFile(final byte[] data) {
		byte[] buffer = new byte[PDF_FILE_HEADER.length()];
		try {
			new ByteArrayInputStream(data).read(buffer);
		}
		catch (final Exception e) {
			buffer = null;
		}
		// Comprobamos que cuente con una cabecera PDF
		if (buffer != null && !PDF_FILE_HEADER.equals(new String(buffer))) {
			return false;
		}
		return true;
	}

}

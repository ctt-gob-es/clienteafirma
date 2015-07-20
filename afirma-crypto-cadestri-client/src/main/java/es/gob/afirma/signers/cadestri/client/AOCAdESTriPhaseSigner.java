/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.cadestri.client;

import static es.gob.afirma.signers.cadestri.client.ProtocolConstants.PROPERTY_NAME_SIGN_SERVER_URL;

import java.io.IOException;
import java.net.URL;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.security.cert.CertificateEncodingException;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.misc.http.UrlHttpManager;
import es.gob.afirma.core.misc.http.UrlHttpManagerFactory;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.util.tree.AOTreeModel;

/** Firmador CAdES en tres fases.
 * @author Tom&acute;s Garc&iacute;a-Mer&aacute;s */
public final class AOCAdESTriPhaseSigner implements AOSigner {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n criptogr&aacute;fica de firma. */
	private static final String CRYPTO_OPERATION_SIGN = "sign"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n criptogr&aacute;fica de cofirma. */
	private static final String CRYPTO_OPERATION_COSIGN = "cosign"; //$NON-NLS-1$

	/** Identificador de la operaci&oacute;n criptogr&aacute;fica de contrafirma. */
	private static final String CRYPTO_OPERATION_COUNTERSIGN = "countersign"; //$NON-NLS-1$

	// Nombres de las propiedades intercambiadas con el servidor como Properties

	/** Nombre de la propiedad con los nodos objetivo para la contrafirma. */
	private static final String PROPERTY_NAME_CS_TARGET = "target"; //$NON-NLS-1$

	/** Etiqueta de cada prefirma en el XML de sesi&oacute;n trif&aacute;sica. */
	private static final String PROPERTY_NAME_PRESIGN = "PRE"; //$NON-NLS-1$

	/** Etiqueta de cada firma PKCS#1 en el XML de sesi&oacute;n trif&aacute;sica. */
	private static final String PROPERTY_NAME_PKCS1_SIGN = "PK1"; //$NON-NLS-1$

	/** Indicador de finalizaci&oacute;n correcta de proceso. */
	private static final String SUCCESS = "OK"; //$NON-NLS-1$


	@Override
	public byte[] sign(final byte[] data,
			final String algorithm,
			final PrivateKey key,
			final Certificate[] certChain,
			final Properties extraParams) throws AOException, IOException {
		return triPhaseOperation(CRYPTO_OPERATION_SIGN, data, algorithm, key, certChain, extraParams);
	}

	@Override
	public byte[] cosign(final byte[] data,
			final byte[] sign,
			final String algorithm,
			final PrivateKey key,
			final Certificate[] certChain,
			final Properties extraParams) throws AOException, IOException {
		return cosign(sign, algorithm, key, certChain, extraParams);
	}

	@Override
	public byte[] cosign(final byte[] sign,
			final String algorithm,
			final PrivateKey key,
			final Certificate[] certChain,
			final Properties extraParams)
					throws AOException, IOException {
		return triPhaseOperation(CRYPTO_OPERATION_COSIGN, sign, algorithm, key, certChain, extraParams);
	}

	@Override
	public byte[] countersign(final byte[] sign,
			final String algorithm,
			final CounterSignTarget targetType,
			final Object[] targets,
			final PrivateKey key,
			final Certificate[] certChain,
			final Properties extraParams) throws AOException, IOException {

		// Si no se ha definido nodos objeto de la contrafirma se definen los nodos hijo
		if (targetType == null) {
			throw new IllegalArgumentException("No se han indicado los nodos objetivo de la contrafirma"); //$NON-NLS-1$
		}

		// Comprobamos si es un tipo de contrafirma soportado
    	if (targetType != CounterSignTarget.TREE && targetType != CounterSignTarget.LEAFS) {
    		throw new UnsupportedOperationException("El objetivo indicado para la contrafirma no esta soportado: " + targetType); //$NON-NLS-1$
    	}

		extraParams.setProperty(PROPERTY_NAME_CS_TARGET, targetType.toString());

		return triPhaseOperation(CRYPTO_OPERATION_COUNTERSIGN, sign, algorithm, key, certChain, extraParams);
	}

	/** {@inheritDoc} */
	@Override
	public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo) throws AOInvalidFormatException {
		throw new UnsupportedOperationException("No se soporta la obtencion de estructura de firmas en firma trifasica"); //$NON-NLS-1$
	}

	/** {@inheritDoc} */
	@Override
	public boolean isSign(final byte[] sign) {
		throw new UnsupportedOperationException("No se soporta comprobacion de si es firma en modo trifasico"); //$NON-NLS-1$
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
		return originalName + (inText != null ? inText : "") + ".csig"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** {@inheritDoc} */
	@Override
	public byte[] getData(final byte[] signData) throws AOException, IOException {
		throw new UnsupportedOperationException("No se soporta ela obtencion de datos n firma trifasica"); //$NON-NLS-1$
	}

	/** {@inheritDoc} */
	@Override
	public AOSignInfo getSignInfo(final byte[] sign) throws AOException {
		throw new UnsupportedOperationException("No se soporta la obtencion de informacion de la firma en modo trifasico"); //$NON-NLS-1$
	}

	/** Ejecuta una operaci&oacute;n de firma/multifirma en 3 fases.
	 * @param cryptoOperation Tipo de operaci&oacute;n.
	 * @param docId Identificador del documento a firmar/multifirmar. Posiblemente, el propio documento.
	 * @param algorithm Algoritmo de firma
	 * @param key Clave privada del certificado de firma.
	 * @param certChain Cadena de certificaci&oacute;n.
	 * @param extraParams Par&aacute;metros para la configuraci&oacute;n de la operaci&oacute;n.
	 * @return Resultado de la operaci&oacute;n de firma.
	 * @throws AOException Cuando se produce un error durante la operaci&oacute;n. */
	private static byte[] triPhaseOperation(final String cryptoOperation,
			                                final byte[] docId,
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
		if (docId == null) {
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
		final String documentId = Base64.encode(docId, true);

		final UrlHttpManager urlManager = UrlHttpManagerFactory.getInstalledManager();

		// ---------
		// PREFIRMA
		// ---------

		// Empezamos la prefirma
		final byte[] preSignResult;
		try {
			preSignResult = PreSigner.preSign(
				algorithm,
				certChain,
				cryptoOperation,
				documentId,
				urlManager,
				signServerUrl,
				extraParams
			);
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
		TriphaseData triphaseData;
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

		try {
			for (int i = 0; i < triphaseData.getSignsCount(); i++) {
				final TriphaseData.TriSign signConfig = triphaseData.getSign(i);
				final byte[] pkcs1sign = new AOPkcs1Signer().sign(
					Base64.decode(signConfig.getProperty(PROPERTY_NAME_PRESIGN)),
					algorithm,
					key,
					certChain,
					null // No hay parametros en PKCS#1
				);
				signConfig.addProperty(PROPERTY_NAME_PKCS1_SIGN, Base64.encode(pkcs1sign));
			}
		}
		catch (final Exception e) {
			LOGGER.severe("Ocurrio un error en la decodificacion de una prefirma: " + e); //$NON-NLS-1$
			throw new AOException("Ocurrio un error en la decodificacion de una prefirma: " + e, e); //$NON-NLS-1$
		}

		final String preResultAsBase64 = Base64.encode(triphaseData.toString().getBytes(), true);

		// ---------
		// POSTFIRMA
		// ---------

		final byte[] triSignFinalResult;
		try {
			triSignFinalResult = PostSigner.postSign(
				algorithm,
				certChain,
				cryptoOperation,
				documentId,
				extraParams,
				true,	// Aqui los datos son el identificador de documento original
				urlManager,
				signServerUrl,
				preResultAsBase64
			);
		}
		catch (final CertificateEncodingException e1) {
			throw new AOException(
					"Error en el tratamiento del certificado del firmante en la postfirma: " + e1, e1); //$NON-NLS-1$
		}
		catch (final IOException e1) {
			throw new AOException("Error en la postfirma: " + e1, e1); //$NON-NLS-1$
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
}
/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.triphase.signer.processors;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOFormatFileException;
import es.gob.afirma.core.RuntimeConfigNeededException;
import es.gob.afirma.core.SigningLTSException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseData.TriSign;
import es.gob.afirma.signers.cades.CAdESExtraParams;
import es.gob.afirma.signers.cades.CAdESParameters;
import es.gob.afirma.signers.cades.CAdESTriPhaseSigner;
import es.gob.afirma.signers.multi.cades.CAdESMultiUtil;
import es.gob.afirma.signers.pkcs7.ContainsNoDataException;
import es.gob.afirma.signers.pkcs7.ObtainContentSignedData;
import es.gob.afirma.signvalidation.InvalidSignatureException;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.ValidateBinarySignature;
import es.gob.afirma.triphase.signer.cades.AOCAdESTriPhaseCoSigner;
import es.gob.afirma.triphase.signer.cades.AOCAdESTriPhaseCounterSigner;

/** Procesador de firmas trif&aacute;sicas CAdES.
 * @author Tom&aacute;s Garc&iacute;a Mer&aacute;s. */
public class CAdESTriPhasePreProcessor implements TriPhasePreProcessor {

	/** Indica si la postfirma requiere los datos. */
	private static final String PROPERTY_NAME_NEED_DATA = "NEED_DATA"; //$NON-NLS-1$

	/** Indica si la postfirma requiere la prefirma. */
	private static final String PROPERTY_NAME_NEED_PRE = "NEED_PRE"; //$NON-NLS-1$

	/** Prefijo para cada prefirma. */
	private static final String PROPERTY_NAME_PRESIGN = "PRE"; //$NON-NLS-1$

	/** Etiqueta de firma PKCS#1 en el XML de sesi&oacute;n trif&aacute;sica. */
	private static final String PROPERTY_NAME_PKCS1_SIGN = "PK1"; //$NON-NLS-1$

	/** Manejador de log. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$


	@Override
	public TriphaseData preProcessPreSign(final byte[] data,
			                        final String algorithm,
			                        final X509Certificate[] certChain,
			                        final Properties params,
		                               final boolean checkSignatures) throws IOException, AOException {

		LOGGER.info("Prefirma CAdES - Firma - INICIO"); //$NON-NLS-1$

		if (data == null || data.length < 1) {
			throw new IllegalArgumentException("Los datos no pueden ser nulos"); //$NON-NLS-1$
		}

		final Properties extraParams = getExtraParams(params);

		final CAdESParameters parameters = CAdESParameters.load(data, algorithm, extraParams);

		LOGGER.fine("Se invocan las funciones internas de prefirma CAdES"); //$NON-NLS-1$
		final byte[] presign = CAdESTriPhaseSigner.preSign(
				certChain,
				new Date(),
				parameters);

		LOGGER.fine("Se prepara la respuesta de la prefirma CAdES"); //$NON-NLS-1$

		// Generamos el mensaje para la configuracion de la operacion
		final TriphaseData triphaseData = new TriphaseData();

		final Map<String, String> signConfig = new ConcurrentHashMap<>();
		signConfig.put(PROPERTY_NAME_PRESIGN, Base64.encode(presign));
		signConfig.put(PROPERTY_NAME_NEED_PRE, Boolean.TRUE.toString());
		// Si se guardo el contenido dentro de la configuracion es que se habia pedido incluir
		// los datos en la firma
		if (parameters.isContentNeeded()) {
			signConfig.put(PROPERTY_NAME_NEED_DATA, Boolean.TRUE.toString());
		}

		triphaseData.addSignOperation(
			new TriSign(
				signConfig,
				TriPhaseUtil.getSignatureId(extraParams)
			)
		);

		LOGGER.info("Prefirma CAdES - Firma - FIN"); //$NON-NLS-1$

		return triphaseData;
	}

	@Override
	public byte[] preProcessPostSign(final byte[] data,
			                         final String algorithm,
			                         final X509Certificate[] cert,
			                         final Properties extraParams,
			                         final byte[] session) throws NoSuchAlgorithmException,
			                                                                AOException,
			                                                                IOException {

		if (session == null) {
			throw new IllegalArgumentException("Los datos de prefirma no pueden ser nulos"); //$NON-NLS-1$
		}

		return preProcessPostSign(data, algorithm, cert, extraParams, TriphaseData.parser(session));
	}

	@Override
	public byte[] preProcessPostSign(final byte[] data,
			                         final String signatureAlgorithm,
			                         final X509Certificate[] cert,
			                         final Properties params,
			                         final TriphaseData triphaseData) throws NoSuchAlgorithmException,
			                                                                 IOException,
			                                                                 AOException {
		LOGGER.info("Postfirma CAdES - Firma - INICIO"); //$NON-NLS-1$

		if (triphaseData == null) {
			throw new IllegalArgumentException("Los datos de prefirma no pueden ser nulos"); //$NON-NLS-1$
		}

		final Properties extraParams = params != null ? params : new Properties();

		boolean omitContent = true;
		if (extraParams.containsKey("mode")) { //$NON-NLS-1$
			omitContent = !"implicit".equalsIgnoreCase(extraParams.getProperty("mode")); //$NON-NLS-1$ //$NON-NLS-2$
		}

		// Cargamos la configuracion de la operacion
		if (triphaseData.getSignsCount() < 1) {
			LOGGER.severe("No se ha encontrado la informacion de firma en la peticion"); //$NON-NLS-1$
			throw new AOException("No se ha encontrado la informacion de firma en la peticion"); //$NON-NLS-1$
		}

		final TriSign config = triphaseData.getSign(0);

		final String preSignB64 = config.getProperty(PROPERTY_NAME_PRESIGN);
		if (preSignB64 == null) {
			throw new IllegalArgumentException(
				"Los datos de sesion trifasica (CAdES) no contienen la prefirma para: " + config.toString() //$NON-NLS-1$
			);
		}

		LOGGER.fine("Se invocan las funciones internas de postfirma CAdES"); //$NON-NLS-1$

		final byte[] signature = CAdESTriPhaseSigner.postSign(
			signatureAlgorithm,
			omitContent ? null : data,
			cert,
			Base64.decode(config.getProperty(PROPERTY_NAME_PKCS1_SIGN)),
			Base64.decode(preSignB64)
		);

		LOGGER.info("Postfirma CAdES - Firma - FIN"); //$NON-NLS-1$

		return signature;
	}

	@Override
	public TriphaseData preProcessPreCoSign(final byte[] sign,
			final String algorithm,
			final X509Certificate[] cert,
			final Properties params,
            final boolean checkSignatures) throws IOException, AOException {

		LOGGER.info("Prefirma CAdES - Cofirma - INICIO"); //$NON-NLS-1$

		if (sign == null || sign.length < 1) {
			throw new IllegalArgumentException("Las firma no puede ser nula ni vacia"); //$NON-NLS-1$
		}

		final Properties extraParams = getExtraParams(params);

		// Comprobamos que no haya firmas de archivo, salvo que nos indiquen que se pueden firmar
		final String allowSignLts = extraParams.getProperty(CAdESExtraParams.ALLOW_SIGN_LTS_SIGNATURES);
		if (allowSignLts == null || !Boolean.parseBoolean(allowSignLts)) {
			try {
				CAdESMultiUtil.checkUnsupportedAttributes(sign);
			}
			catch (final IOException e) {
				throw new AOFormatFileException("Los datos proporcionados no se corresponden con una firma CAdES", e); //$NON-NLS-1$
			} catch (final SigningLTSException e) {
				// Si se indico expresamente que no se debia permitir la cofirma de
				// firmas de archivo, se lanza una excepcion bloqueando la ejecucion.
				// Si no, se informa debidamente para que se consulte al usuario
				if (allowSignLts != null) {
					throw new AOException(e.getMessage());
				}
				throw e;
			} catch (final Exception e) {
				throw new AOFormatFileException("No se ha proporcionado una firma CAdES que se pueda cofirmar", e); //$NON-NLS-1$
			}
		}

		// Comprobamos la validez de la firma de entrada si se solicito
        if (checkSignatures) {
        	final List<SignValidity> validity = new ValidateBinarySignature().validate(sign);
        	if (validity.get(0).getValidity() == SIGN_DETAIL_TYPE.KO) {
        		throw new InvalidSignatureException("La firma que se trata de cofirmar no es valida: " + validity.get(0).getError().toString()); //$NON-NLS-1$
        	}
        }

		final byte[] data = ObtainContentSignedData.obtainData(sign);

		final CAdESParameters parameters = CAdESParameters.load(data, algorithm, extraParams);

		// Si los datos no estaban contenidos en la firma, habra que obtener la huella para firmar
		// de la firma previa
		if (data == null) {
			final byte[] messageDigest = ObtainContentSignedData.obtainMessageDigest(sign, AOSignConstants.getDigestAlgorithmName(algorithm));
			if (messageDigest == null) {
				throw new ContainsNoDataException("No se han encontrado datos dentro de la firma ni una huella digital compatible con el algoritmo: " + algorithm); //$NON-NLS-1$
			}
			parameters.setDataDigest(messageDigest);
		}

		LOGGER.fine("Se invocan las funciones internas de pre-cofirma CAdES"); //$NON-NLS-1$
		final byte[] presign;
		try {
			presign = AOCAdESTriPhaseCoSigner.preCoSign(
				data,
				algorithm,
				cert,
				parameters
			);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error de codificacion de certificado en la pre-cofirma CAdES: " + e, e); //$NON-NLS-1$
		}
		catch (final NoSuchAlgorithmException e) {
			throw new AOException("Error de algoritmo no soportado en la pre-cofirma CAdES: " + e, e); //$NON-NLS-1$
		}

		LOGGER.fine("Se prepara la respuesta de la pre-cofirma CAdES"); //$NON-NLS-1$

		// Ahora pasamos al cliente los datos de la prefirma
		final String presignB64 = Base64.encode(presign).replace("\n", "").replace("\r", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

		final TriphaseData triphaseData = new TriphaseData();

		final Map<String, String> signConfig = new ConcurrentHashMap<>();
		signConfig.put(PROPERTY_NAME_PRESIGN, presignB64);
		signConfig.put(PROPERTY_NAME_NEED_DATA, Boolean.TRUE.toString());
		signConfig.put(PROPERTY_NAME_NEED_PRE, Boolean.TRUE.toString());

		triphaseData.addSignOperation(
			new TriSign(
				signConfig,
				TriPhaseUtil.getSignatureId(extraParams)
			)
		);

		LOGGER.info("Prefirma CAdES - Cofirma - FIN"); //$NON-NLS-1$

		return triphaseData;
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] sign,
			                           final String algorithm,
			                           final X509Certificate[] cert,
			                           final Properties extraParams,
			                           final byte[] session) throws NoSuchAlgorithmException, AOException, IOException {

		if (session == null) {
			throw new IllegalArgumentException("Los datos de prefirma no pueden ser nulos"); //$NON-NLS-1$
		}

		return preProcessPostCoSign(sign, algorithm, cert, extraParams, TriphaseData.parser(session));
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] sign,
			                           final String algorithm,
			                           final X509Certificate[] cert,
			                           final Properties extraParams,
			                           final TriphaseData triphaseData) throws NoSuchAlgorithmException, AOException, IOException {

		LOGGER.info("Postfirma CAdES - Cofirma - INICIO"); //$NON-NLS-1$

		if (triphaseData == null) {
			throw new IllegalArgumentException("Los datos de prefirma no pueden ser nulos"); //$NON-NLS-1$
		}

		byte[] messageDigest = null;
		final byte[] data = ObtainContentSignedData.obtainData(sign);
		if (data == null) {
			messageDigest = ObtainContentSignedData.obtainMessageDigest(sign, AOSignConstants.getDigestAlgorithmName(algorithm));
			if (messageDigest == null) {
				throw new AOException("No se han encontrado datos dentro de la firma ni una huella digital compatible con el algoritmo: " + algorithm); //$NON-NLS-1$
			}
		}

		// Cargamos la configuracion de la operacion
		if (triphaseData.getSignsCount() < 1) {
			LOGGER.severe("No se ha encontrado la informacion de firma en la peticion"); //$NON-NLS-1$
			throw new AOException("No se ha encontrado la informacion de firma en la peticion"); //$NON-NLS-1$
		}

		final TriSign config = triphaseData.getSign(0);

		final byte[] pk1 = Base64.decode(config.getProperty(PROPERTY_NAME_PKCS1_SIGN));
		config.deleteProperty(PROPERTY_NAME_PKCS1_SIGN);

		final byte[] presign = Base64.decode(config.getProperty(PROPERTY_NAME_PRESIGN));
		config.deleteProperty(PROPERTY_NAME_PRESIGN);

		LOGGER.fine("Se invocan las funciones internas de post-cofirma CAdES"); //$NON-NLS-1$
		final byte[] coSignature;
		try {
			coSignature = AOCAdESTriPhaseCoSigner.postCoSign(
					pk1,
					presign,
					data, // Contenido
					algorithm,
					cert,
					sign
			);
		}
		catch (final CertificateEncodingException e) {
			throw new AOException("Error de codificacion de certificado en la post-cofirma CAdES: " + e, e); //$NON-NLS-1$
		}

		LOGGER.info("Postfirma CAdES - Cofirma - FIN"); //$NON-NLS-1$

		return coSignature;
	}

	@Override
	public TriphaseData preProcessPreCounterSign(final byte[] sign,
			                               final String algorithm,
			                               final X509Certificate[] cert,
			                               final Properties params,
			                               final CounterSignTarget targetType,
			                               final boolean checkSignatures) throws IOException,
			                                                                          AOException {

		LOGGER.info("Prefirma CAdES - Contrafirma - INICIO"); //$NON-NLS-1$

		final Properties extraParams = getExtraParams(params);

		// Comprobamos que no haya firmas de archivo, salvo que nos indiquen que pueden firmar
		final String allowSignLts = extraParams.getProperty(CAdESExtraParams.ALLOW_SIGN_LTS_SIGNATURES);
		if (allowSignLts == null || !Boolean.parseBoolean(allowSignLts)) {
			try {
				CAdESMultiUtil.checkUnsupportedAttributes(sign);
			}
			catch (final IOException e) {
				throw new AOFormatFileException("Los datos proporcionados no se corresponden con una firma CAdES", e); //$NON-NLS-1$
			} catch (final RuntimeConfigNeededException e) {
				// Si se indico expresamente que no se debia permitir la cofirma de
				// firmas de archivo, se lanza una excepcion bloqueando la ejecucion.
				// Si no, se informa debidamente para que se consulte al usuario
				if (allowSignLts != null) {
					throw new AOException(e.getMessage());
				}
				throw e;
			} catch (final Exception e) {
				throw new AOFormatFileException("No se ha proporcionado una firma CAdES que se pueda contrafirmar", e); //$NON-NLS-1$
			}
		}

		// Comprobamos la validez de la firma de entrada si se solicito
        if (checkSignatures) {
        	final List<SignValidity> validity = new ValidateBinarySignature().validate(sign);
        	if (validity.get(0).getValidity() == SIGN_DETAIL_TYPE.KO) {
        		throw new InvalidSignatureException("La firma que se trata de contrafirmar no es valida: " + validity.get(0).getError().toString()); //$NON-NLS-1$
        	}
        }

		final TriphaseData triphaseData = AOCAdESTriPhaseCounterSigner.preCountersign(
				sign,
				algorithm,
				targetType,
				null,
				cert,
				extraParams,
				new Date()
			);

		LOGGER.info("Prefirma CAdES - Contrafirma - FIN"); //$NON-NLS-1$

		return triphaseData;
	}

	@Override
	public byte[] preProcessPostCounterSign(final byte[] sign,
			                                final String algorithm,
			                                final X509Certificate[] cert,
			                                final Properties extraParams,
			                                final byte[] session,
			                                final CounterSignTarget targetType) throws NoSuchAlgorithmException,
			                                                                           AOException,
			                                                                           IOException {

		if (session == null) {
			throw new IllegalArgumentException("Los datos de prefirma no pueden ser nulos"); //$NON-NLS-1$
		}

		return preProcessPostCounterSign(sign, algorithm, cert, extraParams, TriphaseData.parser(session), targetType);
	}

	@Override
	public byte[] preProcessPostCounterSign(final byte[] sign,
			                                final String algorithm,
			                                final X509Certificate[] cert,
			                                final Properties extraParams,
			                                final TriphaseData triphaseData,
			                                final CounterSignTarget targetType) throws NoSuchAlgorithmException,
			                                                                           AOException,
			                                                                           IOException {

		LOGGER.info("Postfirma CAdES - Contrafirma - INICIO"); //$NON-NLS-1$

		final byte[] counterSignature = AOCAdESTriPhaseCounterSigner.postCountersign(
				sign,
				algorithm,
				targetType,
				null,
				cert,
				extraParams,
				triphaseData
			);

		LOGGER.info("Postfirma CAdES - Contrafirma - FIN"); //$NON-NLS-1$

		return counterSignature;
	}

    private static Properties getExtraParams(final Properties extraParams) {
    	final Properties newExtraParams = extraParams != null ?
    			(Properties) extraParams.clone() : new Properties();

    	return newExtraParams;
    }
}

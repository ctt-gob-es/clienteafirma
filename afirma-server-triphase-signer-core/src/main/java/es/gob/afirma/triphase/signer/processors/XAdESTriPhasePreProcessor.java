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
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.SignatureException;
import java.security.cert.X509Certificate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import javax.xml.crypto.MarshalException;
import javax.xml.crypto.dsig.XMLSignatureException;
import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.SAXException;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.Pkcs1Utils;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseData.TriSign;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xades.EFacturaAlreadySignedException;
import es.gob.afirma.signers.xades.InvalidEFacturaDataException;
import es.gob.afirma.signers.xml.XMLConstants;
import es.gob.afirma.signvalidation.InvalidSignatureException;
import es.gob.afirma.signvalidation.SignValidity;
import es.gob.afirma.signvalidation.SignValidity.SIGN_DETAIL_TYPE;
import es.gob.afirma.signvalidation.ValidateXMLSignature;
import es.gob.afirma.triphase.signer.xades.XAdESTriPhaseSignerServerSide;
import es.gob.afirma.triphase.signer.xades.XAdESTriPhaseSignerServerSide.Op;
import es.gob.afirma.triphase.signer.xades.XmlPreSignException;
import es.gob.afirma.triphase.signer.xades.XmlPreSignResult;

/** Procesador de firmas trif&aacute;sicas XAdES.
 * @author Tom&aacute;s Garc&iacute;a Mer&aacute;s. */
public class XAdESTriPhasePreProcessor implements TriPhasePreProcessor {

	private final boolean facturae;

	/** Prefijo para cada prefirma. */
	private static final String PROPERTY_NAME_PRESIGN = "PRE"; //$NON-NLS-1$

	/** Firma PKCS#1. */
	private static final String PROPERTY_NAME_PKCS1_SIGN = "PK1"; //$NON-NLS-1$

	/** Indica si la postfirma requiere la prefirma. */
	private static final String PROPERTY_NAME_NEED_PRE = "NEED_PRE"; //$NON-NLS-1$

	/** Nombre de la propiedad que guarda la estructura b&aacute;sica con la composici&oacute;n
	 * de la firma trif&aacute;sica. */
	private static final String PROPERTY_NAME_SCHEMA_BASE = "BASE"; //$NON-NLS-1$

	/** Nombre de la propiedad que guarda la codificaci&oacute;n del XML de firma. */
	private static final String PROPERTY_NAME_XML_ENCODING = "ENCODING"; //$NON-NLS-1$

	/** Nombre de la propiedad  que indica que el PKCS#1 se debe enviar decodificado. */
	private static final String PROPERTY_NAME_PKCS1_DECODED = "PK1_DECODED"; //$NON-NLS-1$

	/** Nombre de la propiedad de configuraci&oacute;n que indica qu&eacute; nodos deben
	 * contrafirmarse. */
	private static final String EXTRAPARAM_NAME_TARGET = "target"; //$NON-NLS-1$

	/** Manejador de log. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Construye un procesador de firmas trif&aacute;sicas XAdES. */
	public XAdESTriPhasePreProcessor() {
		this(false);
	}

	protected XAdESTriPhasePreProcessor(final boolean factura) {
		this.facturae = factura;
	}

	@Override
	public TriphaseData preProcessPreSign(final byte[] data,
			                              final String algorithm,
			                              final X509Certificate[] cert,
			                              final Properties extraParams,
				                          final boolean checkSignatures) throws IOException,
	                                                                           AOException {
		LOGGER.info("Prefirma XAdES - Firma - INICIO"); //$NON-NLS-1$

		// Con FacturaE solo podemos firmar facturas
		final AOSigner facturaESigner = new AOFacturaESigner();
		if (this.facturae && !facturaESigner.isValidDataFile(data)) {
			throw new InvalidEFacturaDataException();
		}

		// Las facturas solo pueden contener una firma
		if (this.facturae && facturaESigner.isSign(data)) {
			throw new EFacturaAlreadySignedException();
		}

		// Si es FacturaE modificamos los parametros adicionales
		final Properties xParams = this.facturae ? AOFacturaESigner.getFacturaEExtraParams(extraParams) : extraParams;

		final TriphaseData presign = preProcessPre(data, algorithm, cert, xParams, Op.SIGN);

		LOGGER.info("Prefirma XAdES - Firma - FIN"); //$NON-NLS-1$

		return presign;
	}

	@Override
	public TriphaseData preProcessPreCoSign(final byte[] data,
			                          final String algorithm,
			                          final X509Certificate[] cert,
			                          final Properties extraParams,
			                          final boolean checkSignatures) throws IOException, AOException {

		LOGGER.info("Prefirma XAdES - Cofirma - INICIO"); //$NON-NLS-1$

		// Comprobamos la validez de la firma de entrada si se solicito
        if (checkSignatures) {
        	final List<SignValidity> validity = new ValidateXMLSignature().validate(data);
        	if (validity.get(0).getValidity() == SIGN_DETAIL_TYPE.KO) {
        		throw new InvalidSignatureException("La firma que se trata de cofirmar no es valida: " + validity.get(0).getError().toString()); //$NON-NLS-1$
        	}
        }

		final TriphaseData presign = preProcessPre(data, algorithm, cert, extraParams, Op.COSIGN);

		LOGGER.info("Prefirma XAdES - Cofirma - FIN"); //$NON-NLS-1$

		return presign;
	}


	@Override
	public TriphaseData preProcessPreCounterSign(final byte[] sign,
			                               final String algorithm,
			                               final X509Certificate[] cert,
			                               final Properties extraParams,
			                               final CounterSignTarget targets,
				                           final boolean checkSignatures) throws IOException,
			                                                                       AOException {
		LOGGER.info("Prefirma XAdES - Contrafirma - INICIO"); //$NON-NLS-1$

		// Comprobamos la validez de la firma de entrada si se solicito
        if (checkSignatures) {
        	final List<SignValidity> validity = new ValidateXMLSignature().validate(sign);
        	if (validity.get(0).getValidity() == SIGN_DETAIL_TYPE.KO) {
        		throw new InvalidSignatureException("La firma que se trata de contrafirmar no es valida: " + validity.get(0).getError().toString()); //$NON-NLS-1$
        	}
        }

		extraParams.setProperty(EXTRAPARAM_NAME_TARGET, targets.name());

		final TriphaseData presign = preProcessPre(sign, algorithm, cert, extraParams, Op.COUNTERSIGN);

		LOGGER.info("Prefirma XAdES - Contrafirma - FIN"); //$NON-NLS-1$

		return presign;
	}

	private static TriphaseData preProcessPre(final byte[] data,
			                                  final String algorithm,
			                                  final X509Certificate[] cert,
			                                  final Properties extraParams,
			                                  final Op op) throws IOException,
			                                                      AOException {

		final String algoUri = XMLConstants.SIGN_ALGOS_URI.get(algorithm);
		if (algoUri == null) {
			throw new AOException(
				"El formato de firma XAdES no soporta el algoritmo de firma '" + algorithm + "'" //$NON-NLS-1$ //$NON-NLS-2$
			);
		}

		// Calculamos la prefirma que en el caso de XAdES va a ser una firma completa con claves
		// dummy que deberemos sustituir a posteriori
		final XmlPreSignResult preSignature;
		try {
			preSignature = XAdESTriPhaseSignerServerSide.preSign(
				data,
				algorithm,
				cert,
				extraParams,
				op
			);
		}
		catch (final InvalidKeyException e) {
			throw new AOException("Error en la prefirma XAdES por problemas con las claves: " + e, e); //$NON-NLS-1$
		}
		catch (final NoSuchAlgorithmException e) {
			throw new AOException("Error en la prefirma XAdES por no soportarse un algoritmo: " + e, e); //$NON-NLS-1$
		}
		catch (final SignatureException e) {
			throw new AOException("Error en la prefirma XAdES en la firma: " + e, e); //$NON-NLS-1$
		}
		catch (final SAXException e) {
			throw new AOException("Error en la prefirma XAdES en el proceso SAX del XML: " + e, e); //$NON-NLS-1$
		}
		catch (final ParserConfigurationException e) {
			throw new AOException("Error en la prefirma XAdES por problemas en el parser SAX: " + e, e); //$NON-NLS-1$
		}
		catch (final MarshalException e) {
			throw new AOException("Error en la prefirma XAdES al empaquetar el XML: " + e, e); //$NON-NLS-1$
		}
		catch (final XMLSignatureException e) {
			throw new AOException("Error en la prefirma XAdES en la firma XMLDSig: " + e, e); //$NON-NLS-1$
		}
		catch (final XmlPreSignException e) {
			throw new AOException("Error en la prefirma XAdES: " + e, e); //$NON-NLS-1$
		}

		// Recuperamos el identificador asociado a la firma u obtenemos uno si no lo tenia
		final String signatureId = TriPhaseUtil.getSignatureId(extraParams);

		// Preparamos los datos de prefirma para devolverselos al cliente
		final TriphaseData triphaseData = new TriphaseData();

		for (int i = 0; i < preSignature.getSignedInfos().size(); i++) {

			// Datos necesarios para la ejecucion del restos de pasos de la firma trifasica
			final Map<String, String> signConfig = new HashMap<>();

			signConfig.put(
				PROPERTY_NAME_PRESIGN,
				Base64.encode(preSignature.getSignedInfos().get(i))
			);

			// Las firmas XAdEScon Apache Santuario genera las firmas DSA/ECDSA de tal forma que el
			// PKCS#1 debe estar decodificado, por lo que el PKCS#1 enciado desde el cliente debe
			// de estarlo. Insertamos una propiedad para indicarselo al cliente
			if (AOSignConstants.isDSAorECDSASignatureAlgorithm(algorithm)) {
				signConfig.put(PROPERTY_NAME_PKCS1_DECODED, Boolean.TRUE.toString());
			}

			//TODO: Idealmente, la prefirma se deberia tomar en la postfirma del parametro BASE en lugar
			// de tener que reenviarla directamente. Asi se reduciria la transmision de datos y se evitaria
			// que el cliente de firma trifasica pudiese enviar una prefirma no valida
			signConfig.put(PROPERTY_NAME_NEED_PRE, Boolean.TRUE.toString());

			// Pasamos como datos de sesion el documento base en donde se realizan las sustituciones,
			// pero solo lo haremos en la primera prefirma ya que todos serian iguales
			if (i == 0) {

				signConfig.put(
					PROPERTY_NAME_SCHEMA_BASE,
					Base64.encode(
						XAdESTriPhaseSignerUtil.removeCommonParts(
							preSignature.getXmlSign(),
							preSignature.getEncoding(),
							extraParams
						)
					)
				);

				if (preSignature.getEncoding() != null) {
					signConfig.put(
						PROPERTY_NAME_XML_ENCODING,
						preSignature.getEncoding()
					);
				}
			}

			triphaseData.addSignOperation(
				new TriSign(
					signConfig,
					signatureId
				)
			);
		}

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

		return preProcessPostSign(data, algorithm, cert, extraParams, TriphaseData.parser(session));
	}

	@Override
	public byte[] preProcessPostSign(final byte[] data,
			                         final String algorithm,
			                         final X509Certificate[] cert,
			                         final Properties extraParams,
			                         final TriphaseData triphaseData) throws NoSuchAlgorithmException,
			                                                                 AOException,
			                                                                 IOException {

		LOGGER.info("Postfirma XAdES - Firma - INICIO"); //$NON-NLS-1$

		// Con FacturaE solo podemos firmar facturas
		if (this.facturae && !new AOFacturaESigner().isValidDataFile(data)) {
			throw new AOInvalidFormatException(
				"Los datos proporcionados no son una factura electronica compatible" //$NON-NLS-1$
			);
		}

		// Si es FacturaE modificamos los parametros adicionales
		final Properties xParams = this.facturae ? AOFacturaESigner.getFacturaEExtraParams(extraParams) : extraParams;

		final byte[] postsign = preProcessPost(data, algorithm, cert, xParams, Op.SIGN, triphaseData);

		LOGGER.info("Postfirma XAdES - Firma - FIN"); //$NON-NLS-1$

		return postsign;
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] data,
			                           final String algorithm,
			                           final X509Certificate[] cert,
			                           final Properties extraParams,
			                           final byte[] session) throws NoSuchAlgorithmException,
			                                                        AOException,
			                                                        IOException {

		LOGGER.info("Postfirma XAdES - Cofirma - INICIO"); //$NON-NLS-1$

		final byte[] postsign = preProcessPost(data, algorithm, cert, extraParams, Op.COSIGN, TriphaseData.parser(session));

		LOGGER.info("Postfirma XAdES - Cofirma - FIN"); //$NON-NLS-1$

		return postsign;
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] data,
			                           final String algorithm,
			                           final X509Certificate[] cert,
			                           final Properties extraParams,
			                           final TriphaseData triphaseData) throws NoSuchAlgorithmException,
			                                                                   AOException,
			                                                                   IOException {

		LOGGER.info("Postfirma XAdES - Cofirma - INICIO"); //$NON-NLS-1$

		final byte[] postsign = preProcessPost(data, algorithm, cert, extraParams, Op.COSIGN, triphaseData);

		LOGGER.info("Postfirma XAdES - Cofirma - FIN"); //$NON-NLS-1$

		return postsign;
	}

	@Override
	public byte[] preProcessPostCounterSign(final byte[] sign,
			                                final String algorithm,
			                                final X509Certificate[] cert,
			                                final Properties extraParams,
			                                final byte[] session,
			                                final CounterSignTarget targets) throws NoSuchAlgorithmException,
			                                                                        AOException,
			                                                                        IOException {

		LOGGER.info("Postfirma XAdES - Contrafirma - INICIO"); //$NON-NLS-1$

		extraParams.setProperty(EXTRAPARAM_NAME_TARGET, targets.name());

		final byte[] postsign = preProcessPost(sign, algorithm, cert, extraParams, Op.COUNTERSIGN, TriphaseData.parser(session));

		LOGGER.info("Postfirma XAdES - Contrafirma - FIN"); //$NON-NLS-1$

		return postsign;
	}

	@Override
	public byte[] preProcessPostCounterSign(final byte[] sign,
			                                final String algorithm,
			                                final X509Certificate[] cert,
			                                final Properties extraParams,
			                                final TriphaseData triphaseData,
			                                final CounterSignTarget targets) throws NoSuchAlgorithmException,
			                                                                        AOException,
			                                                                        IOException {

		LOGGER.info("Postfirma XAdES - Contrafirma - INICIO"); //$NON-NLS-1$

		extraParams.setProperty(EXTRAPARAM_NAME_TARGET, targets.name());

		final byte[] postsign = preProcessPost(sign, algorithm, cert, extraParams, Op.COUNTERSIGN, triphaseData);

		LOGGER.info("Postfirma XAdES - Contrafirma - FIN"); //$NON-NLS-1$

		return postsign;
	}

	private static byte[] preProcessPost(final byte[] data,
                                         final String algorithm,
                                         final X509Certificate[] cert,
                                         final Properties extraParams,
                                         final Op op,
                                         final TriphaseData triphaseData) throws IOException,
                                                                                 AOException {

		if (triphaseData.getSignsCount() < 1) {
			LOGGER.severe("No se ha encontrado la informacion de firma en la peticion"); //$NON-NLS-1$
			throw new AOException("No se ha encontrado la informacion de firma en la peticion"); //$NON-NLS-1$
		}

		// El XML base se incluye como datos de sesion de la primera firma y solo de la primera
		String xmlBase;
		if (triphaseData.getSign(0).getProperty(PROPERTY_NAME_XML_ENCODING) != null) {
			xmlBase = new String(
				Base64.decode(triphaseData.getSign(0).getProperty(PROPERTY_NAME_SCHEMA_BASE)),
				triphaseData.getSign(0).getProperty(PROPERTY_NAME_XML_ENCODING)
			);
		}
		else {
			xmlBase = new String(
				Base64.decode(triphaseData.getSign(0).getProperty(PROPERTY_NAME_SCHEMA_BASE))
			);
		}

		// Sustituimos los valores dummy de la firma por los reales
		for (int i = 0; i < triphaseData.getSignsCount(); i++) {
			String pkcs1Base64 = triphaseData.getSign(i).getProperty(PROPERTY_NAME_PKCS1_SIGN);
			if (pkcs1Base64 == null) {
				throw new IllegalArgumentException("La propiedades adicionales no contienen la firma PKCS#1"); //$NON-NLS-1$
			}

			// Si desde el servidor nos indican que necesitan el PKCS#1 decodificado, pues lo decodificamos
			final boolean decodePkcs1 = Boolean.parseBoolean(triphaseData.getSign(i).getProperty(PROPERTY_NAME_PKCS1_DECODED));
			if (decodePkcs1) {
				try {
					byte[] pkcs1sign = Base64.decode(pkcs1Base64);
					pkcs1sign = Pkcs1Utils.decodeSignature(pkcs1sign);
					pkcs1Base64 = Base64.encode(pkcs1sign);
				} catch (final SignatureException e) {
					LOGGER.warning("No se ha podido decodificar el PKCS#1 del servicio. Se usara el PKCS#1 recibido: " + e);
				}
			}

			// Hacemos la sustitucion del PKCS#1 en la firma
			xmlBase = xmlBase.replace(
				XAdESTriPhaseSignerServerSide.REPLACEMENT_STRING.replace(
					XAdESTriPhaseSignerServerSide.REPLACEMENT_CODE, Integer.toString(i)
				),
				pkcs1Base64.trim()
			);
		}

		// El XML resultante carece de las secciones de datos, KeyInfo y
		// signedProperties, por lo que hay que recrearlas y volverlas a
		// introducir

		final XmlPreSignResult preSignature;
		try {
			preSignature = XAdESTriPhaseSignerServerSide.preSign(
				data,
				algorithm,
				cert,
				extraParams,
				op
			);
		}
		catch (final Exception e) {
			throw new AOException(
				"Error recreando los datos a firmar y la cadena de certificados: " + e, e //$NON-NLS-1$
			);
		}

		byte[] completeSignature;
		try {
			completeSignature = XAdESTriPhaseSignerUtil.insertCommonParts(
				xmlBase.getBytes(preSignature.getEncoding()),
				preSignature.getXmlSign(),
				extraParams
			);
		}
		catch (final Exception e) {
			throw new AOException(
				"Error insertando los datos a firmar y la cadena de certificados: " + e, e //$NON-NLS-1$
			);
		}

		return completeSignature;
	}
}

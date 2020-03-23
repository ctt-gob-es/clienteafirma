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
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.TriphaseData.TriSign;
import es.gob.afirma.signers.xades.AOFacturaESigner;
import es.gob.afirma.signers.xades.EFacturaAlreadySignedException;
import es.gob.afirma.signers.xades.InvalidEFacturaDataException;
import es.gob.afirma.signers.xml.Utils;
import es.gob.afirma.signers.xml.XMLConstants;
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

	/** Nombre de la propiedad que guarda la estructura b&aacute;sica con la composici&oacute;n
	 * de la firma trif&aacute;sica. */
	private static final String PROPERTY_NAME_SCHEMA_BASE = "BASE"; //$NON-NLS-1$

	/** Nombre de la propiedad que guarda la codificaci&oacute;n del XML de firma. */
	private static final String PROPERTY_NAME_XML_ENCODING = "ENCODING"; //$NON-NLS-1$

	/** Nombre de la propiedad que de configuraci&oacute;n que indica qu&eacute; nodos deben
	 * contrafirmarse. */
	private static final String EXTRAPARAM_NAME_TARGET = "target"; //$NON-NLS-1$

	/** Manejador de log. */
	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** Construye un procesador de firmas trif&aacute;sicas XAdES.
	 * @param installXmlDSigProvider Indica si se debe instalar expresamente un proveedor de firmas XML. */
	public XAdESTriPhasePreProcessor(final boolean installXmlDSigProvider) {
		this(false, installXmlDSigProvider);
	}

	protected XAdESTriPhasePreProcessor(final boolean factura, final boolean installXmlDSigProvider) {
		this.facturae = factura;

		Utils.installXmlDSigProvider(installXmlDSigProvider);
	}

	@Override
	public TriphaseData preProcessPreSign(final byte[] data,
			                              final String algorithm,
			                              final X509Certificate[] cert,
			                              final Properties extraParams) throws IOException,
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
			                          final Properties extraParams) throws IOException, AOException {

		LOGGER.info("Prefirma XAdES - Cofirma - INICIO"); //$NON-NLS-1$

		final TriphaseData presign = preProcessPre(data, algorithm, cert, extraParams, Op.COSIGN);

		LOGGER.info("Prefirma XAdES - Cofirma - FIN"); //$NON-NLS-1$

		return presign;
	}


	@Override
	public TriphaseData preProcessPreCounterSign(final byte[] sign,
			                               final String algorithm,
			                               final X509Certificate[] cert,
			                               final Properties extraParams,
			                               final CounterSignTarget targets) throws IOException,
			                                                                       AOException {
		LOGGER.info("Prefirma XAdES - Contrafirma - INICIO"); //$NON-NLS-1$

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

			final Map<String, String> signConfig = new HashMap<>();

			signConfig.put(
				PROPERTY_NAME_PRESIGN,
				Base64.encode(preSignature.getSignedInfos().get(i))
			);

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
		// Con FacturaE solo podemos firmar facturas
		if (this.facturae && !new AOFacturaESigner().isValidDataFile(data)) {
			throw new AOInvalidFormatException(
				"Los datos proporcionados no son una factura electronica compatible" //$NON-NLS-1$
			);
		}

		// Si es FacturaE modificamos los parametros adicionales
		final Properties xParams = this.facturae ? AOFacturaESigner.getFacturaEExtraParams(extraParams) : extraParams;

		return preProcessPost(data, algorithm, cert, xParams, Op.SIGN, triphaseData);
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] data,
			                           final String algorithm,
			                           final X509Certificate[] cert,
			                           final Properties extraParams,
			                           final byte[] session) throws NoSuchAlgorithmException,
			                                                        AOException,
			                                                        IOException {

		return preProcessPost(data, algorithm, cert, extraParams, Op.COSIGN, TriphaseData.parser(session));
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] data,
			                           final String algorithm,
			                           final X509Certificate[] cert,
			                           final Properties extraParams,
			                           final TriphaseData triphaseData) throws NoSuchAlgorithmException,
			                                                                   AOException,
			                                                                   IOException {

		return preProcessPost(data, algorithm, cert, extraParams, Op.COSIGN, triphaseData);
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

		extraParams.setProperty(EXTRAPARAM_NAME_TARGET, targets.name());

		return preProcessPost(sign, algorithm, cert, extraParams, Op.COUNTERSIGN, TriphaseData.parser(session));
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

		extraParams.setProperty(EXTRAPARAM_NAME_TARGET, targets.name());

		return preProcessPost(sign, algorithm, cert, extraParams, Op.COUNTERSIGN, triphaseData);
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
			final String pkcs1Base64 = triphaseData.getSign(i).getProperty(PROPERTY_NAME_PKCS1_SIGN);
			if (pkcs1Base64 == null) {
				throw new IllegalArgumentException("La propiedades adicionales no contienen la firma PKCS#1"); //$NON-NLS-1$
			}

			xmlBase = xmlBase.replace(
				XAdESTriPhaseSignerServerSide.REPLACEMENT_STRING.replace(
					XAdESTriPhaseSignerServerSide.REPLACEMENT_CODE, Integer.toString(i)
				),
				pkcs1Base64.trim()
			);
		}

		// El XML resultante carece de las secciones de datos y KeyInfo, por lo que hay
		// que recrearlas y volverlas a introducir

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
				"Error recreando los datos a firmar y la cadena de certificados: " + e //$NON-NLS-1$
			);
		}

		byte[] counterSignature;
		try {
			counterSignature = XAdESTriPhaseSignerUtil.insertCommonParts(
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
		return counterSignature;
	}

}

/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.xades.asic;

import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.Certificate;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AOSignInfo;
import es.gob.afirma.core.signers.AOSigner;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.asic.ASiCUtil;
import es.gob.afirma.core.util.tree.AOTreeModel;
import es.gob.afirma.signers.xades.AOXAdESSigner;
import es.gob.afirma.signers.xades.XAdESConstants;
import es.gob.afirma.signers.xades.XAdESExtraParams;
import es.gob.afirma.signers.xml.XmlDSigProviderHelper;

/** Manejador de firmas XML XAdES ASiC-S.
 * Todas las firmas XAdES ASiC-S se generan de forma <i>externally detached</i> con MANIFEST para evitar problemas de resoluci&oacute;n en
 * las referencias, y todas contienen el nodo <code>KeyName</code> de la secci&oacute;n <code>KeyInfo</code>.
 * <p>
 *  Debido a errores en algunas versiones del entorno de ejecuci&oacute;n de Java, esta clase puede generar ocasionalmente
 *  mensajes en consola del tipo: <code>[Fatal Error] :1:1: Content is not allowed in prolog.</code>. Estos
 *  deben ignorarse, ya que no indican ninguna condici&oacute;n de error ni malfuncionamiento.
 * </p>
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class AOXAdESASiCSSigner implements AOSigner {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    // Instalamos el proveedor de Apache. Esto es necesario para evitar problemas con los saltos de linea
    // de los Base 64
    static {
    	XmlDSigProviderHelper.configureXmlDSigProvider();
    }

	@Override
	public byte[] sign(final byte[] data,
					   final String algorithm,
					   final PrivateKey key,
					   final Certificate[] certChain,
					   final Properties xParams) throws AOException,
					                                    IOException {

		final Properties extraParams = setASiCProperties(xParams, data);
		extraParams.put("keepKeyInfoUnsigned", Boolean.TRUE.toString()); //$NON-NLS-1$

		// Las firmas ASiC siempre son externally detached, lo que obliga a que al firmador XAdES le pasemos
		// el hash de los datos en lugar de los propios datos
		extraParams.put(XAdESExtraParams.FORMAT, AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED);

		final String digestMethodAlgorithm = extraParams.getProperty(
		        XAdESExtraParams.REFERENCES_DIGEST_METHOD, XAdESConstants.DEFAULT_DIGEST_METHOD);
		final String externalReferencesHashAlgorithm = extraParams.getProperty(
		        XAdESExtraParams.PRECALCULATED_HASH_ALGORITHM, digestMethodAlgorithm);

		byte[] digestValue;
		try {
			digestValue = hash(data, externalReferencesHashAlgorithm);
		}
		catch (final Exception e) {
			throw new AOException("No se reconoce el algoritmo de huella digital", e); //$NON-NLS-1$
		}

		final byte[] xadesSignature = new AOXAdESSigner().sign(
			digestValue,
			algorithm,
			key,
			certChain,
			extraParams
		);

		return ASiCUtil.createSContainer(
			xadesSignature,
			data,
			ASiCUtil.ENTRY_NAME_XML_SIGNATURE,
			extraParams.getProperty(XAdESASiCExtraParams.ASICS_FILENAME)
		);

	}

	/**
	 * Calcula la huella digital de los datos proporcionados.
	 * @param data Datos del que calcular el hash.
	 * @param externalReferencesHashAlgorithm URL de definici&oacute;n del hash.
	 * @return Huella digital de los datos.
	 * @throws NoSuchAlgorithmException Cuando no se reconozca el algoritmo de hash.
	 */
	public static byte[] hash(final byte[] data, final String externalReferencesHashAlgorithm) throws NoSuchAlgorithmException {
		final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(externalReferencesHashAlgorithm);
		final MessageDigest md = MessageDigest.getInstance(digestAlgorithm);
		return md.digest(data);
	}



	@Override
	public byte[] cosign(final byte[] data,
			             final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties xParams) throws AOException,
			                                                  IOException {

		// Extraemos firma y datos del ASiC
		final byte[] packagedSign = ASiCUtil.getASiCSXMLSignature(sign);
		final Map<String, byte[]> signedData = ASiCUtil.getASiCSData(sign);
		final String signedDataName = signedData.keySet().iterator().next();
		final byte[] packagedData = signedData.get(signedDataName);

		final Properties extraParams = setASiCProperties(xParams, packagedData);
		extraParams.put("keepKeyInfoUnsigned", Boolean.TRUE.toString()); //$NON-NLS-1$


		final Map<String, byte[]> externalReferences = new HashMap<>();

		externalReferences.put(signedDataName, packagedData);

		final AOXAdESSigner signer = new AOXAdESSigner();
		signer.setUriDereferencer(new UriAndDataDereferencer(externalReferences ));

		// Creamos la contrafirma
		final byte[] newCoSign = signer.cosign(
			packagedData,
			packagedSign,
			algorithm,
			key,
			certChain,
			extraParams
		);

		return ASiCUtil.createSContainer(
			newCoSign,
			packagedData,
			ASiCUtil.ENTRY_NAME_XML_SIGNATURE,
			signedDataName
		);
	}

	@Override
	public byte[] cosign(final byte[] sign,
			             final String algorithm,
			             final PrivateKey key,
			             final Certificate[] certChain,
			             final Properties xParams) throws AOException,
			                                                  IOException {

		return cosign(null, sign, algorithm, key, certChain, xParams);
	}

	@Override
	public byte[] countersign(final byte[] sign,
			                  final String algorithm,
			                  final CounterSignTarget targetType,
			                  final Object[] targets,
			                  final PrivateKey key,
			                  final Certificate[] certChain,
			                  final Properties xParams) throws AOException,
			                                                   IOException {
		// Extraemos firma y datos del ASiC
		final byte[] packagedSign = ASiCUtil.getASiCSXMLSignature(sign);
		final Map<String, byte[]> signedData = ASiCUtil.getASiCSData(sign);
		final String signedDataName = signedData.keySet().iterator().next();
		final byte[] packagedData = signedData.get(signedDataName);

		final Properties extraParams = setASiCProperties(xParams, packagedData);
		extraParams.put("keepKeyInfoUnsigned", Boolean.TRUE.toString()); //$NON-NLS-1$

		// Creamos la contrafirma
		final byte[] newCounterSign = new AOXAdESSigner().countersign(
			packagedSign,
			algorithm,
			targetType,
			targets,
			key,
			certChain,
			extraParams
		);

		return ASiCUtil.createSContainer(
			newCounterSign,
			packagedData,
			ASiCUtil.ENTRY_NAME_XML_SIGNATURE,
			signedDataName
		);

	}

	@Override
	public AOTreeModel getSignersStructure(final byte[] sign, final boolean asSimpleSignInfo)
			throws AOInvalidFormatException, IOException {
		return getSignersStructure(sign, null, asSimpleSignInfo);
	}

	@Override
	public AOTreeModel getSignersStructure(final byte[] sign, final Properties params, final boolean asSimpleSignInfo)
			throws AOInvalidFormatException, IOException {
		return new AOXAdESSigner().getSignersStructure(
			ASiCUtil.getASiCSXMLSignature(sign),
			asSimpleSignInfo
		);
	}

	@Override
	public boolean isSign(final byte[] is) throws IOException{
		return isSign(is, null);
	}

	@Override
	public boolean isSign(final byte[] is, final Properties params) throws IOException {
		final byte[] sign;
		try {
			sign = ASiCUtil.getASiCSXMLSignature(is);
		}
		catch(final Exception e) {
			return false;
		}
		return new AOXAdESSigner().isSign(sign);
	}

	@Override
	public boolean isValidDataFile(final byte[] data) {
		if (data == null) {
			LOGGER.warning("Se han introducido datos nulos para su comprobacion"); //$NON-NLS-1$
            return false;
        }
        return true;
	}

	@Override
	public String getSignedName(final String originalName, final String inText) {
		return originalName + (inText != null ? inText : "") + ".asics"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	@Override
	public byte[] getData(final byte[] sign) throws AOInvalidFormatException, IOException, AOException {
		return getData(sign, null);
	}

	@Override
	public byte[] getData(final byte[] sign, final Properties params) throws IOException {
		// Devolveremos solo los primeros datos que encontremos
		final Map<String, byte[]> signedData = ASiCUtil.getASiCSData(sign);
		final String signedDataName = signedData.keySet().iterator().next();
		return signedData.get(signedDataName);
	}

	@Override
	public AOSignInfo getSignInfo(final byte[] data) throws AOException, IOException {
		return getSignInfo(data, null);
	}

	@Override
	public AOSignInfo getSignInfo(final byte[] data, final Properties params) throws AOException,
			                                                    IOException {
		return new AOXAdESSigner().getSignInfo(ASiCUtil.getASiCSXMLSignature(data));
	}

	/** Establece los par&aacute;metros necesarios (modificando los actuales incompatibles si fuese necesario)
	 * para crear firmas XAdES-ASiC-S.
	 * @param xParams Par&aacute;metros adicionales actuales de la firma (puede ser <code>null</code> si no
	 *                hay ninguno.
	 * @param data Datos que se van a firmar.
	 * @return Par&aacute;metros actuales con los adicionales necesarios a&ntilde;adidos. */
	public static Properties setASiCProperties(final Properties xParams, final byte[] data) {

		final Properties extraParams = xParams != null ? xParams : new Properties();

		final String dataFilename = extraParams.getProperty(XAdESASiCExtraParams.ASICS_FILENAME) != null ?
				extraParams.getProperty(XAdESASiCExtraParams.ASICS_FILENAME) :
					ASiCUtil.getASiCSDefaultDataFilename(data);

		// Aprovechamos para anadir atributos utiles
		extraParams.put(XAdESExtraParams.ADD_KEY_INFO_KEY_NAME, Boolean.TRUE.toString());

		// La URI de referencia es el nombre de fichero dentro del ASiC
		extraParams.put(XAdESExtraParams.URI, dataFilename);

		// Raiz de ASiC
		extraParams.put(XAdESExtraParams.ROOT_XML_NODE_NAME, "asic:XAdESSignatures"); //$NON-NLS-1$
		extraParams.put(XAdESExtraParams.ROOT_XML_NODE_NAMESPACE, "http://uri.etsi.org/02918/v1.2.1#"); //$NON-NLS-1$
		extraParams.put(XAdESExtraParams.ROOT_XML_NODE_NAMESPACE_PREFIX, "xmlns:asic"); //$NON-NLS-1$

		return extraParams;

	}

}

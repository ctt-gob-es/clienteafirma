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
import java.security.cert.X509Certificate;
import java.util.Properties;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.CounterSignTarget;
import es.gob.afirma.core.signers.TriphaseData;
import es.gob.afirma.core.signers.asic.ASiCUtil;
import es.gob.afirma.signers.xades.XAdESConstants;
import es.gob.afirma.signers.xades.XAdESExtraParams;
import es.gob.afirma.signers.xades.asic.AOXAdESASiCSSigner;

/** Procesador de firmas trif&aacute;sicas XAdES-ASiC-S.
 * @author Tom&aacute;s Garc&iacute;a Mer&aacute;s. */
public final class XAdESASiCSTriPhasePreProcessor extends XAdESTriPhasePreProcessor {

	/** Manejador de firma XAdES-ASiC trif&aacute;sico. */
	public XAdESASiCSTriPhasePreProcessor() {
		super();
	}

	@Override
	public TriphaseData preProcessPreSign(final byte[] data,
			                        final String algorithm,
			                        final X509Certificate[] cert,
			                        final Properties extraParams,
			                        final boolean checkSignatures) throws IOException,
			                                                             AOException {

		final Properties xParams = AOXAdESASiCSSigner.setASiCProperties(extraParams, data);
		xParams.put("keepKeyInfoUnsigned", Boolean.TRUE.toString()); //$NON-NLS-1$

		// Las firmas ASiC siempre son externally detached, lo que obliga a que al firmador XAdES le pasemos
		// el hash de los datos en lugar de los propios datos
		xParams.put(XAdESExtraParams.FORMAT, AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED);

		final String digestMethodAlgorithm = xParams.getProperty(
		        XAdESExtraParams.REFERENCES_DIGEST_METHOD, XAdESConstants.DEFAULT_DIGEST_METHOD);
		final String externalReferencesHashAlgorithm = xParams.getProperty(
		        XAdESExtraParams.PRECALCULATED_HASH_ALGORITHM, digestMethodAlgorithm);

		byte[] digestValue;
		try {
			digestValue = AOXAdESASiCSSigner.hash(data, externalReferencesHashAlgorithm);
		}
		catch (final Exception e) {
			throw new AOException("No se reconoce el algoritmo de huella digital", e); //$NON-NLS-1$
		}

		return super.preProcessPreSign(
			digestValue,
			algorithm,
			cert,
			xParams,
			checkSignatures
		);
	}

	@Override
	public byte[] preProcessPostSign(final byte[] data,
			                         final String algorithm,
			                         final X509Certificate[] cert,
			                         final Properties extraParams,
			                         final byte[] triphaseDataBytes) throws NoSuchAlgorithmException,
			                                                      AOException,
			                                                      IOException {

		return preProcessPostSign(data, algorithm, cert, extraParams, TriphaseData.parser(triphaseDataBytes));
	}

	@Override
	public byte[] preProcessPostSign(final byte[] data,
			                         final String algorithm,
			                         final X509Certificate[] cert,
			                         final Properties extraParams,
			                         final TriphaseData triphaseData) throws NoSuchAlgorithmException,
			                                                      AOException,
			                                                      IOException {

		final Properties xParams = AOXAdESASiCSSigner.setASiCProperties(extraParams, data);
		xParams.put("keepKeyInfoUnsigned", Boolean.TRUE.toString()); //$NON-NLS-1$

		// Las firmas ASiC siempre son externally detached, lo que obliga a que al firmador XAdES le pasemos
		// el hash de los datos en lugar de los propios datos
		xParams.put(XAdESExtraParams.FORMAT, AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED);

		final byte[] xadesSignature = super.preProcessPostSign(
			data,
			algorithm,
			cert,
			xParams,
			triphaseData
		);
		return ASiCUtil.createSContainer(
			xadesSignature,
			data,
			ASiCUtil.ENTRY_NAME_XML_SIGNATURE,
			extraParams.getProperty("asicsFilename") //$NON-NLS-1$
		);
	}

	@Override
	public TriphaseData preProcessPreCoSign(final byte[] data,
			                          final String algorithm,
			                          final X509Certificate[] cert,
			                          final Properties extraParams,
			                          final boolean checkSignatures) throws IOException,
			                                                               AOException {

		throw new UnsupportedOperationException("No se soporta la multifirma de firmas XAdES-ASiC-S"); //$NON-NLS-1$
		//TODO: Descomentar cuando se de soporte completo a las multifirmas ASiC
//		final Map<String, byte[]> signedData = ASiCUtil.getASiCSData(data);
//		final String signedDataName = signedData.keySet().iterator().next();
//		final byte[] packagedData = signedData.get(signedDataName);
//
//
//		final Properties xParams = AOXAdESASiCSSigner.setASiCProperties(extraParams, packagedData);
//		xParams.put("keepKeyInfoUnsigned", Boolean.TRUE.toString()); //$NON-NLS-1$
//
//		xParams.put("asicsFilename", signedDataName); //$NON-NLS-1$
//
//		// Las firmas ASiC siempre son externally detached, lo que obliga a que al firmador XAdES le pasemos
//		// el hash de los datos en lugar de los propios datos
//		xParams.put(XAdESExtraParams.FORMAT, AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED);
//
//		final String digestMethodAlgorithm = xParams.getProperty(
//		        XAdESExtraParams.REFERENCES_DIGEST_METHOD, XAdESConstants.DEFAULT_DIGEST_METHOD);
//		final String externalReferencesHashAlgorithm = xParams.getProperty(
//		        XAdESExtraParams.PRECALCULATED_HASH_ALGORITHM, digestMethodAlgorithm);
//
//		byte[] digestValue;
//		try {
//			digestValue = AOXAdESASiCSSigner.hash(packagedData, externalReferencesHashAlgorithm);
//		}
//		catch (final Exception e) {
//			throw new AOException("No se reconoce el algoritmo de huella digital", e); //$NON-NLS-1$
//		}
//
//		return super.preProcessPreSign(
//			digestValue,
//			algorithm,
//			cert,
//			xParams,
//			checkSignatures
//		);
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] data,
			                           final String algorithm,
			                           final X509Certificate[] cert,
			                           final Properties extraParams,
			                           final byte[] triphaseDataBytes) throws NoSuchAlgorithmException,
			                                                        AOException,
			                                                        IOException {

		throw new UnsupportedOperationException("No se soporta la multifirma de firmas XAdES-ASiC-S"); //$NON-NLS-1$
		//TODO: Descomentar cuando se de soporte completo a las multifirmas ASiC
		//return preProcessPostCoSign(data, algorithm, cert, extraParams, TriphaseData.parser(triphaseDataBytes));
	}

	@Override
	public byte[] preProcessPostCoSign(final byte[] data,
			                           final String algorithm,
			                           final X509Certificate[] cert,
			                           final Properties extraParams,
			                           final TriphaseData triphaseData) throws NoSuchAlgorithmException,
			                                                        AOException,
			                                                        IOException {

		throw new UnsupportedOperationException("No se soporta la multifirma de firmas XAdES-ASiC-S"); //$NON-NLS-1$
		//TODO: Descomentar cuando se de soporte completo a las multifirmas ASiC
//		final Map<String, byte[]> signedData = ASiCUtil.getASiCSData(data);
//		final String signedDataName = signedData.keySet().iterator().next();
//		final byte[] packagedData = signedData.get(signedDataName);
//
//		final Properties xParams = AOXAdESASiCSSigner.setASiCProperties(extraParams, data);
//		xParams.put("keepKeyInfoUnsigned", Boolean.TRUE.toString()); //$NON-NLS-1$
//
//		// Las firmas ASiC siempre son externally detached, lo que obliga a que al firmador XAdES le pasemos
//		// el hash de los datos en lugar de los propios datos
//		xParams.put(XAdESExtraParams.FORMAT, AOSignConstants.SIGN_FORMAT_XADES_EXTERNALLY_DETACHED);
//
//		// Obtenemos la firma XAdES-ASiC original
//		final byte[] signature = ASiCUtil.getASiCSXMLSignature(data);
//
//		// Generamos una nueva firma
//		final byte[] coSignature = super.preProcessPostSign(
//			packagedData,
//			algorithm,
//			cert,
//			xParams,
//			triphaseData
//		);
//
//		// Las unimos dentro del mismo XML y las incluimos en el paquete ASiC
//		final byte[] mergedSignatures = mergeSignatures(signature, coSignature);
//
//		return ASiCUtil.createSContainer(
//			mergedSignatures,
//			packagedData,
//			ASiCUtil.ENTRY_NAME_XML_SIGNATURE,
//			signedDataName
//		);
	}


//	/**
//	 * Incorpora la cofirma al objeto de firma original.
//	 * @param signature
//	 * @param coSignature
//	 * @return
//	 */
//	private static byte[] mergeSignatures(final byte[] signature, final byte[] coSignature) throws AOException {
//
//		Document signDocument;
//    	try {
//    		signDocument = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(signature));
//    	}
//    	catch (final Exception e) {
//    		throw new AOException("No se ha podido cargar el documento XML de firmas", e); //$NON-NLS-1$
//    	}
//
//    	// Identificamos alguna de las firmas
//    	final Element signatureElement = XAdESUtil.getFirstSignatureElement(signDocument.getDocumentElement());
//    	if (signature == null) {
//    		throw new AOException("No se han encontrado firmas en el documento"); //$NON-NLS-1$
//    	}
//
//    	// Identificamos de que nodo cuelgan las firmas
//    	final Node signaturesNode = signatureElement.getParentNode();
//    	if (signaturesNode == null) {
//    		throw new AOException("No se encontro el nodo padre del nodo de firma"); //$NON-NLS-1$
//    	}
//
//    	// Cargamos la cofirma
//    	Document cosignDocument;
//    	try {
//    		cosignDocument = Utils.getNewDocumentBuilder().parse(new ByteArrayInputStream(signature));
//    	}
//    	catch (final Exception e) {
//    		throw new AOException("No se ha podido cargar la firma generada para hacer de cofirma", e); //$NON-NLS-1$
//    	}
//
//    	// Agregamos la cofirma como una firma mas
//    	signaturesNode.appendChild(cosignDocument);
//
//		return Utils.writeXML(signDocument, null, null, null);
//	}

	@Override
	public TriphaseData preProcessPreCounterSign(final byte[] sign,
			                               final String algorithm,
			                               final X509Certificate[] cert,
			                               final Properties extraParams,
			                               final CounterSignTarget targets,
				                           final boolean checkSignatures) throws IOException,
			                                                                       AOException {
		throw new UnsupportedOperationException("No se soporta la multifirma de firmas XAdES-ASiC-S"); //$NON-NLS-1$
	}

	@Override
	public byte[] preProcessPostCounterSign(final byte[] sign,
			                                final String algorithm,
			                                final X509Certificate[] cert,
			                                final Properties extraParams,
			                                final byte[] triphaseDataBytes,
			                                final CounterSignTarget targets) throws NoSuchAlgorithmException,
			                                                                        AOException,
			                                                                        IOException {
		throw new UnsupportedOperationException("No se soporta la multifirma de firmas XAdES-ASiC-S"); //$NON-NLS-1$
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
		throw new UnsupportedOperationException("No se soporta la multifirma de firmas XAdES-ASiC-S"); //$NON-NLS-1$
	}

}

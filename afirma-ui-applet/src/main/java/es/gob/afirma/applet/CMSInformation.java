/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet;

import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Enumeration;
import java.util.logging.Logger;

import org.spongycastle.asn1.ASN1InputStream;
import org.spongycastle.asn1.ASN1Integer;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1Sequence;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.ASN1TaggedObject;
import org.spongycastle.asn1.DERUTCTime;
import org.spongycastle.asn1.cms.AuthEnvelopedData;
import org.spongycastle.asn1.cms.AuthenticatedData;
import org.spongycastle.asn1.cms.CMSAttributes;
import org.spongycastle.asn1.cms.CMSObjectIdentifiers;
import org.spongycastle.asn1.cms.CompressedData;
import org.spongycastle.asn1.cms.ContentInfo;
import org.spongycastle.asn1.cms.EncryptedContentInfo;
import org.spongycastle.asn1.cms.EnvelopedData;
import org.spongycastle.asn1.cms.IssuerAndSerialNumber;
import org.spongycastle.asn1.cms.KeyTransRecipientInfo;
import org.spongycastle.asn1.cms.RecipientInfo;
import org.spongycastle.asn1.cms.SignedData;
import org.spongycastle.asn1.cms.SignerIdentifier;
import org.spongycastle.asn1.cms.SignerInfo;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;

import es.gob.afirma.core.AOInvalidFormatException;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.signers.pkcs7.DigestedData;
import es.gob.afirma.signers.pkcs7.SignedAndEnvelopedData;

/**
 * Clase que obtiene la informaci&oacute;n de los distintos tipos de firma para CMS a partir de un fichero
 * pasado por par&aacute;metro.
 *
 * La informaci&oacute;n es para los tipo:
 *
 * <ul>
 * <li>Data</li>
 * <li>Signed Data</li>
 * <li>Digested Data</li>
 * <li>Encrypted Data</li>
 * <li>Enveloped Data</li>
 * <li>Signed and Enveloped Data</li>
 * <li>Authenticated Data</li>
 * <li>Authenticated and Enveloped Data</li>
 * </ul>
 */
final class CMSInformation {

	private CMSInformation() {
		// No permitimos la instanciacion
	}

	private static final int TYPE_ENVELOPED_DATA = 0;

	private static final int TYPE_AUTHENTICATED_DATA = 1;

	private static final int TYPE_AUTHENTICATED_ENVELOPED_DATA = 2;

	private static final int TYPE_SIGNED_ENVELOPED_DATA = 3;

	private static final int TYPE_SIGNED_DATA = 4;

	private static final int TYPE_ENCRYPTED_DATA = 5;

	private static final String DATA = "Data"; //$NON-NLS-1$

	private static final String DIGESTED_DATA = "DigestedData"; //$NON-NLS-1$

	private static final String COMPRESSED_DATA = "CompressedData"; //$NON-NLS-1$

	private static final String ENVELOPED_DATA = "EnvelopedData"; //$NON-NLS-1$

	private static final String AUTHENTICATED_DATA = "AuthenticatedData"; //$NON-NLS-1$

	private static final String AUTH_ENVELOPED_DATA = "AuthenticatedEnvelopedData"; //$NON-NLS-1$

	private static final String SIGNED_ENVELOPED_DATA = "SignedAndEnvelopedData"; //$NON-NLS-1$

	private static final String SIGNED_DATA = "SignedData"; //$NON-NLS-1$

	private static final String ENCRYPTED_DATA = "EncryptedData"; //$NON-NLS-1$

	private static final int BINARY_SIGN_CMS = 0;

	private static final int BINARY_SIGN_CADES = 1;

	private static final String SP = " "; //$NON-NLS-1$

	private static final String CR = "\n"; //$NON-NLS-1$

	private static final String TB = "\t"; //$NON-NLS-1$

	/**
	 * M&eacute;todo principal que obtiene la informaci&oacute;n a partir de un fichero firmado
	 * de tipo CMS.
	 * @param data Objeto CMS.
	 * @return Texto descriptivo del objeto CMS.
	 * @throws IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
	 * @throws AOInvalidFormatException Error de formato no v&aacute;lido.
	 */
	static String getInformation(final byte[] data) throws IOException, AOInvalidFormatException {

		final ASN1InputStream is = new ASN1InputStream(data);

		// LEEMOS EL FICHERO QUE NOS INTRODUCEN
		final ASN1Sequence dsq = (ASN1Sequence)is.readObject();
		is.close();

		final Enumeration<?> e = dsq.getObjects();

		// Elementos que contienen los elementos OID Data
		final ASN1ObjectIdentifier doi = (ASN1ObjectIdentifier) e.nextElement();

		// Contenido a obtener informacion
		final ASN1TaggedObject doj =(ASN1TaggedObject) e.nextElement();

		final String datos;
		if (doi.equals(PKCSObjectIdentifiers.data)){
			datos = AppletMessages.getString("CMSInformation.0") + SP + DATA + CR; //$NON-NLS-1$
		}
		else if(doi.equals(PKCSObjectIdentifiers.digestedData)){
			datos = getFromDigestedData(doj);
		}
		else if(doi.equals(PKCSObjectIdentifiers.encryptedData)){

			datos = extractData(doj, TYPE_ENCRYPTED_DATA, AppletMessages.getString("CMSInformation.0") + SP + ENCRYPTED_DATA, BINARY_SIGN_CMS); //$NON-NLS-1$
		}
		else if(doi.equals(PKCSObjectIdentifiers.signedData)){
			datos = extractData(doj, TYPE_SIGNED_DATA, AppletMessages.getString("CMSInformation.0") + SP + SIGNED_DATA, BINARY_SIGN_CMS); //$NON-NLS-1$
		}
		else if(doi.equals(PKCSObjectIdentifiers.envelopedData)){
			datos = extractData(doj, TYPE_ENVELOPED_DATA, AppletMessages.getString("CMSInformation.0") + SP + ENVELOPED_DATA, BINARY_SIGN_CMS); //$NON-NLS-1$
		}
		else if(doi.equals(PKCSObjectIdentifiers.signedAndEnvelopedData)){
			datos = extractData(doj, TYPE_SIGNED_ENVELOPED_DATA, AppletMessages.getString("CMSInformation.0") + SP + SIGNED_ENVELOPED_DATA, BINARY_SIGN_CMS); //$NON-NLS-1$
		}
		else if(doi.equals(PKCSObjectIdentifiers.id_ct_authData)){
			datos = extractData(doj, TYPE_AUTHENTICATED_DATA, AppletMessages.getString("CMSInformation.0") + SP + AUTHENTICATED_DATA, BINARY_SIGN_CMS); //$NON-NLS-1$
		}
		else if(doi.equals(PKCSObjectIdentifiers.id_ct_authEnvelopedData)){
			datos = extractData(doj, TYPE_AUTHENTICATED_ENVELOPED_DATA, AppletMessages.getString("CMSInformation.0") + SP + AUTH_ENVELOPED_DATA, BINARY_SIGN_CMS); //$NON-NLS-1$
		}
		else if(doi.equals(CMSObjectIdentifiers.compressedData)){
			datos = getFromCompressedData(doj);
		}
		else {
			throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un tipo de objeto CMS soportado"); //$NON-NLS-1$
		}

		return datos;
	}

	/** Obtiene la informaci&oacute;n de un tipo DigestedData.
	 * @param doj <i>DigestedData</i> de PKCS#7 ASN.1.
	 * @return  Representaci&oacute;n de los datos. */
	private static String getFromDigestedData(final ASN1TaggedObject doj) {
		String detalle = ""; //$NON-NLS-1$
		detalle = detalle + AppletMessages.getString("CMSInformation.0") + SP + DIGESTED_DATA + CR; //$NON-NLS-1$

		//obtenemos el digestedData
		final DigestedData dd = new DigestedData((ASN1Sequence)doj.getObject());

		//obtenemos la version
		detalle = detalle + AppletMessages.getString("CMSInformation.1") + SP + dd.getVersion() + CR; //$NON-NLS-1$

		//obtenemos el algoritmo
		detalle = detalle + AppletMessages.getString("CMSInformation.9") + SP + dd.getDigestAlgorithm() + CR; //$NON-NLS-1$

		//obtenemos el tipo de contenido
		detalle = detalle + AppletMessages.getString("CMSInformation.10") + SP + dd.getContentType() + CR; //$NON-NLS-1$

		return detalle;
	}

	/** Obtiene la informaci&oacute;n de un tipo Compressed Data.
	 * @param doj <i>CompressedData</i> de PKCS#7 ASN.1.
	 * @return  Representaci&oacute;n de los datos. */
	private static String getFromCompressedData(final ASN1TaggedObject doj) {
		String detalle = ""; //$NON-NLS-1$
		detalle = detalle + "Tipo:" + SP + COMPRESSED_DATA + CR; //$NON-NLS-1$
		final CompressedData ed = CompressedData.getInstance(doj.getObject());

		//obtenemos la version
		detalle = detalle + AppletMessages.getString("CMSInformation.1") + SP + ed.getVersion() + CR; //$NON-NLS-1$

		final AlgorithmIdentifier aid = ed.getCompressionAlgorithmIdentifier();
		if (aid.getAlgorithm().toString().equals("1.2.840.113549.1.9.16.3.8")){ //$NON-NLS-1$
			detalle = detalle + "OID del Algoritmo de firma: ZLIB" + CR; //$NON-NLS-1$
		}
		else{
			detalle = detalle + "OID del Algoritmo de firma:" + SP + aid.getAlgorithm() + CR; //$NON-NLS-1$
		}

		return detalle;
	}

	/**
	 * Obtiene la informaci&oacute;n de diferentes tipos de formatos.
	 * @param doj Etiqueta ASN.1 de la que se obtienen los datos.
	 * @param envelopeType	Tipo de formato:
	 * <ul>
	 *  <li>0: EnvelopedData</li>
	 *  <li>1: AuthenticatedData</li>
	 *  <li>2: AuthEnvelopedData</li>
	 *  <li>3: SignedAndEnvelopedData</li>
	 *  <li>4: SignedData</li>
	 *  <li>5: Encrypted</li>
	 * </ul>
	 * @param tipoDetalle	Tipo de datos (literal)
	 * @param signBinaryType Tipo de firmado binario (CADES o CMS)
	 * @return  Representaci&oacute;n de los datos.
	 */
	private static String extractData(final ASN1TaggedObject doj, final int envelopeType,
			final String tipoDetalle, final int signBinaryType) {
		String detalle = ""; //$NON-NLS-1$
		detalle = detalle + tipoDetalle + CR;

		ASN1Set rins = null;
		EncryptedContentInfo encryptedContentInfo = null;
		ASN1Set unprotectedAttrs = null;
		ASN1Integer version = null;
		AlgorithmIdentifier aid = null;
		ContentInfo ci = null;
		ASN1Set authAttrs = null;
		ASN1Set ds = null;
		ASN1Set signerInfosSd = null;

		switch (envelopeType) {
		case TYPE_ENVELOPED_DATA:
			final EnvelopedData enveloped = EnvelopedData.getInstance(doj.getObject());
			version = enveloped.getVersion();
			rins = enveloped.getRecipientInfos();
			encryptedContentInfo = enveloped.getEncryptedContentInfo();
			unprotectedAttrs = enveloped.getUnprotectedAttrs();
			break;
		case TYPE_AUTHENTICATED_DATA:
			final AuthenticatedData authenticated = AuthenticatedData.getInstance(doj.getObject());
			version = authenticated.getVersion();
			rins = authenticated.getRecipientInfos();
			aid = authenticated.getMacAlgorithm();
			ci = authenticated.getEncapsulatedContentInfo();
			authAttrs = authenticated.getAuthAttrs();
			unprotectedAttrs = authenticated.getUnauthAttrs();
			break;
		case TYPE_AUTHENTICATED_ENVELOPED_DATA:
			final AuthEnvelopedData authEnveloped = AuthEnvelopedData.getInstance(doj.getObject());
			version = authEnveloped.getVersion();
			rins = authEnveloped.getRecipientInfos();
			encryptedContentInfo = authEnveloped.getAuthEncryptedContentInfo();
			authAttrs = authEnveloped.getAuthAttrs();
			unprotectedAttrs = authEnveloped.getUnauthAttrs();
			break;
		case TYPE_SIGNED_ENVELOPED_DATA:
			final SignedAndEnvelopedData signedEnv = new SignedAndEnvelopedData((ASN1Sequence)doj.getObject());
			version = signedEnv.getVersion();
			rins = signedEnv.getRecipientInfos();
			encryptedContentInfo = signedEnv.getEncryptedContentInfo();
			signerInfosSd = signedEnv.getSignerInfos();
			break;
		case TYPE_SIGNED_DATA:
			final SignedData signed = SignedData.getInstance(doj.getObject());
			version = signed.getVersion();
			ds = signed.getDigestAlgorithms();
			ci = signed.getEncapContentInfo();
			signerInfosSd = signed.getSignerInfos();
			break;
		case TYPE_ENCRYPTED_DATA:
			final ASN1Sequence encrypted = (ASN1Sequence) doj.getObject();
			version = ASN1Integer.getInstance(encrypted.getObjectAt(0));
			encryptedContentInfo = EncryptedContentInfo.getInstance(encrypted.getObjectAt(1));
			if (encrypted.size() == 3) {
				unprotectedAttrs = (ASN1Set) encrypted.getObjectAt(2);
			}
			break;
		default:
			throw new IllegalArgumentException("Tipo de sobre no soportado: " + envelopeType); //$NON-NLS-1$
		}

		//obtenemos la version
		detalle = detalle + AppletMessages.getString("CMSInformation.1") + SP + version + CR; //$NON-NLS-1$

		//recipientInfo
		if (rins != null) {
			if (envelopeType != TYPE_SIGNED_DATA && envelopeType != TYPE_ENCRYPTED_DATA && rins.size() > 0) {
				detalle = detalle + AppletMessages.getString("CMSInformation.13") + CR; //$NON-NLS-1$
			}
			for (int i=0; i<rins.size(); i++){
				final KeyTransRecipientInfo kti = KeyTransRecipientInfo.getInstance(RecipientInfo.getInstance(rins.getObjectAt(i)).getInfo());
				detalle = detalle + AppletMessages.getString("CMSInformation.14") + SP + (i+1) + ":" + CR;  //$NON-NLS-1$//$NON-NLS-2$
				final AlgorithmIdentifier diAlg= kti.getKeyEncryptionAlgorithm();

				//issuer y serial
				final IssuerAndSerialNumber iss =
					(IssuerAndSerialNumber) SignerIdentifier.getInstance(kti.getRecipientIdentifier().getId()).getId();
				detalle = detalle + TB + AppletMessages.getString("CMSInformation.15") + SP + iss.getName().toString() + CR; //$NON-NLS-1$
				detalle = detalle + TB + AppletMessages.getString("CMSInformation.16") + SP + iss.getSerialNumber() + CR; //$NON-NLS-1$

				// el algoritmo de cifrado de los datos
				AOCipherAlgorithm algorithm = null;
				final AOCipherAlgorithm[] algos = AOCipherAlgorithm.values();

				// obtenemos el algoritmo usado para cifrar la pass
				for (final AOCipherAlgorithm algo : algos) {
					if (algo.getOid().equals(diAlg.getAlgorithm().toString())){
						algorithm = algo;
					}
				}
				if (algorithm != null){
					detalle = detalle + TB + AppletMessages.getString("CMSInformation.17") + SP + algorithm.getName() + CR; //$NON-NLS-1$
				}
				else {
					detalle = detalle + TB + AppletMessages.getString("CMSInformation.18") + SP + diAlg.getAlgorithm() + CR; //$NON-NLS-1$
				}
			}
		}

		if (envelopeType == TYPE_ENVELOPED_DATA || envelopeType == TYPE_ENCRYPTED_DATA) {
			//obtenemos datos de los datos cifrados.
			detalle = detalle + AppletMessages.getString("CMSInformation.19") + CR; //$NON-NLS-1$
			detalle = detalle + getEncryptedContentInfo(encryptedContentInfo);
		}
		else if (envelopeType == TYPE_AUTHENTICATED_DATA && aid != null && ci != null){
			// mac algorithm
			detalle = detalle + AppletMessages.getString("CMSInformation.20") + SP + aid.getAlgorithm() + CR; //$NON-NLS-1$

			//digestAlgorithm
			final ASN1Sequence seq =(ASN1Sequence)doj.getObject();
			final ASN1TaggedObject da = (ASN1TaggedObject)seq.getObjectAt(4);
			final AlgorithmIdentifier dai = AlgorithmIdentifier.getInstance(da.getObject());
			detalle = detalle + AppletMessages.getString("CMSInformation.21") + SP + dai.getAlgorithm() + CR; //$NON-NLS-1$

			//obtenemos datos de los datos cifrados.
			detalle = detalle + AppletMessages.getString("CMSInformation.22") + SP + ci.getContentType() + CR; //$NON-NLS-1$

			detalle = getObligatorieAtrib(signBinaryType, detalle, authAttrs);
		}
		else if (envelopeType == TYPE_AUTHENTICATED_ENVELOPED_DATA) {
			detalle = detalle + AppletMessages.getString("CMSInformation.19") + CR; //$NON-NLS-1$
			detalle = detalle + getEncryptedContentInfo(encryptedContentInfo);

			detalle = getObligatorieAtrib(signBinaryType, detalle, authAttrs);
		}
		else if (envelopeType == TYPE_SIGNED_ENVELOPED_DATA) {
			//algoritmo de firma
			final ASN1Sequence seq =(ASN1Sequence)doj.getObject();
			final ASN1Set da = (ASN1Set)seq.getObjectAt(2);
			final AlgorithmIdentifier dai = AlgorithmIdentifier.getInstance(da.getObjectAt(0));
			detalle = detalle + AppletMessages.getString("CMSInformation.21") + SP + dai.getAlgorithm() + CR; //$NON-NLS-1$

			//obtenemos datos de los datos cifrados.
			detalle = detalle + AppletMessages.getString("CMSInformation.19") + CR; //$NON-NLS-1$
			detalle = detalle + getEncryptedContentInfo(encryptedContentInfo);
		}
		else if (envelopeType == TYPE_SIGNED_DATA && ci != null && ds != null) {
			//algoritmo de firma
			final AlgorithmIdentifier dai = AlgorithmIdentifier.getInstance(ds.getObjectAt(0));
			detalle = detalle + AppletMessages.getString("CMSInformation.21") + SP + dai.getAlgorithm() + CR; //$NON-NLS-1$
			detalle = detalle + AppletMessages.getString("CMSInformation.22") + SP + ci.getContentType() + CR; //$NON-NLS-1$
		}

		//obtenemos lo atributos opcionales
		if (envelopeType != TYPE_SIGNED_ENVELOPED_DATA) {
			if (unprotectedAttrs == null){
				detalle = detalle + AppletMessages.getString("CMSInformation.28") + CR; //$NON-NLS-1$
			}
			else {
				final String atributos = getUnSignedAttributes(unprotectedAttrs.getObjects());
				detalle = detalle + AppletMessages.getString("CMSInformation.29") + CR; //$NON-NLS-1$
				detalle = detalle + atributos;
			}
		}
		else if ((envelopeType == TYPE_SIGNED_ENVELOPED_DATA || envelopeType == TYPE_SIGNED_DATA) &&  signerInfosSd != null) {
			//obtenemos el(los) firmate(s)
			if (signerInfosSd.size()>0){
				detalle = detalle + AppletMessages.getString("CMSInformation.30") + CR; //$NON-NLS-1$
			}
			for(int i =0; i< signerInfosSd.size(); i++){
				final SignerInfo si = SignerInfo.getInstance(signerInfosSd.getObjectAt(i));

				detalle = detalle + AppletMessages.getString("CMSInformation.31") + SP + (i+1) + ":" + CR;  //$NON-NLS-1$//$NON-NLS-2$
				// version
				detalle = detalle + TB + AppletMessages.getString("CMSInformation.1") + SP + si.getVersion() + CR; //$NON-NLS-1$
				//signerIdentifier
				final SignerIdentifier sident = si.getSID();
				final IssuerAndSerialNumber iss = IssuerAndSerialNumber.getInstance(sident.getId());
				detalle = detalle + TB + AppletMessages.getString("CMSInformation.15") + SP + iss.getName().toString() + CR; //$NON-NLS-1$
				detalle = detalle + TB + AppletMessages.getString("CMSInformation.16") + SP + iss.getSerialNumber() + CR; //$NON-NLS-1$

				//digestAlgorithm
				final AlgorithmIdentifier algId = si.getDigestAlgorithm();
				detalle = detalle + TB + AppletMessages.getString("CMSInformation.35") + SP + algId.getAlgorithm() + CR; //$NON-NLS-1$

				//obtenemos lo atributos obligatorios
				final ASN1Set sa =si.getAuthenticatedAttributes();
				String satributes = ""; //$NON-NLS-1$
				if (sa != null){
					satributes = getsignedAttributes(sa, signBinaryType);
				}
				detalle = detalle + TB + AppletMessages.getString("CMSInformation.36") + CR; //$NON-NLS-1$
				detalle = detalle + satributes;
			}
		}
		return detalle;
	}

	private static String getObligatorieAtrib(final int signBinaryType,
			                                  final String detalle,
			                                  final ASN1Set authAttrs) {
		String det = detalle;

		//obtenemos lo atributos obligatorios
		if (authAttrs == null){
			det = det + AppletMessages.getString("CMSInformation.37") + CR; //$NON-NLS-1$
		}
		else{
			final String atributos = getsignedAttributes(authAttrs, signBinaryType);
			det = det + AppletMessages.getString("CMSInformation.38") + CR; //$NON-NLS-1$
			det = det + atributos;
		}
		return det;
	}

	/** Obtiene los atributos obligatorios de una firma.
	 * @param attributes Grupo de atributos opcionales
	 * @param binarySignType Identifica el tipo de firma binaria (CMS o CADES)
	 * @return Lista de atributos concatenados.
	 */
	private static String getsignedAttributes(final ASN1Set attributes, final int binarySignType){
		String attributos = ""; //$NON-NLS-1$

		final Enumeration<?> e = attributes.getObjects();

		while (e.hasMoreElements()){
			final ASN1Sequence a = (ASN1Sequence)e.nextElement();
			final ASN1ObjectIdentifier derIden = (ASN1ObjectIdentifier) a.getObjectAt(0);
			// tipo de contenido de la firma.
			if (derIden.equals(CMSAttributes.contentType)){
				attributos = attributos + TB + TB + AppletMessages.getString("CMSInformation.22") + SP + a.getObjectAt(1) + CR; //$NON-NLS-1$
			}
			//Message digest de  la firma
			if (derIden.equals(CMSAttributes.messageDigest)){
				attributos = attributos + TB + TB + AppletMessages.getString("CMSInformation.43") + CR; //$NON-NLS-1$
			}
			//la fecha de firma. obtenemos y casteamos a algo legible.
			if (derIden.equals(CMSAttributes.signingTime)){
				final ASN1Set time = (ASN1Set)a.getObjectAt(1);
				final DERUTCTime d = (DERUTCTime)time.getObjectAt(0);
				Date date = null;
				try {
					date = d.getDate();
				} catch (final ParseException ex) {
					Logger.getLogger("es.gob.afirma").warning("No es posible convertir la fecha"); //$NON-NLS-1$ //$NON-NLS-2$
				}
				final SimpleDateFormat formatter = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss"); //$NON-NLS-1$
				final String ds = formatter.format(date);

				attributos = attributos + TB + TB + AppletMessages.getString("CMSInformation.39") + SP + ds + CR; //$NON-NLS-1$
			}
			if (binarySignType == BINARY_SIGN_CADES) {
				//atributo signing certificate v2
				if (derIden.equals(PKCSObjectIdentifiers.id_aa_signingCertificateV2)){
					attributos = attributos + TB + TB + AppletMessages.getString("CMSInformation.40") + CR; //$NON-NLS-1$
				}
				//Politica de firma.
				if (derIden.equals(PKCSObjectIdentifiers.id_aa_ets_sigPolicyId)){
					attributos = attributos + TB + TB + AppletMessages.getString("CMSInformation.41") + CR; //$NON-NLS-1$
				}
			}
		}
		return attributos;
	}

	/** Obtiene los atributos opcionales de una firma cualquiera.
	 * En caso de ser EncryptedData, usar el otro metodo, ya que por construcci&oacute;n
	 * no es posible utilizar este.
	 * @param e Grupo de atributos opcionales.
	 * @return Lista de atributos concatenados. */
	private static String getUnSignedAttributes(final Enumeration<?> e){
		String attributos = ""; //$NON-NLS-1$

		while (e.hasMoreElements()){
			final ASN1Sequence a = (ASN1Sequence)e.nextElement();
			final ASN1ObjectIdentifier derIden = (ASN1ObjectIdentifier) a.getObjectAt(0);
			// tipo de contenido de la firma.
			if (derIden.equals(CMSAttributes.contentType)){
				attributos = attributos + TB + AppletMessages.getString("CMSInformation.22") + SP + a.getObjectAt(1) + CR; //$NON-NLS-1$
			}
			//Message digest de  la firma
			if (derIden.equals(CMSAttributes.messageDigest)){
				attributos = attributos + TB + AppletMessages.getString("CMSInformation.43") + CR; //$NON-NLS-1$
			}
			//la fecha de firma. obtenemos y casteamos a algo legible.
			if (derIden.equals(CMSAttributes.signingTime)){
				final ASN1Set time = (ASN1Set)a.getObjectAt(1);
				final DERUTCTime d = (DERUTCTime)time.getObjectAt(0);
				Date date = null;
				try {
					date = d.getDate();
				} catch (final ParseException ex) {
					Logger.getLogger("es.gob.afirma").warning("No es posible convertir la fecha"); //$NON-NLS-1$ //$NON-NLS-2$
				}
				final SimpleDateFormat formatter = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss"); //$NON-NLS-1$
				final String ds = formatter.format(date);

				attributos = attributos + TB + AppletMessages.getString("CMSInformation.39") + SP + ds + CR; //$NON-NLS-1$
			}
			//contrafirma de la firma.
			if (derIden.equals(CMSAttributes.counterSignature)){
				attributos = attributos + TB + AppletMessages.getString("CMSInformation.45") + CR; //$NON-NLS-1$
			}

		}
		return attributos;
	}

	/**
	 * Obtiene los datos de cifrado usados.
	 *
	 * @param datos     informacion de los datos cifrados sin formatear.
	 * @return          informacion de los datos cifrados.
	 */
	private static String getEncryptedContentInfo(final EncryptedContentInfo datos){
		String info = ""; //$NON-NLS-1$

		//especificamos el tipo de contenido
		if (datos.getContentType().equals(PKCSObjectIdentifiers.encryptedData)){
			info = info + TB + AppletMessages.getString("CMSInformation.0") + SP + ENCRYPTED_DATA + CR; //$NON-NLS-1$
		}
		else{
			info = info + TB + AppletMessages.getString("CMSInformation.0") + SP + datos.getContentType() + CR; //$NON-NLS-1$
		}

		// el algoritmo de cifrado de los datos
		final AlgorithmIdentifier ai =datos.getContentEncryptionAlgorithm();
		AOCipherAlgorithm algorithm = null;
		final AOCipherAlgorithm[] algos = AOCipherAlgorithm.values();

		// obtenemos el algoritmo usado para cifrar la pass
		for (final AOCipherAlgorithm algo : algos) {
			if (algo.getOid().equals(ai.getAlgorithm().toString())){
				algorithm = algo;
			}
		}

		if (algorithm != null){
			info = info + TB + AppletMessages.getString("CMSInformation.17") + SP + algorithm.getName() + CR; //$NON-NLS-1$
		}
		else {
			info = info + TB + AppletMessages.getString("CMSInformation.18") + SP + ai.getAlgorithm().toString() + CR; //$NON-NLS-1$
		}

		return info;
	}
}
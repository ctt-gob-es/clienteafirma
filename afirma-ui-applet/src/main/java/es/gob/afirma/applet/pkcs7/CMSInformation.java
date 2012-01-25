/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.applet.pkcs7;

import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Enumeration;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DERInteger;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DERUTCTime;
import org.bouncycastle.asn1.cms.AuthEnvelopedData;
import org.bouncycastle.asn1.cms.AuthenticatedData;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.CMSObjectIdentifiers;
import org.bouncycastle.asn1.cms.CompressedData;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.EnvelopedData;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.KeyTransRecipientInfo;
import org.bouncycastle.asn1.cms.RecipientInfo;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.cms.SignerIdentifier;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

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
public final class CMSInformation {

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
	public static String getInformation(final byte[] data) throws IOException, AOInvalidFormatException {
		String datos = ""; //$NON-NLS-1$

		final ASN1InputStream is = new ASN1InputStream(data);
		// LEEMOS EL FICHERO QUE NOS INTRODUCEN
		ASN1Sequence dsq = null;
		dsq=(ASN1Sequence)is.readObject();
		final Enumeration<?> e = dsq.getObjects();
		// Elementos que contienen los elementos OID Data
		final DERObjectIdentifier doi = (DERObjectIdentifier)e.nextElement();
		// Contenido a obtener informacion
		final ASN1TaggedObject doj =(ASN1TaggedObject) e.nextElement();
		if (doi.equals(PKCSObjectIdentifiers.data)){
			datos = "Tipo:" + SP + DATA + CR;
		}
		else if(doi.equals(PKCSObjectIdentifiers.digestedData)){
			datos = getFromDigestedData(doj);
		}
		else if(doi.equals(PKCSObjectIdentifiers.encryptedData)){

			datos = extractData(doj, TYPE_ENCRYPTED_DATA, "Tipo:" + SP + ENCRYPTED_DATA, BINARY_SIGN_CMS);
		}
		else if(doi.equals(PKCSObjectIdentifiers.signedData)){
			datos = extractData(doj, TYPE_SIGNED_DATA, "Tipo:" + SP + SIGNED_DATA, BINARY_SIGN_CMS);
		}
		else if(doi.equals(PKCSObjectIdentifiers.envelopedData)){
			datos = extractData(doj, TYPE_ENVELOPED_DATA, "Tipo:" + SP + ENVELOPED_DATA, BINARY_SIGN_CMS);
		}
		else if(doi.equals(PKCSObjectIdentifiers.signedAndEnvelopedData)){
			datos = extractData(doj, TYPE_SIGNED_ENVELOPED_DATA, "Tipo:" + SP + SIGNED_ENVELOPED_DATA, BINARY_SIGN_CMS);
		}
		else if(doi.equals(PKCSObjectIdentifiers.id_ct_authData)){
			datos = extractData(doj, TYPE_AUTHENTICATED_DATA, "Tipo:" + SP + AUTHENTICATED_DATA, BINARY_SIGN_CMS);
		}
		else if(doi.equals(PKCSObjectIdentifiers.id_ct_authEnvelopedData)){
			datos = extractData(doj, TYPE_AUTHENTICATED_ENVELOPED_DATA, "Tipo:" + SP + AUTH_ENVELOPED_DATA, BINARY_SIGN_CMS);
		}
		else if(doi.equals(CMSObjectIdentifiers.compressedData)){
			datos = getFromCompressedData(doj);
		}
		else {
			throw new AOInvalidFormatException("Los datos introducidos no se corresponden con un tipo de objeto CMS soportado"); //$NON-NLS-1$
		}

		return datos;
	}

	/**
	 * Obtiene la informaci&oacute;n de un tipo Digested Data.
	 * @return  Representaci&oacute;n de los datos.
	 */
	private static String getFromDigestedData(final ASN1TaggedObject doj) {
		String detalle = ""; //$NON-NLS-1$
		detalle = detalle + "Tipo:" + SP + DIGESTED_DATA + CR;

		//obtenemos el digestedData
		final DigestedData dd = new DigestedData((ASN1Sequence)doj.getObject());

		//obtenemos la version
		detalle = detalle + "Version:" + SP + dd.getVersion() + CR;

		//obtenemos el algoritmo
		detalle = detalle + "Algoritmo de firma:" + SP + dd.getDigestAlgorithm() + CR;

		//obtenemos el tipo de contenido
		detalle = detalle + "Tipo de Contenido:" + SP + dd.getContentType() + CR;

		return detalle;
	}

	/**
	 * Obtiene la informaci&oacute;n de un tipo Compressed Data.
	 * @return  Representaci&oacute;n de los datos.
	 */
	private static String getFromCompressedData(final ASN1TaggedObject doj) {
		String detalle = ""; //$NON-NLS-1$
		detalle = detalle + "Tipo:" + SP + COMPRESSED_DATA + CR; //$NON-NLS-1$
		final CompressedData ed = new CompressedData((ASN1Sequence)doj.getObject());

		//obtenemos la version
		detalle = detalle + "Version:" + SP + ed.getVersion() + CR;

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
	 * <li>0: EnvelopedData</li>
	 * <li>1: AuthenticatedData</li>
	 * <li>2: AuthEnvelopedData</li>
	 * <li>3: SignedAndEnvelopedData</li>
	 * <li>4: SignedData</li>
	 * <li>5: Encrypted</li>
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
		DERInteger version = null;
		AlgorithmIdentifier aid = null;
		ContentInfo ci = null;
		ASN1Set authAttrs = null;
		ASN1Set ds = null;
		ASN1Set signerInfosSd = null;

		switch (envelopeType) {
		case TYPE_ENVELOPED_DATA:
			final EnvelopedData enveloped = new EnvelopedData((ASN1Sequence)doj.getObject());
			version = enveloped.getVersion();
			rins = enveloped.getRecipientInfos();
			encryptedContentInfo = enveloped.getEncryptedContentInfo();
			unprotectedAttrs = enveloped.getUnprotectedAttrs();
			break;
		case TYPE_AUTHENTICATED_DATA:
			final AuthenticatedData authenticated = new AuthenticatedData((ASN1Sequence)doj.getObject());
			version = authenticated.getVersion();
			rins = authenticated.getRecipientInfos();
			aid = authenticated.getMacAlgorithm();
			ci = authenticated.getEncapsulatedContentInfo();
			authAttrs = authenticated.getAuthAttrs();
			unprotectedAttrs = authenticated.getUnauthAttrs();
			break;
		case TYPE_AUTHENTICATED_ENVELOPED_DATA:
			final AuthEnvelopedData authEnveloped = new AuthEnvelopedData((ASN1Sequence)doj.getObject());
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
			final SignedData signed = new SignedData((ASN1Sequence)doj.getObject());
			version = signed.getVersion();
			ds = signed.getDigestAlgorithms();
			ci = signed.getEncapContentInfo();
			signerInfosSd = signed.getSignerInfos();
			break;
		case TYPE_ENCRYPTED_DATA:
			final ASN1Sequence encrypted = (ASN1Sequence) doj.getObject();
			version = DERInteger.getInstance(encrypted.getObjectAt(0));
			encryptedContentInfo = EncryptedContentInfo.getInstance(encrypted.getObjectAt(1));
			if (encrypted.size() == 3) {
				unprotectedAttrs = (ASN1Set) encrypted.getObjectAt(2);
			}
			break;
		}

		//obtenemos la version
		detalle = detalle + "Version:" + SP + version + CR;

		//recipientInfo
		if (rins != null) {
			if (envelopeType != TYPE_SIGNED_DATA && envelopeType != TYPE_ENCRYPTED_DATA && rins.size() > 0) {
				detalle = detalle + "Destinatarios:" + CR;
			}
			for (int i=0; i<rins.size(); i++){
				final KeyTransRecipientInfo kti = KeyTransRecipientInfo.getInstance(RecipientInfo.getInstance(rins.getObjectAt(i)).getInfo());
				detalle = detalle + " - Informacion de destino de firma" + SP + (i+1) + ":" + CR; //$NON-NLS-2$
				final AlgorithmIdentifier diAlg= kti.getKeyEncryptionAlgorithm();

				//issuer y serial
				final IssuerAndSerialNumber iss =
					(IssuerAndSerialNumber) SignerIdentifier.getInstance(kti.getRecipientIdentifier().getId()).getId();
				detalle = detalle + TB + "Issuer:" + SP + iss.getName().toString() + CR;
				detalle = detalle + TB + "Numero de serie:" + SP + iss.getSerialNumber() + CR;

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
					detalle = detalle + TB + "Algoritmo de cifrado:" + SP + algorithm.getName() + CR;
				}
				else {
					detalle = detalle + TB + "OID del algoritmo de cifrado:" + SP + diAlg.getAlgorithm() + CR;
				}
			}
		}

		if (envelopeType == TYPE_ENVELOPED_DATA || envelopeType == TYPE_ENCRYPTED_DATA) {
			//obtenemos datos de los datos cifrados.
			detalle = detalle + "Informacion de los datos cifrados:" + CR;
			detalle = detalle + getEncryptedContentInfo(encryptedContentInfo);
		}
		else if (envelopeType == TYPE_AUTHENTICATED_DATA && aid != null && ci != null){
			// mac algorithm
			detalle = detalle + "OID del Algoritmo de MAC:" + SP + aid.getAlgorithm() + CR;

			//digestAlgorithm
			final ASN1Sequence seq =(ASN1Sequence)doj.getObject();
			final ASN1TaggedObject da = (ASN1TaggedObject)seq.getObjectAt(4);
			final AlgorithmIdentifier dai = AlgorithmIdentifier.getInstance(da.getObject());
			detalle = detalle + "OID del Algoritmo de firma:" + SP + dai.getAlgorithm() + CR;

			//obtenemos datos de los datos cifrados.
			detalle = detalle + "OID del tipo de contenido:" + SP + ci.getContentType() + CR;

			detalle = getObligatorieAtrib(signBinaryType, detalle, authAttrs);
		}
		else if (envelopeType == TYPE_AUTHENTICATED_ENVELOPED_DATA) {
			detalle = detalle + "Informacion de los datos cifrados:" + CR;
			detalle = detalle + getEncryptedContentInfo(encryptedContentInfo);

			detalle = getObligatorieAtrib(signBinaryType, detalle, authAttrs);
		}
		else if (envelopeType == TYPE_SIGNED_ENVELOPED_DATA) {
			//algoritmo de firma
			final ASN1Sequence seq =(ASN1Sequence)doj.getObject();
			final ASN1Set da = (ASN1Set)seq.getObjectAt(2);
			final AlgorithmIdentifier dai = AlgorithmIdentifier.getInstance(da.getObjectAt(0));
			detalle = detalle + "OID del Algoritmo de firma:" + SP + dai.getAlgorithm() + CR;

			//obtenemos datos de los datos cifrados.
			detalle = detalle + "Informacion de los datos cifrados:" + CR;
			detalle = detalle + getEncryptedContentInfo(encryptedContentInfo);
		}
		else if (envelopeType == TYPE_SIGNED_DATA && ci != null && ds != null) {
			//algoritmo de firma
			final AlgorithmIdentifier dai = AlgorithmIdentifier.getInstance(ds.getObjectAt(0));
			detalle = detalle + "OID del Algoritmo de firma:" + SP + dai.getAlgorithm() + CR;
			detalle = detalle + "OID del tipo de contenido:" + SP + ci.getContentType() + CR;
		}

		//obtenemos lo atributos opcionales
		if (envelopeType != TYPE_SIGNED_ENVELOPED_DATA) {
			if (unprotectedAttrs == null){
				detalle = detalle + "Atributos: No tiene atributos opcionales" + CR;
			}
			else{
				final String atributos = getUnSignedAttributes(unprotectedAttrs.getObjects());
				detalle = detalle + "Atributos:" + CR;
				detalle = detalle + atributos;
			}
		} else if (envelopeType == TYPE_SIGNED_ENVELOPED_DATA || envelopeType == TYPE_SIGNED_DATA) {
			//obtenemos el(los) firmate(s)
			if (signerInfosSd != null) {
				if (signerInfosSd.size()>0){
					detalle = detalle + "Firmantes:" + CR;
				}
				for(int i =0; i< signerInfosSd.size(); i++){
					final SignerInfo si = new SignerInfo((ASN1Sequence)signerInfosSd.getObjectAt(i));

					detalle = detalle + "- firmante" + SP + (i+1) + ":" + CR; //$NON-NLS-2$
					// version
					detalle = detalle + TB + "version:" + SP + si.getVersion() + CR;
					//signerIdentifier
					final SignerIdentifier sident = si.getSID();
					final IssuerAndSerialNumber iss = IssuerAndSerialNumber.getInstance(sident.getId());
					detalle = detalle + TB + "Issuer:" + SP + iss.getName().toString() + CR;
					detalle = detalle + TB + "Numero de serie:" + SP + iss.getSerialNumber() + CR;

					//digestAlgorithm
					final AlgorithmIdentifier algId = si.getDigestAlgorithm();
					detalle = detalle + TB + "OID del algoritmo de firma de este firmante:" + SP + algId.getAlgorithm() + CR;

					//obtenemos lo atributos obligatorios
					final ASN1Set sa =si.getAuthenticatedAttributes();
					String satributes = ""; //$NON-NLS-1$
					if (sa != null){
						satributes = getsignedAttributes(sa, signBinaryType);
					}
					detalle = detalle + TB + "Atributos obligatorios:" + CR;
					detalle = detalle + satributes;
				}
			}
		}
		return detalle;
	}

	/**
	 * Obtiene los atributos obligatorios
	 * @param signBinaryType	Tipo de firma binaria (CADES o CMS)
	 * @param detalle
	 * @param authAttrs
	 * @return
	 */
	private static String getObligatorieAtrib(final int signBinaryType,
			                                  final String detalle,
			                                  final ASN1Set authAttrs) {

		String det = detalle;

		//obtenemos lo atributos obligatorios
		if (authAttrs == null){
			det = det + "Atributos Autenticados: No tiene atributos autenticados" + CR;
		}
		else{
			final String atributos = getsignedAttributes(authAttrs, signBinaryType);
			det = det + "Atributos Autenticados:" + CR;
			det = det + atributos;
		}
		return det;
	}

	/**
	 * Obtiene los atributos obligatorios de una firma.
	 *
	 * @param attributes    Grupo de atributos opcionales
	 * @param binarySignType	Identifica el tipo de firma binaria (CMS o CADES)
	 * @return              lista de atributos concatenados.
	 */
	private static String getsignedAttributes(final ASN1Set attributes, final int binarySignType){
		String attributos = ""; //$NON-NLS-1$

		final Enumeration<?> e = attributes.getObjects();

		while (e.hasMoreElements()){
			final ASN1Sequence a = (ASN1Sequence)e.nextElement();
			final DERObjectIdentifier derIden = (DERObjectIdentifier)a.getObjectAt(0);
			// tipo de contenido de la firma.
			if (derIden.equals(CMSAttributes.contentType)){
				attributos = attributos + TB + TB + "OID del tipo de contenido:" + SP + a.getObjectAt(1) + CR; //$NON-NLS-1$
			}
			//Message digest de  la firma
			if (derIden.equals(CMSAttributes.messageDigest)){
				attributos = attributos + TB + TB + "Contiene el atributo \"MessageDigest\"" + CR; //$NON-NLS-1$
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

				attributos = attributos + TB + TB + "Contiene fecha de firma:" + SP + ds + CR;
			}
			if (binarySignType == BINARY_SIGN_CADES) {
				//atributo signing certificate v2
				if (derIden.equals(PKCSObjectIdentifiers.id_aa_signingCertificateV2)){
					attributos = attributos + TB + TB + "Contiene el atributo \"Signing Certificate V2\"" + CR;
				}
				//Politica de firma.
				if (derIden.equals(PKCSObjectIdentifiers.id_aa_ets_sigPolicyId)){
					attributos = attributos + TB + TB + "Contiene la politica de la firma" + CR;
				}
			}
		}
		return attributos;
	}

	/**
	 * Obtiene los atributos opcionales de una firma cualquiera.
	 * En caso de ser EncryptedData, usar el otro metodo, ya que por construccion
	 * no es posible utilizar este.
	 *
	 * @param attributes    Grupo de atributos opcionales
	 * @return              lista de atributos concatenados.
	 */
	private static String getUnSignedAttributes(final Enumeration<?> e){
		String attributos = ""; //$NON-NLS-1$

		while (e.hasMoreElements()){
			final ASN1Sequence a = (ASN1Sequence)e.nextElement();
			final DERObjectIdentifier derIden = (DERObjectIdentifier)a.getObjectAt(0);
			// tipo de contenido de la firma.
			if (derIden.equals(CMSAttributes.contentType)){
				attributos = attributos + TB + "OID del tipo de contenido:" + SP + a.getObjectAt(1) + CR;
			}
			//Message digest de  la firma
			if (derIden.equals(CMSAttributes.messageDigest)){
				attributos = attributos + TB + "Contiene el atributo \"MessageDigest\"" + CR;
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

				attributos = attributos + TB + "Contiene fecha de firma:" + SP + ds + CR;
			}
			//contrafirma de la firma.
			if (derIden.equals(CMSAttributes.counterSignature)){
				attributos = attributos + TB + "Contiene la contrafirma de la firma" + CR;
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
			info = info + TB + "Tipo:" + SP + ENCRYPTED_DATA + CR;
		}
		else{
			info = info + TB + "Tipo:" + SP + datos.getContentType() + CR;
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
			info = info + TB + "Algoritmo de cifrado:" + SP + algorithm.getName() + CR;
		}
		else {
			info = info + TB + "OID del Algoritmo de cifrado:" + SP + ai.getAlgorithm().toString() + CR;
		}

		return info;
	}
}
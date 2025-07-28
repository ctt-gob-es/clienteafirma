/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.multi.cades;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Properties;

import org.spongycastle.asn1.ASN1EncodableVector;
import org.spongycastle.asn1.ASN1Encoding;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1OctetString;
import org.spongycastle.asn1.ASN1Primitive;
import org.spongycastle.asn1.ASN1Sequence;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.BEROctetString;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.DERSet;
import org.spongycastle.asn1.cms.AttributeTable;
import org.spongycastle.asn1.cms.CMSAttributes;
import org.spongycastle.asn1.cms.ContentInfo;
import org.spongycastle.asn1.cms.IssuerAndSerialNumber;
import org.spongycastle.asn1.cms.SignedData;
import org.spongycastle.asn1.cms.SignerIdentifier;
import org.spongycastle.asn1.cms.SignerInfo;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.spongycastle.asn1.x500.X500Name;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;
import org.spongycastle.asn1.x509.TBSCertificate;
import org.spongycastle.cms.CMSException;
import org.spongycastle.cms.CMSProcessable;
import org.spongycastle.cms.CMSProcessableByteArray;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.cades.CAdESParameters;
import es.gob.afirma.signers.cades.CAdESUtils;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.ContainsNoDataException;
import es.gob.afirma.signers.pkcs7.SigUtils;

/** Implementa la cofirma digital CADES SignedData. La
 * implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para
 * crear un mensaje SignedData de SpongyCastle pero con la
 * peculiaridad de que es una Cofirma.
 * Para ello, debe incluirse el atributo de pol&iacute;tica en la
 * identificaci&oacute;n del firmante de la siguiente manera:
 *
 * <pre>
 * <code>
 *
 * SignaturePolicyId ::= SEQUENCE {
 *  sigPolicyId           SigPolicyId,
 *  sigPolicyHash         SigPolicyHash,
 *  sigPolicyQualifiers   SEQUENCE SIZE (1..MAX) OF
 *                          AOSigPolicyQualifierInfo OPTIONAL}
 *
 *  SigPolicyId ::= OBJECT IDENTIFIER
 *
 *  OtherHashAlgAndValue ::= SEQUENCE {
 *     hashAlgorithm    AlgorithmIdentifier,
 *     hashValue        OCTET STRING }
 *
 *  AOSigPolicyQualifierInfo ::= SEQUENCE {
 *       SigPolicyQualifierId  SigPolicyQualifierId,
 *       SigQualifier          ANY DEFINED BY policyQualifierId }
 *
 *  SigPolicyQualifierId ::= OBJECT IDENTIFIER
 *
 *      id-spq-ets-uri OBJECT IDENTIFIER ::= { iso(1)
 *      member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs9(9)
 *      smime(16) id-spq(5) 1 }
 *
 *     SPuri ::= IA5String
 *
 *      id-spq-ets-unotice OBJECT IDENTIFIER ::= { iso(1)
 *      member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs9(9)
 *      smime(16) id-spq(5) 2 }
 *
 *     SPUserNotice ::= SEQUENCE {
 *          noticeRef        NoticeReference OPTIONAL,
 *          explicitText     DisplayText OPTIONAL
 *  }
 *
 *     NoticeReference ::= SEQUENCE {
 *          organization     DisplayText,
 *          noticeNumbers    SEQUENCE OF INTEGER
 *  }
 *
 *     DisplayText ::= CHOICE {
 *          visibleString    VisibleString  (SIZE (1..200)),
 *          bmpString        BMPString      (SIZE (1..200)),
 *          utf8String       UTF8String     (SIZE (1..200))
 *  }
 *
 * </code>
 * </pre> */
final class CAdESCoSigner {

	/** Se crea una cofirma a partir de los datos del firmante, el archivo
	 * que se firma y el archivo que contiene las firmas.
	 * @param signature Archivo que contiene las firmas.
	 * @param signatureAlgorithm Algoritmo de firma.
	 * @param key Clave privada usada para firmar.
	 * @param certChain Cadena de certificados del firmante.
	 * @param config Configuraci&oacute;n de la firma a generar.
	 * @return El archivo de firmas con la nueva firma.
	 * @throws IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
	 * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella
	 *                                  digital
	 * @throws CertificateException Si se produce alguna excepci&oacute;n con los certificados de
	 *                              firma.
     * @throws AOException Cuando ocurre un error durante la generacion de PKCS#1 de la firma.
     */
	static byte[] coSigner(
			final byte[] signature,
			final String signatureAlgorithm,
			final PrivateKey key,
			final java.security.cert.Certificate[] certChain,
			final CAdESParameters config
                    ) throws IOException,
			                                                  NoSuchAlgorithmException,
			                                                  CertificateException,
			                                                  AOException {
		// Leemos la firma de entrada
		final SignedData sd = CAdESMultiUtil.readData(signature);
		return coSigner(sd, signatureAlgorithm, key, certChain, config);
	}

	/**
	 * Se crea una cofirma a partir de los datos del firmante, el archivo
	 * que se firma y el archivo que contiene las firmas.
	 * @param signedData Datos firmados.
	 * @param signatureAlgorithm Algoritmo de firma.
	 * @param key Clave privada usada para firmar.
	 * @param certChain Cadena de certificados del firmante.
	 * @param config Configuraci&oacute;n de la firma a generar.
	 * @return El archivo de firmas con la nueva firma.
	 * @throws IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
	 * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella
	 *                                  digital
	 * @throws CertificateException Si se produce alguna excepci&oacute;n con los certificados de
	 *                              firma.
     * @throws AOException Cuando ocurre un error durante la generacion de PKCS#1 de la firma.
     */
	static byte[] coSigner(
			final SignedData signedData,
			final String signatureAlgorithm,
			final PrivateKey key,
			final java.security.cert.Certificate[] certChain,
			final CAdESParameters config
                    ) throws IOException,
			                                                  NoSuchAlgorithmException,
			                                                  CertificateException,
			                                                  AOException {

		// 3. CONTENTINFO
		// si se introduce el contenido o no
		ContentInfo encInfo;

		// Si se proporcionaron los datos, los uso para identificar el contenido
		if (config.getContentData() != null) {
			final ASN1ObjectIdentifier contentTypeOID = new ASN1ObjectIdentifier(PKCSObjectIdentifiers.data.getId());
			final ByteArrayOutputStream bOut = new ByteArrayOutputStream();
			final CMSProcessable msg = new CMSProcessableByteArray(config.getContentData());
			try {
				msg.write(bOut);
			}
			catch (final CMSException ex) {
				throw new IOException("Error en la escritura del procesable CMS: " + ex, ex); //$NON-NLS-1$
			}
			encInfo = new ContentInfo(contentTypeOID, new BEROctetString(bOut.toByteArray()));
		}
		// Si no, obtenemos la informacion directamente de la firma. Si, ademas, no tenemos la huella de los datos
		// y la firma contenia el documento
		else {
			encInfo = signedData.getEncapContentInfo();

			if (config.getDataDigest() == null) {
				final DEROctetString contentData = (DEROctetString) encInfo.getContent();
				if (contentData != null) {
					final byte[] data;
					try (
						final InputStream is = contentData.getOctetStream()
					) {
						data = AOUtil.getDataFromInputStream(is);
					}
					config.setDataDigest(MessageDigest.getInstance(config.getDigestAlgorithm()).digest(data));
				}
			}
		}

		// 4. CERTIFICADOS
		// obtenemos la lista de certificados
		final ASN1Set certificates = CAdESMultiUtil.addCertificates(signedData, certChain);

		// buscamos que tipo de algoritmo es y lo codificamos con su OID
		final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);
		final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));

		// Identificador del firmante ISSUER AND SERIAL-NUMBER
		final TBSCertificate tbs = TBSCertificate.getInstance(
			ASN1Primitive.fromByteArray(((X509Certificate) certChain[0]).getTBSCertificate())
		);
		final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(
			X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue()
		);
		final SignerIdentifier identifier = new SignerIdentifier(encSid);

		final String keyType = certChain[0].getPublicKey().getAlgorithm();

		final String algorithmName = AOSignConstants.composeSignatureAlgorithmName(digestAlgorithm, keyType);

		// digEncryptionAlgorithm
		final AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(algorithmName));

		// 5. SIGNERINFO
		// raiz de la secuencia de SignerInfo
		// Obtenemos los signerInfos del SignedData
		final ASN1Set signerInfosSd = signedData.getSignerInfos();

		// introducimos los SignerInfos Existentes
		final ASN1EncodableVector signerInfos = new ASN1EncodableVector();
		// introducimos el nuevo SignerInfo del firmante actual.

		// Obtenemos el listado de firmantes y, si no tenemos la huella digital de los
		// datos, aprovechamos para buscarla, teniendo en cuenta que debe haberse
		// generado con el mismo algoritmo que
		// queremos utilizar. Si existe, se usara esa huella y, de esta forma, nos
		// aseguraremos de que estamos firmando los mismos datos que la firma original
		for (int i = 0; i < signerInfosSd.size(); i++) {
			final SignerInfo si = SignerInfo.getInstance(signerInfosSd.getObjectAt(i));
			signerInfos.add(si);
			if (config.getDataDigest() == null) {
				final AlgorithmIdentifier algHash = si.getDigestAlgorithm();
				if (algHash.getAlgorithm().toString().equals(AOAlgorithmID.getOID(digestAlgorithm))) {
					final ASN1Set signedAttrib = si.getAuthenticatedAttributes();
					for (int s = 0; s < signedAttrib.size(); s++) {
						final ASN1Sequence elemento = (ASN1Sequence) signedAttrib.getObjectAt(s);
						final ASN1ObjectIdentifier oids = (ASN1ObjectIdentifier) elemento.getObjectAt(0);
						if (CMSAttributes.messageDigest.getId().equals(oids.toString())) {
							final DERSet derSetHash = (DERSet) elemento.getObjectAt(1);
							final DEROctetString derHash = (DEROctetString) derSetHash.getObjectAt(0);
							config.setDataDigest(derHash.getOctets());
						}
					}
				}
			}
		}

		// === PREFIRMA ===

		if (config.getDataDigest() == null && config.getContentData() == null) {
			throw new ContainsNoDataException("No se puede crear la cofirma ya que no se han encontrado ni los datos firmados ni una huella digital compatible con el algoritmo de firma"); //$NON-NLS-1$
		}
		final ASN1EncodableVector signedAttributes = CAdESUtils.generateSignedAttributes(
				certChain[0],
				config,
				false);
		final ASN1Set signedAttr = SigUtils.getAttributeSet(new AttributeTable(signedAttributes));

		// === FIRMA ===

		final ASN1OctetString signValue = generateSignValue(
				signedAttr,
				signatureAlgorithm,
				key,
				certChain,
				config.getExtraParams()
				);

		// === POSTFIRMA ===

		// Creamos los signerInfos del SignedData
		signerInfos.add(
				new SignerInfo(
						identifier,	// Identificador del firmante
						digAlgId,	// Identificador del algoritmo de huella
						signedAttr, // Atributos firmados
						encAlgId,	// Identificador del algoritmo de encriptado
						signValue,  // valor de firma generado (PKCS#1)
						null));		// Atributos no firmados

		// construimos el Signed Data y lo devolvemos
		return new ContentInfo(
			PKCSObjectIdentifiers.signedData,
			new SignedData(
				signedData.getDigestAlgorithms(),
				encInfo,
				certificates,
				signedData.getCRLs(),
				//null,	// CRLS no usado
				new DERSet(signerInfos)// SignerInfos
			)
		).getEncoded(ASN1Encoding.DER);
	}

	/**
	 * Genera el PKCS#1 de los atributos de firma.
	 * @param signedAttr Atributos a firmar.
	 * @param signatureAlgorithm
	 *        Algoritmo para la firma
	 * @param key Clave para firmar.
	 * @param certChain Cadena de certificados del firmante.
	 * @param extraParams Par&aacute;metros adicionales para el PKCS#1.
	 * @return Firma de los atributos.
	 * @throws AOException Cuando ocurre cualquier tipo de error
	 */
	private static ASN1OctetString generateSignValue(
			final ASN1Set signedAttr,
			final String signatureAlgorithm,
			final PrivateKey key,
			final java.security.cert.Certificate[] certChain,
			final Properties extraParams) throws AOException {

		final byte[] tmp;
		try {
			tmp = signedAttr.getEncoded(ASN1Encoding.DER);
		}
		catch (final IOException ex) {
			throw new AOException("Error al obtener los datos a firmar", ex); //$NON-NLS-1$
		}

		return new DEROctetString(
			new AOPkcs1Signer().sign(
				tmp,
				signatureAlgorithm,
				key,
				certChain,
				extraParams
			)
		);

	}
}

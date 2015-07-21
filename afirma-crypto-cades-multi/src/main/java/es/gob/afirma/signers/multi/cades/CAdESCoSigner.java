/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.multi.cades;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.List;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Encoding;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Primitive;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.BEROctetString;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.cms.SignerIdentifier;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.Certificate;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;
import org.bouncycastle.cms.CMSException;
import org.bouncycastle.cms.CMSProcessable;
import org.bouncycastle.cms.CMSProcessableByteArray;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOPkcs1Signer;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signers.cades.CAdESSignerMetadata;
import es.gob.afirma.signers.cades.CAdESUtils;
import es.gob.afirma.signers.cades.CommitmentTypeIndicationBean;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.ContainsNoDataException;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.SigUtils;

/** Clase que implementa la cofirma digital CADES SignedData La
 * implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para
 * crear un mensaje SignedData de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> pero con la
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

	private static SignedData readData(final byte[] signature) throws IOException {
		// LEEMOS EL FICHERO QUE NOS INTRODUCEN
		final ASN1InputStream is = new ASN1InputStream(signature);
		final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
		is.close();
		final Enumeration<?> e = dsq.getObjects();
		// Elementos que contienen los elementos OID SignedData
		e.nextElement();
		// Contenido de SignedData
		final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
		final ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject(); // contenido del SignedData

		return SignedData.getInstance(contentSignedData);
	}

	private ASN1Set signedAttr2;

	/** Se crea una cofirma a partir de los datos del firmante, el archivo
	 * que se firma y el archivo que contiene las firmas.
	 * @param parameters Par&aacute;metros necesarios que contienen tanto la firma del
	 *                   archivo a firmar como los datos del firmante.
	 * @param signature Archivo que contiene las firmas.
	 * @param omitContent Si se omite el contenido o no, es decir,si se hace de forma
	 *                    Expl&iacute;cita o Impl&iacute;cita.
	 * @param policy Pol&iacute;tica de firma
	 * @param signingCertificateV2 <code>true</code> si se desea usar la versi&oacute;n 2 del
	 *                             atributo <i>Signing Certificate</i> <code>false</code> para
	 *                             usar la versi&oacute;n 1
	 * @param key Clave privada usada para firmar.
	 * @param certChain Cadena de certificados del firmante.
	 * @param messageDigest Hash espec&iacute;fico para una firma.
	 * @param contentType Tipo de contenido definido por su OID.
	 * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
	 * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
	 * @param csm Metadatos sobre el firmante
	 * @return El archivo de firmas con la nueva firma.
	 * @throws IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
	 * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella
	 *                                  digital
	 * @throws CertificateException Si se produce alguna excepci&oacute;n con los certificados de
	 *                              firma.*/
	byte[] coSigner(final P7ContentSignerParameters parameters,
			        final byte[] signature,
			        final boolean omitContent,
			        final AdESPolicy policy,
			        final boolean signingCertificateV2,
			        final PrivateKey key,
			        final java.security.cert.Certificate[] certChain,
			        final byte[] messageDigest,
			        final String contentType,
			        final String contentDescription,
			        final List<CommitmentTypeIndicationBean> ctis,
			        final CAdESSignerMetadata csm) throws IOException,
			                                              NoSuchAlgorithmException,
			                                              CertificateException {
		final SignedData sd = readData(signature);

		// 3. CONTENTINFO
		// si se introduce el contenido o no
		ContentInfo encInfo = null;
		final ASN1ObjectIdentifier contentTypeOID = new ASN1ObjectIdentifier(PKCSObjectIdentifiers.data.getId());

		// Ya que el contenido puede ser grande, lo recuperamos solo una vez porque se clona
		// al recuperarlo
		byte[] content2 = null;

		if (!omitContent) {
			final ByteArrayOutputStream bOut = new ByteArrayOutputStream();
			content2 = parameters.getContent();
			final CMSProcessable msg = new CMSProcessableByteArray(content2);
			try {
				msg.write(bOut);
			}
			catch (final CMSException ex) {
				throw new IOException("Error en la escritura del procesable CMS: " + ex, ex); //$NON-NLS-1$
			}
			encInfo = new ContentInfo(contentTypeOID, new BEROctetString(bOut.toByteArray()));
		}
		else {
			encInfo = new ContentInfo(contentTypeOID, null);
		}

		// 4. CERTIFICADOS
		// obtenemos la lista de certificados
		final ASN1Set certificates = CAdESMultiUtil.addCertificates(sd, certChain);

		// buscamos que tipo de algoritmo es y lo codificamos con su OID
		final String signatureAlgorithm = parameters.getSignatureAlgorithm();
		final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);
		final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(AOSignConstants.getDigestAlgorithmName(signatureAlgorithm)));

		// Identificador del firmante ISSUER AND SERIAL-NUMBER
		final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(
			ASN1Primitive.fromByteArray(((X509Certificate)certChain[0]).getTBSCertificate())
		);
		final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
		final SignerIdentifier identifier = new SignerIdentifier(encSid);

		// // ATRIBUTOS

		final ASN1Set signedAttr;
		if (messageDigest == null) {
			final ASN1EncodableVector contextExpecific = CAdESUtils.generateSignerInfo(
				certChain[0],
				digestAlgorithm,
				content2 != null ? content2 : parameters.getContent(),
				policy,
				signingCertificateV2,
				null,
				new Date(),
				false,
				contentType,
				contentDescription,
				ctis,
				csm,
                false  // No es contrafirma
			);
			this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(contextExpecific));
			signedAttr = SigUtils.getAttributeSet(new AttributeTable(contextExpecific));
		}
		else {
			final ASN1EncodableVector contextExpecific = CAdESUtils.generateSignerInfo(
				certChain[0],
				digestAlgorithm,
				null,
				policy,
				signingCertificateV2,
				messageDigest,
				new Date(),
				false,
				contentType,
				contentDescription,
				ctis,
				csm,
                false  // No es contrafirma
			);
			this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(contextExpecific));
			signedAttr = SigUtils.getAttributeSet(new AttributeTable(contextExpecific));
		}

		// digEncryptionAlgorithm
		final AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID("RSA")); //$NON-NLS-1$

		// 5. SIGNERINFO
		// raiz de la secuencia de SignerInfo
		// Obtenemos los signerInfos del SignedData
		final ASN1Set signerInfosSd = sd.getSignerInfos();

		// introducimos los SignerInfos Existentes
		final ASN1EncodableVector signerInfos = new ASN1EncodableVector();
		// introducimos el nuevo SignerInfo del firmante actual.

		for (int i = 0; i < signerInfosSd.size(); i++) {
			signerInfos.add(SignerInfo.getInstance(signerInfosSd.getObjectAt(i)));
		}

		final ASN1OctetString sign2;
		try {
			sign2 = firma(signatureAlgorithm, key, certChain);
		}
		catch (final AOException ex) {
			throw new IOException("Error al realizar la firma: " + ex, ex); //$NON-NLS-1$
		}

		// Creamos los signerInfos del SignedData
		signerInfos.add(new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, null));

		// construimos el Signed Data y lo devolvemos
		return new ContentInfo(
			PKCSObjectIdentifiers.signedData,
			new SignedData(
				sd.getDigestAlgorithms(),
				encInfo,
				certificates,
				null,	// CRLS no usado
				new DERSet(signerInfos)// unsignedAttr
			)
		).getEncoded(ASN1Encoding.DER);
	}

	/** Constructor de la clase. Se crea una cofirma a partir de los datos del
	 * firmante y del archivo que contiene las firmas.
	 * @param signatureAlgorithm Algoritmo para la firma
	 * @param signerCertificateChain Cadena de certificados para la construccion de los parametros de firma.
	 * @param signature Archivo que contiene las firmas.
	 * @param policy Pol&iacute;tica de firma
	 * @param signingCertificateV2
	 *        <code>true</code> si se desea usar la versi&oacute;n 2 del
	 *        atributo <i>Signing Certificate</i> <code>false</code> para
	 *        usar la versi&oacute;n 1
	 * @param key Clave privada usada para firmar.
	 * @param certChain Cadena de certificados del firmante.
	 * @param md Huella digital espec&iacute;fica para una firma.
	 * @param contentType Tipo de contenido definido por su OID.
	 * @param contentDescription Descripci&oacute;n textual del tipo de contenido firmado.
	 * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
	 * @param csm Metadatos sobre el firmante
	 * @return El archivo de firmas con la nueva firma.
	 * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
	 * @throws NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella digital
	 * @throws CertificateException Si se produce alguna excepci&oacute;n con los certificados de firma.
	 * @throws ContainsNoDataException Cuando la firma no contiene los datos ni fue generada con el mismo
	 *                                 algoritmo de firma. */
	byte[] coSigner(final String signatureAlgorithm,
			        final X509Certificate[] signerCertificateChain,
			        final InputStream signature,
			        final AdESPolicy policy,
			        final boolean signingCertificateV2,
			        final PrivateKey key,
			        final java.security.cert.Certificate[] certChain,
			        final byte[] md,
			        final String contentType,
			        final String contentDescription,
			        final List<CommitmentTypeIndicationBean> ctis,
                    final CAdESSignerMetadata csm) throws IOException,
			                                              NoSuchAlgorithmException,
			                                              CertificateException,
			                                              ContainsNoDataException {
		// LEEMOS EL FICHERO QUE NOS INTRODUCEN
		final ASN1InputStream is = new ASN1InputStream(signature);
		final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
		is.close();
		final Enumeration<?> e = dsq.getObjects();

		// Elementos que contienen los elementos OID SignedData
		e.nextElement();

		// Contenido de SignedData
		final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
		final ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject();// contenido

		// El SignedData
		final SignedData sd = SignedData.getInstance(contentSignedData);

		// 3. CONTENTINFO
		// si se introduce el contenido o no
		final ContentInfo encInfo = sd.getEncapContentInfo();

		final DEROctetString contenido = (DEROctetString) encInfo.getContent();
		byte[] contenidoDatos = null;
		if (contenido != null) {
			contenidoDatos = AOUtil.getDataFromInputStream(contenido.getOctetStream());
		}

		// 4. CERTIFICADOS
		// obtenemos la lista de certificados
		ASN1Set certificates = null;
		final ASN1Set certificatesSigned = sd.getCertificates();
		final ASN1EncodableVector vCertsSig = new ASN1EncodableVector();
		final Enumeration<?> certs = certificatesSigned.getObjects();

		// COGEMOS LOS CERTIFICADOS EXISTENTES EN EL FICHERO
		while (certs.hasMoreElements()) {
			vCertsSig.add((ASN1Encodable) certs.nextElement());
		}

		if (signerCertificateChain.length != 0) {
			final List<ASN1Encodable> ce = new ArrayList<ASN1Encodable>();
			for (final X509Certificate element : signerCertificateChain) {
				ce.add(Certificate.getInstance(ASN1Primitive.fromByteArray(element.getEncoded())));
			}
			certificates = SigUtils.fillRestCerts(ce, vCertsSig);
		}

		// buscamos que timo de algoritmo es y lo codificamos con su OID
		final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);
		final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));

		// Identificador del firmante ISSUER AND SERIAL-NUMBER
		final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Primitive.fromByteArray(signerCertificateChain[0].getTBSCertificate()));
		final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
		final SignerIdentifier identifier = new SignerIdentifier(encSid);

		// digEncryptionAlgorithm
		final AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID("RSA")); //$NON-NLS-1$

		// 5. SIGNERINFO
		// raiz de la secuencia de SignerInfo
		// Obtenemos los signerInfos del SignedData
		final ASN1Set signerInfosSd = sd.getSignerInfos();

		// introducimos los SignerInfos Existentes
		final ASN1EncodableVector signerInfos = new ASN1EncodableVector();
		// introducimos el nuevo SignerInfo del firmante actual.

		byte[] messageDigest = md != null ? md.clone() : null;

		for (int i = 0; i < signerInfosSd.size(); i++) {
			final SignerInfo si = SignerInfo.getInstance(signerInfosSd.getObjectAt(i));
			final AlgorithmIdentifier algHash = si.getDigestAlgorithm();
			if (algHash.getAlgorithm().toString().equals(AOAlgorithmID.getOID(digestAlgorithm))) {
				final ASN1Set signedAttrib = si.getAuthenticatedAttributes();
				for (int s = 0; s < signedAttrib.size(); s++) {
					final ASN1Sequence elemento = (ASN1Sequence) signedAttrib.getObjectAt(s);
					final ASN1ObjectIdentifier oids = (ASN1ObjectIdentifier) elemento.getObjectAt(0);
					if (CMSAttributes.messageDigest.getId().equals(oids.toString())) {
						final DERSet derSetHash = (DERSet) elemento.getObjectAt(1);
						final DEROctetString derHash = (DEROctetString) derSetHash.getObjectAt(0);
						messageDigest = derHash.getOctets();
					}
				}
			}
			signerInfos.add(si);
		}

		// // ATRIBUTOS

		final ASN1Set signedAttr;
		// atributos firmados
		if (contenidoDatos != null) {
			final ASN1EncodableVector contextExpecific = CAdESUtils.generateSignerInfo(
				signerCertificateChain[0],
				digestAlgorithm,
				contenidoDatos,
				policy,
				signingCertificateV2,
				null, // MessageDigest
				new Date(),
				false,
				contentType,
				contentDescription,
				ctis,
				csm,
                false  // No es contrafirma
			);
			this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(contextExpecific));
			signedAttr = SigUtils.getAttributeSet(new AttributeTable(contextExpecific));
		}
		else if (messageDigest != null) {
			final ASN1EncodableVector contextExpecific = CAdESUtils.generateSignerInfo(
				signerCertificateChain[0],
				digestAlgorithm,
				null,
				policy,
				signingCertificateV2,
				messageDigest,
				new Date(),
				false,
				contentType,
				contentDescription,
				ctis,
				csm,
                false  // No es contrafirma
			);
			this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(contextExpecific));
			signedAttr = SigUtils.getAttributeSet(new AttributeTable(contextExpecific));
		}
		else {
			throw new ContainsNoDataException("No se puede crear la cofirma ya que no se han encontrado ni los datos firmados ni una huella digital compatible con el algoritmo de firma"); //$NON-NLS-1$
		}

		final ASN1OctetString sign2;
		try {
			sign2 = firma(signatureAlgorithm, key, certChain);
		}
		catch (final AOException ex) {
			throw new IOException("Error al realizar la firma: " + ex, ex); //$NON-NLS-1$
		}

		// Creamos los signerInfos del SignedData
		signerInfos.add(new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, null // unsignedAttr
				));

		// construimos el Signed Data y lo devolvemos
		return new ContentInfo(
			PKCSObjectIdentifiers.signedData,
			new SignedData(
				sd.getDigestAlgorithms(),
				encInfo,
				certificates,
				null,	// CRLS no usado
				new DERSet(signerInfos)// unsignedAttr
			)
		).getEncoded(ASN1Encoding.DER);

	}

	/** Realiza la firma usando los atributos del firmante.
	 * @param signatureAlgorithm
	 *        Algoritmo para la firma
	 * @param key Clave para firmar.
	 * @param certChain Cadena de certificados del firmante.
	 * @return Firma de los atributos.
	 * @throws AOException Cuando ocurre cualquier tipo de error */
	private ASN1OctetString firma(final String signatureAlgorithm,
			                      final PrivateKey key,
			                      final java.security.cert.Certificate[] certChain) throws AOException {

		final byte[] tmp;
		try {
			tmp = this.signedAttr2.getEncoded(ASN1Encoding.DER);
		}
		catch (final IOException ex) {
			throw new AOException("Error al obtener los datos a firmar", ex); //$NON-NLS-1$
		}

		return new DEROctetString(new AOPkcs1Signer().sign(tmp, signatureAlgorithm, key, certChain, null));

	}
}

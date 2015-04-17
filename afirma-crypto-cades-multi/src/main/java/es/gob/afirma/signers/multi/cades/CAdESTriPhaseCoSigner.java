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
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.List;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Encoding;
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

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signers.cades.CAdESSignerMetadata;
import es.gob.afirma.signers.cades.CAdESUtils;
import es.gob.afirma.signers.cades.CommitmentTypeIndicationBean;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.SigUtils;

/** Cofirmador CAdES en tres fases. Necesita que se proporcionen tanto firma como datos a cofirmar.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s Capote. */
public final class CAdESTriPhaseCoSigner {

	private CAdESTriPhaseCoSigner() {
		// No permitimos la instanciacion
	}

	/** Realiza una pre-cofirma CAdES.
	 * @param content Contenido a cofirmar
	 * @param signatureAlgorithm Algoritmo de firma
	 * @param signerCertificateChain Cadena de certificados del firmante
	 * @param policy Pol&iacute;tica de firma
	 * @param signingCertificateV2 Si se establece a <code>true</code> se usa SigningCertificateV2, y se usa
	 *                             SigningCertificateV1 en caso contrario
	 * @param messageDigest Huella digital de los datos a firmar (necesario si no se indica el contenido a firmar)
	 * @param contentType Tipo de los datos a firmar
	 * @param contentDescription Contenido de los datos a firmar
	 * @param signDate Fecha de la firma
	 * @param ctis Indicaciones sobre los tipos de compromisos adquiridos con la firma.
	 * @param csm Metadatos sobre el firmante
	 * @return Pre-cofirma CAdES (SignedAttributes de CAdES)
	 * @throws CertificateEncodingException Si alguno de los certificados proporcionados tienen problemas de formatos
	 * @throws NoSuchAlgorithmException Si no se soporta alg&uacute;n algoritmo necesario
	 * @throws IOException Cuando ocurren problemas de entrada / salida */
	public static byte[] preCoSign(final byte[] content,
			                       final String signatureAlgorithm,
			                       final X509Certificate[] signerCertificateChain,
			                       final AdESPolicy policy,
			                       final boolean signingCertificateV2,
			                       final byte[] messageDigest,
			                       final String contentType,
			                       final String contentDescription,
			                       final Date signDate,
			                       final List<CommitmentTypeIndicationBean> ctis,
			                       final CAdESSignerMetadata csm) throws CertificateEncodingException,
			                                                                             NoSuchAlgorithmException,
			                                                                             IOException {
		return getSignedAttributes(
			messageDigest,
			signerCertificateChain,
			AOSignConstants.getDigestAlgorithmName(signatureAlgorithm),
			content,
			signingCertificateV2,
			policy,
			contentType,
			contentDescription,
			signDate,
			ctis,
			csm
		).getEncoded(ASN1Encoding.DER);
	}

	private static ASN1Set getSignedAttributes(final byte[] messageDigest,
			                                   final X509Certificate[] signerCertificateChain,
			                                   final String digestAlgorithm,
			                                   final byte[] content,
			                                   final boolean signingCertificateV2,
			                                   final AdESPolicy policy,
			                                   final String contentType,
			                                   final String contentDescription,
			                                   final Date signDate,
			                                   final List<CommitmentTypeIndicationBean> ctis,
			                                   final CAdESSignerMetadata csm) throws CertificateEncodingException,
			                                                                         NoSuchAlgorithmException,
			                                                                         IOException {

		final ASN1EncodableVector contextExpecific = CAdESUtils.generateSignerInfo(
			signerCertificateChain[0],
			digestAlgorithm,
			content,
			policy,
			signingCertificateV2,
			messageDigest,
			signDate,
			false,
			contentType,
			contentDescription,
			ctis,
			csm,
            false  // No es contrafirma
		);
		return SigUtils.getAttributeSet(new AttributeTable(contextExpecific));
	}


	/** Realiza una post-cofirma CAdES.
	 * @param pkcs1sign Firma PKCS#1 de la pre-cofirma (de los SignedAttributes)
	 * @param preCoSign Pre-cofirma CAdES (SignedAttributes)
	 * @param content Contenido a post-cofirmar (indicar null si se desea omitir su inclusi&oacute;n en la firma)
	 * @param signatureAlgorithm Algoritmo de firma
	 * @param signerCertificateChain Cadena de certificados del firmante
	 * @param sign Firma donde insertar la cofirma
	 * @return Cofirma CAdES
	 * @throws IOException Cuando ocurren problemas de entrada / salida
	 * @throws CertificateEncodingException Si alguno de los certificados proporcionados tienen problemas de formatos */
	public static byte[] postCoSign(final byte[] pkcs1sign,
			final byte[] preCoSign,
			final byte[] content,
			final String signatureAlgorithm,
			final X509Certificate[] signerCertificateChain,
			final byte[] sign) throws IOException,
			CertificateEncodingException {
		// Firma en ASN.1
		final ASN1OctetString asn1sign = new DEROctetString(pkcs1sign);

		// Identificador del firmante ISSUER AND SERIAL-NUMBER
		final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Primitive.fromByteArray(signerCertificateChain[0].getTBSCertificate()));
		final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
		final SignerIdentifier identifier = new SignerIdentifier(encSid);

		// digEncryptionAlgorithm
		final AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID("RSA")); //$NON-NLS-1$
		final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(AOSignConstants.getDigestAlgorithmName(signatureAlgorithm)));

		final ASN1Sequence contentSignedData = getContentSignedData(sign);
		final SignedData sd = SignedData.getInstance(contentSignedData);

		// CERTIFICADOS
		// Obtenemos la lista de certificados

		final ASN1Set certificatesSigned = sd.getCertificates();
		final Enumeration<?> certs = certificatesSigned.getObjects();
		final ASN1EncodableVector vCertsSig = new ASN1EncodableVector();

		// COGEMOS LOS CERTIFICADOS EXISTENTES EN EL FICHERO
		while (certs.hasMoreElements()) {
			vCertsSig.add((ASN1Encodable) certs.nextElement());
		}

		ASN1Set certificates = null;
		if (signerCertificateChain.length != 0) {
			final List<ASN1Encodable> ce = new ArrayList<ASN1Encodable>();
			for (final X509Certificate element : signerCertificateChain) {
				ce.add(Certificate.getInstance(ASN1Primitive.fromByteArray(element.getEncoded())));
			}
			certificates = SigUtils.fillRestCerts(ce, vCertsSig);
		}

		// SIGNERINFO
		// raiz de la secuencia de SignerInfo
		// Obtenemos los signerInfos del SignedData
		final ASN1Set signerInfosSd = sd.getSignerInfos();

		// introducimos los SignerInfos Existentes
		final ASN1EncodableVector signerInfos = new ASN1EncodableVector();

		// introducimos el nuevo SignerInfo del firmante actual.
		for (int i = 0; i < signerInfosSd.size(); i++) {
			signerInfos.add(SignerInfo.getInstance(signerInfosSd.getObjectAt(i)));
		}

		// Creamos los signerInfos del SignedData
		signerInfos.add(
				new SignerInfo(
						identifier,
						digAlgId,
						ASN1Set.getInstance(preCoSign),
						encAlgId,
						asn1sign,
						null // unsignedAttr
						)
				);

		final ContentInfo encInfo = getContentInfoFromContent(content);

		// Construimos el Signed Data y lo devolvemos
		return new ContentInfo(
			PKCSObjectIdentifiers.signedData,
			new SignedData(
					sd.getDigestAlgorithms(),
					encInfo,
					certificates,
					(ASN1Set) null, // CRLS no usado
					new DERSet(signerInfos) // UnsignedAttr
			)
		).getEncoded(ASN1Encoding.DER);

	}

	private static ContentInfo getContentInfoFromContent(final byte[] content) throws IOException {
		// Ya que el contenido puede ser grande, lo recuperamos solo una vez
		final ASN1ObjectIdentifier contentTypeOID = new ASN1ObjectIdentifier(
			PKCSObjectIdentifiers.data.getId()
		);
		// si se introduce el contenido o no
		if (content != null) {
			final ByteArrayOutputStream bOut = new ByteArrayOutputStream();
			final CMSProcessable msg = new CMSProcessableByteArray(content);
			try {
				msg.write(bOut);
			}
			catch (final CMSException ex) {
				throw new IOException("Error en la escritura del procesable CMS: " + ex, ex); //$NON-NLS-1$
			}
			return new ContentInfo(contentTypeOID, new BEROctetString(bOut.toByteArray()));
		}
		return new ContentInfo(contentTypeOID, null);
	}

	private static ASN1Sequence getContentSignedData(final byte[] sign) {
		// LEEMOS LOS DATOS PROPORCIONADOS (la firma previa)
		final ASN1Sequence dsq = ASN1Sequence.getInstance(sign);
		final Enumeration<?> e = dsq.getObjects();
		// Elementos que contienen los elementos OID SignedData
		e.nextElement();
		// Contenido de SignedData
		final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
		return (ASN1Sequence) doj.getObject();
	}

}

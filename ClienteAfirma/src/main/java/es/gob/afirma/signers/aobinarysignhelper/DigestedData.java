/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import java.util.Enumeration;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.BERSequence;
import org.bouncycastle.asn1.DERInteger;
import org.bouncycastle.asn1.DERObject;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

/**
 * Clase base para la implementaci&oacute;n del tipo DigestedData La Estructura
 * del mensaje es la siguiente:<br>
 * 
 * <pre>
 * <code>
 *  DigestedData ::= SEQUENCE {
 *        version CMSVersion,
 *        digestAlgorithm DigestAlgorithmIdentifier,
 *        encapContentInfo EncapsulatedContentInfo,
 *        digest Digest }
 * 
 *  Digest ::= OCTET STRING
 * </code>
 * </pre>
 * 
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un mensaje DigestedData de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a>
 */
final class DigestedData extends ASN1Encodable {
	private DERInteger version;
	private AlgorithmIdentifier digestAlgorithm;
	private ContentInfo contentInfo;
	private ASN1OctetString digest;

	static DigestedData getInstance(Object o) {
		if (o instanceof DigestedData) {
			return (DigestedData) o;
		} else if (o instanceof ASN1Sequence) {
			return new DigestedData((ASN1Sequence) o);
		}

		throw new IllegalArgumentException("unknown object in factory: "
				+ o.getClass().getName());
	}

	DigestedData(AlgorithmIdentifier digestAlgorithms, ContentInfo contentInfo,
			ASN1OctetString digest) {
		this.version = new DERInteger(0);
		this.digestAlgorithm = digestAlgorithms;
		this.contentInfo = contentInfo;
		this.digest = digest;
	}

	DigestedData(ASN1Sequence seq) {
		Enumeration<?> e = seq.getObjects();

		version = (DERInteger) e.nextElement();
		digestAlgorithm = new AlgorithmIdentifier(
				(ASN1Sequence) e.nextElement());
		contentInfo = new ContentInfo((ASN1Sequence) e.nextElement());
		digest = ((ASN1OctetString) (e.nextElement()));

	}

	DERInteger getVersion() {
		return version;
	}

	AlgorithmIdentifier getDigestAlgorithm() {
		return digestAlgorithm;
	}

	ASN1OctetString getDigest() {
		return digest;
	}

	ContentInfo getContentInfo() {
		return contentInfo;
	}

	/**
	 * Produce an object suitable for an ASN1OutputStream.
	 * 
	 * <pre>
	 * DigestedData ::= SEQUENCE {
	 *     version CMSVersion,
	 *     digestAlgorithms DigestAlgorithmIdentifiers,
	 *     encapContentInfo EncapsulatedContentInfo,
	 *     digest  Digest
	 *   }
	 * 
	 * Digest ::= OCTET STRING
	 * </pre>
	 */
	@Override
	public DERObject toASN1Object() {
		ASN1EncodableVector v = new ASN1EncodableVector();

		v.add(version);
		v.add(digestAlgorithm);
		v.add(contentInfo);
		v.add(digest);

		return new BERSequence(v);
	}
}

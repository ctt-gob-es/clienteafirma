/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pkcs7;

import java.io.IOException;
import java.util.Enumeration;
import java.util.logging.Logger;

import org.spongycastle.asn1.ASN1InputStream;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1Sequence;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.ASN1TaggedObject;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.DERSet;
import org.spongycastle.asn1.cms.CMSAttributes;
import org.spongycastle.asn1.cms.ContentInfo;
import org.spongycastle.asn1.cms.SignedData;
import org.spongycastle.asn1.cms.SignerInfo;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;

import es.gob.afirma.core.AOInvalidFormatException;

/** Clase que obtiene el contenido de un fichero en formato SignedData. de CMS o
 * CADES. */
public final class ObtainContentSignedData {

	private ObtainContentSignedData() {
		// No permitimos la instanciacion
	}

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	/** M&eacute;todo que obtiene el contenido firmado de un tipo Signed Data
	 * tanto en CADES como en CMS. Si la firma no contiene los datos, devuelve <code>null</code>.
	 * @param data Datos que contienen la firma.
	 * @return El contenido firmado o {@code null} si no es una firma con contenido..
	 * @throws AOInvalidFormatException Cuando los datos proporcionados no tienen la estructura
	 *                                  b&aacute;sica de firma ASN.1. */
	public static byte[] obtainData(final byte[] data) throws AOInvalidFormatException {
		byte[] contenido = null;

		ASN1ObjectIdentifier doi;
		ASN1TaggedObject doj;
		try {

			final ASN1Sequence dsq;
			try (
				final ASN1InputStream is = new ASN1InputStream(data);
			) {
				dsq  = (ASN1Sequence) is.readObject();
			}

			final Enumeration<?> e = dsq.getObjects();
			// Elementos que contienen los elementos OID Data
			doi = (ASN1ObjectIdentifier) e.nextElement();
			// Contenido a obtener informacion
			doj = (ASN1TaggedObject) e.nextElement();
		}
		catch (final Exception e) {
			throw new AOInvalidFormatException("Error al parsear la firma ASN.1: " + e, e); //$NON-NLS-1$
		}

		// buscamos si es signedData
		if (doi.equals(PKCSObjectIdentifiers.signedData)) {
			// obtenemos el signed Data
			final SignedData sd = SignedData.getInstance(doj.getObject());
			final ContentInfo ci = sd.getEncapContentInfo();
			// obtenemos el contenido si lo tiene.
			if (ci.getContent() != null) {
				contenido = ((DEROctetString) ci.getContent()).getOctets();
			}
			else {
				LOGGER.info("No existe contenido en esta firma. Se devolvera null"); //$NON-NLS-1$
			}
		}
		else {
			LOGGER.warning("No se puede obtener el contenido de esta firma."); //$NON-NLS-1$
		}

		return contenido;
	}

	/** M&eacute;todo que obtiene la huella digital de los datos firmados en una firma CMS/CAdES.
	 * La huella se obtenida estar&aacute; generada con el algoritmo de huella indicado, si este
	 * algoritmo es el que se utiliz&oacute; en alguna de las operaci&oacute;nes de firma con la
	 * que se gener&oacute; esta firma. Si no se utiliz&oacute; este algoritmo, se devuelve
	 * {@code null}.
	 * @param signature Firma de la que obtener la huella digital.
	 * @param digestAlgorithm Algoritmo con el que se gener&oacute; la huella digital que buscamos.
	 * @return La huella digital de los datos firmados generada con el algoritmo indicado, o
	 * {@code null} si esta no se encuentra en la firma.
	 * @throws IOException Si no se pueden leer los datos */
	public static byte[] obtainMessageDigest(final byte[] signature, final String digestAlgorithm) throws IOException {

		final ASN1Sequence dsq;
		try (
			final ASN1InputStream is = new ASN1InputStream(signature);
		) {
			dsq  = (ASN1Sequence) is.readObject();
		}

		final Enumeration<?> e = dsq.getObjects();

		// Elementos que contienen los elementos OID Data
		final ASN1ObjectIdentifier doi = (ASN1ObjectIdentifier) e.nextElement();

		// Comprobamos que sea una firma
		if (!doi.equals(PKCSObjectIdentifiers.signedData)) {
			LOGGER.warning("No se puede obtener el contenido de esta firma."); //$NON-NLS-1$
			return null;
		}

		// Contenido a obtener informacion
		final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
		final SignedData sd = SignedData.getInstance(doj.getObject());
		final ASN1Set signerInfosSd = sd.getSignerInfos();

		byte[] messageDigest = null;

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
						break;
					}
				}
				if (messageDigest != null) {
					break;
				}
			}
		}
		if (messageDigest == null) {
			LOGGER.warning("No se ha encontrado en la firma una huella digital generada con el algoritmo: " + digestAlgorithm); //$NON-NLS-1$
		}
		return messageDigest;
	}
}

/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.multi.cades;

import java.io.IOException;
import java.security.cert.CertificateEncodingException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;

import org.spongycastle.asn1.ASN1Encodable;
import org.spongycastle.asn1.ASN1EncodableVector;
import org.spongycastle.asn1.ASN1InputStream;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1Primitive;
import org.spongycastle.asn1.ASN1Sequence;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.ASN1TaggedObject;
import org.spongycastle.asn1.BERSet;
import org.spongycastle.asn1.cms.AttributeTable;
import org.spongycastle.asn1.cms.SignedData;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.spongycastle.asn1.x509.Certificate;
import org.spongycastle.cms.CMSException;
import org.spongycastle.cms.CMSSignedData;
import org.spongycastle.cms.SignerInformation;
import org.spongycastle.cms.SignerInformationStore;

import es.gob.afirma.core.SigningLTSException;

/** Utilidades para las multifirmas CAdES. */
public final class CAdESMultiUtil {

	private static final ASN1ObjectIdentifier ARCHIVE_TIMESTAMP_V2_OID = new ASN1ObjectIdentifier(
		"1.2.840.113549.1.9.16.2.48"); //$NON-NLS-1$

	private static final ASN1ObjectIdentifier ARCHIVE_TIMESTAMP_V3_OID = new ASN1ObjectIdentifier(
			"0.4.0.1733.2.4"); //$NON-NLS-1$

	private static final ASN1ObjectIdentifier LONG_TERM_VALIDATION_OID = new ASN1ObjectIdentifier(
			"0.4.0.1733.2.2"); //$NON-NLS-1$

	private static final List<ASN1ObjectIdentifier> UNSUPPORTED_ATTRIBUTES = new ArrayList<>(8);
	static {
		UNSUPPORTED_ATTRIBUTES.add(ARCHIVE_TIMESTAMP_V2_OID);
		UNSUPPORTED_ATTRIBUTES.add(ARCHIVE_TIMESTAMP_V3_OID);
		UNSUPPORTED_ATTRIBUTES.add(LONG_TERM_VALIDATION_OID);
		UNSUPPORTED_ATTRIBUTES.add(PKCSObjectIdentifiers.id_aa_ets_archiveTimestamp);
	}

	private CAdESMultiUtil() {
		// No instanciable
	}

	public static ASN1Set addCertificates(final SignedData sd,
			                       final java.security.cert.Certificate[] certChain) throws CertificateEncodingException,
			                                                                                IOException {
		final ASN1Set certificatesSigned = sd.getCertificates();
		final ASN1EncodableVector vCertsSig = new ASN1EncodableVector();
		final Enumeration<?> certs = certificatesSigned.getObjects();

		// COGEMOS LOS CERTIFICADOS EXISTENTES EN EL FICHERO
		while (certs.hasMoreElements()) {
			vCertsSig.add((ASN1Encodable) certs.nextElement());
		}

		// Y ANADIMOS LOS DE LA NUEVA CADENA
		if (certChain.length != 0) {
			for (final java.security.cert.Certificate element : certChain) {
				vCertsSig.add(Certificate.getInstance(ASN1Primitive.fromByteArray(element.getEncoded())));
			}
		}
		return new BERSet(vCertsSig);
	}

	/** Comprueba si hay atributos no soportados en un <code>SignedData</code> de CMS.
	 * @param signedDataBytes <code>SignedData</code> de CMS.
	 * @throws SigningLTSException Si hay atributos no soportados en el <code>SignedData</code> proporcionado.
	 * @throws IOException Si no se puede tratar el <code>SignedData</code> proporcionado.
	 */
	public static void checkUnsupportedAttributes(final byte[] signedDataBytes) throws SigningLTSException, IOException {
		final CMSSignedData signedData;
		try {
			signedData = new CMSSignedData(signedDataBytes);
		}
		catch (final CMSException e) {
			throw new IOException(
				"La firma proporcionada no es un SignedData compatible CMS: " + e, e //$NON-NLS-1$
			);
		}
		checkUnsupportedAttributes(signedData);
	}

	private static void checkUnsupportedAttributes(final CMSSignedData signedData) throws SigningLTSException {
		final SignerInformationStore signerInfos = signedData.getSignerInfos();
		final Iterator<SignerInformation> signerInfoIt = signerInfos.iterator();
		while (signerInfoIt.hasNext()) {
			checkUnsupportedAttributes(signerInfoIt.next());
		}
	}

    private static void checkUnsupportedAttributes(final SignerInformation signerInformation) throws SigningLTSException {
    	final AttributeTable at = signerInformation.getUnsignedAttributes();
    	if (at != null) {

    		// Identificados si la firma es de archivo
    		for (final ASN1ObjectIdentifier unsupOid : UNSUPPORTED_ATTRIBUTES) {
    			if (at.get(unsupOid) != null) {
        			throw new SigningLTSException(
    					"No se soportan multifirmas de firmas con atributos longevos (encontrado OID=" + unsupOid + ")" //$NON-NLS-1$ //$NON-NLS-2$
    				);
    			}
    		}
    	}
    }

    /**
     * Comprueba si un atributo es de los que impiden agregar nuevas firmas sobre una anterior.
     * @param oid Identificador de atributo.
     * @throws SigningLTSException Cuando el atributo impide agregar nuevas firmas.
     */
    static void checkUnsupported(final ASN1ObjectIdentifier oid) throws  SigningLTSException {
    	for (final ASN1ObjectIdentifier unsupOid : UNSUPPORTED_ATTRIBUTES) {
    		if (unsupOid.equals(oid)) {
    			throw new SigningLTSException(
					"No se soportan multifirmas de firmas con atributos longevos (encontrado OID=" + oid + ")" //$NON-NLS-1$ //$NON-NLS-2$
				);
    		}
    	}
    }

    public static boolean isCounterSignature(final ASN1ObjectIdentifier oid) {
    	return PKCSObjectIdentifiers.pkcs_9_at_counterSignature.equals(oid);
    }


    /**
     * Carga datos firmados.
     * @param signature Datos firmados.
     * @return Estructura de datos firmados.
     * @throws IOException Cuando falla la carga de la firma
     */
	static SignedData readData(final byte[] signature) throws IOException {
		final ASN1Sequence dsq;
		try (
			final ASN1InputStream is = new ASN1InputStream(signature);
		) {
			dsq = (ASN1Sequence) is.readObject();
		}
		final Enumeration<?> e = dsq.getObjects();
		// Elementos que contienen los elementos OID SignedData
		e.nextElement();
		// Contenido de SignedData
		final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
		final ASN1Sequence contentSignedData = (ASN1Sequence) doj.getObject(); // contenido del SignedData

		return SignedData.getInstance(contentSignedData);
	}
}

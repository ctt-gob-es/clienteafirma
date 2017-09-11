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

import java.io.IOException;
import java.security.cert.CertificateEncodingException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import org.spongycastle.asn1.ASN1Encodable;
import org.spongycastle.asn1.ASN1EncodableVector;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1Primitive;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.BERSet;
import org.spongycastle.asn1.cms.AttributeTable;
import org.spongycastle.asn1.cms.SignedData;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.spongycastle.asn1.x509.Certificate;
import org.spongycastle.cms.CMSException;
import org.spongycastle.cms.CMSSignedData;
import org.spongycastle.cms.SignerInformation;

import es.gob.afirma.core.AOFormatFileException;

final class CAdESMultiUtil {

	private static final ASN1ObjectIdentifier ARCHIVE_TIMESTAMP_V2_OID = new ASN1ObjectIdentifier(
		"1.2.840.113549.1.9.16.2.48" //$NON-NLS-1$
	);

	private static final List<ASN1ObjectIdentifier> UNSUPPORTED_ATTRIBUTES = new ArrayList<>(8);
	static {
		UNSUPPORTED_ATTRIBUTES.add(ARCHIVE_TIMESTAMP_V2_OID);
		UNSUPPORTED_ATTRIBUTES.add(PKCSObjectIdentifiers.id_aa_ets_revocationRefs);
		UNSUPPORTED_ATTRIBUTES.add(PKCSObjectIdentifiers.id_aa_ets_archiveTimestamp);
		UNSUPPORTED_ATTRIBUTES.add(PKCSObjectIdentifiers.id_aa_ets_certificateRefs);
		UNSUPPORTED_ATTRIBUTES.add(PKCSObjectIdentifiers.id_aa_ets_revocationValues);
		UNSUPPORTED_ATTRIBUTES.add(PKCSObjectIdentifiers.id_aa_ets_certValues);
		UNSUPPORTED_ATTRIBUTES.add(PKCSObjectIdentifiers.id_aa_ets_escTimeStamp);
	}

	private CAdESMultiUtil() {
		// No instanciable
	}

	static ASN1Set addCertificates(final SignedData sd,
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

	static void checkUnsupportedAttributes(final byte[] signedDataBytes) throws AOFormatFileException, IOException {
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

	private static void checkUnsupportedAttributes(final CMSSignedData signedData) throws AOFormatFileException {
		checkUnsupportedAttributes(
			signedData.getSignerInfos().getSigners().iterator().next()
		);
	}

    private static void checkUnsupportedAttributes(final SignerInformation signerInformation) throws AOFormatFileException {
    	final AttributeTable at = signerInformation.getUnsignedAttributes();
    	if (at != null) {
    		for (final ASN1ObjectIdentifier unsupOid : UNSUPPORTED_ATTRIBUTES) {
    			if (at.get(unsupOid) != null) {
        			throw new AOFormatFileException(
    					"No se soportan multifirmas de firmas con atributos longevos (encontrado OID=" + unsupOid + ")" //$NON-NLS-1$ //$NON-NLS-2$
    				);
    			}
    		}
    	}
    }

    static void checkUnsupported(final ASN1ObjectIdentifier oid) throws AOFormatFileException {
    	for (final ASN1ObjectIdentifier unsupOid : UNSUPPORTED_ATTRIBUTES) {
    		if (unsupOid.equals(oid)) {
    			throw new AOFormatFileException(
					"No se soportan multifirmas de firmas con atributos longevos (encontrado OID=" + oid + ")" //$NON-NLS-1$ //$NON-NLS-2$
				);
    		}
    	}
    }

    static boolean isCounterSignature(final ASN1ObjectIdentifier oid) {
    	return PKCSObjectIdentifiers.pkcs_9_at_counterSignature.equals(oid);
    }
}

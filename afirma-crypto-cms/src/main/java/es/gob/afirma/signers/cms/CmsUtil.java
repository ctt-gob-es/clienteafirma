/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.cms;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.PrivateKey;
import java.security.Signature;
import java.security.SignatureException;

import org.spongycastle.asn1.ASN1Encoding;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1OctetString;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.BEROctetString;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.cms.ContentInfo;
import org.spongycastle.cms.CMSProcessable;
import org.spongycastle.cms.CMSProcessableByteArray;

import es.gob.afirma.core.AOException;

final class CmsUtil {

	private CmsUtil() {
		// No instanciable
	}

    /** Realiza la firma usando los atributos del firmante.
     * @param signatureAlgorithm Algoritmo para la firma.
     * @param key Clave para firmar.
     * @param signedAttr2 Atributos firmados CMS.
     * @return Firma de los atributos.
     * @throws AOException Cuando ocurre cualquier problema durante el proceso */
    static ASN1OctetString firma(final String signatureAlgorithm, final PrivateKey key, final ASN1Set signedAttr2) throws AOException {

        final Signature sig;
        try {
            sig = Signature.getInstance(signatureAlgorithm);
        }
        catch (final Exception e) {
            throw new AOException("Error obteniendo la clase de firma para el algoritmo " + signatureAlgorithm, e); //$NON-NLS-1$
        }

        final byte[] tmp;
        try {
            tmp = signedAttr2.getEncoded(ASN1Encoding.DER);
        }
        catch (final IOException ex) {
            throw new AOException("Error obteniendo los atributos firmados", ex); //$NON-NLS-1$
        }

        // Indicar clave privada para la firma
        try {
            sig.initSign(key);
        }
        catch (final Exception e) {
            throw new AOException("Error al inicializar la firma con la clave privada", e); //$NON-NLS-1$
        }

        // Actualizamos la configuracion de firma
        try {
            sig.update(tmp);
        }
        catch (final SignatureException e) {
            throw new AOException("Error al configurar la informacion de firma", e); //$NON-NLS-1$
        }

        // firmamos.
        final byte[] realSig;
        try {
            realSig = sig.sign();
        }
        catch (final Exception e) {
            throw new AOException("Error durante el proceso de firma", e); //$NON-NLS-1$
        }

        return new DEROctetString(realSig);

    }

    static ContentInfo getContentInfo(final byte[] content2, final boolean omitContent, final String dataType) throws IOException {

        final ASN1ObjectIdentifier contentTypeOID = new ASN1ObjectIdentifier(dataType);

        if (!omitContent) {
            final ByteArrayOutputStream bOut = new ByteArrayOutputStream();
            final CMSProcessable msg = new CMSProcessableByteArray(content2);
            try {
                msg.write(bOut);
            }
            catch (final Exception ex) {
                throw new IOException("Error en la escritura del procesable CMS: " + ex, ex); //$NON-NLS-1$
            }
            return new ContentInfo(contentTypeOID, new BEROctetString(bOut.toByteArray()));
        }
		return new ContentInfo(contentTypeOID, null);
    }

}

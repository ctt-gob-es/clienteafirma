/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.pkcs7;

import java.util.List;
import java.util.logging.Logger;

import org.spongycastle.asn1.ASN1Encodable;
import org.spongycastle.asn1.ASN1EncodableVector;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.BERSet;
import org.spongycastle.asn1.DERNull;
import org.spongycastle.asn1.DERSet;
import org.spongycastle.asn1.cms.AttributeTable;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;

/** Clase que contiene una serie de m&eacute;todos utilizados por GenSignedData,
 * GenCadesSignedData, CoSigner y CounterSigner. */
public final class SigUtils {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private SigUtils() {
        // No permitimos la instanciacion
    }

    /** M&eacute;todo que devuelve el Identificador del algoritmo.
     * @param oid OID del algoritmo a idenfiticar
     * @return El identificador del algoritmo formateado y listo para introducir
     *         en el cms. */
    public static AlgorithmIdentifier makeAlgId(final String oid) {
        return new AlgorithmIdentifier(new ASN1ObjectIdentifier(oid), DERNull.INSTANCE);
    }

    /** Genera un estructura de tipo SET de formato ASN1.
     * @param derObjects Una lista con los objetos a obtener el tipo SET
     * @return Un SET de ASN1 con los elementos de la lista introducida. */
    public static ASN1Set createBerSetFromList(final List<ASN1Encodable> derObjects) {
        final ASN1EncodableVector v = new ASN1EncodableVector();
        for (final ASN1Encodable d : derObjects) {
            v.add(d);
        }
        return new BERSet(v);
    }

    /** Genera un atributo de un SET en formato DER
     * @param attr
     *        Atributo a formatear.
     * @return SET en formato DER del atributo. */
    public static ASN1Set getAttributeSet(final AttributeTable attr) {
        if (attr == null) {
        	LOGGER.warning("Los atributos eran nulos, se devolvera null"); //$NON-NLS-1$
            return null;
        }
        return new DERSet(attr.toASN1EncodableVector());
    }

    /** Genera un estructura de tipo SET de formato ASN1 a partir de una lista de objectos ya existente.
     * @param derObjects
     *        Una lista con los nuevos objetos a obtener el tipo SET
     * @param v Vector con los objectos ya existentes
     * @return Un SET de ASN1 con los elementos de la lista introducida. */
    public static ASN1Set fillRestCerts(final List<ASN1Encodable> derObjects, final ASN1EncodableVector v) {
        for (final ASN1Encodable d : derObjects) {
            v.add(d);
        }
        return new BERSet(v);
    }
}

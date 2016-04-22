/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.envelopers.cms;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.spongycastle.asn1.ASN1Encodable;
import org.spongycastle.asn1.ASN1EncodableVector;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.BERSet;
import org.spongycastle.asn1.DERNull;
import org.spongycastle.asn1.DERPrintableString;
import org.spongycastle.asn1.DERSet;
import org.spongycastle.asn1.cms.Attribute;
import org.spongycastle.asn1.cms.AttributeTable;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;

/** Clase que contiene una serie de m&eacute;todos utilizados por GenSignedData,
 * GenCadesSignedData, CoSigner y CounterSigner. */
final class EvelopUtils {

    private EvelopUtils() {
        // No permitimos la instanciacion
    }

    /** M&eacute;todo que devuelve el Identificador del algoritmo.
     * @param oid OID del algoritmo a idenfiticar
     * @return Identificador del algoritmo formateado y listo para introducir
     *         en el cms. */
    static AlgorithmIdentifier makeAlgId(final String oid) {
        return new AlgorithmIdentifier(new ASN1ObjectIdentifier(oid), DERNull.INSTANCE);
    }

    /** Genera un estructura de tipo SET de formato ASN1.
     * @param derObjects
     *        Una lista con los objetos a obtener el tipo SET
     * @return Un SET de ASN1 con los elementos de la lista introducida. */
    static ASN1Set createBerSetFromList(final List<ASN1Encodable> derObjects) {
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
    static ASN1Set getAttributeSet(final AttributeTable attr) {
        if (attr != null) {
            return new DERSet(attr.toASN1EncodableVector());
        }
        Logger.getLogger("es.gob.afirma").warning("Los atributos eran nulos, se devolvera null"); //$NON-NLS-1$ //$NON-NLS-2$
        return null;
    }

    /** Genera la parte que contiene la informaci&oacute;n del Usuario.
     * Se generan los atributos no firmados.
     * @param uatrib Lista de atributos no firmados que se insertar&aacute;n dentro
     *               del archivo de firma.
     * @return Los atributos no firmados de la firma. */
    static ASN1Set generateUnsignedInfo(final Map<String, byte[]> uatrib) {

        // // ATRIBUTOS

        // agregamos la lista de atributos a mayores.
        if (uatrib.size() != 0) {

            // authenticatedAttributes
            final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();

            final Iterator<Map.Entry<String, byte[]>> it = uatrib.entrySet().iterator();
            while (it.hasNext()) {
                final Map.Entry<String, byte[]> e = it.next();
                contexExpecific.add(new Attribute(
            		// el oid
                    new ASN1ObjectIdentifier(e.getKey().toString()),
                    // el array de bytes en formato string
                    new DERSet(new DERPrintableString(new String(e.getValue())))
                ));
            }
            return EvelopUtils.getAttributeSet(new AttributeTable(contexExpecific));
        }
        return null;
    }
}

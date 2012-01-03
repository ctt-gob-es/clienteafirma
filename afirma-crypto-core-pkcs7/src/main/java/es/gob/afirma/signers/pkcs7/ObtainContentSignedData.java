/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.signers.pkcs7;

import java.util.Enumeration;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.SignedData;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;

/** Clase que obtiene el contenido de un fichero en formato SignedData. de CMS o
 * CADES. */
public final class ObtainContentSignedData {
    
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** M&eacute;todo que obtiene el contenido firmado de un tipo Signed Data
     * tanto en CADES como en CMS. Si la firma no contiene los datos, devuelve <code>null</code>.
     * @param data
     *        datos que contienen la firma.
     * @return el contenido firmado. */
    public static byte[] obtainData(final byte[] data) {
        byte[] contenido = null;
        try {
            final ASN1InputStream is = new ASN1InputStream(data);
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            final ASN1Sequence dsq  = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            // Contenido a obtener informacion
            final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();

            // buscamos si es signedData
            if (doi.equals(PKCSObjectIdentifiers.signedData)) {
                // obtenemos el signed Data
                final SignedData sd = new SignedData((ASN1Sequence) doj.getObject());
                final ContentInfo ci = sd.getEncapContentInfo();
                // obtenemos el contenido si lo tiene.
                if (ci.getContent() != null) {
                    final DEROctetString os = (DEROctetString) ci.getContent();
                    contenido = os.getOctets();
                }
                else {
                    LOGGER.warning("No existe contenido en esta firma."); //$NON-NLS-1$
                }
            }
            else {
                LOGGER.warning("No se puede obtener el contenido de esta firma."); //$NON-NLS-1$
            }

        }
        catch (final Exception e) {
            LOGGER.severe("No se pudieron recuperar los datos contenidos en la firma, se devolvera null: " + e); //$NON-NLS-1$
            return null;
        }

        return contenido;
    }

}

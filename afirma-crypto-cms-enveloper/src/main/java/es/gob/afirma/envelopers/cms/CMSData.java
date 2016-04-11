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

import java.io.IOException;

import org.spongycastle.asn1.ASN1Encoding;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.cms.ContentInfo;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;

/** Clase que implementa firma digital PKCS#7/CMS Data. La Estructura del mensaje
 * es la siguiente:<br>
 *
 * <pre>
 * <code>
 *
 *  id-data OBJECT IDENTIFIER ::= { iso(1) member-body(2)
 *         us(840) rsadsi(113549) pkcs(1) pkcs7(7) 1 }
 *
 *  Data ::= OCTET STRING
 *
 * </code>
 * </pre>
 *
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un mensaje Data de SpongyCastle. */

final class CMSData {

	private CMSData() {
		// No permitimos la instanciacion
	}

    /** M&eacute;odo que genera una estructura CMS de tipo Data.
     * @param content
     *        Datos que se desean envolver.
     * @return El envoltorio de tipo data.
     * @throws IOException En caso de error en la lectura o tratamiento de datos */
    static byte[] genData(final byte[] content) throws IOException {
        return new ContentInfo(PKCSObjectIdentifiers.data, new DEROctetString(content)).getEncoded(ASN1Encoding.DER);
    }

}

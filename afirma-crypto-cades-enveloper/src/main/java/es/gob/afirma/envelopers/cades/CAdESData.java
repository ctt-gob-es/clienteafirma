/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.envelopers.cades;

import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;

import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;

/** Clase que implementa firma digital Data de CADES, que se basa en PKCS#7/CMS
 * Data. La Estructura del mensaje es la siguiente:<br>
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
 * para crear un mensaje Data de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> */

final class CAdESData {

    /** M&eacute;odo que genera una firma digital usando el sitema conocido como
     * Data y que consiste en el contenido del fichero codificado como un
     * conjunto de bytes.
     * @param parameters
     *        Par&aacute;metros necesarios para obtener los datos de
     *        SignedData.
     * @return El contenido del fichero en formato Data. */
    byte[] genData(final P7ContentSignerParameters parameters) {

        // construimos el Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.data, new DEROctetString(parameters.getContent())).getDEREncoded();
    }

}

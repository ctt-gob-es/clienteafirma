/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.envelopers.cms;

import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;

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
 * para crear un mensaje Data de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> */

final class CMSData {

    /** M&eacute;odo que genera una estructura CMS de tipo Data.
     * @param content
     *        Datos que se desean envolver.
     * @return El envoltorio de tipo data. */
    byte[] genData(final byte[] content) {
        return new ContentInfo(PKCSObjectIdentifiers.data, new DEROctetString(content)).getDEREncoded();
    }

}

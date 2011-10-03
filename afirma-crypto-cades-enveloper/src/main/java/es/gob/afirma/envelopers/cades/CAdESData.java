/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

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

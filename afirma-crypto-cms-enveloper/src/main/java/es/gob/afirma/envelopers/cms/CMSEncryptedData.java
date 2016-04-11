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
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.util.Map;
import java.util.logging.Logger;

import org.spongycastle.asn1.ASN1Encoding;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.cms.ContentInfo;
import org.spongycastle.asn1.cms.EncryptedContentInfo;
import org.spongycastle.asn1.cms.EncryptedData;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;

import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.signers.AOSignConstants;


/** Clase que implementa firma digital PKCS#7/CMS EncryptedData. La Estructura
 * del mensaje es la siguiente:<br>
 *
 * <pre>
 * <code>
 *
 *  id-encryptedData OBJECT IDENTIFIER ::= { iso(1) member-body(2)
 *          us(840) rsadsi(113549) pkcs(1) pkcs7(7) 6 }
 *
 *  EncryptedData ::= SEQUENCE {
 *        version CMSVersion,
 *        encryptedContentInfo EncryptedContentInfo,
 *        unprotectedAttrs [1] IMPLICIT UnprotectedAttributes OPTIONAL }
 *
 * </code>
 * </pre>
 *
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un mensaje EncryptedData de SpongyCastle. */

final class CMSEncryptedData {

	private CMSEncryptedData() {
		// No permitimos la instanciacion
	}

    /** M&eacute;todo principal que genera la firma de tipo EncryptedData.
     * @param data
     *        Datos que queremos envolver.
     * @param digAlg
     *        Algoritmo para realizar el Digest.
     * @param config
     *        Configuraci&oacute;n del algoritmo para cifrar.
     * @param cipherKey
     *        Clave de cifrado.
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @param uatrib
     *        Conjunto de atributos no firmados.
     * @return la firma de tipo EncryptedData.
     * @throws java.security.NoSuchAlgorithmException
     *         Si no se soporta alguno de los algoritmos de firma o huella
     *         digital
     * @throws IOException
     *         Cuando se produce algun error al codificar los datos.
     */
    static byte[] genEncryptedData(final byte[] data,
    		                       final String digAlg,
    		                       final AOCipherConfig config,
    		                       final Key cipherKey,
    		                       final String dataType,
    		                       final Map<String, byte[]> uatrib) throws NoSuchAlgorithmException, IOException {

        // Datos previos &uacute;tiles
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(digAlg);

        // generamos el contenedor de cifrado
        EncryptedContentInfo encInfo = null;
        try {
            // 3. ENCRIPTEDCONTENTINFO
            encInfo = Utils.getEncryptedContentInfo(data, cipherKey, config);
        }
        catch (final Exception ex) {
            Logger.getLogger("es.gob.afirma").severe("Error durante el proceso cifrado: " + ex); //$NON-NLS-1$ //$NON-NLS-2$
        }

        // 4. ATRIBUTOS
        // obtenemos la lista de certificados
        ASN1Set unprotectedAttrs = null;
        unprotectedAttrs = Utils.generateSignerInfo(digestAlgorithm, data, dataType, uatrib);

        // construimos el Enveloped Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.encryptedData, new EncryptedData(encInfo, unprotectedAttrs)).getEncoded(ASN1Encoding.DER);
    }

}

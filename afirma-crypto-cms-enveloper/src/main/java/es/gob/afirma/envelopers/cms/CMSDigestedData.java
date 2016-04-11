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
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import org.spongycastle.asn1.ASN1Encoding;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.cms.ContentInfo;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.DigestedData;

/** Clase base para la implementaci&oacute;n del tipo DigestedData La Estructura
 * del mensaje es la siguiente:<br>
 *
 * <pre>
 * <code>
 *  DigestedData ::= SEQUENCE {
 *        version CMSVersion,
 *        digestAlgorithm DigestAlgorithmIdentifier,
 *        encapContentInfo EncapsulatedContentInfo,
 *        digest Digest }
 *
 *  Digest ::= OCTET STRING
 * </code>
 * </pre>
 *
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un mensaje DigestedData de SpongyCastle. */
final class CMSDigestedData {

	private CMSDigestedData() {
		// No permitimos la instanciacion
	}

    /** Genera una estructura de tipo digestedData.
     * @param content
     *        Contenido original
     * @param digestAlgorithm
     *        Algoritmo de huella digital (<i>digest</i>) a usar
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @return Mensaje firmado en tipo Digested Data.
     * @throws java.security.NoSuchAlgorithmException
     *         Si no se soporta alguno de los algoritmos de firma o huella
     *         digital
     * @throws java.io.IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos */
    static byte[] genDigestedData(final byte[] content,
    		                      final String digestAlgorithm,
    		                      final String dataType) throws NoSuchAlgorithmException, IOException {

        // Obtenemos el algoritmo para hacer el digest
        final AlgorithmIdentifier digAlgId = EvelopUtils.makeAlgId(
                AOAlgorithmID.getOID(AOSignConstants.getDigestAlgorithmName(digestAlgorithm)));

        // indicamos el tipo de contenido
        final ASN1ObjectIdentifier contentTypeOID = new ASN1ObjectIdentifier(dataType);
        final ContentInfo encInfo = new ContentInfo(contentTypeOID, null);

        // digest
        final DEROctetString digest = new DEROctetString(MessageDigest.getInstance(digestAlgorithm).digest(content));

        // construimos el digestedData.
        return new ContentInfo(PKCSObjectIdentifiers.digestedData, new DigestedData(digAlgId, encInfo, digest)).getEncoded(ASN1Encoding.DER);
    }
}

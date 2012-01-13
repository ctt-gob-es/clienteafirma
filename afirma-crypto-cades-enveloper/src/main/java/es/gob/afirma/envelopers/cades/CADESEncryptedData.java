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

import java.io.InputStream;
import java.security.NoSuchAlgorithmException;

import javax.crypto.SecretKey;

import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.EncryptedData;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pkcs7.SigUtils;

/** Clase que implementa firma digital CADES EncryptedData basado en PKCS#7/CMS
 * EncryptedData. La Estructura del mensaje es la siguiente:<br>
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
 * para crear un mensaje EncryptedData de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> */
final class CADESEncryptedData {
	
	private CADESEncryptedData() {
		// No permitimos la instanciacion
	}

    /** M&eacute;todo principal que genera la firma de tipo EncryptedData.
     * @param file
     *        Archivo espec&iacute;fico a cifrar.
     * @param digAlg
     *        ALgoritmo para realizar el Digest.
     * @param config
     *        Configuraci&oacute;n del algoritmo para firmar.
     * @param pass
     *        Cadena que se usará paa cifrar los datos.
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @return la firma de tipo EncryptedData.
     * @throws java.security.NoSuchAlgorithmException
     *         Si no se soporta alguno de los algoritmos de firma o huella
     *         digital */
    static byte[] genEncryptedData(final InputStream file, 
                            final String digAlg, 
                            final AOCipherConfig config, 
                            final String pass, 
                            final String dataType) throws NoSuchAlgorithmException, AOException {
        
        

        final byte[] codeFile;
        try {
            codeFile = AOUtil.getDataFromInputStream(file);
        }
        catch(final Exception e) {
            throw new AOException("No se ha podido leer el fujo de entrada", e); //$NON-NLS-1$
        }

        // Asignamos la clave de cifrado
        final SecretKey cipherKey = CAdESUtils.assignKey(config, pass);

        // Datos previos &uacute;tiles
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(digAlg);

        // generamos el contenedor de cifrado
        final EncryptedContentInfo encInfo;
        try {
            // 3. ENCRIPTEDCONTENTINFO
            encInfo = CAdESUtils.getEncryptedContentInfo(codeFile, config, cipherKey);
        }
        catch (final Exception ex) {
            throw new AOException("Error durante el proceso de cifrado", ex); //$NON-NLS-1$
        }

        // 4. ATRIBUTOS
        // obtenemos la lista de certificados
        final ASN1Set unprotectedAttrs = SigUtils.getAttributeSet(new AttributeTable(CAdESUtils.initContexExpecific(digestAlgorithm, codeFile, dataType, null)));

        // construimos el Enveloped Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.encryptedData, new EncryptedData(encInfo, unprotectedAttrs)).getDEREncoded();

    }

}

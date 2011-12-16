/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either versión 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.envelopers.cms;

import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.util.Map;
import java.util.logging.Logger;

import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.EncryptedData;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.ietf.jgss.Oid;

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
 * para crear un mensaje EncryptedData de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> */

final class CMSEncryptedData {

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
     *         digital */
    byte[] genEncryptedData(final byte[] data, final String digAlg, final AOCipherConfig config, final Key cipherKey, final Oid dataType, final Map<Oid, byte[]> uatrib) throws NoSuchAlgorithmException {

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
        return new ContentInfo(PKCSObjectIdentifiers.encryptedData, new EncryptedData(encInfo, unprotectedAttrs)).getDEREncoded();
    }

    // /**
    // * Asigna la clave para firmar el contenido del fichero que queremos
    // envolver
    // * y qeu m&aacute;s tarde ser&aacute; cifrada con la clave p&uacute;blica
    // del usuario que
    // * hace la firma.
    // *
    // * @param config Configuraci&oacute;n necesaria para crear la clave.
    // * @param key Contrase&ntilde;a que se va a usar para cifrar.
    // */
    // private void assignKey(AOCipherConfig config, String key){
    //
    // // Generamos la clave necesaria para el cifrado
    // if ((config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHMD5ANDDES)) ||
    // (config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHSHA1ANDDESEDE)) ||
    // (config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHSHA1ANDRC2_40))){
    // try {
    // this.cipherKey =
    // SecretKeyFactory.getInstance(config.getAlgorithm().getName())
    // .generateSecret(new PBEKeySpec(key.toCharArray(), SALT,
    // ITERATION_COUNT));
    // } catch (Exception ex) {
    // Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de asignacion de la clave (a partir de password): "
    // + ex);
    // }
    // }
    // else{
    // try {
    // this.cipherKey = new SecretKeySpec(new BASE64Decoder().decodeBuffer(key),
    // config.getAlgorithm().getName());
    // } catch (IOException ex) {
    // Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de asignacion de la clave (a partir de key): "
    // + ex);
    // }
    // }
    //
    //
    // }

    // /**
    // * Genera el proveedor de cifrado.
    // *
    // * @param providerName Nombre del proveedor.
    // * @return El proveedor.
    // * @throws java.security.NoSuchProviderException
    // */
    // private static Provider getProvider(String providerName)
    // throws NoSuchProviderException
    // {
    // if (providerName != null)
    // {
    // Provider prov = Security.getProvider(providerName);
    // if (prov != null)
    // {
    // return prov;
    // }
    // throw new NoSuchProviderException("provider " + providerName +
    // " not found.");
    // }
    // return null;
    // }
}

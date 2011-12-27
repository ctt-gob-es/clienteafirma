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

import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.Enumeration;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;

import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.EncryptedData;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.util.encoders.Base64;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;

/** Clase que descifra el contenido de un fichero en formato EncryptedData de
 * CMS.
 * Se usa para ello una clave del usuario. */
final class CMSDecipherEncryptedData {

    /** Clave de cifrado. La almacenamos internamente porque no hay forma de
     * mostrarla directamente al usuario. */
    private SecretKey cipherKey;

    private AOCipherConfig config;

    /** M&eacute;todo principal que descifra datos del tipo de EncryptedData.
     * @param encryptedData
     *        Datos del tipo CMS EncryptedData.
     * @param pass
     *        Contrase&ntilde;a o clave que se uso para cifrar los datos.
     * @return Datos sin encriptar.
     * @throws AOException
     *         Cuando ocurre un error durante el proceso de descifrado
     *         (formato o clave incorrecto,...)
     * @throws InvalidKeyException
     *         Cuando se proporciona una clave incorrecta para el
     *         descifrado. 
     * @throws BadPaddingException 
     * @throws IllegalBlockSizeException 
     * @throws InvalidAlgorithmParameterException 
     * @throws NoSuchPaddingException 
     * @throws NoSuchAlgorithmException */
    @SuppressWarnings("unused")
    byte[] dechiperEncryptedData(final byte[] encryptedData, final String pass) throws AOException, 
                                                                                       InvalidKeyException, 
                                                                                       NoSuchAlgorithmException, 
                                                                                       NoSuchPaddingException, 
                                                                                       InvalidAlgorithmParameterException, 
                                                                                       IllegalBlockSizeException, 
                                                                                       BadPaddingException {

        AlgorithmIdentifier alg = null;
        EncryptedContentInfo eci = null;

        // donde se guardara el resultad.
        final byte[] deciphered;

        try {
            final ASN1Sequence contentEncryptedData = Utils.fetchWrappedData(encryptedData);

            // Obtenemos los datos del encryptedData.
            final Enumeration<?> e2 = contentEncryptedData.getObjects();
            // version
            e2.nextElement();
            // EncryptedContentInfo. donde está lo que necesitamos.
            eci = EncryptedContentInfo.getInstance(e2.nextElement());

            // Obtenemos el agoritmo de cifrado
            alg = eci.getContentEncryptionAlgorithm();

            // Se intenta obtener el encrypted data.
            // Si no puede convertirse, dara error.
            // "EncryptedData EncryptedData" no se usara. solo es para verificar
            // que es de este tipo.
            new EncryptedData(eci);
        }
        catch (final Exception ex) {
            throw new AOException("El fichero no contiene un tipo EncryptedData", ex); //$NON-NLS-1$
        }

        // asignamos la clave de descifrado a partir del algoritmo.
        assignKey(alg, pass);

        // Obtenemos el contenido cifrado.
        final byte[] contCifrado = eci.getEncryptedContent().getOctets();

        // Desciframos.
        return Utils.deCipherContent(contCifrado, this.config, this.cipherKey);

    }

    /** Asigna la clave para firmar el contenido del fichero que queremos
     * envolver y qeu m&aacute;s tarde ser&aacute; cifrada con la clave
     * p&uacute;blica del usuario que hace la firma.
     * @param alg
     *        Algoritmo necesario para crear la clave.
     * @param key
     *        Contrase&ntilde;a que se va a usar para cifrar.
     * @throws AOException
     *         Cuando la clave o password no son v&aacute;lidas. */
    private void assignKey(final AlgorithmIdentifier alg, final String key) throws AOException {

        // obtenemos el oid del algoritmo.
        final String algoritmoOid = alg.getAlgorithm().toString();

        AOCipherAlgorithm algorithm = null;
        AOCipherAlgorithm aux = null;

        // A partir de los tipos de algoritmos, buscamos el que coincida
        // con el oid de cifrado obtenido del fichero de firma.
        final AOCipherAlgorithm[] algoritmos = AOCipherAlgorithm.values();
        for (final AOCipherAlgorithm algoritmo : algoritmos) {
            aux = algoritmo;
            if (aux.getOid().equals(algoritmoOid)) {
                algorithm = aux;
            }
        }
        // Creamos una configuraci&oacute;n partir del algoritmo.
        this.config = new AOCipherConfig(algorithm, null, null);

        // Generamos la clave necesaria para el cifrado
        if ((this.config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHMD5ANDDES)) || (this.config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHSHA1ANDDESEDE))
            || (this.config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHSHA1ANDRC2_40))) {
            try {
                this.cipherKey = Utils.loadCipherKey(this.config, key);
            }
            catch (final Exception ex) {
                throw new AOException("Error durante el proceso de asignacion de la clave (a partir de password)", ex); //$NON-NLS-1$
            }
        }
        else {
            try {
                this.cipherKey = new SecretKeySpec(Base64.decode(key), this.config.getAlgorithm().getName());
            }
            catch (final Exception ex) {
                throw new AOException("Error durante el proceso de asignacion de la clave (a partir de key)", ex); //$NON-NLS-1$
            }
        }
    }

}

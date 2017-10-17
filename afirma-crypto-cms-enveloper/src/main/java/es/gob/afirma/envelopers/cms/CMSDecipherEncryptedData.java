/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.envelopers.cms;

import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.util.Enumeration;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;

import org.spongycastle.asn1.ASN1Sequence;
import org.spongycastle.asn1.cms.EncryptedContentInfo;
import org.spongycastle.asn1.cms.EncryptedData;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.misc.Base64;

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
     * @throws BadPaddingException Cuando hay problemas con un relleno de datos.
     * @throws IllegalBlockSizeException Cuando hay problemas internos con los tama&ntilde;os de bloque de cifrado.
     * @throws InvalidAlgorithmParameterException Si no se soporta un par&aacute;metro necesario para un algoritmo.
     * @throws NoSuchPaddingException Cuando no se soporta un tipo de relleno necesario.
     * @throws NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario
     * @throws IOException En caso de error en la lectura o tratamiento de datos
     * @throws InvalidKeySpecException Cuando ocurren problemas relacionados con la estructura interna de las claves */
    @SuppressWarnings("unused")
    byte[] dechiperEncryptedData(final byte[] encryptedData, final String pass) throws AOException,
                                                                                       InvalidKeyException,
                                                                                       NoSuchAlgorithmException,
                                                                                       NoSuchPaddingException,
                                                                                       InvalidAlgorithmParameterException,
                                                                                       IllegalBlockSizeException,
                                                                                       BadPaddingException,
                                                                                       InvalidKeySpecException,
                                                                                       IOException {

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
            // EncryptedContentInfo. donde esta lo que necesitamos.
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
     * @throws NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario
     * @throws InvalidKeySpecException Cuando ocurren problemas relacionados con la estructura interna de las claves
     * @throws IOException En caso de error en la lectura o tratamiento de datos */
    private void assignKey(final AlgorithmIdentifier alg, final String key) throws InvalidKeySpecException, NoSuchAlgorithmException, IOException {

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
        if (this.config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHMD5ANDDES) ||
            this.config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHSHA1ANDDESEDE) ||
            this.config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHSHA1ANDRC2_40)) {
                    this.cipherKey = Utils.loadCipherKey(this.config, key);
        }
        else {
           this.cipherKey = new SecretKeySpec(Base64.decode(key), this.config.getAlgorithm().getName());
        }
    }

}

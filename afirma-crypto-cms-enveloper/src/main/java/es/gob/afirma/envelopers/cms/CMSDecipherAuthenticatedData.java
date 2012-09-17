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
import java.security.InvalidKeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Enumeration;

import javax.crypto.Cipher;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;

import org.bouncycastle.asn1.ASN1Encoding;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.cms.AuthenticatedData;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;


/** Clase que extrae el contenido de un fichero en formato AuthenticatedData. de
 * CMS. */
final class CMSDecipherAuthenticatedData {

    /** Clave de cifrado. La almacenamos internamente porque no hay forma de
     * mostrarla directamente al usuario. */
    private SecretKey cipherKey;
    private AOCipherAlgorithm macAlgorithmConfig;

    /** @param cmsData
     *        Datos del tipo EnvelopedData.
     * @param keyEntry
     *        Clave privada del certificado usado para descifrar el
     *        contenido.
     * @return El contenido de una firma de tipo authenticatedData.
     * @throws IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos
     * @throws CertificateEncodingException
     *         Si se produce alguna excepci&oacute;n con los certificados de
     *         firma.
     * @throws AOException
     *         Cuando ocurre un error durante el proceso de descifrado
     *         (formato o clave incorrecto,...)
     * @throws AOInvalidRecipientException
     *         Cuando se indica un certificado que no est&aacute; entre los
     *         destinatarios del sobre.
     * @throws InvalidKeyException
     *         Cuando la clave almacenada en el sobre no es v&aacute;lida.
     * @throws NoSuchAlgorithmException
     *         Cuando no se reconozca el algoritmo utilizado para generar el
     *         c&oacute;digo de autenticaci&oacute;n.
     */
    byte[] decipherAuthenticatedData(final byte[] cmsData, final PrivateKeyEntry keyEntry) throws IOException,
                                                                                     CertificateEncodingException,
                                                                                     AOException,
                                                                                     InvalidKeyException, NoSuchAlgorithmException {
        byte[] contenido = new byte[0];

        AuthenticatedData authenticated = null;

        final Enumeration<?> elementRecipient;
        try {
            final ASN1Sequence authenticatedData = Utils.fetchWrappedData(cmsData);

            authenticated = AuthenticatedData.getInstance(authenticatedData);
            elementRecipient = authenticated.getRecipientInfos().getObjects();
        }
        catch (final Exception ex) {
            throw new AOException("El fichero no contiene un tipo EnvelopedData", ex); //$NON-NLS-1$
        }

        final X509Certificate userCert = (X509Certificate) keyEntry.getCertificate();
        final EncryptedKeyDatas encryptedKeyDatas = Utils.fetchEncryptedKeyDatas(userCert, elementRecipient);

        // Asignamos la clave de descifrado del contenido.
        assignKey(encryptedKeyDatas.getEncryptedKey(), keyEntry, encryptedKeyDatas.getAlgEncryptedKey());

        final ASN1Set authAttr = authenticated.getAuthAttrs();

        final byte[] macGenerada = Utils.genMac(this.macAlgorithmConfig.getName(), authAttr.getEncoded(ASN1Encoding.DER), this.cipherKey);

        final byte[] macObtenida = authenticated.getMac().getOctets();

        if (java.util.Arrays.equals(macGenerada, macObtenida)) {
            contenido = ((DEROctetString) authenticated.getEncapsulatedContentInfo().getContent()).getOctets();
        }

        return contenido;
    }

    /** Asigna la clave para firmar el contenido del fichero que queremos
     * envolver y que m&aacute;s tarde ser&aacute; cifrada con la clave
     * p&uacute;blica del usuario que hace la firma.
     * @param passCiphered
     *        Clave cifrada.
     * @param keyEntry
     *        Contrase&ntilde;a que se va a usar para descifrar.
     * @param algClave
     *        Algoritmo necesario para crear la clave.
     * @throws AOException
     *         Cuando no se pudo descifrar la clave con el certificado de
     *         usuario. */
    private void assignKey(final byte[] passCiphered, final PrivateKeyEntry keyEntry, final AlgorithmIdentifier algClave) throws AOException {

        AOCipherAlgorithm algorithmConfig = null;

        // obtenemos el algoritmo usado para cifrar la pass
        for (final AOCipherAlgorithm config : AOCipherAlgorithm.values()) {
            if (config.getOid().equals(algClave.getAlgorithm().toString())) {
                algorithmConfig = config;
                break;
            }
        }

        if (algorithmConfig == null) {
            throw new AOException("No se ha podido obtener el algoritmo para cifrar la contrasena"); //$NON-NLS-1$
        }

        this.macAlgorithmConfig = algorithmConfig;

        // Desembolvemos la clave usada para cifrar el contenido
        // a partir de la clave privada del certificado del usuario.
        try {
            final Cipher cipher = createCipher(keyEntry.getPrivateKey().getAlgorithm());
            cipher.init(Cipher.UNWRAP_MODE, keyEntry.getPrivateKey());
            this.cipherKey = (SecretKey) cipher.unwrap(passCiphered, algorithmConfig.getName(), Cipher.SECRET_KEY);
        }
        catch (final Exception e) {
            throw new AOException("Error al recuperar la clave de cifrado del sobre autenticado", e); //$NON-NLS-1$
        }
    }

    /** Crea el cifrador usado para cifrar tanto el fichero como la clave usada
     * para cifrar dicho fichero.
     * @param algName
     *        algoritmo utilizado para cifrar.
     * @return Cifrador.
     * @throws java.security.NoSuchAlgorithmException
     * @throws javax.crypto.NoSuchPaddingException */
    private static Cipher createCipher(final String algName) throws NoSuchAlgorithmException, NoSuchPaddingException {
        return Cipher.getInstance(algName);
    }
}

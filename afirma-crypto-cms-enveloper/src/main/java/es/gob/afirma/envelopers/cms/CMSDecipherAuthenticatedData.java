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

import org.spongycastle.asn1.ASN1Encoding;
import org.spongycastle.asn1.ASN1Sequence;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.cms.AuthenticatedData;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;

/** Extractor de contenido de un fichero en formato AuthenticatedData de CMS. */
final class CMSDecipherAuthenticatedData {

    /** Clave de cifrado. La almacenamos internamente porque no hay forma de
     * mostrarla directamente al usuario. */
    private SecretKey cipherKey;

    private AOCipherAlgorithm macAlgorithmConfig;

    /** Descifra un PKCS#7 <code>AuthenticatedData</code>.
     * @param cmsData Datos del tipo EnvelopedData.
     * @param keyEntry Clave privada del certificado usado para descifrar el contenido.
     * @return El contenido de una firma de tipo authenticatedData.
     * @throws IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos.
     * @throws CertificateEncodingException Si se produce alguna excepci&oacute;n con los certificados de firma.
     * @throws AOException Cuando ocurre un error durante el proceso de descifrado
     *                     (formato o clave incorrecto,...)
     * @throws AOInvalidRecipientException Cuando se indica un certificado que no est&aacute; entre los
     *                                     destinatarios del sobre.
     * @throws InvalidKeyException Cuando la clave almacenada en el sobre no es v&aacute;lida.
     * @throws NoSuchAlgorithmException Cuando no se reconozca el algoritmo utilizado para generar el
     *                                  c&oacute;digo de autenticaci&oacute;n.
     * @throws NoSuchPaddingException Cuando no se soporta un tipo de relleno necesario. */
    byte[] decipherAuthenticatedData(final byte[] cmsData,
    		                         final PrivateKeyEntry keyEntry) throws IOException,
                                                                            CertificateEncodingException,
                                                                            AOException,
                                                                            InvalidKeyException,
                                                                            NoSuchAlgorithmException,
                                                                            NoSuchPaddingException {
        final AuthenticatedData authenticated;

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

        final byte[] macGenerada = Utils.genMac(
        	this.macAlgorithmConfig.getName(),
        	authAttr.getEncoded(ASN1Encoding.DER),
        	this.cipherKey
    	);

        final byte[] macObtenida = authenticated.getMac().getOctets();

        if (java.util.Arrays.equals(macGenerada, macObtenida)) {
            return ((DEROctetString) authenticated.getEncapsulatedContentInfo().getContent()).getOctets();
        }

        return new byte[0];
    }

    /** Asigna la clave para firmar el contenido del fichero que queremos
     * envolver y que m&aacute;s tarde ser&aacute; cifrada con la clave
     * p&uacute;blica del usuario que hace la firma.
     * @param passCiphered Clave cifrada.
     * @param keyEntry Contrase&ntilde;a que se va a usar para descifrar.
     * @param algClave Algoritmo necesario para crear la clave.
     * @throws InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave.
     * @throws NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario
     * @throws NoSuchPaddingException Cuando no se soporta un tipo de relleno necesario. */
    private void assignKey(final byte[] passCiphered,
    		               final PrivateKeyEntry keyEntry,
    		               final AlgorithmIdentifier algClave) throws InvalidKeyException,
    		                                                          NoSuchAlgorithmException,
    		                                                          NoSuchPaddingException {
        AOCipherAlgorithm algorithmConfig = null;
        final String currentAlgorithm = algClave.getAlgorithm().toString();

        // obtenemos el algoritmo usado para cifrar la pass
        for (final AOCipherAlgorithm config : AOCipherAlgorithm.values()) {
            if (config.getOid().equals(currentAlgorithm)) {
                algorithmConfig = config;
                break;
            }
        }

        if (algorithmConfig == null) {
            throw new NoSuchAlgorithmException(
        		"No se ha podido obtener el algoritmo para cifrar la contrasena: " + currentAlgorithm //$NON-NLS-1$
    		);
        }

        this.macAlgorithmConfig = algorithmConfig;

        // Desembolvemos la clave usada para cifrar el contenido
        // a partir de la clave privada del certificado del usuario.
        final Cipher cipher = createCipher(keyEntry.getPrivateKey().getAlgorithm());
        cipher.init(Cipher.UNWRAP_MODE, keyEntry.getPrivateKey());
        this.cipherKey = (SecretKey) cipher.unwrap(passCiphered, algorithmConfig.getName(), Cipher.SECRET_KEY);
    }

    /** Crea el cifrador usado para cifrar tanto el fichero como la clave usada
     * para cifrar dicho fichero.
     * @param algName algoritmo utilizado para cifrar.
     * @return Cifrador.
     * @throws java.security.NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario
     * @throws javax.crypto.NoSuchPaddingException Cuando no se soporta un tipo de relleno necesario. */
    private static Cipher createCipher(final String algName) throws NoSuchAlgorithmException, NoSuchPaddingException {
        return Cipher.getInstance(algName);
    }
}

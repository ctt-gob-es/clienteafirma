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
import java.security.InvalidKeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Enumeration;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;

import org.spongycastle.asn1.ASN1Encoding;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.cms.AuthenticatedData;

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
     * @throws NoSuchPaddingException Cuando no se soporta un tipo de relleno necesario.
     * @throws BadPaddingException Si el relleno a usar en la desenvoltura es distinto al que se us&oacute;
     *                             en la envoltura.
     * @throws IllegalBlockSizeException Si el tipo de bloque a usar en la desenvoltura es distinto al que se us&oacute;
     *                                   en la envoltura. */
    byte[] decipherAuthenticatedData(final byte[] cmsData,
    		                         final PrivateKeyEntry keyEntry) throws IOException,
                                                                            CertificateEncodingException,
                                                                            AOException,
                                                                            InvalidKeyException,
                                                                            NoSuchAlgorithmException,
                                                                            NoSuchPaddingException,
                                                                            IllegalBlockSizeException,
                                                                            BadPaddingException {
        final AuthenticatedData authenticated;

        final Enumeration<?> elementRecipient;
        try {
            authenticated = AuthenticatedData.getInstance(
        		Utils.fetchWrappedData(cmsData)
    		);
            elementRecipient = authenticated.getRecipientInfos().getObjects();
        }
        catch (final Exception ex) {
            throw new AOException("El fichero no contiene un tipo EnvelopedData", ex); //$NON-NLS-1$
        }

        final X509Certificate userCert = (X509Certificate) keyEntry.getCertificate();
        final EncryptedKeyDatas encryptedKeyDatas = Utils.fetchEncryptedKeyDatas(userCert, elementRecipient);

        // Asignamos la clave de descifrado del contenido.
        Utils.assignKey(encryptedKeyDatas.getEncryptedKey(), keyEntry, encryptedKeyDatas.getAlgEncryptedKey());

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

}

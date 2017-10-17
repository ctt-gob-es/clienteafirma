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
import java.security.KeyStore.PrivateKeyEntry;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Enumeration;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;

import org.spongycastle.asn1.cms.AuthEnvelopedData;
import org.spongycastle.asn1.cms.EncryptedContentInfo;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;

import es.gob.afirma.core.AOException;


/** Clase que descifra el contenido de un fichero en formato
 * AuthenticatedEnvelopedData. de CMS.
 * Se usa para ello una clave del usuario. */
public final class CMSDecipherAuthenticatedEnvelopedData {

	private CMSDecipherAuthenticatedEnvelopedData() {
		// No permitimos la instanciacion
	}

    /** Descifra el contenido de un <code>CMSAuthenticatedEnvelopedData</code>.
     * @param cmsData Datos del tipo AuthenticatedEnvelopedData para obtener los
     *                datos cifrados.
     * @param keyEntry Clave privada del certificado usado para descifrar el
     *                 contenido.
     * @return El contenido descifrado del EnvelopedData.
     * @throws IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *                     datos
     * @throws CertificateEncodingException Si se produce alguna excepci&oacute;n con los certificados de
     *                                      firma.
     * @throws AOException Cuando ocurre un error durante el proceso de descifrado
     *                     (formato o clave incorrecto,...)
     * @throws AOInvalidRecipientException Cuando se indica un certificado que no est&aacute; entre los
     *                                     destinatarios del sobre.
     * @throws InvalidKeyException Cuando la clave almacenada en el sobre no es v&aacute;lida.
     * @throws NoSuchPaddingException Cuando no se soporta un tipo de relleno necesario.
     * @throws NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario
     * @throws BadPaddingException Cuando hay problemas con un relleno de datos.
     * @throws IllegalBlockSizeException Cuando hay problemas internos con los tama&ntilde;os de bloque de cifrado.
     * @throws InvalidAlgorithmParameterException Si no se soporta un par&aacute;metro necesario para un algoritmo. */
    public static byte[] dechiperAuthenticatedEnvelopedData(final byte[] cmsData,
    		                                                final PrivateKeyEntry keyEntry) throws IOException,
                                                                                                   CertificateEncodingException,
                                                                                                   AOException,
                                                                                                   InvalidKeyException,
                                                                                                   NoSuchAlgorithmException,
                                                                                                   NoSuchPaddingException,
                                                                                                   InvalidAlgorithmParameterException,
                                                                                                   IllegalBlockSizeException,
                                                                                                   BadPaddingException {
        // Contendra el contenido a tratar.
        final AuthEnvelopedData authEnvelopedData;

        final Enumeration<?> elementRecipient;
        try {
            authEnvelopedData = AuthEnvelopedData.getInstance(Utils.fetchWrappedData(cmsData));
            elementRecipient = authEnvelopedData.getRecipientInfos().getObjects();
        }
        catch (final Exception ex) {
            throw new AOException("El fichero no contiene un tipo AuthenticatedEnvelopedData", ex); //$NON-NLS-1$
        }

        final EncryptedKeyDatas encryptedKeyDatas = Utils.fetchEncryptedKeyDatas(
    		(X509Certificate) keyEntry.getCertificate(),
    		elementRecipient
		);

        // Obtenemos el contenido cifrado
        final EncryptedContentInfo contenidoCifrado = authEnvelopedData.getAuthEncryptedContentInfo();

        // Obtenemos el algoritmo usado para cifrar la clave generada.
        final AlgorithmIdentifier algClave = contenidoCifrado.getContentEncryptionAlgorithm();

        // Asignamos la clave de descifrado del contenido.
        final KeyAsigned keyAsigned = Utils.assignKey(encryptedKeyDatas.getEncryptedKey(), keyEntry, algClave);

        // Desciframos el contenido.
        return Utils.deCipherContent(
               contenidoCifrado.getEncryptedContent().getOctets(),
               keyAsigned.getConfig(),
               keyAsigned.getCipherKey()
        );
    }
}

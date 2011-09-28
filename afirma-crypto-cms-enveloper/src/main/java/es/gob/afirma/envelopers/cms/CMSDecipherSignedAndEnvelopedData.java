/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.envelopers.cms;

import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Enumeration;

import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

import es.gob.afirma.core.AOException;
import es.gob.afirma.signers.pkcs7.SignedAndEnvelopedData;


/** Clase que descifra el contenido de un fichero en formato
 * SignedAndEnvelopedData. de CMS. */
public final class CMSDecipherSignedAndEnvelopedData {

    /** &Eacute;ste m&eacute;todo descifra el contenido de un CMS
     * SignedAndEnvelopData.
     * @param cmsData
     *        Datos del tipo SignedAndEnvelopData para obtener los datos
     *        cifrados.
     * @param keyEntry
     *        Clave privada del certificado usado para descifrar el
     *        contenido.
     * @return El contenido descifrado del SignedAndEnvelopData.
     * @throws java.io.IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos
     * @throws java.security.cert.CertificateEncodingException
     *         Si se produce alguna excepci&oacute;n con los certificados de
     *         firma.
     * @throws AOException
     *         Cuando ocurre un error durante el proceso de descifrado
     *         (formato o clave incorrecto,...)
     * @throws AOInvalidRecipientException
     *         Cuando se indica un certificado que no est&aacute; entre los
     *         destinatarios del sobre.
     * @throws InvalidKeyException
     *         Cuando la clave almacenada en el sobre no es v&aacute;lida. */
    public byte[] dechiperSignedAndEnvelopData(final byte[] cmsData, final PrivateKeyEntry keyEntry) throws IOException,
                                                                                        CertificateEncodingException,
                                                                                        AOException,
                                                                                        AOInvalidRecipientException,
                                                                                        InvalidKeyException {

        // Contendra el contenido a tratar.
        SignedAndEnvelopedData sigAndEnveloped = null;

        Enumeration<?> elementRecipient;

        try {
            final ASN1Sequence contentSignedAndEnvelopedData = Utils.fetchWrappedData(cmsData);

            sigAndEnveloped = SignedAndEnvelopedData.getInstance(contentSignedAndEnvelopedData);
            elementRecipient = sigAndEnveloped.getRecipientInfos().getObjects();
        }
        catch (final Exception ex) {
            throw new AOException("El fichero no contiene un tipo SignedAndEnvelopedData", ex); //$NON-NLS-1$
        }

        final EncryptedKeyDatas encryptedKeyDatas = Utils.fetchEncryptedKeyDatas((X509Certificate) keyEntry.getCertificate(), elementRecipient);

        // Obtenemos el contenido cifrado
        final EncryptedContentInfo contenidoCifrado = sigAndEnveloped.getEncryptedContentInfo();

        // Obtenemos el algoritmo usado para cifrar la clave generada.
        final AlgorithmIdentifier algClave = contenidoCifrado.getContentEncryptionAlgorithm();

        // Asignamos la clave de descifrado del contenido.
        final KeyAsigned keyAsigned = Utils.assignKey(encryptedKeyDatas.getEncryptedKey(), keyEntry, algClave);

        // Desciframos el contenido.
        final byte[] deciphered;
        final byte[] contCifrado = contenidoCifrado.getEncryptedContent().getOctets();
        try {
            deciphered = Utils.deCipherContent(contCifrado, keyAsigned.getConfig(), keyAsigned.getCipherKey());
        }
        catch (final InvalidKeyException ex) {
            throw ex;
        }
        catch (final Exception ex) {
            throw new AOException("Error al descifrar los contenidos del sobre digital", ex); //$NON-NLS-1$
        }
        return deciphered;
    }
}

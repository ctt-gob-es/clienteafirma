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
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Enumeration;
import java.util.Map;

import javax.crypto.SecretKey;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.EnvelopedData;
import org.bouncycastle.asn1.cms.OriginatorInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.ietf.jgss.Oid;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;

/** Clase que implementa firma digital PKCS#7/CMS EnvelopedData. La Estructura
 * del mensaje es la siguiente:<br>
 *
 * <pre>
 * <code>
 *
 *  EnvelopedData ::= SEQUENCE {
 *      version CMSVersion,
 *      originatorInfo [0] IMPLICIT OriginatorInfo OPTIONAL,
 *      recipientInfos RecipientInfos,
 *      encryptedContentInfo EncryptedContentInfo,
 *      unprotectedAttrs [1] IMPLICIT UnprotectedAttributes OPTIONAL
 *  }
 *
 * </code>
 * </pre>
 *
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un mensaje Data de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> */

public final class CMSEnvelopedData {

    /** Clave de cifrado. La almacenamos internamente porque no hay forma de
     * mostrarla directamente al usuario. */
    private SecretKey cipherKey;

    /** M&eacute;todo que genera la firma de tipo EnvelopedData.
     * @param parameters
     *        Par&aacute;metros necesarios para la generaci&oacute;n de este
     *        tipo.
     * @param config
     *        Configuraci&oacute;n del algoritmo para firmar
     * @param certDest
     *        Certificado del destino al cual va dirigido la firma.
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @param uatrib
     *        Conjunto de atributos no firmados.
     * @return la firma de tipo EnvelopedData.
     * @throws java.io.IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos
     * @throws java.security.cert.CertificateEncodingException
     *         Si se produce alguna excepci&oacute;n con los certificados de
     *         firma.
     * @throws java.security.NoSuchAlgorithmException
     *         Si no se soporta alguno de los algoritmos de firma o huella
     *         digital 
     * @throws AOException
     *         Cuando ocurre un error al generar el n&uacute;cleo del envoltorio.
     */
    byte[] genEnvelopedData(final P7ContentSignerParameters parameters,
                                   final AOCipherConfig config,
                                   final X509Certificate[] certDest,
                                   final Oid dataType,
                                   final Map<Oid, byte[]> uatrib) throws IOException, CertificateEncodingException, NoSuchAlgorithmException, AOException {
        this.cipherKey = Utils.initEnvelopedData(config, certDest);

        // Datos previos &uacute;tiles
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(parameters.getSignatureAlgorithm());

        // 1. ORIGINATORINFO
        // obtenemos la lista de certificados
        final X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();
        final ASN1Set certificates = Utils.fetchCertificatesList(signerCertificateChain);
        final ASN1Set certrevlist = null;

        OriginatorInfo origInfo = null;
        if (signerCertificateChain.length != 0) {
            origInfo = new OriginatorInfo(certificates, certrevlist);
        }

        // 2. RECIPIENTINFOS
        final Info infos = Utils.initVariables(parameters.getContent(), config, certDest, this.cipherKey);

        // 4. ATRIBUTOS
        final ASN1Set unprotectedAttrs = Utils.generateSignerInfo(digestAlgorithm, parameters.getContent(), dataType, uatrib);

        // construimos el Enveloped Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.envelopedData, new EnvelopedData(origInfo,
                                                                                      new DERSet(infos.getRecipientInfos()),
                                                                                      infos.getEncInfo(),
                                                                                      unprotectedAttrs)).getDEREncoded();
    }

    /** M&eacute;todo que genera la firma de tipo EnvelopedData.
     * @param data
     *        Datos binarios a firmar
     * @param digestAlg
     *        Algoritmo de hash
     * @param config
     *        Configuraci&oacute;n del algoritmo para firmar
     * @param certDest
     *        Certificado del destino al cual va dirigido la firma.
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @param uatrib
     *        Conjunto de atributos no firmados.
     * @return la firma de tipo EnvelopedData.
     * @throws java.io.IOException
     * @throws java.security.cert.CertificateEncodingException
     * @throws java.security.NoSuchAlgorithmException 
     * @throws AOException
     *         Cuando ocurre un error al generar el n&uacute;cleo del envoltorio.
     */
    byte[] genEnvelopedData(final byte[] data,
                                   final String digestAlg,
                                   final AOCipherConfig config,
                                   final X509Certificate[] certDest,
                                   final Oid dataType,
                                   final Map<Oid, byte[]> uatrib) throws IOException, CertificateEncodingException, NoSuchAlgorithmException, AOException {

        // Comprobamos que el archivo a tratar no sea nulo.
        this.cipherKey = Utils.initEnvelopedData(config, certDest);

        // Datos previos utiles
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(digestAlg);

        // 1. ORIGINATORINFO
        final OriginatorInfo origInfo = null;

        // 2. RECIPIENTINFOS
        final Info infos = Utils.initVariables(data, config, certDest, this.cipherKey);

        // 4. ATRIBUTOS
        final ASN1Set unprotectedAttrs = Utils.generateSignerInfo(digestAlgorithm, data, dataType, uatrib);

        // construimos el Enveloped Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.envelopedData, new EnvelopedData(origInfo,
                                                                                      new DERSet(infos.getRecipientInfos()),
                                                                                      infos.getEncInfo(),
                                                                                      unprotectedAttrs)).getDEREncoded();

    }

    /** M&eacute;todo que inserta remitentes en el "OriginatorInfo" de un sobre
     * de tipo envelopedData.
     * @param data
     *        Datos CMS que admiten multiples remitentes/firmantes.
     * @param signerCertificateChain
     *        Cadena de certificados a agregar.
     * @return La nueva firma enveloped con los remitentes que ten&iacute;a (si
     *         los tuviera) con la cadena de certificados nueva. 
     * @throws AOException
     *         Cuando ocurra un error al insertar el nuevo destinatario en el envoltorio. 
     */
    public byte[] addOriginatorInfo(final byte[] data, final X509Certificate[] signerCertificateChain) throws AOException {
        byte[] retorno = null;

        try {
            final ASN1InputStream is = new ASN1InputStream(data);
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
            final Enumeration<?> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            final DERObjectIdentifier doi = (DERObjectIdentifier) e.nextElement();
            if (doi.equals(PKCSObjectIdentifiers.envelopedData)) {
                // Contenido de Data
                final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();

                final EnvelopedData ed = new EnvelopedData((ASN1Sequence) doj.getObject());

                // Obtenemos los originatorInfo
                OriginatorInfo origInfo = ed.getOriginatorInfo();
                ASN1Set certs = null;
                if (origInfo != null) {
                    certs = origInfo.getCertificates();
                }

                // Si no hay certificados, se deja como esta.
                OriginatorInfo origInfoChecked = Utils.checkCertificates(signerCertificateChain, certs);
                if (origInfoChecked != null) {
                    origInfo = origInfoChecked;
                }
                
                // Se crea un nuevo EnvelopedData a partir de los datos
                // anteriores con los nuevos originantes.
                retorno =
                        new ContentInfo(PKCSObjectIdentifiers.envelopedData, new EnvelopedData(origInfo,
                                                                                               ed.getRecipientInfos(),
                                                                                               ed.getEncryptedContentInfo(),
                                                                                               ed.getUnprotectedAttrs())).getDEREncoded();
            }
        }
        catch (final Exception ex) {
            throw new AOException("Error durante el proceso de insercion", ex); //$NON-NLS-1$
        }

        return retorno;
    }
}

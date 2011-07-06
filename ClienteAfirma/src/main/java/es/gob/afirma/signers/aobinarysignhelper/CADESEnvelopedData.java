/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import static es.gob.afirma.signers.aobinarysignhelper.SigUtils.getAttributeSet;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;

import javax.crypto.SecretKey;

import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.EnvelopedData;
import org.bouncycastle.asn1.cms.OriginatorInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.ietf.jgss.Oid;

import es.gob.afirma.ciphers.AOCipherConfig;
import es.gob.afirma.misc.AOCryptoUtil;

/** Clase que implementa firma digital EnvelopedData en CADES basada en
 * PKCS#7/CMS EnvelopedData. La Estructura del mensaje es la siguiente:<br>
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

public final class CADESEnvelopedData {

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
     * @return la firma de tipo EnvelopedData.
     * @throws java.io.IOException
     *         Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos
     * @throws java.security.cert.CertificateEncodingException
     *         Si se produce alguna excepci&oacute;n con los certificados de
     *         firma.
     * @throws java.security.NoSuchAlgorithmException
     *         Si no se soporta alguno de los algoritmos de firma o huella
     *         digital */
    public byte[] genEnvelopedData(final P7ContentSignerParameters parameters, final AOCipherConfig config, final X509Certificate[] certDest, final Oid dataType) throws IOException,
                                                                                                                                         CertificateEncodingException,
                                                                                                                                         NoSuchAlgorithmException {

        this.cipherKey = Utils.initEnvelopedData(config, certDest);

        // Datos previos &uacute;tiles
        final String digestAlgorithm = AOCryptoUtil.getDigestAlgorithmName(parameters.getSignatureAlgorithm());

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
        final Info infos = Utils.initVariables(parameters.getContent(), config, certDest, cipherKey);

        // 3. ATRIBUTOS
        final ASN1Set unprotectedAttrs =
                getAttributeSet(new AttributeTable(Utils.initContexExpecific(digestAlgorithm, parameters.getContent(), dataType, null)));

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
     * @return la firma de tipo EnvelopedData.
     * @throws java.io.IOException
     *         Si hay problemas en la lectura de datos
     * @throws java.security.cert.CertificateEncodingException
     *         Cuando el certificado proporcionado no est&aacute; codificado
     *         adecuadamente
     * @throws java.security.NoSuchAlgorithmException
     *         Si no se soporta alguno de los algoritmos indicados */
    public byte[] genEnvelopedData(final byte[] data, final String digestAlg, final AOCipherConfig config, final X509Certificate[] certDest, final Oid dataType) throws IOException,
                                                                                                                                  CertificateEncodingException,
                                                                                                                                  NoSuchAlgorithmException {
        this.cipherKey = Utils.initEnvelopedData(config, certDest);

        // Datos previos &uacute;tiles
        final String digestAlgorithm = AOCryptoUtil.getDigestAlgorithmName(digestAlg);

        // 1. ORIGINATORINFO
        final OriginatorInfo origInfo = null;

        // 2. RECIPIENTINFOS
        final Info infos = Utils.initVariables(data, config, certDest, cipherKey);

        // 3. ATRIBUTOS
        final ASN1Set unprotectedAttrs = getAttributeSet(new AttributeTable(Utils.initContexExpecific(digestAlgorithm, data, dataType, null)));

        // construimos el Enveloped Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.envelopedData, new EnvelopedData(origInfo,
                                                                                      new DERSet(infos.getRecipientInfos()),
                                                                                      infos.getEncInfo(),
                                                                                      unprotectedAttrs)).getDEREncoded();
    }
}

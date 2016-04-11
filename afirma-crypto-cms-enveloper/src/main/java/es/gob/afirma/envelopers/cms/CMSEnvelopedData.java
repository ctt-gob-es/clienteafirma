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
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Enumeration;
import java.util.Map;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;

import org.spongycastle.asn1.ASN1Encoding;
import org.spongycastle.asn1.ASN1InputStream;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1Sequence;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.ASN1TaggedObject;
import org.spongycastle.asn1.DERSet;
import org.spongycastle.asn1.cms.ContentInfo;
import org.spongycastle.asn1.cms.EnvelopedData;
import org.spongycastle.asn1.cms.OriginatorInfo;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;

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
 * para crear un mensaje Data de SpongyCastle. */

public final class CMSEnvelopedData {

    /** Clave de cifrado. La almacenamos internamente porque no hay forma de
     * mostrarla directamente al usuario. */
    private SecretKey cipherKey;

    /** Genera una estructura PKCS#7 <code>EnvelopedData</code>.
     * @param parameters Par&aacute;metros necesarios para la generaci&oacute;n de este
     *                   tipo.
     * @param signerCertificateChain Cadena de certificados del firmante.
     * @param config Configuraci&oacute;n del algoritmo para firmar
     * @param certDest Certificado del destino al cual va dirigido la firma.
     * @param dataType Identifica el tipo del contenido a firmar.
     * @param uatrib Conjunto de atributos no firmados.
     * @param keySize Tama&ntilde;o de la clave AES.
     * @return la firma de tipo EnvelopedData.
     * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *                             datos
     * @throws java.security.cert.CertificateEncodingException Si se produce alguna excepci&oacute;n con los certificados de
     *                                                         firma.
     * @throws java.security.NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella
     *                                                digital
     * @throws BadPaddingException Cuando hay problemas con un relleno de datos.
     * @throws IllegalBlockSizeException Cuando hay problemas internos con los tama&ntilde;os de bloque de cifrado.
     * @throws InvalidAlgorithmParameterException Si no se soporta un par&aacute;metro necesario para un algoritmo.
     * @throws NoSuchPaddingException Cuando no se soporta un tipo de relleno necesario.
     * @throws InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave. */
    byte[] genEnvelopedData(final P7ContentSignerParameters parameters,
    		                final X509Certificate[] signerCertificateChain,
                            final AOCipherConfig config,
                            final X509Certificate[] certDest,
                            final String dataType,
                            final Map<String, byte[]> uatrib,
                            final Integer keySize) throws IOException,
                                                          CertificateEncodingException,
                                                          NoSuchAlgorithmException,
                                                          InvalidKeyException,
                                                          NoSuchPaddingException,
                                                          InvalidAlgorithmParameterException,
                                                          IllegalBlockSizeException,
                                                          BadPaddingException {

        this.cipherKey = Utils.initEnvelopedData(config, keySize);

        // Ya que el contenido puede ser grande, lo recuperamos solo una vez
        final byte[] content2 = parameters.getContent();

        // Datos previos &uacute;tiles
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(parameters.getSignatureAlgorithm());

        // 1. ORIGINATORINFO
        // obtenemos la lista de certificados
        final ASN1Set certificates = Utils.fetchCertificatesList(signerCertificateChain);
        final ASN1Set certrevlist = null;

        OriginatorInfo origInfo = null;
        if (signerCertificateChain.length != 0) {
            origInfo = new OriginatorInfo(certificates, certrevlist);
        }

        // 2. RECIPIENTINFOS
        final Info infos = Utils.initVariables(content2, config, certDest, this.cipherKey);

        // 4. ATRIBUTOS
        final ASN1Set unprotectedAttrs = Utils.generateSignerInfo(digestAlgorithm, content2, dataType, uatrib);

        // construimos el Enveloped Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.envelopedData, new EnvelopedData(origInfo,
                                                                                      new DERSet(infos.getRecipientInfos()),
                                                                                      infos.getEncInfo(),
                                                                                      unprotectedAttrs)).getEncoded(ASN1Encoding.DER);
    }

    /** Genera una estructura PKCS#7 <code>EnvelopedData</code>.
     * @param data Datos binarios a firmar.
     * @param digestAlg Algoritmo de huella digital.
     * @param config Configuraci&oacute;n del algoritmo para firmar.
     * @param certDest Certificado del destino al cual va dirigido la firma.
     * @param dataType Identifica el tipo del contenido a firmar.
     * @param uatrib Conjunto de atributos no firmados.
     * @param keySize Tama&ntilde;o de la clave AES.
     * @return la firma de tipo EnvelopedData.
     * @throws java.io.IOException En caso de error en la lectura o tratamiento de datos
     * @throws java.security.cert.CertificateEncodingException Cuando hay problemas relacionados con la
     *                                                         codificaci&oacute;n de los certificados X.509.
     * @throws java.security.NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario
     * @throws BadPaddingException Cuando hay problemas con un relleno de datos.
     * @throws IllegalBlockSizeException Cuando hay problemas internos con los tama&ntilde;os de bloque de cifrado.
     * @throws InvalidAlgorithmParameterException Si no se soporta un par&aacute;metro necesario para un algoritmo.
     * @throws NoSuchPaddingException Cuando no se soporta un tipo de relleno necesario.
     * @throws InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave. */
    byte[] genEnvelopedData(final byte[] data,
                                   final String digestAlg,
                                   final AOCipherConfig config,
                                   final X509Certificate[] certDest,
                                   final String dataType,
                                   final Map<String, byte[]> uatrib,
                                   final Integer keySize) throws IOException,
                                                                 CertificateEncodingException,
                                                                 NoSuchAlgorithmException,
                                                                 InvalidKeyException,
                                                                 NoSuchPaddingException,
                                                                 InvalidAlgorithmParameterException,
                                                                 IllegalBlockSizeException,
                                                                 BadPaddingException {

        this.cipherKey = Utils.initEnvelopedData(config, keySize);

        // Datos previos utiles
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(digestAlg);

        // 1. ORIGINATORINFO
        final OriginatorInfo origInfo = null;

        // 2. RECIPIENTINFOS
        final Info infos = Utils.initVariables(data, config, certDest, this.cipherKey);

        // 4. ATRIBUTOS
        final ASN1Set unprotectedAttrs = Utils.generateSignerInfo(
        		digestAlgorithm,
        		data,
        		dataType,
        		uatrib
		);

        // construimos el Enveloped Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.envelopedData, new EnvelopedData(origInfo,
                                                                                      new DERSet(infos.getRecipientInfos()),
                                                                                      infos.getEncInfo(),
                                                                                      unprotectedAttrs)).getEncoded(ASN1Encoding.DER);

    }

    /** M&eacute;todo que inserta remitentes en el "OriginatorInfo" de un sobre
     * de tipo envelopedData.
     * @param data
     *        Datos CMS que admiten multiples remitentes/firmantes.
     * @param signerCertificateChain
     *        Cadena de certificados a agregar.
     * @return La nueva firma enveloped con los remitentes que ten&iacute;a (si
     *         los tuviera) con la cadena de certificados nueva.
     * @throws IOException Si hay errores de lectura de datos
     * @throws CertificateEncodingException Cuando el certificado proporcionado es inv&aacute;lido */
    public static byte[] addOriginatorInfo(final byte[] data, final X509Certificate[] signerCertificateChain) throws IOException, CertificateEncodingException {

    	final ASN1Sequence dsq;
    	try (
			final ASN1InputStream is = new ASN1InputStream(data);
		) {
	        // LEEMOS EL FICHERO QUE NOS INTRODUCEN
	        dsq = (ASN1Sequence) is.readObject();
    	}

        final Enumeration<?> e = dsq.getObjects();
        // Elementos que contienen los elementos OID Data
        final ASN1ObjectIdentifier doi = (ASN1ObjectIdentifier) e.nextElement();
        if (doi.equals(PKCSObjectIdentifiers.envelopedData)) {
            // Contenido de Data
            final ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();

            final EnvelopedData ed = EnvelopedData.getInstance(doj.getObject());

            // Obtenemos los originatorInfo
            OriginatorInfo origInfo = ed.getOriginatorInfo();
            ASN1Set certs = null;
            if (origInfo != null) {
                certs = origInfo.getCertificates();
            }

            // Si no hay certificados, se deja como esta.
            final OriginatorInfo origInfoChecked = Utils.checkCertificates(signerCertificateChain, certs);
            if (origInfoChecked != null) {
                origInfo = origInfoChecked;
            }

            // Se crea un nuevo EnvelopedData a partir de los datos
            // anteriores con los nuevos originantes.
            return new ContentInfo(
        		PKCSObjectIdentifiers.envelopedData,
        		new EnvelopedData(
    				origInfo,
                    ed.getRecipientInfos(),
                    ed.getEncryptedContentInfo(),
                    ed.getUnprotectedAttrs()
                )
    		).getEncoded(ASN1Encoding.DER);
        }

        return null;
    }
}

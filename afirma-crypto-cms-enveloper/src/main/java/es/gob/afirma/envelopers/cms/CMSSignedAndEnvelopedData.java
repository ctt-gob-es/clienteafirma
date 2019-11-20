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
import java.security.SignatureException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Iterator;
import java.util.Map;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;

import org.spongycastle.asn1.ASN1EncodableVector;
import org.spongycastle.asn1.ASN1Encoding;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1OctetString;
import org.spongycastle.asn1.ASN1Primitive;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.DERPrintableString;
import org.spongycastle.asn1.DERSet;
import org.spongycastle.asn1.cms.Attribute;
import org.spongycastle.asn1.cms.AttributeTable;
import org.spongycastle.asn1.cms.ContentInfo;
import org.spongycastle.asn1.cms.IssuerAndSerialNumber;
import org.spongycastle.asn1.cms.SignerIdentifier;
import org.spongycastle.asn1.cms.SignerInfo;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.spongycastle.asn1.x500.X500Name;
import org.spongycastle.asn1.x500.style.RFC4519Style;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;
import org.spongycastle.asn1.x509.TBSCertificate;

import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.P7ContentSignerParameters;
import es.gob.afirma.signers.pkcs7.SignedAndEnvelopedData;

/** Clase que implementa firma digital PKCS#7/CMS SignedAndEnvelopedData basado
 * en las especificaciones de RFC-2315.
 * La Estructura del mensaje es la siguiente:<br>
 *
 * <pre>
 * <code>
 *    SignedAndEnvelopedData ::= SEQUENCE {
 *    version Version,
 *    recipientInfos RecipientInfos,
 *    digestAlgorithms DigestAlgorithmIdentifiers,
 *    encryptedContentInfo EncryptedContentInfo,
 *    certificates
 *      [0] IMPLICIT ExtendedCertificatesAndCertificates
 *         OPTIONAL,
 *    crls
 *      [1] IMPLICIT CertificateRevocationLists OPTIONAL,
 *    signerInfos SignerInfos }
 *
 * </code>
 * </pre>
 *
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios
 * para crear un mensaje SignedAndEnvelopedData de SpongyCastle. */
final class CMSSignedAndEnvelopedData {

    private ASN1Set signedAttr2;

    /** M&eacute;todo que genera la firma de tipo SignedAndEnvelopedData.
     * @param parameters Par&aacute;metros necesarios para la generaci&oacute;n de este
     *                   tipo.
     * @param signerCertificateChain Cadena de certificados del firmante.
     * @param config Configuraci&oacute;n del algoritmo para firmar
     * @param certDest Certificado del destino al cual va dirigido la firma.
     * @param dataType Identifica el tipo del contenido a firmar.
     * @param keyEntry Entrada hacia la clave privada para firma.
     * @param atrib Conjunto de atributos firmados.
     * @param uatrib Conjunto de atributos no firmados.
     * @param keySize Tama&ntilde;o de la clave AES.
     * @return La firma de tipo SignedAndEnvelopedData.
     * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *                             datos
     * @throws java.security.cert.CertificateEncodingException
     *         Si se produce alguna excepci&oacute;n con los certificados de
     *         firma.
     * @throws java.security.NoSuchAlgorithmException
     *         Si no se soporta alguno de los algoritmos de firma o huella
     *         digital
     * @throws BadPaddingException Cuando hay problemas con un relleno de datos.
     * @throws IllegalBlockSizeException Cuando hay problemas internos con los tama&ntilde;os de bloque de cifrado.
     * @throws InvalidAlgorithmParameterException Si no se soporta un par&aacute;metro necesario para un algoritmo.
     * @throws NoSuchPaddingException Cuando no se soporta un tipo de relleno necesario.
     * @throws InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave.
     * @throws SignatureException  Cuando ocurren problemas en la firma PKCS#1 */
    byte[] genSignedAndEnvelopedData(final P7ContentSignerParameters parameters,
    		                         final X509Certificate[] signerCertificateChain,
                                     final AOCipherConfig config,
                                     final X509Certificate[] certDest,
                                     final String dataType,
                                     final PrivateKeyEntry keyEntry,
                                     final Map<String, byte[]> atrib,
                                     final Map<String, byte[]> uatrib,
                                     final Integer keySize) throws IOException,
                                                                   CertificateEncodingException,
                                                                   NoSuchAlgorithmException,
                                                                   InvalidKeyException,
                                                                   NoSuchPaddingException,
                                                                   InvalidAlgorithmParameterException,
                                                                   IllegalBlockSizeException,
                                                                   BadPaddingException,
                                                                   SignatureException {

    	final SecretKey cipherKey = Utils.initEnvelopedData(config, keySize);

        // 1. VERSION
        // la version se mete en el constructor del signedAndEnvelopedData y es
        // 1

        // 2. DIGESTALGORITM
        // buscamos que timo de algoritmo es y lo codificamos con su OID

        final String signatureAlgorithm = parameters.getSignatureAlgorithm();
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);
        final AlgorithmIdentifier digAlgId = EvelopUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));

        final ASN1EncodableVector digestAlgs = new ASN1EncodableVector();
        digestAlgs.add(digAlgId);

        // LISTA DE CERTIFICADOS: obtenemos la lista de certificados
        final ASN1Set certificates = Utils.fetchCertificatesList(signerCertificateChain);

        // Ya que el contenido puede ser grande, lo recuperamos solo una vez
        final byte[] content2 = parameters.getContent();

        // 2. RECIPIENTINFOS
        final Info infos = Utils.initVariables(content2, config, certDest, cipherKey);

        // 4. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        final ASN1EncodableVector signerInfos = new ASN1EncodableVector();

        final TBSCertificate tbs2 = TBSCertificate.getInstance(
    		ASN1Primitive.fromByteArray(signerCertificateChain[0].getTBSCertificate())
		);

        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(
    		X500Name.getInstance(tbs2.getIssuer()), tbs2.getSerialNumber().getValue()
		);

        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // // ATRIBUTOS
        final ASN1Set signedAttr = generateSignerInfo(signerCertificateChain[0], digestAlgorithm, content2, dataType, atrib);

        ASN1Set unSignedAttr = null;
        unSignedAttr = EvelopUtils.generateUnsignedInfo(uatrib);

        // digEncryptionAlgorithm
        //TODO: En RSA seria conveniente usar el OID del algoritmo de huella, y no solo el de RSA
        final AlgorithmIdentifier encAlgId = EvelopUtils.makeAlgId(
    		signatureAlgorithm.contains("withRSA") ? //$NON-NLS-1$
				AOAlgorithmID.getOID("RSA") : //$NON-NLS-1$
					AOAlgorithmID.getOID(signatureAlgorithm)
		);

        final ASN1OctetString sign2 = Utils.firma(signatureAlgorithm, keyEntry, this.signedAttr2);

        signerInfos.add(new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, unSignedAttr));

        final ASN1Set certrevlist = null;

        // construimos el Signed And Enveloped Data y lo devolvemos
        return new ContentInfo(
    		PKCSObjectIdentifiers.signedAndEnvelopedData,
    		new SignedAndEnvelopedData(
				new DERSet(infos.getRecipientInfos()),
				new DERSet(digestAlgs),
				infos.getEncInfo(),
				certificates,
				certrevlist,
				new DERSet(signerInfos)
    		)
		).getEncoded(ASN1Encoding.DER);
    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * firmante. Se generan los atributos que se necesitan para generar la firma.
     * @param cert Certificado del firmante.
     * @param digestAlgorithm Algoritmo Firmado.
     * @param datos Datos firmados.
     * @param dataType Identifica el tipo del contenido a firmar.
     * @param atrib Conjunto de atributos firmados.
     * @return Los datos necesarios para generar la firma referente a los datos
     *         del usuario.
     * @throws java.security.NoSuchAlgorithmException Si el JRE no soporta alg&uacute;n algoritmo necesario */
    private ASN1Set generateSignerInfo(final X509Certificate cert,
    		                           final String digestAlgorithm,
    		                           final byte[] datos,
    		                           final String dataType,
    		                           final Map<String,
    		                           byte[]> atrib) throws NoSuchAlgorithmException {
        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = Utils.initContexExpecific(digestAlgorithm, datos, dataType, null);

        // Serial Number
        // comentar lo de abajo para version del rfc 3852
        contexExpecific.add(new Attribute(RFC4519Style.serialNumber, new DERSet(new DERPrintableString(cert.getSerialNumber().toString()))));

        // agregamos la lista de atributos a mayores.
        if (atrib.size() != 0) {
            final Iterator<Map.Entry<String, byte[]>> it = atrib.entrySet().iterator();
            while (it.hasNext()) {
                final Map.Entry<String, byte[]> e = it.next();
                contexExpecific.add(new Attribute(
                        // el oid
                        new ASN1ObjectIdentifier(e.getKey().toString()),
                        // el array de bytes en formato string
                        new DERSet(new DERPrintableString(new String(e.getValue()))))
                );
            }
        }

        this.signedAttr2 = EvelopUtils.getAttributeSet(new AttributeTable(contexExpecific));

        return EvelopUtils.getAttributeSet(new AttributeTable(contexExpecific));
    }

}

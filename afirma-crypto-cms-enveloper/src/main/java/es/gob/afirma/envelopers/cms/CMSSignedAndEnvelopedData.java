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
import java.security.KeyStore.PrivateKeyEntry;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.crypto.SecretKey;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Encoding;
import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Primitive;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.DERPrintableString;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.SignerIdentifier;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x500.style.RFC4519Style;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.GenSignedData;
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
 * para crear un mensaje SignedAndEnvelopedData de BouncyCastle: <a
 * href="http://www.bouncycastle.org/">www.bouncycastle.org</a> */
final class CMSSignedAndEnvelopedData {

    private ASN1Set signedAttr2;

    /** M&eacute;todo que genera la firma de tipo SignedAndEnvelopedData.
     * @param parameters
     *        Par&aacute;metros necesarios para la generaci&oacute;n de este
     *        tipo.
     * @param config
     *        Configuraci&oacute;n del algoritmo para firmar
     * @param certDest
     *        Certificado del destino al cual va dirigido la firma.
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @param keyEntry
     *        Eatrada a la clave privada para firma
     * @param atrib
     *        Conjunto de atributos firmados.
     * @param uatrib
     *        Conjunto de atributos no firmados.
     * @return La firma de tipo SignedAndEnvelopedData.
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
    byte[] genSignedAndEnvelopedData(final P7ContentSignerParameters parameters,
                                            final AOCipherConfig config,
                                            final X509Certificate[] certDest,
                                            final String dataType,
                                            final PrivateKeyEntry keyEntry,
                                            final Map<String, byte[]> atrib,
                                            final Map<String, byte[]> uatrib) throws IOException, CertificateEncodingException, NoSuchAlgorithmException, AOException {

    	final SecretKey cipherKey = Utils.initEnvelopedData(config, certDest);

        // 1. VERSION
        // la version se mete en el constructor del signedAndEnvelopedData y es
        // 1

        // 2. DIGESTALGORITM
        // buscamos que timo de algoritmo es y lo codificamos con su OID

        final String signatureAlgorithm = parameters.getSignatureAlgorithm();
        final String digestAlgorithm = AOSignConstants.getDigestAlgorithmName(signatureAlgorithm);
        final AlgorithmIdentifier digAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithm));

        final ASN1EncodableVector digestAlgs = new ASN1EncodableVector();
        digestAlgs.add(digAlgId);

        // LISTA DE CERTIFICADOS: obtenemos la lista de certificados
        final X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();
        final ASN1Set certificates = Utils.fetchCertificatesList(signerCertificateChain);

        // Ya que el contenido puede ser grande, lo recuperamos solo una vez
        final byte[] content2 = parameters.getContent();

        // 2. RECIPIENTINFOS
        final Info infos = Utils.initVariables(content2, config, certDest, cipherKey);

        // 4. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        final ASN1EncodableVector signerInfos = new ASN1EncodableVector();

        final TBSCertificateStructure tbs2 = TBSCertificateStructure.getInstance(ASN1Primitive.fromByteArray(signerCertificateChain[0].getTBSCertificate()));

        final IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(X500Name.getInstance(tbs2.getIssuer()), tbs2.getSerialNumber().getValue());

        final SignerIdentifier identifier = new SignerIdentifier(encSid);

        // // ATRIBUTOS
        final ASN1Set signedAttr = generateSignerInfo(signerCertificateChain[0], digestAlgorithm, content2, dataType, atrib);

        ASN1Set unSignedAttr = null;
        unSignedAttr = generateUnsignerInfo(uatrib);

        // digEncryptionAlgorithm
        final AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID("RSA")); //$NON-NLS-1$

        ASN1OctetString sign2 = null;
        try {
            sign2 = Utils.firma(signatureAlgorithm, keyEntry, this.signedAttr2);
        }
        catch (final AOException ex) {
            Logger.getLogger(GenSignedData.class.getName()).log(Level.SEVERE, null, ex);
        }

        signerInfos.add(new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, unSignedAttr));

        final ASN1Set certrevlist = null;

        // construimos el Signed And Enveloped Data y lo devolvemos
        return new ContentInfo(PKCSObjectIdentifiers.signedAndEnvelopedData, new SignedAndEnvelopedData(new DERSet(infos.getRecipientInfos()),
                                                                                                        new DERSet(digestAlgs),
                                                                                                        infos.getEncInfo(),
                                                                                                        certificates,
                                                                                                        certrevlist,
                                                                                                        new DERSet(signerInfos))).getEncoded(ASN1Encoding.DER);
    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos que se necesitan para generar la firma.
     * @param digestAlgorithm
     *        Algoritmo Firmado.
     * @param datos
     *        Datos firmados.
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @param atrib
     *        Conjunto de atributos firmados.
     * @return Los datos necesarios para generar la firma referente a los datos
     *         del usuario.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.security.cert.CertificateException
     * @throws java.io.IOException */
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
                        new ASN1ObjectIdentifier((e.getKey()).toString()),
                        // el array de bytes en formato string
                        new DERSet(new DERPrintableString(new String(e.getValue()))))
                );
            }
        }

        this.signedAttr2 = SigUtils.getAttributeSet(new AttributeTable(contexExpecific));

        return SigUtils.getAttributeSet(new AttributeTable(contexExpecific));
    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos no firmados.
     * @param uatrib
     *        Lista de atributos no firmados que se insertar&aacute;n dentro
     *        del archivo de firma.
     * @return Los atributos no firmados de la firma. */
    private static ASN1Set generateUnsignerInfo(final Map<String, byte[]> uatrib) {

        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();

        // agregamos la lista de atributos a mayores.
        if (uatrib.size() != 0) {
            final Iterator<Map.Entry<String, byte[]>> it = uatrib.entrySet().iterator();
            while (it.hasNext()) {
                final Map.Entry<String, byte[]> e = it.next();
                contexExpecific.add(new Attribute(
                		// el oid
                        new ASN1ObjectIdentifier((e.getKey()).toString()),
                        // el array de bytes en formato string
                        new DERSet(new DERPrintableString(new String(e.getValue())))
                ));
            }
        }
        else {
            return null;
        }

        return SigUtils.getAttributeSet(new AttributeTable(contexExpecific));

    }
}

/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.envelopers.cades;

import java.io.IOException;
import java.security.AlgorithmParameters;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.Signature;
import java.security.SignatureException;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.security.spec.AlgorithmParameterSpec;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.List;
import java.util.logging.Logger;

import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.KeyGenerator;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.PBEParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1Encoding;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1ObjectIdentifier;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Primitive;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.DERIA5String;
import org.bouncycastle.asn1.DERNull;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERPrintableString;
import org.bouncycastle.asn1.DERSequence;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.DERUTCTime;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.KeyTransRecipientInfo;
import org.bouncycastle.asn1.cms.RecipientIdentifier;
import org.bouncycastle.asn1.cms.RecipientInfo;
import org.bouncycastle.asn1.cms.SignerIdentifier;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.ess.ESSCertID;
import org.bouncycastle.asn1.ess.ESSCertIDv2;
import org.bouncycastle.asn1.ess.SigningCertificate;
import org.bouncycastle.asn1.ess.SigningCertificateV2;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x500.style.RFC4519Style;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.Certificate;
import org.bouncycastle.asn1.x509.DigestInfo;
import org.bouncycastle.asn1.x509.GeneralName;
import org.bouncycastle.asn1.x509.GeneralNames;
import org.bouncycastle.asn1.x509.IssuerSerial;
import org.bouncycastle.asn1.x509.PolicyInformation;
import org.bouncycastle.asn1.x509.PolicyQualifierId;
import org.bouncycastle.asn1.x509.PolicyQualifierInfo;
import org.bouncycastle.asn1.x509.SubjectPublicKeyInfo;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherBlockMode;
import es.gob.afirma.core.misc.Base64;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.signers.pkcs7.AOAlgorithmID;
import es.gob.afirma.signers.pkcs7.SigUtils;
import es.gob.afirma.signers.pkcs7.SignedAndEnvelopedData;

final class CAdESUtils {

    private CAdESUtils() {
        // No permitimos la instanciacion
    }

    /** Vector de inicializacion de 8 bytes. Un vector de inicializaci&oacute;n
     * de 8 bytes es necesario para el uso de los algoritmos DES y DESede. */
    private static final byte[] IV_8 = {
            (byte) 0xC6, (byte) 0xBA, (byte) 0xDE, (byte) 0xA4, (byte) 0x76, (byte) 0x43, (byte) 0x32, (byte) 0x6B
    };

    /** Vector de inicializacion de 16 bytes. Un vector de inicializaci&oacute;n
     * de 16 bytes es necesario para el uso de los algoritmos DES y DESede. */
    private static final byte[] IV_16 = {
            (byte) 0xB2,
            (byte) 0xBA,
            (byte) 0xDE,
            (byte) 0xA4,
            (byte) 0x41,
            (byte) 0x7F,
            (byte) 0x97,
            (byte) 0x4B,
            (byte) 0xAC,
            (byte) 0x63,
            (byte) 0xAC,
            (byte) 0xAA,
            (byte) 0x76,
            (byte) 0x73,
            (byte) 0x12,
            (byte) 0x6B
    };

    /** Semilla para uso en par&aacute;metros de cifrados basados en contrase&ntilde;a. */
    private static final byte[] SALT = {
            (byte) 0xA2, (byte) 0x35, (byte) 0xDC, (byte) 0xA4, (byte) 0x11, (byte) 0x7C, (byte) 0x99, (byte) 0x4B
    };

    /** N&uacute;mero de iteraciones para uso en par&aacute;metros de cifrados basados en contrase&ntilde;a. */
    private static final int ITERATION_COUNT = 9;

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Obtiene un <code>Info</code> que contiene los RecipientInfos y el EncryptedContentInfo.
     * @param data Datos a incluir en el sobre
     * @param config Configuraci&oacute;n de cifrado a aplicar
     * @param certDest Certificados de los destinatarios
     * @param cipherKey Clave de cifrado
     * @return <code>Info</code> que contiene los RecipientInfos y el EncryptedContentInfo
     * @throws IOException en caso de error de entrada / salida
     * @throws CertificateEncodingException en caso de errores de codificaci&oacute;n en los certificados
     */
    static Info getEnvelopeInfo(final byte[] data, final AOCipherConfig config, final X509Certificate[] certDest, final SecretKey cipherKey) throws IOException,
                                                                                                                         CertificateEncodingException {

        // Reiniciamos las dos variables
        final Info infos = new Info();

        final ASN1EncodableVector recipientInfos = new ASN1EncodableVector();
        X509Certificate cert;
        TBSCertificateStructure tbs;
        IssuerAndSerialNumber isse;
        RecipientIdentifier rid;
        PublicKey pubKey;
        AlgorithmIdentifier keyEncAlg;
        SubjectPublicKeyInfo info;
        // Cifrado de la clave
        byte[] encryptedKey = null;
        // generamos el contenedor de cifrado

        RecipientInfo recipient = null;

        for (final X509Certificate element : certDest) {
            cert = element;
            tbs = TBSCertificateStructure.getInstance(ASN1Primitive.fromByteArray(cert.getTBSCertificate()));
            // Obtenemos el Isuer & serial number
            isse = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
            // Creamos el recipientInfo
            rid = new RecipientIdentifier(isse);
            // Obtenemos la clave publica
            pubKey = cert.getPublicKey();
            // obtenemos la informacion de la clave publica
            info = tbs.getSubjectPublicKeyInfo();
            // obtenemos el algoritmo de cifrado.
            keyEncAlg = info.getAlgorithm();

            try {
                // ciframos la clave
                encryptedKey = cipherKey(pubKey, cipherKey);
            }
            catch (final Exception e) {
                LOGGER.severe("Error durante el proceso cifrado de la clave: " + e); //$NON-NLS-1$
            }
            // creamos el recipiente con los datos del destinatario.
            final KeyTransRecipientInfo keyTransRecipientInfo = new KeyTransRecipientInfo(rid, keyEncAlg, new DEROctetString(encryptedKey));

            recipient = new RecipientInfo(keyTransRecipientInfo);
            // Lo a&ntilde;adimos al recipiente de destinatarios.
            recipientInfos.add(recipient);
        }

        // 3. ENCRIPTEDCONTENTINFO
        try {
            infos.setEncInfo(getEncryptedContentInfo(data, config, cipherKey));
        }
        catch (final Exception e) {
            LOGGER.severe("Error durante el proceso cifrado de la clave: " + e); //$NON-NLS-1$
        }

        infos.setRecipientInfos(recipientInfos);

        return infos;
    }

    /** M&eacute;todo cifra la clave usada para cifrar el archivo usando para
     * ello la clave p&uacute;blica del certificado del usuario.
     * @param pKey
     *        Clave pública del certificado.
     * @param cipherKey
     *        Clave de cifrado
     * @return La clave cifrada en "WRAP_MODE".
     * @throws java.security.NoSuchAlgorithmException
     * @throws javax.crypto.NoSuchPaddingException
     * @throws java.security.InvalidKeyException
     * @throws java.security.InvalidAlgorithmParameterException
     * @throws javax.crypto.IllegalBlockSizeException
     * @throws javax.crypto.BadPaddingException */
    private static byte[] cipherKey(final PublicKey pKey, final SecretKey cipherKey) throws NoSuchAlgorithmException,
                                                                        NoSuchPaddingException,
                                                                        InvalidKeyException,
                                                                        InvalidAlgorithmParameterException,
                                                                        IllegalBlockSizeException {

        final Cipher cipher = createCipher(pKey.getAlgorithm());
        final AlgorithmParameters params = cipher.getParameters();
        cipher.init(Cipher.WRAP_MODE, pKey, params);
        final byte[] ciphered = cipher.wrap(cipherKey);

        return ciphered;
    }

    /** M&eacute;todo que obtiene el EncriptedContentInfo a partir del archivo a
     * cifrar. El contenido es el siguiente:
     *
     * <pre>
     * <code>
     * EncryptedContentInfo ::= SEQUENCE {
     *     contentType ContentType,
     *     contentEncryptionAlgorithm ContentEncryptionAlgorithmIdentifier,
     *     encryptedContent [0] IMPLICIT EncryptedContent OPTIONAL
     * }
     * </code>
     * </pre>
     * @param file
     *        Archivo a cifrar.
     * @param config
     *        Configuracion de la clave de cifrado
     * @param cipherKey
     *        Clave de cifrado
     * @return Un sistema EncryptedContentInfo.
     * @throws java.security.NoSuchAlgorithmException
     * @throws javax.crypto.NoSuchPaddingException
     * @throws java.security.InvalidAlgorithmParameterException
     * @throws java.security.InvalidKeyException
     * @throws java.io.IOException
     * @throws org.bouncycastle.cms.CMSException */
    static EncryptedContentInfo getEncryptedContentInfo(final byte[] file, final AOCipherConfig config, final SecretKey cipherKey) throws NoSuchAlgorithmException,
                                                                                                                       NoSuchPaddingException,
                                                                                                                       InvalidAlgorithmParameterException,
                                                                                                                       InvalidKeyException,
                                                                                                                       IOException {

        final AlgorithmParameterSpec params = getParams(config);
        final Cipher cipher = createCipher(config.toString());
        cipher.init(Cipher.ENCRYPT_MODE, cipherKey, params);
        return getEncryptedContentInfo(file, config, params, cipher);
    }

    /** Obtiene el contenido de un archivo encriptado
     * @param file Archivo con los datos
     * @param config Configuracion de cifrado
     * @param params Parametros
     * @param cipher Encriptador */
    private static EncryptedContentInfo getEncryptedContentInfo(final byte[] file, final AOCipherConfig config, final AlgorithmParameterSpec params, final Cipher cipher) throws IOException {
        final byte[] ciphered;
        try {
            ciphered = cipher.doFinal(file);
        }
        catch (final Exception e) {
            LOGGER.severe("No se ha podido completar el cifrado, se devolvera null: " + e); //$NON-NLS-1$
            return null;
        }

        ASN1Encodable asn1Params;
        if (params != null) {
            final ASN1InputStream aIn = new ASN1InputStream(cipher.getParameters().getEncoded("ASN.1")); //$NON-NLS-1$
            asn1Params = aIn.readObject();
        }
        else {
            asn1Params = new DERNull();
        }

        // obtenemos el OID del algoritmo de cifrado
        final AlgorithmIdentifier encAlgId = new AlgorithmIdentifier(new ASN1ObjectIdentifier(config.getAlgorithm().getOid()), asn1Params);

        // Obtenemos el identificador
        final ASN1ObjectIdentifier contentType = PKCSObjectIdentifiers.encryptedData;
        return new EncryptedContentInfo(contentType, encAlgId, new DEROctetString(ciphered));
    }

    /** Crea el cifrador usado para cifrar tanto el fichero como la clave usada
     * para cifrar dicho fichero.
     * @param algName
     *        algoritmo utilizado para cifrar.
     * @throws java.security.NoSuchAlgorithmException
     * @throws javax.crypto.NoSuchPaddingException */
    private static Cipher createCipher(final String algName) throws NoSuchAlgorithmException, NoSuchPaddingException {
        return Cipher.getInstance(algName);
    }

    /** Genera los par&aacute;metros necesarios para poder operar con una
     * configuracion concreta de cifrado. Si no es necesario ning&uacute;n
     * par&aacute;metro especial, devolvemos <code>null</code>.
     * @param algorithmConfig
     *        Configuracion de cifrado que debemos parametrizar
     * @return Par&aacute;metros para operar */
    private static AlgorithmParameterSpec getParams(final AOCipherConfig algorithmConfig) {

        AlgorithmParameterSpec params = null;
        if (algorithmConfig.getAlgorithm().supportsPassword()) {
            params = new PBEParameterSpec(SALT, ITERATION_COUNT);
        }
        else {
            if (!algorithmConfig.getBlockMode().equals(AOCipherBlockMode.ECB)) {
                params = new IvParameterSpec(algorithmConfig.getAlgorithm().equals(AOCipherAlgorithm.AES) ? IV_16 : IV_8);
            }
        }

        return params;
    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos que se necesitan para generar la firma.
     * @param cert Certificado del firmante
     * @param datos Datos firmados
     * @param policy Pol&iacute;tica de firma
     * @param messageDigest
     * @return Los datos necesarios para generar la firma referente a los datos
     *         del usuario.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws CertificateEncodingException */
    static ASN1EncodableVector generateSignerInfo(final X509Certificate cert,
                                                         final String digestAlgorithmName,
                                                         final byte[] datos,
                                                         final AdESPolicy policy,
                                                         final byte[] messageDigest) throws NoSuchAlgorithmException,
                                                                              IOException,
                                                                              CertificateEncodingException {

        // ALGORITMO DE HUELLA DIGITAL
        final AlgorithmIdentifier digestAlgorithmOID = SigUtils.makeAlgId(AOAlgorithmID.getOID(digestAlgorithmName));

        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = initContexExpecific(digestAlgorithmName, datos, PKCSObjectIdentifiers.data.getId(), messageDigest);

        // Serial Number
        // comentar lo de abajo para version del rfc 3852
        contexExpecific.add(new Attribute(RFC4519Style.serialNumber, new DERSet(new DERPrintableString(cert.getSerialNumber().toString()))));

        if (!"SHA1".equals(AOSignConstants.getDigestAlgorithmName(digestAlgorithmName))) { //$NON-NLS-1$

            //********************************************/
            //***** La Nueva operatividad esta comentada */
            //********************************************/
            // INICIO SINGING CERTIFICATE-V2

            /** IssuerSerial ::= SEQUENCE { issuer GeneralNames, serialNumber
             * CertificateSerialNumber */

            final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Primitive.fromByteArray(cert.getTBSCertificate()));

            /** ESSCertIDv2 ::= SEQUENCE { hashAlgorithm AlgorithmIdentifier
             * DEFAULT {algorithm id-sha256}, certHash Hash, issuerSerial
             * IssuerSerial OPTIONAL }
             * Hash ::= OCTET STRING */

            final byte[] certHash = MessageDigest.getInstance(digestAlgorithmName).digest(cert.getEncoded());
            final ESSCertIDv2[] essCertIDv2 = {
                new ESSCertIDv2(
                    digestAlgorithmOID,
                    certHash,
                    new IssuerSerial(
                         new GeneralNames(new GeneralName(tbs.getIssuer())),
                         tbs.getSerialNumber()
                     )
                )
            };

            /** PolicyInformation ::= SEQUENCE { policyIdentifier CertPolicyId,
             * policyQualifiers SEQUENCE SIZE (1..MAX) OF PolicyQualifierInfo
             * OPTIONAL }
             * CertPolicyId ::= OBJECT IDENTIFIER
             * PolicyQualifierInfo ::= SEQUENCE { policyQualifierId
             * PolicyQualifierId, qualifier ANY DEFINED BY policyQualifierId } */

            final SigningCertificateV2 scv2;
            if(policy.getPolicyIdentifier() != null) {

                /** SigningCertificateV2 ::= SEQUENCE { certs SEQUENCE OF
                 * ESSCertIDv2, policies SEQUENCE OF PolicyInformation OPTIONAL
                 * } */
                scv2 = new SigningCertificateV2(essCertIDv2, getPolicyInformation(policy)); // con
                                                                  // politica
            }
            else {
                scv2 = new SigningCertificateV2(essCertIDv2); // Sin politica
            }

            // Secuencia con singningCertificate
            contexExpecific.add(new Attribute(PKCSObjectIdentifiers.id_aa_signingCertificateV2, new DERSet(scv2)));

            // FIN SINGING CERTIFICATE-V2

        }
        else {

            // INICIO SINGNING CERTIFICATE

            /** IssuerSerial ::= SEQUENCE { issuer GeneralNames, serialNumber
             * CertificateSerialNumber } */

            final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Primitive.fromByteArray(cert.getTBSCertificate()));

            final IssuerSerial isuerSerial = new IssuerSerial(new GeneralNames(new GeneralName(tbs.getIssuer())), tbs.getSerialNumber());

            /** ESSCertID ::= SEQUENCE { certHash Hash, issuerSerial IssuerSerial
             * OPTIONAL }
             * Hash ::= OCTET STRING -- SHA1 hash of entire certificate */
            final ESSCertID essCertID = new ESSCertID(MessageDigest.getInstance(digestAlgorithmName).digest(cert.getEncoded()), isuerSerial);

            /** PolicyInformation ::= SEQUENCE { policyIdentifier CertPolicyId,
             * policyQualifiers SEQUENCE SIZE (1..MAX) OF PolicyQualifierInfo
             * OPTIONAL }
             * CertPolicyId ::= OBJECT IDENTIFIER
             * PolicyQualifierInfo ::= SEQUENCE { policyQualifierId
             * PolicyQualifierId, qualifier ANY DEFINED BY policyQualifierId } */

            final SigningCertificate scv;
            if (policy.getPolicyIdentifier() != null) {

                /** SigningCertificateV2 ::= SEQUENCE { certs SEQUENCE OF
                 * ESSCertIDv2, policies SEQUENCE OF PolicyInformation OPTIONAL
                 * } */
                /*
                 * HAY QUE HACER UN SEQUENCE, YA QUE EL CONSTRUCTOR DE BOUNCY
                 * CASTLE NO TIENE DICHO CONSTRUCTOR.
                 */
                final ASN1EncodableVector v = new ASN1EncodableVector();
                v.add(new DERSequence(essCertID));
                v.add(new DERSequence(getPolicyInformation(policy)));
                scv = SigningCertificate.getInstance(new DERSequence(v)); // con politica
            }
            else {
                scv = new SigningCertificate(essCertID); // Sin politica
            }

            /** id-aa-signingCertificate OBJECT IDENTIFIER ::= { iso(1)
             * member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs9(9) smime(16)
             * id-aa(2) 12 } */
            // Secuencia con singningCertificate
            contexExpecific.add(new Attribute(PKCSObjectIdentifiers.id_aa_signingCertificate, new DERSet(scv)));
        }

        // INICIO SIGPOLICYID ATTRIBUTE

        if (policy.getPolicyIdentifier() != null) {
            /*
             * SigPolicyId ::= OBJECT IDENTIFIER Politica de firma.
             */
            final ASN1ObjectIdentifier doiSigPolicyId = new ASN1ObjectIdentifier(policy.getPolicyIdentifier().toLowerCase().replace("urn:oid:", "")); //$NON-NLS-1$ //$NON-NLS-2$

            /*
             *   OtherHashAlgAndValue ::= SEQUENCE {
             *     hashAlgorithm    AlgorithmIdentifier,
             *     hashValue        OCTET STRING }
             *
             */


            // Algoritmo para el hash
            final AlgorithmIdentifier hashid;
            // si tenemos algoritmo de calculo de hash, lo ponemos
            if(policy.getPolicyIdentifierHashAlgorithm()!=null){
                hashid = SigUtils.makeAlgId(
                                    AOAlgorithmID.getOID(
                                    AOSignConstants.getDigestAlgorithmName(
                                       policy.getPolicyIdentifierHashAlgorithm())));
            }
            // si no tenemos, ponemos el algoritmo de firma.
            else{
                hashid= digestAlgorithmOID;
            }
            // hash del documento, descifrado en b64
            final byte[] hashed;
            if(policy.getPolicyIdentifierHash()!=null) {
            	hashed = Base64.decode(policy.getPolicyIdentifierHash());
            }
            else{
                hashed = new byte[]{0};
            }

            final DigestInfo otherHashAlgAndValue = new DigestInfo(hashid, hashed);

            /*
             *   SigPolicyQualifierInfo ::= SEQUENCE {
             *       SigPolicyQualifierId  SigPolicyQualifierId,
             *       SigQualifier          ANY DEFINED BY policyQualifierId }
             */
            SigPolicyQualifierInfo spqInfo = null;
            if(policy.getPolicyQualifier()!=null){
                spqInfo = new SigPolicyQualifierInfo(policy.getPolicyQualifier().toString());
            }

            /*
             * SignaturePolicyId ::= SEQUENCE {
             *  sigPolicyId           SigPolicyId,
             *  sigPolicyHash         SigPolicyHash,
             *  sigPolicyQualifiers   SEQUENCE SIZE (1..MAX) OF
             *                          SigPolicyQualifierInfo OPTIONAL}
             *
             */
            final ASN1EncodableVector v = new ASN1EncodableVector();
            // sigPolicyId
            v.add(doiSigPolicyId);
            // sigPolicyHash
            v.add(otherHashAlgAndValue.toASN1Primitive()); // como sequence
            // sigPolicyQualifiers
            if(spqInfo!=null) {
                v.add(spqInfo.toASN1Primitive());
            }

            final DERSequence ds = new DERSequence(v);

            // Secuencia con singningCertificate
            contexExpecific.add(new Attribute(PKCSObjectIdentifiers.id_aa_ets_sigPolicyId, new DERSet(ds.toASN1Primitive())));
            // FIN SIGPOLICYID ATTRIBUTE
        }

        return contexExpecific;
    }

    /**
     * Obtiene un PolicyInformation a partir de los datos de la pol&iacute;tica.
     * Sirve para los datos de SigningCertificate y SigningCertificateV2. Tiene que llevar algunos
     * datos de la pol&iacute;tica.
     * <pre>
     * PolicyInformation ::= SEQUENCE {
     * policyIdentifier   CertPolicyId,
     * policyQualifiers   SEQUENCE SIZE (1..MAX) OF
     *                          PolicyQualifierInfo OPTIONAL }
     *
     *
     * CertPolicyId ::= OBJECT IDENTIFIER
     *
     * PolicyQualifierInfo ::= SEQUENCE {
     *      policyQualifierId  PolicyQualifierId,
     *      qualifier          ANY DEFINED BY policyQualifierId }
     *
     * -- policyQualifierIds for Internet policy qualifiers
     *
     * id-qt          OBJECT IDENTIFIER ::=  { id-pkix 2 }
     * id-qt-cps      OBJECT IDENTIFIER ::=  { id-qt 1 }
     * id-qt-unotice  OBJECT IDENTIFIER ::=  { id-qt 2 }
     *
     * PolicyQualifierId ::=
     *      OBJECT IDENTIFIER ( id-qt-cps | id-qt-unotice )
     *
     * Qualifier ::= CHOICE {
     *      cPSuri           CPSuri,
     *      userNotice       UserNotice }
     *
     * CPSuri ::= IA5String
     *
     * UserNotice ::= SEQUENCE {
     *      noticeRef        NoticeReference OPTIONAL,
     *      explicitText     DisplayText OPTIONAL}
     *
     * NoticeReference ::= SEQUENCE {
     *      organization     DisplayText,
     *      noticeNumbers    SEQUENCE OF INTEGER }
     *
     * DisplayText ::= CHOICE {
     *      ia5String        IA5String      (SIZE (1..200)),
     *      visibleString    VisibleString  (SIZE (1..200)),
     *      bmpString        BMPString      (SIZE (1..200)),
     *      utf8String       UTF8String     (SIZE (1..200)) }
     * </pre>
     *
     * @param policy    Pol&iacute;tica de la firma.
     * @return          Estructura con la pol&iacute;tica preparada para insertarla en la firma.
     */
    private static PolicyInformation[] getPolicyInformation(final AdESPolicy policy){

        if (policy == null) {
            throw new IllegalArgumentException("La politica de firma no puede ser nula en este punto"); //$NON-NLS-1$
        }

        /*
         * PolicyQualifierInfo ::= SEQUENCE {
         *          policyQualifierId  PolicyQualifierId,
         *          qualifier          ANY DEFINED BY policyQualifierId }
         */

        final PolicyQualifierId pqid = PolicyQualifierId.id_qt_cps;
        DERIA5String uri = null;

        if (policy.getPolicyQualifier()!=null && !policy.getPolicyQualifier().equals("")){ //$NON-NLS-1$
            uri = new DERIA5String(policy.getPolicyQualifier().toString());
        }

        final ASN1EncodableVector v = new ASN1EncodableVector();
        PolicyQualifierInfo pqi = null;
        if(uri != null){
            v.add(pqid);
            v.add(uri);
            pqi = new PolicyQualifierInfo(new DERSequence(v));
        }

        /*
         * PolicyInformation ::= SEQUENCE {
         *     policyIdentifier   CertPolicyId,
         *     policyQualifiers   SEQUENCE SIZE (1..MAX) OF
         *                          PolicyQualifierInfo OPTIONAL }
         */

        if (policy.getPolicyQualifier()==null || pqi == null) {
            return new PolicyInformation[] {
                new PolicyInformation(new ASN1ObjectIdentifier(policy.getPolicyIdentifier().toLowerCase().replace("urn:oid:", ""))) //$NON-NLS-1$ //$NON-NLS-2$
            };
        }

        return new PolicyInformation[] {
            new PolicyInformation(new ASN1ObjectIdentifier(policy.getPolicyIdentifier().toLowerCase().replace("urn:oid:", "")), new DERSequence(pqi)) //$NON-NLS-1$ //$NON-NLS-2$
        };

    }

    /** Inicializa el contexto. */
    static ASN1EncodableVector initContexExpecific(final String digestAlgorithm,
                                                   final byte[] datos,
                                                   final String dataType,
                                                   final byte[] messageDigest) throws NoSuchAlgorithmException {
        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();

        // tipo de contenido
        if (dataType != null) {
            contexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new DERObjectIdentifier(dataType))));
        }

        // fecha de firma
        contexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));

        // MessageDigest
        contexExpecific.add(new Attribute(CMSAttributes.messageDigest, new DERSet(new DEROctetString((messageDigest != null) ? messageDigest : MessageDigest.getInstance(digestAlgorithm).digest(datos)))));

        return contexExpecific;
    }

    /** Firma y envuelve */
    static SignerInfo signAndEnvelope(final PrivateKeyEntry keyEntry,
                                             final String signatureAlgorithm,
                                             final AlgorithmIdentifier digAlgId,
                                             final SignerIdentifier identifier,
                                             final ASN1Set signedAttr,
                                             final ASN1Set unSignedAttr,
                                             final String keyAlgorithm,
                                             final ASN1Set signedAttr2) throws IOException {

        final AlgorithmIdentifier encAlgId = SigUtils.makeAlgId(AOAlgorithmID.getOID(keyAlgorithm));

        final ASN1OctetString sign2;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry, signedAttr2);
        }
        catch (final AOException ex) {
            throw new IOException("Error durante la firma: " + ex); //$NON-NLS-1$
        }

        // EN ESTE PUNTO YA TENEMOS EL NUEVO SIGNER
        return new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, unSignedAttr);

    }

    /** Obtiene la estructura ASN.1 de firma usando los atributos del firmante.
     * @param signatureAlgorithm
     *        Algoritmo para la firma
     * @param keyEntry
     *        Clave para firmar.
     * @param signedAttr2
     *        Atributos firmados
     * @return Firma de los atributos.
     * @throws AOException
     *         si ocurre cualquier error durante la firma */
    static ASN1OctetString firma(final String signatureAlgorithm, final PrivateKeyEntry keyEntry, final ASN1Set signedAttr2) throws AOException {

        final Signature sig;
        try {
            sig = Signature.getInstance(signatureAlgorithm);
        }
        catch (final Exception e) {
            throw new AOException("Error obteniendo la clase de firma para el algoritmo " + signatureAlgorithm, e); //$NON-NLS-1$
        }

        final byte[] tmp;
        try {
            tmp = signedAttr2.getEncoded(ASN1Encoding.DER);
        }
        catch (final IOException ex) {
            throw new AOException("Error obteniendo el contenido a firmar", ex); //$NON-NLS-1$
        }

        // Indicar clave privada para la firma
        try {
            sig.initSign(keyEntry.getPrivateKey());
        }
        catch (final Exception e) {
            throw new AOException("Error al inicializar la firma con la clave privada", e); //$NON-NLS-1$
        }

        // Actualizamos la configuracion de firma
        try {
            sig.update(tmp);
        }
        catch (final SignatureException e) {
            throw new AOException("Error al configurar la informacion de firma", e); //$NON-NLS-1$
        }

        // firmamos.
        final byte[] realSig;
        try {
            realSig = sig.sign();
        }
        catch (final Exception e) {
            throw new AOException("Error durante el proceso de firma", e); //$NON-NLS-1$
        }

        return new DEROctetString(realSig);
    }

    /** Carga la lista de certificados
     * @param signEnv
     * @param signerCertificateChain
     * @return ASN1EncodableVector
     * @throws IOException
     * @throws CertificateEncodingException */
    static ASN1EncodableVector loadCertificatesList(final SignedAndEnvelopedData signEnv, final X509Certificate[] signerCertificateChain) throws IOException,
                                                                                                                                    CertificateEncodingException {
        final ASN1EncodableVector signCerts = new ASN1EncodableVector();

        final Enumeration<?> cers = signEnv.getCertificates().getObjects();
        while (cers.hasMoreElements()) {
            signCerts.add((ASN1Sequence) cers.nextElement());
        }

        if (signerCertificateChain.length != 0) {
            for (final X509Certificate element : signerCertificateChain) {
                signCerts.add(Certificate.getInstance(ASN1Primitive.fromByteArray(element.getEncoded())));
            }
        }

        return signCerts;
    }

    /** Comprueba que el archivo a tratar no es nulo e inicializa la clave de
     * cifrado
     * @param config
     *        Configuracion de cifrado
     * @param certDest
     *        Certificado
     * @return Clave secreta
     * @throws NullPointerException */
    static SecretKey initEnvelopedData(final AOCipherConfig config, final X509Certificate[] certDest) {
        // Comprobamos que el archivo a tratar no sea nulo.
        if (certDest == null || certDest.length == 0) {
            throw new IllegalArgumentException("No se pueden envolver datos sin certificados destino."); //$NON-NLS-1$
        }

        // Asignamos la clave de cifrado
        try {
            return assignKey(config);
        }
        catch (final Exception ex) {
            LOGGER.severe("Error durante el proceso de asignado de clave: " + ex); //$NON-NLS-1$
        }

        return null;
    }

    /** Asigna la clave para firmar el contenido del fichero que queremos
     * envolver y qeu m&aacute;s tarde ser&aacute; cifrada con la clave
     * p&uacute;blica del usuario que hace la firma.
     * @param config configuraci&oacute;n necesaria para crear la clave */
    private static SecretKey assignKey(final AOCipherConfig config) throws NoSuchAlgorithmException {
        final KeyGenerator kg = KeyGenerator.getInstance(config.getAlgorithm().getName());
        kg.init(new SecureRandom());
        return kg.generateKey();
    }

    /** Asigna la clave para firmar el contenido del fichero que queremos
     * envolver y qeu m&aacute;s tarde ser&aacute; cifrada con la clave
     * p&uacute;blica del usuario que hace la firma.
     * @param config
     *        Configuraci&oacute;n necesaria para crear la clave.
     * @param key
     *        Contrase&ntilde;a que se va a usar para cifrar.
     * @return Clave secreta */
    static SecretKey assignKey(final AOCipherConfig config, final String key) {

        // Generamos la clave necesaria para el cifrado
        if ((config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHMD5ANDDES)) || (config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHSHA1ANDDESEDE))
            || (config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHSHA1ANDRC2_40))) {
            try {
                return SecretKeyFactory.getInstance(config.getAlgorithm().getName()).generateSecret(new PBEKeySpec(key.toCharArray(),
                                                                                                                   SALT,
                                                                                                                   ITERATION_COUNT));
            }
            catch (final Exception ex) {
                LOGGER.severe("Error durante el proceso de asignacion de la clave (a partir de password): " + ex); //$NON-NLS-1$
            }
        }
        else {
            try {
				return new SecretKeySpec(Base64.decode(key), config.getAlgorithm().getName());
			} catch (final Exception e) {
				LOGGER.severe("La clave introducida no es un Base64 valido: " + e); //$NON-NLS-1$
			}
        }
        return null;
    }

    /** Obtiene un listado de certificados
     * @param signerCertificateChain
     *        Cadena de certificados firmantes
     * @return ASN1Set
     * @throws IOException
     * @throws CertificateEncodingException */
    static ASN1Set fetchCertificatesList(final X509Certificate[] signerCertificateChain) throws IOException, CertificateEncodingException {
        ASN1Set certificates = null;

        if (signerCertificateChain.length != 0) {
            final List<ASN1Encodable> ce = new ArrayList<ASN1Encodable>();
            for (final X509Certificate element : signerCertificateChain) {
                ce.add(Certificate.getInstance(ASN1Primitive.fromByteArray(element.getEncoded())));
            }
            certificates = SigUtils.createBerSetFromList(ce);
        }

        return certificates;
    }

}

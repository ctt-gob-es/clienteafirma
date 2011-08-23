/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.signers.pkcs7;

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
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.DEREncodable;
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
import org.bouncycastle.asn1.x509.DigestInfo;
import org.bouncycastle.asn1.x509.GeneralName;
import org.bouncycastle.asn1.x509.GeneralNames;
import org.bouncycastle.asn1.x509.IssuerSerial;
import org.bouncycastle.asn1.x509.PolicyInformation;
import org.bouncycastle.asn1.x509.PolicyQualifierInfo;
import org.bouncycastle.asn1.x509.SubjectPublicKeyInfo;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;
import org.bouncycastle.asn1.x509.X509CertificateStructure;
import org.bouncycastle.util.encoders.Base64;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.misc.AOConstants.AOCipherAlgorithm;
import es.gob.afirma.core.misc.AOConstants.AOCipherBlockMode;

/** Clase que contiene funciones comunes para CADES y CMS */
public final class Utils {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
    
    private static final byte[] SALT = {
            (byte) 0xA2, (byte) 0x35, (byte) 0xDC, (byte) 0xA4, (byte) 0x11, (byte) 0x7C, (byte) 0x99, (byte) 0x4B
    };

    private static final int ITERATION_COUNT = 9;

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

    /** Comprueba que el archivo a tratar no es nulo e inicializa la clave de
     * cifrado
     * @param config
     *        Configuracion de cifrado
     * @param certDest
     *        Certificado
     * @return Clave secreta
     * @throws NullPointerException */
    public static SecretKey initEnvelopedData(final AOCipherConfig config, final X509Certificate[] certDest) throws NullPointerException {
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
     * @param config
     *        configuraci&oacute;n necesaria para crear la clave.
     * @return */
    private static SecretKey assignKey(final AOCipherConfig config) throws NoSuchAlgorithmException {
        final SecureRandom rand = new SecureRandom();

        final KeyGenerator kg = KeyGenerator.getInstance(config.getAlgorithm().getName());
        kg.init(rand);
        final SecretKey encKey = kg.generateKey();
        return encKey;
    }

    /** Asigna la clave para firmar el contenido del fichero que queremos
     * envolver y qeu m&aacute;s tarde ser&aacute; cifrada con la clave
     * p&uacute;blica del usuario que hace la firma.
     * @param config
     *        Configuraci&oacute;n necesaria para crear la clave.
     * @param key
     *        Contrase&ntilde;a que se va a usar para cifrar.
     * @return Clave secreta */
    public static SecretKey assignKey(final AOCipherConfig config, final String key) {

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
            return new SecretKeySpec(Base64.decode(key), config.getAlgorithm().getName());
        }
        return null;
    }

    /** Obtiene un listado de certificados
     * @param signerCertificateChain
     *        Cadena de certificados firmantes
     * @return ASN1Set
     * @throws IOException
     * @throws CertificateEncodingException */
    public static ASN1Set fetchCertificatesList(final X509Certificate[] signerCertificateChain) throws IOException, CertificateEncodingException {
        ASN1Set certificates = null;

        if (signerCertificateChain.length != 0) {
            final List<DEREncodable> ce = new ArrayList<DEREncodable>();
            for (final X509Certificate element : signerCertificateChain) {
                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(element.getEncoded())));
            }
            certificates = SigUtils.createBerSetFromList(ce);
        }

        return certificates;
    }

    public static Info initVariables(final byte[] data, final AOCipherConfig config, final X509Certificate[] certDest, final SecretKey cipherKey) throws IOException,
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
            tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(cert.getTBSCertificate()));
            // Obtenemos el Isuer & serial number
            isse = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());
            // Creamos el recipientInfo
            rid = new RecipientIdentifier(isse);
            // Obtenemos la clave publica
            pubKey = cert.getPublicKey();
            // obtenemos la información de la clave publica
            info = tbs.getSubjectPublicKeyInfo();
            // obtenemos el algoritmo de cifrado.
            keyEncAlg = info.getAlgorithmId();

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
    public static EncryptedContentInfo getEncryptedContentInfo(final byte[] file, final AOCipherConfig config, final SecretKey cipherKey) throws NoSuchAlgorithmException,
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
     * @param file
     *        Archivo con los datos
     * @param config
     *        Configuracion de cifrado
     * @param params
     *        Parametros
     * @param cipher
     *        Encriptador
     * @return
     * @throws IOException */
    private static EncryptedContentInfo getEncryptedContentInfo(final byte[] file, final AOCipherConfig config, final AlgorithmParameterSpec params, final Cipher cipher) throws IOException {
        final byte[] ciphered;
        try {
            ciphered = cipher.doFinal(file);
        }
        catch (final Exception e) {
            LOGGER.severe("No se ha podido completar el cifrado, se devolvera null: " + e); //$NON-NLS-1$
            return null;
        }

        DEREncodable asn1Params;
        if (params != null) {
            final ASN1InputStream aIn = new ASN1InputStream(cipher.getParameters().getEncoded("ASN.1")); //$NON-NLS-1$
            asn1Params = aIn.readObject();
        }
        else {
            asn1Params = new DERNull();
        }

        // obtenemos el OID del algoritmo de cifrado
        final AlgorithmIdentifier encAlgId = new AlgorithmIdentifier(new DERObjectIdentifier(config.getAlgorithm().getOid()), asn1Params);

        // Obtenemos el identificador
        final DERObjectIdentifier contentType = PKCSObjectIdentifiers.encryptedData;
        return new EncryptedContentInfo(contentType, encAlgId, new DEROctetString(ciphered));
    }

    /** Crea el cifrador usado para cifrar tanto el fichero como la clave usada
     * para cifrar dicho fichero.
     * @param algName
     *        algoritmo utilizado para cifrar.
     * @param provider
     *        Proveedor que se utiliza para cifrar.
     * @throws java.security.NoSuchAlgorithmException
     * @throws javax.crypto.NoSuchPaddingException */
    private static Cipher createCipher(final String algName) throws NoSuchAlgorithmException, NoSuchPaddingException {
        return Cipher.getInstance(algName);
    }

    /** Genera los par&aacute;metros necesarios para poder operar con una
     * configuracion concreta de cifrado. Si no es necesario ning&uacute;n
     * par&aacute;metro especial, devolvemos <code>null</code>.
     * @param algorithmConfig
     *        Configuracion de cifrado que debemos parametrizar.
     * @return Par&aacute;metros para operar. */
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

    /** Inicializa el context
     * @param digestAlgorithm
     * @param datos
     * @param dataType
     * @param messageDigest
     * @return ASN1EncodableVector
     * @throws NoSuchAlgorithmException */
    public static ASN1EncodableVector initContexExpecific(final String digestAlgorithm, final byte[] datos, final String dataType, final byte[] messageDigest) throws NoSuchAlgorithmException {
        // authenticatedAttributes
        final ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

        // tipo de contenido
        if (dataType != null) {
            ContexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new DERObjectIdentifier(dataType.toString()))));
        }

        // fecha de firma
        ContexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));

        // MessageDigest�
        ContexExpecific.add(new Attribute(CMSAttributes.messageDigest, new DERSet(new DEROctetString((messageDigest != null) ? messageDigest : MessageDigest.getInstance(digestAlgorithm).digest(datos)))));

        return ContexExpecific;
    }

    /** Carga la lista de certificados
     * @param signEnv
     * @param signerCertificateChain
     * @return ASN1EncodableVector
     * @throws IOException
     * @throws CertificateEncodingException */
    public static ASN1EncodableVector loadCertificatesList(final SignedAndEnvelopedData signEnv, final X509Certificate[] signerCertificateChain) throws IOException,
                                                                                                                                    CertificateEncodingException {
        final ASN1EncodableVector signCerts = new ASN1EncodableVector();

        final Enumeration<?> cers = signEnv.getCertificates().getObjects();
        while (cers.hasMoreElements()) {
            signCerts.add((ASN1Sequence) cers.nextElement());
        }

        if (signerCertificateChain.length != 0) {
            for (final X509Certificate element : signerCertificateChain) {
                signCerts.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(element.getEncoded())));
            }
        }

        return signCerts;
    }

    /** Firma y envuelve
     * @param keyEntry
     * @param signatureAlgorithm
     * @param digAlgId
     * @param identifier
     * @param signedAttr
     * @param unSignedAttr
     * @param digestAlgorithmIdEnc
     * @param signedAttr2
     * @return SignerInfo
     * @throws IOException */
    public static SignerInfo signAndEnvelope(final PrivateKeyEntry keyEntry,
                                             final String signatureAlgorithm,
                                             final AlgorithmIdentifier digAlgId,
                                             final SignerIdentifier identifier,
                                             final ASN1Set signedAttr,
                                             final ASN1Set unSignedAttr,
                                             final sun.security.x509.AlgorithmId digestAlgorithmIdEnc,
                                             final ASN1Set signedAttr2) throws IOException {
        AlgorithmIdentifier encAlgId;
        try {
            encAlgId = SigUtils.makeAlgId(digestAlgorithmIdEnc.getOID().toString());
        }
        catch (final Exception e3) {
            throw new IOException("Error de codificacion: " + e3); //$NON-NLS-1$
        }

        ASN1OctetString sign2 = null;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry, signedAttr2);
        }
        catch (final AOException ex) {
            throw new IOException("Error durante la firma: " + ex); //$NON-NLS-1$
        }

        // EN ESTE PUNTO YA TENEMOS EL NUEVO SIGNER
        final SignerInfo nuevoSigner = new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, unSignedAttr// null //unsignedAttr
                );

        return nuevoSigner;
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
    public static ASN1OctetString firma(final String signatureAlgorithm, final PrivateKeyEntry keyEntry, final ASN1Set signedAttr2) throws AOException {

        Signature sig = null;
        try {
            sig = Signature.getInstance(signatureAlgorithm);
        }
        catch (final Exception e) {
            throw new AOException("Error obteniendo la clase de firma para el algoritmo " + signatureAlgorithm, e); //$NON-NLS-1$
        }

        byte[] tmp = null;

        try {
            tmp = signedAttr2.getEncoded(ASN1Encodable.DER);
        }
        catch (final IOException ex) {
            Logger.getLogger("No se han podido codificar en ASN.1 los atributos firmados: " + ex); //$NON-NLS-1$
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
            if (tmp != null) {
                sig.update(tmp);
            }
        }
        catch (final SignatureException e) {
            throw new AOException("Error al configurar la informacion de firma", e); //$NON-NLS-1$
        }

        // firmamos.
        byte[] realSig = null;
        try {
            realSig = sig.sign();
        }
        catch (final Exception e) {
            throw new AOException("Error durante el proceso de firma", e); //$NON-NLS-1$
        }

        final ASN1OctetString encDigest = new DEROctetString(realSig);

        return encDigest;
    }

    /** Obtiene la clave del algoritmo
     * @param signatureAlgorithm
     * @param digestAlgorithmId
     * @return keyAlgorithm
     * @throws IOException */
    public static String getKeyAlgorithm(final String signatureAlgorithm) throws IOException {
        try {
            String keyAlgorithm = null;
            final int with = signatureAlgorithm.indexOf("with"); //$NON-NLS-1$
            if (with > 0) {
                final int and = signatureAlgorithm.indexOf("and", with + 4); //$NON-NLS-1$
                if (and > 0) {
                    keyAlgorithm = signatureAlgorithm.substring(with + 4, and);
                }
                else {
                    keyAlgorithm = signatureAlgorithm.substring(with + 4);
                }
            }

            return keyAlgorithm;
        }
        catch (final Exception e) {
            throw new IOException("Error de codificacion: " + e); //$NON-NLS-1$
        }
    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos que se necesitan para generar la firma.
     * @param cert
     *        Certificado de firma.
     * @param digestAlgorithm
     *        Algoritmo Firmado.
     * @param digAlgId
     * @param datos
     *        Datos firmados.
     * @param politica
     * @param qualifier
     * @param signingCertificateV2
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @param messageDigest
     * @return Los datos necesarios para generar la firma referente a los datos
     *         del usuario.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.io.IOException
     * @throws CertificateEncodingException */
    public static ASN1EncodableVector generateSignerInfo(final X509Certificate cert,
                                                         final String digestAlgorithm,
                                                         final AlgorithmIdentifier digAlgId,
                                                         final byte[] datos,
                                                         final String politica,
                                                         final String qualifier,
                                                         final boolean signingCertificateV2,
                                                         final String dataType,
                                                         final byte[] messageDigest) throws NoSuchAlgorithmException,
                                                                              IOException,
                                                                              CertificateEncodingException {
        
        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector ContexExpecific = initContexExpecific(digestAlgorithm, datos, dataType, messageDigest);

        // Serial Number
        // comentar lo de abajo para version del rfc 3852
        ContexExpecific.add(new Attribute(RFC4519Style.serialNumber, new DERSet(new DERPrintableString(cert.getSerialNumber().toString()))));

        if (signingCertificateV2) {

            /********************************************/
            /* La Nueva operatividad esta comentada */
            /********************************************/
            // INICIO SINGING CERTIFICATE-V2

            /** IssuerSerial ::= SEQUENCE { issuer GeneralNames, serialNumber
             * CertificateSerialNumber */

            final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(cert.getTBSCertificate()));
            final GeneralName gn = new GeneralName(tbs.getIssuer());
            final GeneralNames gns = new GeneralNames(gn);

            final IssuerSerial isuerSerial = new IssuerSerial(gns, tbs.getSerialNumber());

            /** ESSCertIDv2 ::= SEQUENCE { hashAlgorithm AlgorithmIdentifier
             * DEFAULT {algorithm id-sha256}, certHash Hash, issuerSerial
             * IssuerSerial OPTIONAL }
             * Hash ::= OCTET STRING */

            final MessageDigest md = MessageDigest.getInstance(digestAlgorithm);
            final byte[] certHash = md.digest(cert.getEncoded());
            final ESSCertIDv2[] essCertIDv2 = {
                new ESSCertIDv2(digAlgId, certHash, isuerSerial)
            };

            /** PolicyInformation ::= SEQUENCE { policyIdentifier CertPolicyId,
             * policyQualifiers SEQUENCE SIZE (1..MAX) OF PolicyQualifierInfo
             * OPTIONAL }
             * CertPolicyId ::= OBJECT IDENTIFIER
             * PolicyQualifierInfo ::= SEQUENCE { policyQualifierId
             * PolicyQualifierId, qualifier ANY DEFINED BY policyQualifierId } */

            PolicyInformation[] pI;
            SigningCertificateV2 scv2 = null;
            if (qualifier != null) {

                final DERObjectIdentifier oidQualifier = new DERObjectIdentifier(qualifier.toString());
                if (politica.equals("")) { //$NON-NLS-1$
                    pI = new PolicyInformation[] {
                        new PolicyInformation(oidQualifier)
                    };
                }
                else {
                    final PolicyQualifierInfo pqInfo = new PolicyQualifierInfo(politica);
                    pI = new PolicyInformation[] {
                        new PolicyInformation(oidQualifier, new DERSequence(pqInfo))
                    };
                }

                /** SigningCertificateV2 ::= SEQUENCE { certs SEQUENCE OF
                 * ESSCertIDv2, policies SEQUENCE OF PolicyInformation OPTIONAL
                 * } */
                scv2 = new SigningCertificateV2(essCertIDv2, pI); // con
                                                                  // politica
            }
            else {
                scv2 = new SigningCertificateV2(essCertIDv2); // Sin politica
            }

            // Secuencia con singningCertificate
            ContexExpecific.add(new Attribute(PKCSObjectIdentifiers.id_aa_signingCertificateV2, new DERSet(scv2)));

            // FIN SINGING CERTIFICATE-V2

        }
        else {

            // INICIO SINGNING CERTIFICATE

            /** IssuerSerial ::= SEQUENCE { issuer GeneralNames, serialNumber
             * CertificateSerialNumber } */

            final TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(cert.getTBSCertificate()));
            final GeneralName gn = new GeneralName(tbs.getIssuer());
            final GeneralNames gns = new GeneralNames(gn);

            final IssuerSerial isuerSerial = new IssuerSerial(gns, tbs.getSerialNumber());

            /** ESSCertID ::= SEQUENCE { certHash Hash, issuerSerial IssuerSerial
             * OPTIONAL }
             * Hash ::= OCTET STRING -- SHA1 hash of entire certificate */
            final MessageDigest md = MessageDigest.getInstance(digestAlgorithm);
            final byte[] certHash = md.digest(cert.getEncoded());
            final ESSCertID essCertID = new ESSCertID(certHash, isuerSerial);

            /** PolicyInformation ::= SEQUENCE { policyIdentifier CertPolicyId,
             * policyQualifiers SEQUENCE SIZE (1..MAX) OF PolicyQualifierInfo
             * OPTIONAL }
             * CertPolicyId ::= OBJECT IDENTIFIER
             * PolicyQualifierInfo ::= SEQUENCE { policyQualifierId
             * PolicyQualifierId, qualifier ANY DEFINED BY policyQualifierId } */

            PolicyInformation[] pI;
            SigningCertificate scv = null;
            if (qualifier != null) {

                final DERObjectIdentifier oidQualifier = new DERObjectIdentifier(qualifier.toString());
                if (politica.equals("")) { //$NON-NLS-1$
                    pI = new PolicyInformation[] {
                        new PolicyInformation(oidQualifier)
                    };
                }
                else {
                    final PolicyQualifierInfo pqInfo = new PolicyQualifierInfo(politica);
                    pI = new PolicyInformation[] {
                        new PolicyInformation(oidQualifier, new DERSequence(pqInfo))
                    };
                }

                /** SigningCertificateV2 ::= SEQUENCE { certs SEQUENCE OF
                 * ESSCertIDv2, policies SEQUENCE OF PolicyInformation OPTIONAL
                 * } */
                /*
                 * HAY QUE HACER UN SEQUENCE, YA QUE EL CONSTRUCTOR DE BOUNCY
                 * CASTLE NO TIENE DICHO CONSTRUCTOR.
                 */
                final ASN1EncodableVector v = new ASN1EncodableVector();
                v.add(new DERSequence(essCertID));
                v.add(new DERSequence(pI));
                scv = new SigningCertificate(new DERSequence(v)); // con
                                                                  // politica
            }
            else {
                scv = new SigningCertificate(essCertID); // Sin politica
            }

            /** id-aa-signingCertificate OBJECT IDENTIFIER ::= { iso(1)
             * member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs9(9) smime(16)
             * id-aa(2) 12 } */
            // Secuencia con singningCertificate
            ContexExpecific.add(new Attribute(PKCSObjectIdentifiers.id_aa_signingCertificate, new DERSet(scv)));
        }

        // INICIO SIGPOLICYID ATTRIBUTE

        if (qualifier != null) {
            /*
             * SigPolicyId ::= OBJECT IDENTIFIER Politica de firma.
             */
            final DERObjectIdentifier DOISigPolicyId = new DERObjectIdentifier(qualifier.toString());

            /*
             * OtherHashAlgAndValue ::= SEQUENCE { hashAlgorithm
             * AlgorithmIdentifier, hashValue OCTET STRING }
             */
            final MessageDigest mdgest = MessageDigest.getInstance(digestAlgorithm);
            final byte[] hashed = mdgest.digest(politica.getBytes());
            final DigestInfo OtherHashAlgAndValue = new DigestInfo(digAlgId, hashed);

            /*
             * SigPolicyQualifierInfo ::= SEQUENCE { SigPolicyQualifierId
             * SigPolicyQualifierId, SigQualifier ANY DEFINED BY
             * policyQualifierId }
             */

            final SigPolicyQualifierInfo spqInfo = new SigPolicyQualifierInfo(politica);

            /*
             * SignaturePolicyId ::= SEQUENCE { sigPolicyId SigPolicyId,
             * sigPolicyHash SigPolicyHash, sigPolicyQualifiers SEQUENCE SIZE
             * (1..MAX) OF SigPolicyQualifierInfo OPTIONAL}
             */
            final ASN1EncodableVector v = new ASN1EncodableVector();
            // sigPolicyId
            v.add(DOISigPolicyId);
            // sigPolicyHash
            v.add(OtherHashAlgAndValue.toASN1Object()); // como sequence
            // sigPolicyQualifiers
            v.add(spqInfo.toASN1Object());

            final DERSequence ds = new DERSequence(v);

            // Secuencia con singningCertificate
            ContexExpecific.add(new Attribute(PKCSObjectIdentifiers.id_aa_ets_sigPolicyId, new DERSet(ds.toASN1Object())));
            // FIN SIGPOLICYID ATTRIBUTE
        }

        return ContexExpecific;
    }
}

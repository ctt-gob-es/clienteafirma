package es.gob.afirma.signers.aobinarysignhelper;

import static es.gob.afirma.signers.aobinarysignhelper.SigUtils.createBerSetFromList;
import static es.gob.afirma.signers.aobinarysignhelper.SigUtils.getAttributeSet;
import static es.gob.afirma.signers.aobinarysignhelper.SigUtils.makeAlgId;

import java.io.IOException;
import java.security.AlgorithmParameters;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.Key;
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
import java.security.spec.InvalidKeySpecException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.KeyGenerator;
import javax.crypto.Mac;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.PBEParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.apache.commons.codec.binary.Base64;
import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.BERSet;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DERInteger;
import org.bouncycastle.asn1.DERNull;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERPrintableString;
import org.bouncycastle.asn1.DERSequence;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.DERUTCTime;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.AuthEnvelopedData;
import org.bouncycastle.asn1.cms.AuthenticatedData;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.EnvelopedData;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.KeyTransRecipientInfo;
import org.bouncycastle.asn1.cms.OriginatorInfo;
import org.bouncycastle.asn1.cms.RecipientIdentifier;
import org.bouncycastle.asn1.cms.RecipientInfo;
import org.bouncycastle.asn1.cms.SignedData;
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
import org.ietf.jgss.Oid;

import sun.security.x509.AlgorithmId;
import es.gob.afirma.ciphers.AOCipherConfig;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOInvalidRecipientException;
import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;
import es.gob.afirma.misc.AOConstants.AOCipherBlockMode;
import es.gob.afirma.misc.AOCryptoUtil;

/** Clase que contiene funciones comunes para CADES y CMS */
final class Utils {

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

    private static final String ENCRYPTION_ALG_DEFAULT = "HmacSHA512";

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
            throw new NullPointerException("No se pueden envolver datos sin certificados destino.");
        }

        // Asignamos la clave de cifrado
        try {
            return assignKey(config);
        }
        catch (Exception ex) {
            Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de asignado de clave: " + ex);
        }

        return null;
    }

    /** Asigna la clave para firmar el contenido del fichero que queremos
     * envolver y qeu m&aacute;s tarde ser&aacute; cifrada con la clave
     * p&uacute;blica del usuario que hace la firma.
     * @param config
     *        configuraci&oacute;n necesaria para crear la clave.
     * @return */
    private static SecretKey assignKey(AOCipherConfig config) throws NoSuchAlgorithmException {
        SecureRandom rand = new SecureRandom();

        KeyGenerator kg = KeyGenerator.getInstance(config.getAlgorithm().getName());
        kg.init(rand);
        SecretKey encKey = kg.generateKey();
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
            catch (Exception ex) {
                Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de asignacion de la clave (a partir de password): " + ex);
            }
        }
        else {
            return new SecretKeySpec(Base64.decodeBase64(key), config.getAlgorithm().getName());
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
            List<DEREncodable> ce = new ArrayList<DEREncodable>();
            for (int i = 0; i < signerCertificateChain.length; i++)
                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[i].getEncoded())));
            certificates = createBerSetFromList(ce);
        }

        return certificates;
    }

    public static Info initVariables(byte[] data, AOCipherConfig config, X509Certificate[] certDest, SecretKey cipherKey) throws IOException,
                                                                                                                         CertificateEncodingException {

        // Reiniciamos las dos variables
        Info infos = new Info();

        ASN1EncodableVector recipientInfos = new ASN1EncodableVector();
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

        for (int contCert = 0; contCert < certDest.length; contCert++) {
            cert = certDest[contCert];
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
                Logger.getLogger("es.gob.afirma").severe("Error durante el proceso cifrado de la clave: " + e);
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
            Logger.getLogger("es.gob.afirma").severe("Error durante el proceso cifrado de la clave: " + e);
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
    public static EncryptedContentInfo getEncryptedContentInfo(byte[] file, AOCipherConfig config, SecretKey cipherKey) throws NoSuchAlgorithmException,
                                                                                                                       NoSuchPaddingException,
                                                                                                                       InvalidAlgorithmParameterException,
                                                                                                                       InvalidKeyException,
                                                                                                                       IOException {

        AlgorithmParameterSpec params = getParams(config);
        Cipher cipher = createCipher(config.toString());
        cipher.init(Cipher.ENCRYPT_MODE, cipherKey, params);
        return getEncryptedContentInfo(file, config, params, cipher);
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
     * @param cipherKey
     *        Clave de cifrado.
     * @param config
     *        Configuraci&oacute;n de cifrado.
     * @return Un sistema EncryptedContentInfo.
     * @throws java.security.NoSuchProviderException
     * @throws java.security.NoSuchAlgorithmException
     * @throws javax.crypto.NoSuchPaddingException
     * @throws java.security.InvalidAlgorithmParameterException
     * @throws java.security.InvalidKeyException
     * @throws java.io.IOException
     * @throws javax.crypto.IllegalBlockSizeException
     * @throws javax.crypto.BadPaddingException */
    public static EncryptedContentInfo getEncryptedContentInfo(final byte[] file, final Key cipherKey, final AOCipherConfig config) throws NoSuchAlgorithmException,
                                                                                                                                   NoSuchPaddingException,
                                                                                                                                   InvalidAlgorithmParameterException,
                                                                                                                                   InvalidKeyException,
                                                                                                                                   IOException {
        AlgorithmParameterSpec params = Utils.getParams(config);
        Cipher cipher = createCipher(config.toString());
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
    private static EncryptedContentInfo getEncryptedContentInfo(byte[] file, AOCipherConfig config, AlgorithmParameterSpec params, Cipher cipher) throws IOException {
        byte[] ciphered = null;
        try {
            ciphered = cipher.doFinal(file);
        }
        catch (IllegalBlockSizeException ex) {
            Logger.getLogger(CMSEnvelopedData.class.getName()).log(Level.SEVERE, null, ex);
        }
        catch (BadPaddingException ex) {
            Logger.getLogger(CMSEnvelopedData.class.getName()).log(Level.SEVERE, null, ex);
        }

        DEREncodable asn1Params;
        if (params != null) {
            ASN1InputStream aIn = new ASN1InputStream(cipher.getParameters().getEncoded("ASN.1"));
            asn1Params = aIn.readObject();
        }
        else {
            asn1Params = new DERNull();
        }

        // obtenemos el OID del algoritmo de cifrado
        AlgorithmIdentifier encAlgId = new AlgorithmIdentifier(new DERObjectIdentifier(config.getAlgorithm().getOid()), asn1Params);

        // Obtenemos el identificador
        DERObjectIdentifier contentType = PKCSObjectIdentifiers.encryptedData;
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
    private static Cipher createCipher(String algName) throws NoSuchAlgorithmException, NoSuchPaddingException {
        return Cipher.getInstance(algName);
    }

    /** Genera los par&aacute;metros necesarios para poder operar con una
     * configuracion concreta de cifrado. Si no es necesario ning&uacute;n
     * par&aacute;metro especial, devolvemos <code>null</code>.
     * @param algorithmConfig
     *        Configuracion de cifrado que debemos parametrizar.
     * @return Par&aacute;metros para operar. */
    public static AlgorithmParameterSpec getParams(AOCipherConfig algorithmConfig) {

        AlgorithmParameterSpec params = null;
        if (algorithmConfig.getAlgorithm().supportsPassword()) params = new PBEParameterSpec(SALT, ITERATION_COUNT);
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
    private static byte[] cipherKey(PublicKey pKey, SecretKey cipherKey) throws NoSuchAlgorithmException,
                                                                        NoSuchPaddingException,
                                                                        InvalidKeyException,
                                                                        InvalidAlgorithmParameterException,
                                                                        IllegalBlockSizeException {

        Cipher cipher = createCipher(pKey.getAlgorithm());
        AlgorithmParameters params = cipher.getParameters();
        cipher.init(Cipher.WRAP_MODE, pKey, params);
        byte[] ciphered = cipher.wrap(cipherKey);

        return ciphered;
    }

    /** Inicializa el context
     * @param digestAlgorithm
     * @param datos
     * @param dataType
     * @param messageDigest
     * @return ASN1EncodableVector
     * @throws NoSuchAlgorithmException */
    public static ASN1EncodableVector initContexExpecific(String digestAlgorithm, byte[] datos, Oid dataType, byte[] messageDigest) throws NoSuchAlgorithmException {
        // authenticatedAttributes
        ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

        // tipo de contenido
        if (dataType != null) ContexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new DERObjectIdentifier(dataType.toString()))));

        // fecha de firma
        ContexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));

        // Si el hash nos viene de fuera lo reutilizamos
        if (messageDigest == null) {
            messageDigest = MessageDigest.getInstance(digestAlgorithm).digest(datos);
        }

        // MessageDigest�
        ContexExpecific.add(new Attribute(CMSAttributes.messageDigest, new DERSet(new DEROctetString(messageDigest))));

        return ContexExpecific;
    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos no firmados.
     * @param uatrib
     *        Lista de atributos no firmados que se insertar&aacute;n dentro
     *        del archivo de firma.
     * @return Los atributos no firmados de la firma. */
    public static ASN1Set generateUnsignedAtt(Map<Oid, byte[]> uatrib) {

        // // ATRIBUTOS

        // authenticatedAttributes
        ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

        // agregamos la lista de atributos a mayores.
        if (uatrib.size() != 0) {
            Iterator<Map.Entry<Oid, byte[]>> it = uatrib.entrySet().iterator();
            while (it.hasNext()) {
                Map.Entry<Oid, byte[]> e = it.next();
                ContexExpecific.add(new Attribute(
                // el oid
                                                  new DERObjectIdentifier((e.getKey()).toString()),
                                                  // el array de bytes en formato string
                                                  new DERSet(new DERPrintableString(e.getValue()))));
            }
        }
        else {
            return null;
        }

        return getAttributeSet(new AttributeTable(ContexExpecific));
    }

    public static byte[] genMac(String encryptionAlg, byte[] content, SecretKey ciphKey) throws NoSuchAlgorithmException, IOException {
        Mac mac;

        if (encryptionAlg == null || encryptionAlg.equals("")) mac = Mac.getInstance(ENCRYPTION_ALG_DEFAULT);
        else mac = Mac.getInstance(encryptionAlg);
        try {
            mac.init(ciphKey);
            return mac.doFinal(content);
        }
        catch (final Exception e) {
            throw new IOException("Error al generar el codigo de autenticacion Mac: " + e);
        }
    }

    public static OriginatorInfo checkCertificates(X509Certificate[] signerCertificateChain, OriginatorInfo origInfo, ASN1Set certs) throws IOException,
                                                                                                                                    CertificateEncodingException {
        // Si no hay certificados, se deja como esta.
        if (signerCertificateChain.length != 0) {
            // no tiene remitentes
            if (certs == null) {
                ASN1Set certificates = null;
                ASN1Set certrevlist = null;
                List<DEREncodable> ce = new ArrayList<DEREncodable>();
                for (int i = 0; i < signerCertificateChain.length; i++)
                    if (signerCertificateChain[i] != null) ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[i].getEncoded())));
                // se introducen la nueva cadena de certificados.
                if (ce.size() != 0) {
                    certificates = createBerSetFromList(ce);
                    origInfo = new OriginatorInfo(certificates, certrevlist);
                }
            }
            // tiene remitentes
            else {
                // Se obtienen los certificados que tenia la firma.
                ASN1EncodableVector v = new ASN1EncodableVector();
                if (certs.getObjectAt(0) instanceof DERSequence) {
                    ASN1EncodableVector subv = new ASN1EncodableVector();
                    for (int i = 0; i < certs.size(); i++) {
                        subv.add(certs.getObjectAt(i));
                    }
                    v.add(new BERSet(subv));
                }
                else {
                    for (int i = 0; i < certs.size(); i++) {
                        v.add(certs.getObjectAt(i));
                    }
                }

                ASN1Set certificates = null;
                ASN1Set certrevlist = new BERSet(new ASN1EncodableVector());
                List<DEREncodable> ce = new ArrayList<DEREncodable>();
                for (int i = 0; i < signerCertificateChain.length; i++)
                    if (signerCertificateChain[i] != null) ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[i].getEncoded())));
                // se introducen la nueva cadena de certificados.
                if (ce.size() != 0) {
                    certificates = createBerSetFromList(ce);
                    v.add(certificates);
                    origInfo = new OriginatorInfo(new BERSet(v), certrevlist);
                }
            }
        }
        return origInfo;
    }

    /** Obtiene los parametros de los certificados
     * @param userCert
     *        Certificado del usuario
     * @param elementRecipient
     *        Listado de destinatarios
     * @return EncryptedKeyDatas
     * @throws AOInvalidRecipientException
     * @throws IOException
     * @throws CertificateEncodingException */
    public static EncryptedKeyDatas fetchEncryptedKeyDatas(X509Certificate userCert, Enumeration<?> elementRecipient) throws AOInvalidRecipientException,
                                                                                                                     IOException,
                                                                                                                     CertificateEncodingException {

        EncryptedKeyDatas encryptedKeyDatas = new EncryptedKeyDatas();
        AlgorithmIdentifier algEncryptedKey = null;
        byte[] encryptedKey = null;

        // Obtenemos los datos del certificado destino.
        IssuerAndSerialNumber isse;
        TBSCertificateStructure tbs = null;

        tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(userCert.getTBSCertificate()));
        // Obtenemos el Isuer & serial number
        isse = new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue());

        // obtenesmos los recipientInfo.
        RecipientInfo reci = null;
        while (elementRecipient.hasMoreElements()) {
            // obtengo los recipientInfo
            ASN1Sequence intermedio = (ASN1Sequence) elementRecipient.nextElement();
            reci = RecipientInfo.getInstance(intermedio);
            KeyTransRecipientInfo kri = KeyTransRecipientInfo.getInstance(reci.getDERObject());
            IssuerAndSerialNumber actual = IssuerAndSerialNumber.getInstance(kri.getRecipientIdentifier().getDERObject());
            // Comparo el issuer y el serial number con el certificado que me
            // pasan para descifrar.
            if (actual.equals(isse)) {
                // Obtengo los datos para descifrar.
                encryptedKey = kri.getEncryptedKey().getOctets();
                algEncryptedKey = kri.getKeyEncryptionAlgorithm();
            }
        }

        // si no se encuentran coincidencias es tonteria continuar.
        if ((encryptedKey == null) || (algEncryptedKey == null)) {
            throw new AOInvalidRecipientException("El usuario indicado no es uno de los destinatarios del sobre digital.");
        }

        encryptedKeyDatas.setAlgEncryptedKey(algEncryptedKey);
        encryptedKeyDatas.setEncryptedKey(encryptedKey);

        return encryptedKeyDatas;
    }

    /** Obtiene los datos envueltos
     * @param cmsData
     *        Bytes con los datos
     * @return ASN1Sequence
     * @throws IOException */
    public static ASN1Sequence fetchWrappedData(byte[] cmsData) throws IOException {
        // Leemos el fichero que contiene el envoltorio
        ASN1InputStream is = new ASN1InputStream(cmsData);

        // Comenzamos a obtener los datos.
        ASN1Sequence dsq = (ASN1Sequence) is.readObject();
        Enumeration<?> e = dsq.getObjects();

        // Elementos que contienen los elementos OID EnvelopedData.
        e.nextElement();

        // Contenido de EnvelopedData
        ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
        ASN1Sequence authenticatedData = (ASN1Sequence) doj.getObject();
        return authenticatedData;
    }

    /** Descifra el contenido a partir de un fichero usando la clave del usuario.
     * @param file
     *        Contenido cifrado del sobre digital.
     * @param config
     *        Configuracion
     * @param cipherKey
     *        Clave de cifrado
     * @return Conteido descifrado.
     * @throws java.security.NoSuchAlgorithmException
     * @throws javax.crypto.NoSuchPaddingException
     * @throws java.security.InvalidAlgorithmParameterException
     * @throws java.security.InvalidKeyException
     * @throws java.io.IOException
     * @throws org.bouncycastle.cms.CMSException
     * @throws javax.crypto.IllegalBlockSizeException
     * @throws javax.crypto.BadPaddingException */
    public static byte[] deCipherContent(byte[] file, AOCipherConfig config, SecretKey cipherKey) throws NoSuchAlgorithmException,
                                                                                                 NoSuchPaddingException,
                                                                                                 InvalidAlgorithmParameterException,
                                                                                                 InvalidKeyException,
                                                                                                 IllegalBlockSizeException,
                                                                                                 BadPaddingException {
        // asignamos los par&aacute;metros
        AlgorithmParameterSpec params = getParams(config);
        // Creamos el cipher
        Cipher cipher = createCipher(config.toString());
        // inicializamos el cipher
        cipher.init(Cipher.DECRYPT_MODE, cipherKey, params);

        // desciframos.
        return cipher.doFinal(file);
    }

    /** Carga la clave de cifrado
     * @param config
     *        Configuracion
     * @param key
     *        Clave
     * @return Clave secreta
     * @throws InvalidKeySpecException
     * @throws NoSuchAlgorithmException */
    public static SecretKey loadCipherKey(AOCipherConfig config, String key) throws InvalidKeySpecException, NoSuchAlgorithmException {
        SecretKey cipherKey =
                SecretKeyFactory.getInstance(config.getAlgorithm().getName())
                                .generateSecret(new PBEKeySpec(key.toCharArray(), SALT, ITERATION_COUNT));

        return cipherKey;
    }

    /** Asigna la clave para firmar el contenido del fichero que queremos
     * envolver y que m&aacute;s tarde ser&aacute; cifrada con la clave
     * p&uacute;blica del usuario que hace la firma.
     * @param passCiphered
     *        Clave cifrada.
     * @param keyEntry
     *        Contrase&ntilde;a que se va a usar para descifrar.
     * @param algClave
     *        Algoritmo necesario para crear la clave.
     * @return Objeto con la configuracion y la clave de cifrado
     * @throws AOException
     *         Cuando no se pudo descifrar la clave con el certificado de
     *         usuario. */
    public static KeyAsigned assignKey(final byte[] passCiphered, final PrivateKeyEntry keyEntry, final AlgorithmIdentifier algClave) throws AOException {

        final KeyAsigned keyAsigned = new KeyAsigned();

        AOCipherAlgorithm algorithm = null;

        // obtenemos el algoritmo usado para cifrar la pass
        for (AOCipherAlgorithm algo : AOCipherAlgorithm.values()) {
            if (algo.getOid().equals(algClave.getAlgorithm().toString())) {
                algorithm = algo;
                break;
            }
        }

        if (algorithm == null) throw new AOException("No se ha podido determinal el algoritmo de cifrado de la clave");

        // establecemos como configuraci&oacute;n para descifrar el contenido
        // del paquete despu&eacute;s,
        keyAsigned.setConfig(new AOCipherConfig(algorithm, null, null));

        // Desembolvemos la clave usada para cifrar el contenido
        // a partir de la clave privada del certificado del usuario.
        try {
            byte[] encrypted = passCiphered;
            Cipher cipher2 = Cipher.getInstance("RSA/ECB/PKCS1Padding");
            cipher2.init(Cipher.UNWRAP_MODE, keyEntry.getPrivateKey());
            keyAsigned.setCipherKey((SecretKey) cipher2.unwrap(encrypted, algorithm.getName(), Cipher.SECRET_KEY));
            return keyAsigned;
        }
        catch (Exception e) {
            Logger.getLogger("es.gob.afirma").severe("Ocurri\u00F3 un error al recuperar la clave de cifrado del sobre digital: " + e);
            throw new AOException("Ocurri\u00F3 un error al recuperar la clave de cifrado del sobre digital", e);
        }
    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * usuario. Se generan los atributos que se necesitan para generar la firma.
     * @param digestAlgorithm
     *        Identifica el algoritmo utilizado firmado.
     * @param datos
     *        Datos firmados.
     * @param dataType
     *        Identifica el tipo del contenido a firmar.
     * @param uatrib
     *        Conjunto de atributos no firmados.
     * @return Los datos necesarios para generar la firma referente a los datos
     *         del usuario.
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.security.cert.CertificateException
     * @throws java.io.IOException */
    public static ASN1Set generateSignerInfo(String digestAlgorithm, byte[] datos, Oid dataType, Map<Oid, byte[]> uatrib) throws NoSuchAlgorithmException {

        // // ATRIBUTOS

        // authenticatedAttributes
        ASN1EncodableVector ContexExpecific = Utils.initContexExpecific(digestAlgorithm, datos, dataType, null);

        // agregamos la lista de atributos a mayores.
        if (uatrib.size() != 0) {
            Iterator<Entry<Oid, byte[]>> it = uatrib.entrySet().iterator();
            while (it.hasNext()) {
                Map.Entry<Oid, byte[]> e = it.next();
                ContexExpecific.add(new Attribute(
                // el oid
                                                  new DERObjectIdentifier((e.getKey()).toString()),
                                                  // el array de bytes en formato string
                                                  new DERSet(new DERPrintableString(e.getValue()))));
            }
        }
        else return null;

        return getAttributeSet(new AttributeTable(ContexExpecific));
    }

    /** Obtiene los datos de cifrado usados.
     * @param datos
     *        informacion de los datos cifrados sin formatear.
     * @return informacion de los datos cifrados. */
    public static String getEncryptedContentInfo(EncryptedContentInfo datos) {
        String info = "";

        // especificamos el tipo de contenido
        if (datos.getContentType().equals(PKCSObjectIdentifiers.encryptedData)) {
            info = info + "\tTipo: EncryptedData\n";
        }
        else {
            info = info + "\tTipo: " + datos.getContentType() + "\n";
        }

        // el algoritmo de cifrado de los datos
        AlgorithmIdentifier ai = datos.getContentEncryptionAlgorithm();
        AOCipherAlgorithm algorithm = null;
        AOCipherAlgorithm[] algos = AOCipherAlgorithm.values();

        // obtenemos el algoritmo usado para cifrar la pass
        for (int i = 0; i < algos.length; i++) {
            if (algos[i].getOid().equals(ai.getAlgorithm().toString())) {
                algorithm = algos[i];
            }
        }

        if (algorithm != null) {
            info = info + "\tAlgoritmo de cifrado: " + algorithm.getName() + "\n";
        }
        else {
            info = info + "\tOID del Algoritmo de cifrado: " + ai.getAlgorithm().toString() + "\n";
        }

        return info;
    }

    /** Carga la lista de certificados
     * @param signEnv
     * @param signerCertificateChain
     * @return ASN1EncodableVector
     * @throws IOException
     * @throws CertificateEncodingException */
    public static ASN1EncodableVector loadCertificatesList(SignedAndEnvelopedData signEnv, X509Certificate[] signerCertificateChain) throws IOException,
                                                                                                                                    CertificateEncodingException {
        ASN1EncodableVector signCerts = new ASN1EncodableVector();

        Enumeration<?> cers = signEnv.getCertificates().getObjects();
        while (cers.hasMoreElements()) {
            signCerts.add((ASN1Sequence) cers.nextElement());
        }

        if (signerCertificateChain.length != 0) {
            for (int i = 0; i < signerCertificateChain.length; i++)
                signCerts.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[i].getEncoded())));
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
    public static SignerInfo signAndEnvelope(PrivateKeyEntry keyEntry,
                                             String signatureAlgorithm,
                                             AlgorithmIdentifier digAlgId,
                                             SignerIdentifier identifier,
                                             ASN1Set signedAttr,
                                             ASN1Set unSignedAttr,
                                             AlgorithmId digestAlgorithmIdEnc,
                                             ASN1Set signedAttr2) throws IOException {
        AlgorithmIdentifier encAlgId;
        try {
            encAlgId = makeAlgId(digestAlgorithmIdEnc.getOID().toString(), digestAlgorithmIdEnc.getEncodedParams());
        }
        catch (final Exception e3) {
            throw new IOException("Error de codificacion: " + e3);
        }

        ASN1OctetString sign2 = null;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry, signedAttr2);
        }
        catch (AOException ex) {
            Logger.getLogger(GenSignedData.class.getName()).log(Level.SEVERE, null, ex);
        }

        // EN ESTE PUNTO YA TENEMOS EL NUEVO SIGNER
        SignerInfo nuevoSigner = new SignerInfo(identifier, digAlgId, signedAttr, encAlgId, sign2, unSignedAttr// null //unsignedAttr
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
            throw new AOException("Error obteniendo la clase de firma para el algoritmo " + signatureAlgorithm, e);
        }

        byte[] tmp = null;

        try {
            tmp = signedAttr2.getEncoded(ASN1Encodable.DER);
        }
        catch (final IOException ex) {
            Logger.getLogger("No se han podido codificar en ASN.1 los atributos firmados: " + ex);
        }

        // Indicar clave privada para la firma
        try {
            sig.initSign(keyEntry.getPrivateKey());
        }
        catch (final Exception e) {
            throw new AOException("Error al inicializar la firma con la clave privada", e);
        }

        // Actualizamos la configuracion de firma
        try {
            if (tmp != null) sig.update(tmp);
        }
        catch (final SignatureException e) {
            throw new AOException("Error al configurar la informacion de firma", e);
        }

        // firmamos.
        byte[] realSig = null;
        try {
            realSig = sig.sign();
        }
        catch (final Exception e) {
            throw new AOException("Error durante el proceso de firma", e);
        }

        ASN1OctetString encDigest = new DEROctetString(realSig);

        return encDigest;
    }

    /** Obtiene la informaci&oacute;n de diferentes tipos de formatos.
     * @param doj
     * @param envelopeType
     *        Tipo de formato: <li>0: EnvelopedData</li> <li>1: AuthenticatedData</li> <li>2: AuthEnvelopedData</li> <li>3: SignedAndEnvelopedData</li>
     *        <li>4: SignedData</li> <li>5: Encrypted</li>
     * @param tipoDetalle
     *        Tipo de datos (literal)
     * @param signBinaryType
     *        Tipo de firmado binario (CADES o CMS)
     * @return Representaci&oacute;n de los datos. */
    public static String extractData(ASN1TaggedObject doj, String envelopeType, String tipoDetalle, String signBinaryType) {
        String detalle = "";
        detalle = detalle + tipoDetalle;

        ASN1Set rins = null;
        EncryptedContentInfo encryptedContentInfo = null;
        ASN1Set unprotectedAttrs = null;
        DERInteger version = null;
        AlgorithmIdentifier aid = null;
        ContentInfo ci = null;
        ASN1Set authAttrs = null;
        ASN1Set ds = null;
        ASN1Set signerInfosSd = null;

        if (envelopeType.equals("0")) {
            EnvelopedData ed = new EnvelopedData((ASN1Sequence) doj.getObject());
            version = ed.getVersion();
            rins = ed.getRecipientInfos();
            encryptedContentInfo = ed.getEncryptedContentInfo();
            unprotectedAttrs = ed.getUnprotectedAttrs();
        }
        else if (envelopeType.equals("1")) {
            AuthenticatedData ed = new AuthenticatedData((ASN1Sequence) doj.getObject());
            version = ed.getVersion();
            rins = ed.getRecipientInfos();
            aid = ed.getMacAlgorithm();
            ci = ed.getEncapsulatedContentInfo();
            authAttrs = ed.getAuthAttrs();
            unprotectedAttrs = ed.getUnauthAttrs();
        }
        else if (envelopeType.equals("2")) {
            AuthEnvelopedData ed = new AuthEnvelopedData((ASN1Sequence) doj.getObject());
            version = ed.getVersion();
            rins = ed.getRecipientInfos();
            encryptedContentInfo = ed.getAuthEncryptedContentInfo();
            authAttrs = ed.getAuthAttrs();
            unprotectedAttrs = ed.getUnauthAttrs();
        }
        else if (envelopeType.equals("3")) {
            SignedAndEnvelopedData ed = new SignedAndEnvelopedData((ASN1Sequence) doj.getObject());
            version = ed.getVersion();
            rins = ed.getRecipientInfos();
            encryptedContentInfo = ed.getEncryptedContentInfo();
            signerInfosSd = ed.getSignerInfos();
        }
        else if (envelopeType.equals("4")) {
            SignedData ed = new SignedData((ASN1Sequence) doj.getObject());
            version = ed.getVersion();
            ds = ed.getDigestAlgorithms();
            ci = ed.getEncapContentInfo();
            signerInfosSd = ed.getSignerInfos();
        }
        else if (envelopeType.equals("5")) {
            ASN1Sequence ed = (ASN1Sequence) doj.getObject();
            version = DERInteger.getInstance(ed.getObjectAt(0));
            encryptedContentInfo = EncryptedContentInfo.getInstance(ed.getObjectAt(1));
            if (ed.size() == 3) unprotectedAttrs = (ASN1Set) ed.getObjectAt(2);
        }

        // obtenemos la version
        detalle = detalle + "Version: " + version + "\n";

        // recipientInfo
        if (rins != null) {
            if (!envelopeType.equals("4") && !envelopeType.equals("5") && rins.size() > 0) {
                detalle = detalle + "Destinatarios: \n";
            }
            for (int i = 0; i < rins.size(); i++) {
                KeyTransRecipientInfo kti = KeyTransRecipientInfo.getInstance(RecipientInfo.getInstance(rins.getObjectAt(i)).getInfo());
                detalle = detalle + " - Informacion de destino de firma " + (i + 1) + ":\n";
                final AlgorithmIdentifier diAlg = kti.getKeyEncryptionAlgorithm();

                // issuer y serial
                final IssuerAndSerialNumber iss = (IssuerAndSerialNumber) SignerIdentifier.getInstance(kti.getRecipientIdentifier().getId()).getId();
                detalle = detalle + "\tIssuer: " + iss.getName().toString() + "\n";
                detalle = detalle + "\tNumero de serie: " + iss.getSerialNumber() + "\n";

                // el algoritmo de cifrado de los datos
                AOCipherAlgorithm algorithm = null;
                AOCipherAlgorithm[] algos = AOCipherAlgorithm.values();

                // obtenemos el algoritmo usado para cifrar la pass
                for (int j = 0; j < algos.length; j++) {
                    if (algos[j].getOid().equals(diAlg.getAlgorithm().toString())) {
                        algorithm = algos[j];
                    }
                }
                if (algorithm != null) {
                    detalle = detalle + "\tAlgoritmo de cifrado: " + algorithm.getName() + "\n";
                }
                else {
                    detalle = detalle + "\tOID del algoritmo de cifrado: " + diAlg.getAlgorithm() + "\n";

                }
            }
        }

        if (envelopeType.equals("0") || envelopeType.equals("5")) {
            // obtenemos datos de los datos cifrados.
            detalle = detalle + "Informacion de los datos cifrados:\n";
            detalle = detalle + Utils.getEncryptedContentInfo(encryptedContentInfo);
        }
        else if (envelopeType.equals("1") && aid != null && ci != null) {
            // mac algorithm
            detalle = detalle + "OID del Algoritmo de MAC: " + aid.getAlgorithm() + "\n";

            // digestAlgorithm
            ASN1Sequence seq = (ASN1Sequence) doj.getObject();
            ASN1TaggedObject da = (ASN1TaggedObject) seq.getObjectAt(4);
            AlgorithmIdentifier dai = AlgorithmIdentifier.getInstance(da.getObject());
            detalle = detalle + "OID del Algoritmo de firma: " + dai.getAlgorithm() + "\n";

            // obtenemos datos de los datos cifrados.
            detalle = detalle + "OID del tipo de contenido: " + ci.getContentType() + "\n";

            detalle = getObligatorieAtrib(signBinaryType, detalle, authAttrs);
        }
        else if (envelopeType.equals("2")) {
            detalle = detalle + "Informacion de los datos cifrados:\n";
            detalle = detalle + getEncryptedContentInfo(encryptedContentInfo);

            detalle = getObligatorieAtrib(signBinaryType, detalle, authAttrs);
        }
        else if (envelopeType.equals("3")) {
            // algoritmo de firma
            ASN1Sequence seq = (ASN1Sequence) doj.getObject();
            ASN1Set da = (ASN1Set) seq.getObjectAt(2);
            AlgorithmIdentifier dai = AlgorithmIdentifier.getInstance(da.getObjectAt(0));
            detalle = detalle + "OID del Algoritmo de firma: " + dai.getAlgorithm() + "\n";

            // obtenemos datos de los datos cifrados.
            detalle = detalle + "Informacion de los datos cifrados:\n";
            detalle = detalle + Utils.getEncryptedContentInfo(encryptedContentInfo);
        }
        else if (envelopeType.equals("4") && ci != null && ds != null) {
            // algoritmo de firma
            AlgorithmIdentifier dai = AlgorithmIdentifier.getInstance(ds.getObjectAt(0));
            detalle = detalle + "OID del Algoritmo de firma: " + dai.getAlgorithm() + "\n";
            detalle = detalle + "OID del tipo de contenido: " + ci.getContentType() + "\n";
        }

        // obtenemos lo atributos opcionales
        if (!envelopeType.equals("3")) if (unprotectedAttrs == null) {
            detalle = detalle + "Atributos : No tiene atributos opcionales\n";
        }
        else {
            String atributos = getUnSignedAttributes(unprotectedAttrs.getObjects());
            detalle = detalle + "Atributos : \n";
            detalle = detalle + atributos;
        }
        else if (envelopeType.equals("3") || envelopeType.equals("4")) {
            // obtenemos el(los) firmate(s)
            if (signerInfosSd != null) {
                if (signerInfosSd.size() > 0) {
                    detalle = detalle + "Firmantes:\n";
                }
                for (int i = 0; i < signerInfosSd.size(); i++) {
                    SignerInfo si = new SignerInfo((ASN1Sequence) signerInfosSd.getObjectAt(i));

                    detalle = detalle + "- firmante " + (i + 1) + " :\n";
                    // version
                    detalle = detalle + "\tversion: " + si.getVersion() + "\n";
                    // signerIdentifier
                    SignerIdentifier sident = si.getSID();
                    IssuerAndSerialNumber iss = IssuerAndSerialNumber.getInstance(sident.getId());
                    detalle = detalle + "\tIssuer: " + iss.getName().toString() + "\n";
                    detalle = detalle + "\tNumero de serie: " + iss.getSerialNumber() + "\n";

                    // digestAlgorithm
                    AlgorithmIdentifier algId = si.getDigestAlgorithm();
                    detalle = detalle + "\tOID del algoritmo de firma de este firmante: " + algId.getAlgorithm() + "\n";

                    // obtenemos lo atributos obligatorios
                    ASN1Set sa = si.getAuthenticatedAttributes();
                    String satributes = "";
                    if (sa != null) {
                        satributes = getsignedAttributes(sa, signBinaryType);
                    }
                    detalle = detalle + "\tAtributos obligatorios : \n";
                    detalle = detalle + satributes;

                }
            }
        }
        return detalle;
    }

    /** Obtiene los atributos obligatorios
     * @param signBinaryType
     *        Tipo de firma binaria (CADES o CMS)
     * @param detalle
     * @param authAttrs
     * @return */
    private static String getObligatorieAtrib(String signBinaryType, String detalle, ASN1Set authAttrs) {
        // obtenemos lo atributos obligatorios
        if (authAttrs == null) {
            detalle = detalle + "Atributos Autenticados: No tiene atributos autenticados\n";
        }
        else {
            String atributos = getsignedAttributes(authAttrs, signBinaryType);
            detalle = detalle + "Atributos Autenticados: \n";
            detalle = detalle + atributos;
        }
        return detalle;
    }

    /** Obtiene los atributos obligatorios de una firma.
     * @param attributes
     *        Grupo de atributos opcionales
     * @param binarySignType
     *        Identifica el tipo de firma binaria (CMS o CADES)
     * @return lista de atributos concatenados. */
    public static String getsignedAttributes(ASN1Set attributes, String binarySignType) {
        String attributos = "";

        Enumeration<?> e = attributes.getObjects();

        while (e.hasMoreElements()) {
            ASN1Sequence a = (ASN1Sequence) e.nextElement();
            DERObjectIdentifier derIden = (DERObjectIdentifier) a.getObjectAt(0);
            // tipo de contenido de la firma.
            if (derIden.equals(CMSAttributes.contentType)) {
                attributos = attributos + "\t\tOID del tipo de contenido: " + a.getObjectAt(1) + "\n";
            }
            // Message digest de la firma
            if (derIden.equals(CMSAttributes.messageDigest)) {
                attributos = attributos + "\t\tContiene el atributo \"MessageDigest\"\n";
            }
            // la fecha de firma. obtenemos y casteamos a algo legible.
            if (derIden.equals(CMSAttributes.signingTime)) {
                ASN1Set time = (ASN1Set) a.getObjectAt(1);
                DERUTCTime d = (DERUTCTime) time.getObjectAt(0);
                Date date = null;
                try {
                    date = d.getDate();
                }
                catch (ParseException ex) {
                    Logger.getLogger("es.gob.afirma").warning("No es posible convertir la fecha");
                }
                SimpleDateFormat formatter = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss");
                String ds = formatter.format(date);

                attributos = attributos + "\t\tContiene fecha de firma: " + ds + "\n";
            }
            if (binarySignType.equals("CADES")) {
                // atributo signing certificate v2
                if (derIden.equals(PKCSObjectIdentifiers.id_aa_signingCertificateV2)) {
                    attributos = attributos + "\t\tContiene el atributo \"Signing Certificate V2\" \n";
                }
                // Politica de firma.
                if (derIden.equals(PKCSObjectIdentifiers.id_aa_ets_sigPolicyId)) {
                    attributos = attributos + "\t\tContiene la politica de la firma \n";
                }
            }
        }
        return attributos;
    }

    /** Obtiene los atributos opcionales de una firma cualquiera. En caso de ser
     * EncryptedData, usar el otro metodo, ya que por construccion no es posible
     * utilizar este.
     * @param attributes
     *        Grupo de atributos opcionales
     * @return lista de atributos concatenados. */
    private static String getUnSignedAttributes(Enumeration<?> e) {
        String attributos = "";

        while (e.hasMoreElements()) {
            ASN1Sequence a = (ASN1Sequence) e.nextElement();
            DERObjectIdentifier derIden = (DERObjectIdentifier) a.getObjectAt(0);
            // tipo de contenido de la firma.
            if (derIden.equals(CMSAttributes.contentType)) {
                attributos = attributos + "\tOID del tipo de contenido: " + a.getObjectAt(1) + "\n";
            }
            // Message digest de la firma
            if (derIden.equals(CMSAttributes.messageDigest)) {
                attributos = attributos + "\tContiene el atributo \"MessageDigest\"\n";
            }
            // la fecha de firma. obtenemos y casteamos a algo legible.
            if (derIden.equals(CMSAttributes.signingTime)) {
                ASN1Set time = (ASN1Set) a.getObjectAt(1);
                DERUTCTime d = (DERUTCTime) time.getObjectAt(0);
                Date date = null;
                try {
                    date = d.getDate();
                }
                catch (ParseException ex) {
                    Logger.getLogger("es.gob.afirma").warning("No es posible convertir la fecha");
                }
                SimpleDateFormat formatter = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss");
                String ds = formatter.format(date);

                attributos = attributos + "\tContiene fecha de firma: " + ds + "\n";
            }
            // contrafirma de la firma.
            if (derIden.equals(CMSAttributes.counterSignature)) {
                attributos = attributos + "\tContiene la contrafirma de la firma \n";
            }

        }
        return attributos;
    }

    /** Obtiene la informaci&oacute;n de un tipo Data.
     * @return Representaci&oacute;n de los datos. */
    public static String getFromData() {
        String detalle = "";
        detalle = detalle + "Tipo: Data\n";
        return detalle;
    }

    /** Obtiene el la clave del algoritmo
     * @param signatureAlgorithm
     * @param digestAlgorithmId
     * @return keyAlgorithm
     * @throws IOException */
    public static String getKeyAlgorithm(String signatureAlgorithm, AlgorithmId digestAlgorithmId) throws IOException {
        try {
            String keyAlgorithm = null;
            int with = signatureAlgorithm.indexOf("with");
            if (with > 0) {
                int and = signatureAlgorithm.indexOf("and", with + 4);
                if (and > 0) keyAlgorithm = signatureAlgorithm.substring(with + 4, and);
                else keyAlgorithm = signatureAlgorithm.substring(with + 4);
            }

            return keyAlgorithm;
        }
        catch (final Exception e) {
            throw new IOException("Error de codificacion: " + e);
        }
    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos que se necesitan para generar la firma.
     * @param cert
     *        Certificado de firma.
     * @param digestAlgorithmId
     *        Identificador del algoritmo de firma.
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
    public static ASN1EncodableVector generateSignerInfo(X509Certificate cert,
                                                         AlgorithmId digestAlgorithmId,
                                                         String digestAlgorithm,
                                                         AlgorithmIdentifier digAlgId,
                                                         byte[] datos,
                                                         String politica,
                                                         Oid qualifier,
                                                         boolean signingCertificateV2,
                                                         Oid dataType,
                                                         byte[] messageDigest) throws NoSuchAlgorithmException,
                                                                              IOException,
                                                                              CertificateEncodingException {
        // // ATRIBUTOS

        // authenticatedAttributes
        ASN1EncodableVector ContexExpecific = initContexExpecific(digestAlgorithm, datos, dataType, messageDigest);

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

            TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(cert.getTBSCertificate()));
            GeneralName gn = new GeneralName(tbs.getIssuer());
            GeneralNames gns = new GeneralNames(gn);

            IssuerSerial isuerSerial = new IssuerSerial(gns, tbs.getSerialNumber());

            /** ESSCertIDv2 ::= SEQUENCE { hashAlgorithm AlgorithmIdentifier
             * DEFAULT {algorithm id-sha256}, certHash Hash, issuerSerial
             * IssuerSerial OPTIONAL }
             * Hash ::= OCTET STRING */

            MessageDigest md = MessageDigest.getInstance(AOCryptoUtil.getDigestAlgorithmName(digestAlgorithmId.getName()));
            byte[] certHash = md.digest(cert.getEncoded());
            ESSCertIDv2[] essCertIDv2 = {
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

                DERObjectIdentifier oidQualifier = new DERObjectIdentifier(qualifier.toString());
                if (politica.equals("")) pI = new PolicyInformation[] {
                    new PolicyInformation(oidQualifier)
                };
                else {
                    PolicyQualifierInfo pqInfo = new PolicyQualifierInfo(politica);
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

            TBSCertificateStructure tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(cert.getTBSCertificate()));
            GeneralName gn = new GeneralName(tbs.getIssuer());
            GeneralNames gns = new GeneralNames(gn);

            IssuerSerial isuerSerial = new IssuerSerial(gns, tbs.getSerialNumber());

            /** ESSCertID ::= SEQUENCE { certHash Hash, issuerSerial IssuerSerial
             * OPTIONAL }
             * Hash ::= OCTET STRING -- SHA1 hash of entire certificate */
            // MessageDigest
            // Los DigestAlgorithms con SHA-2 tienen un guion:
            String digestAlgorithmName = AOCryptoUtil.getDigestAlgorithmName(digestAlgorithmId.getName());
            MessageDigest md = MessageDigest.getInstance(digestAlgorithmName);
            byte[] certHash = md.digest(cert.getEncoded());
            ESSCertID essCertID = new ESSCertID(certHash, isuerSerial);

            /** PolicyInformation ::= SEQUENCE { policyIdentifier CertPolicyId,
             * policyQualifiers SEQUENCE SIZE (1..MAX) OF PolicyQualifierInfo
             * OPTIONAL }
             * CertPolicyId ::= OBJECT IDENTIFIER
             * PolicyQualifierInfo ::= SEQUENCE { policyQualifierId
             * PolicyQualifierId, qualifier ANY DEFINED BY policyQualifierId } */

            PolicyInformation[] pI;
            SigningCertificate scv = null;
            if (qualifier != null) {

                DERObjectIdentifier oidQualifier = new DERObjectIdentifier(qualifier.toString());
                if (politica.equals("")) {
                    pI = new PolicyInformation[] {
                        new PolicyInformation(oidQualifier)
                    };
                }
                else {
                    PolicyQualifierInfo pqInfo = new PolicyQualifierInfo(politica);
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
                ASN1EncodableVector v = new ASN1EncodableVector();
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
            DERObjectIdentifier DOISigPolicyId = new DERObjectIdentifier(qualifier.toString());

            /*
             * OtherHashAlgAndValue ::= SEQUENCE { hashAlgorithm
             * AlgorithmIdentifier, hashValue OCTET STRING }
             */
            MessageDigest mdgest = MessageDigest.getInstance(digestAlgorithm);
            byte[] hashed = mdgest.digest(politica.getBytes());
            DigestInfo OtherHashAlgAndValue = new DigestInfo(digAlgId, hashed);

            /*
             * SigPolicyQualifierInfo ::= SEQUENCE { SigPolicyQualifierId
             * SigPolicyQualifierId, SigQualifier ANY DEFINED BY
             * policyQualifierId }
             */

            SigPolicyQualifierInfo spqInfo = new SigPolicyQualifierInfo(politica);

            /*
             * SignaturePolicyId ::= SEQUENCE { sigPolicyId SigPolicyId,
             * sigPolicyHash SigPolicyHash, sigPolicyQualifiers SEQUENCE SIZE
             * (1..MAX) OF SigPolicyQualifierInfo OPTIONAL}
             */
            ASN1EncodableVector v = new ASN1EncodableVector();
            // sigPolicyId
            v.add(DOISigPolicyId);
            // sigPolicyHash
            v.add(OtherHashAlgAndValue.toASN1Object()); // como sequence
            // sigPolicyQualifiers
            v.add(spqInfo.toASN1Object());

            DERSequence ds = new DERSequence(v);

            // Secuencia con singningCertificate
            ContexExpecific.add(new Attribute(PKCSObjectIdentifiers.id_aa_ets_sigPolicyId, new DERSet(ds.toASN1Object())));
            // FIN SIGPOLICYID ATTRIBUTE
        }

        return ContexExpecific;
    }
}

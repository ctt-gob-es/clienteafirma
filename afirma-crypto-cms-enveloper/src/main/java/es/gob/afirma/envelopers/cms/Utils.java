/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.envelopers.cms;

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
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
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
import org.bouncycastle.asn1.DERNull;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERPrintableString;
import org.bouncycastle.asn1.DERSequence;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.DERUTCTime;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.KeyTransRecipientInfo;
import org.bouncycastle.asn1.cms.OriginatorInfo;
import org.bouncycastle.asn1.cms.RecipientIdentifier;
import org.bouncycastle.asn1.cms.RecipientInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x500.X500Name;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.SubjectPublicKeyInfo;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;
import org.bouncycastle.asn1.x509.X509CertificateStructure;
import org.ietf.jgss.Oid;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherBlockMode;

/** Clase que contiene funciones comunes para CADES y CMS */
final class Utils {
    
    private Utils() {
        // No permitimos la instancacion
    }

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

    /** Algoritmo de autenticaci&oacute;n. */
    private static final String ENCRYPTION_ALG_DEFAULT = "HmacSHA512"; //$NON-NLS-1$

    /** Objeto para mostrar los logs de la clase. */
    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$
    
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
            throw new IllegalArgumentException("No se pueden envolver datos sin certificados destino"); //$NON-NLS-1$
        }

        // Asignamos la clave de cifrado
        try {
            return assignKey(config);
        }
        catch (final Exception ex) {
            LOGGER.severe("Error durante el proceso de asignado de clave, se devolvera null: " + ex); //$NON-NLS-1$
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

    /** Obtiene un listado de certificados
     * @param signerCertificateChain
     *        Cadena de certificados firmantes
     * @return ASN1Set
     * @throws IOException
     * @throws CertificateEncodingException */
    static ASN1Set fetchCertificatesList(final X509Certificate[] signerCertificateChain) throws IOException, CertificateEncodingException {
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

    /**
     * Crea la estructura interna para el ensobrado de datos.
     * @param data Datos que se desean ensobrar.
     * @param config Configraci&oacute;n para el cifrado.
     * @param certDest Certificados de los destinatarios del sobre.
     * @param cipherKey Clave para la identificaci&oacute;n del remitente..
     * @return Objeto con la informaci&oacute;n para la generaci&oacute;n del sobre.
     * @throws IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos.
     * @throws CertificateEncodingException Si se produce alguna excepci&oacute;n
     *         con los certificados de los usuarios.
     * @throws AOException Cuando ocurre un error durante la generaci&oacute;n del
     *         n&uacute;cleo del sobre.
     */
    static Info initVariables(final byte[] data, final AOCipherConfig config, final X509Certificate[] certDest, final SecretKey cipherKey) throws AOException, CertificateEncodingException, IOException {

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
            // obtenemos la informacion de la clave publica
            info = tbs.getSubjectPublicKeyInfo();
            // obtenemos el algoritmo de cifrado.
            keyEncAlg = info.getAlgorithmId();

            try {
                // ciframos la clave
                encryptedKey = cipherKey(pubKey, cipherKey);
            }
            catch (final Exception e) {
                throw new AOException("Error durante el proceso cifrado de la clave: " + e); //$NON-NLS-1$
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
            throw new AOException("Error durante el proceso cifrado de la clave: " + e); //$NON-NLS-1$
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
     * @throws BadPaddingException 
     * @throws IllegalBlockSizeException 
     * @throws org.bouncycastle.cms.CMSException */
    static EncryptedContentInfo getEncryptedContentInfo(final byte[] file, final AOCipherConfig config, final SecretKey cipherKey) throws NoSuchAlgorithmException,
                                                                                                                       NoSuchPaddingException,
                                                                                                                       InvalidAlgorithmParameterException,
                                                                                                                       InvalidKeyException,
                                                                                                                       IOException,
                                                                                                                       IllegalBlockSizeException,
                                                                                                                       BadPaddingException {

        final AlgorithmParameterSpec params = getParams(config);
        final Cipher cipher = createCipher(config.toString());
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
    static EncryptedContentInfo getEncryptedContentInfo(final byte[] file, final Key cipherKey, final AOCipherConfig config) throws NoSuchAlgorithmException,
                                                                                                                                   NoSuchPaddingException,
                                                                                                                                   InvalidAlgorithmParameterException,
                                                                                                                                   InvalidKeyException,
                                                                                                                                   IOException,
                                                                                                                                   IllegalBlockSizeException,
                                                                                                                                   BadPaddingException {
        final AlgorithmParameterSpec params = Utils.getParams(config);
        final Cipher cipher = createCipher(config.toString());
        cipher.init(Cipher.ENCRYPT_MODE, cipherKey, params);
        return getEncryptedContentInfo(file, config, params, cipher);
    }

    /** Obtiene el contenido de un archivo encriptado
     * @param file Archivo con los datos
     * @param config Configuracion de cifrado
     * @param params Parametros
     * @param cipher Encriptador */
    private static EncryptedContentInfo getEncryptedContentInfo(final byte[] file, final AOCipherConfig config, final AlgorithmParameterSpec params, final Cipher cipher) throws IOException, IllegalBlockSizeException, BadPaddingException {

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
        return new EncryptedContentInfo(contentType, encAlgId, new DEROctetString(cipher.doFinal(file)));
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
    static ASN1EncodableVector initContexExpecific(final String digestAlgorithm, final byte[] datos, final Oid dataType, byte[] messageDigest) throws NoSuchAlgorithmException {
        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();

        // tipo de contenido
        if (dataType != null) {
            contexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new DERObjectIdentifier(dataType.toString()))));
        }

        // fecha de firma
        contexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));

        // MessageDigest
        contexExpecific.add(new Attribute(CMSAttributes.messageDigest,
                new DERSet(new DEROctetString(
                        messageDigest != null ?
                                messageDigest :
                                    MessageDigest.getInstance(digestAlgorithm).digest(datos))))
        );

        return contexExpecific;
    }

    /** M&eacute;todo que genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos no firmados.
     * @param uatrib
     *        Lista de atributos no firmados que se insertar&aacute;n dentro
     *        del archivo de firma.
     * @return Los atributos no firmados de la firma. */
    static ASN1Set generateUnsignedAtt(final Map<Oid, byte[]> uatrib) {

        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();

        // agregamos la lista de atributos a mayores.
        if (uatrib.size() != 0) {
            final Iterator<Map.Entry<Oid, byte[]>> it = uatrib.entrySet().iterator();
            while (it.hasNext()) {
                final Map.Entry<Oid, byte[]> e = it.next();
                contexExpecific.add(new Attribute(
                // el oid
                                                  new DERObjectIdentifier((e.getKey()).toString()),
                                                  // el array de bytes en formato string
                                                  new DERSet(new DERPrintableString(e.getValue()))));
            }
        }
        else {
            return null;
        }

        return SigUtils.getAttributeSet(new AttributeTable(contexExpecific));
    }

    static byte[] genMac(final String encryptionAlg, final byte[] content, final SecretKey ciphKey) throws NoSuchAlgorithmException, IOException {
        Mac mac;
        if (encryptionAlg == null || encryptionAlg.equals("")) { //$NON-NLS-1$
            mac = Mac.getInstance(ENCRYPTION_ALG_DEFAULT);
        }
        else {
            mac = Mac.getInstance(encryptionAlg);
        }
        try {
            mac.init(ciphKey);
            return mac.doFinal(content);
        }
        catch (final Exception e) {
            throw new IOException("Error al generar el codigo de autenticacion Mac: " + e); //$NON-NLS-1$
        }
    }

    static OriginatorInfo checkCertificates(final X509Certificate[] signerCertificateChain, final ASN1Set certs) throws IOException,
                                                                                                                                    CertificateEncodingException {
        OriginatorInfo origInfo = null;
        // Si no hay certificados, se deja como esta.
        if (signerCertificateChain.length != 0) {
            // no tiene remitentes
            if (certs == null) {
                ASN1Set certificates = null;
                final ASN1Set certrevlist = null;
                final List<DEREncodable> ce = new ArrayList<DEREncodable>();
                for (final X509Certificate element : signerCertificateChain) {
                    if (element != null) {
                        ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(element.getEncoded())));
                    }
                }
                // se introducen la nueva cadena de certificados.
                if (ce.size() != 0) {
                    certificates = SigUtils.createBerSetFromList(ce);
                    origInfo = new OriginatorInfo(certificates, certrevlist);
                }
            }
            // tiene remitentes
            else {
                // Se obtienen los certificados que tenia la firma.
                final ASN1EncodableVector v = new ASN1EncodableVector();
                if (certs.getObjectAt(0) instanceof DERSequence) {
                    final ASN1EncodableVector subv = new ASN1EncodableVector();
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
                final ASN1Set certrevlist = new BERSet(new ASN1EncodableVector());
                final List<DEREncodable> ce = new ArrayList<DEREncodable>();
                for (final X509Certificate element : signerCertificateChain) {
                    if (element != null) {
                        ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(element.getEncoded())));
                    }
                }
                // se introducen la nueva cadena de certificados.
                if (ce.size() != 0) {
                    certificates = SigUtils.createBerSetFromList(ce);
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
    static EncryptedKeyDatas fetchEncryptedKeyDatas(final X509Certificate userCert, final Enumeration<?> elementRecipient) throws AOInvalidRecipientException,
                                                                                                                     IOException,
                                                                                                                     CertificateEncodingException {

        final EncryptedKeyDatas encryptedKeyDatas = new EncryptedKeyDatas();
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
            final ASN1Sequence intermedio = (ASN1Sequence) elementRecipient.nextElement();
            reci = RecipientInfo.getInstance(intermedio);
            final KeyTransRecipientInfo kri = KeyTransRecipientInfo.getInstance(reci.getDERObject());
            final IssuerAndSerialNumber actual = IssuerAndSerialNumber.getInstance(kri.getRecipientIdentifier().getDERObject());
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
            throw new AOInvalidRecipientException("El usuario indicado no es uno de los destinatarios del sobre digital."); //$NON-NLS-1$
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
    static ASN1Sequence fetchWrappedData(final byte[] cmsData) throws IOException {
        // Leemos el fichero que contiene el envoltorio
        final ASN1InputStream is = new ASN1InputStream(cmsData);

        // Comenzamos a obtener los datos.
        final ASN1Sequence dsq = (ASN1Sequence) is.readObject();
        final Enumeration<?> e = dsq.getObjects();

        // Elementos que contienen los elementos OID EnvelopedData.
        e.nextElement();

        // Contenido de EnvelopedData
        return (ASN1Sequence) ((ASN1TaggedObject) e.nextElement()).getObject();

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
    static byte[] deCipherContent(final byte[] file, final AOCipherConfig config, final SecretKey cipherKey) throws NoSuchAlgorithmException,
                                                                                                 NoSuchPaddingException,
                                                                                                 InvalidAlgorithmParameterException,
                                                                                                 InvalidKeyException,
                                                                                                 IllegalBlockSizeException,
                                                                                                 BadPaddingException {
        // asignamos los par&aacute;metros
        final AlgorithmParameterSpec params = getParams(config);
        // Creamos el cipher
        final Cipher cipher = createCipher(config.toString());
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
    static SecretKey loadCipherKey(final AOCipherConfig config, final String key) throws InvalidKeySpecException, NoSuchAlgorithmException {
        final SecretKey cipherKey =
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
    static KeyAsigned assignKey(final byte[] passCiphered, final PrivateKeyEntry keyEntry, final AlgorithmIdentifier algClave) throws AOException {

        final KeyAsigned keyAsigned = new KeyAsigned();

        AOCipherAlgorithm algorithm = null;

        // obtenemos el algoritmo usado para cifrar la pass
        for (final AOCipherAlgorithm algo : AOCipherAlgorithm.values()) {
            if (algo.getOid().equals(algClave.getAlgorithm().toString())) {
                algorithm = algo;
                break;
            }
        }

        if (algorithm == null) {
            throw new AOException("No se ha podido determinar el algoritmo de cifrado de la clave"); //$NON-NLS-1$
        }

        // establecemos como configuraci&oacute;n para descifrar el contenido
        // del paquete despu&eacute;s,
        keyAsigned.setConfig(new AOCipherConfig(algorithm, null, null));

        // Desembolvemos la clave usada para cifrar el contenido
        // a partir de la clave privada del certificado del usuario.
        try {
            final byte[] encrypted = passCiphered;
            final Cipher cipher2 = Cipher.getInstance("RSA/ECB/PKCS1Padding"); //$NON-NLS-1$
            cipher2.init(Cipher.UNWRAP_MODE, keyEntry.getPrivateKey());
            keyAsigned.setCipherKey((SecretKey) cipher2.unwrap(encrypted, algorithm.getName(), Cipher.SECRET_KEY));
            return keyAsigned;
        }
        catch (final Exception e) {
            throw new AOException("Ocurri\u00F3 un error al recuperar la clave de cifrado del sobre digital", e); //$NON-NLS-1$
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
    static ASN1Set generateSignerInfo(final String digestAlgorithm, final byte[] datos, final Oid dataType, final Map<Oid, byte[]> uatrib) throws NoSuchAlgorithmException {

        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = Utils.initContexExpecific(digestAlgorithm, datos, dataType, null);

        // agregamos la lista de atributos a mayores.
        if (uatrib.size() != 0) {
            final Iterator<Entry<Oid, byte[]>> it = uatrib.entrySet().iterator();
            while (it.hasNext()) {
                final Map.Entry<Oid, byte[]> e = it.next();
                contexExpecific.add(new Attribute(
                // el oid
                                                  new DERObjectIdentifier((e.getKey()).toString()),
                                                  // el array de bytes en formato string
                                                  new DERSet(new DERPrintableString(e.getValue()))));
            }
        }
        else {
            return null;
        }

        return SigUtils.getAttributeSet(new AttributeTable(contexExpecific));
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
            tmp = signedAttr2.getEncoded(ASN1Encodable.DER);
        }
        catch (final IOException ex) {
            throw new AOException("No se han podido codificar en ASN.1 los atributos firmados", ex); //$NON-NLS-1$
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
        final byte[] realSig;
        try {
            realSig = sig.sign();
        }
        catch (final Exception e) {
            throw new AOException("Error durante el proceso de firma", e); //$NON-NLS-1$
        }

        return new DEROctetString(realSig);

    }
}

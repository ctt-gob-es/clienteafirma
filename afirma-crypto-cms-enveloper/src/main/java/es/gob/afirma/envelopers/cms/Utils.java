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
import javax.crypto.spec.SecretKeySpec;

import org.spongycastle.asn1.ASN1Encodable;
import org.spongycastle.asn1.ASN1EncodableVector;
import org.spongycastle.asn1.ASN1Encoding;
import org.spongycastle.asn1.ASN1InputStream;
import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1OctetString;
import org.spongycastle.asn1.ASN1Primitive;
import org.spongycastle.asn1.ASN1Sequence;
import org.spongycastle.asn1.ASN1Set;
import org.spongycastle.asn1.ASN1TaggedObject;
import org.spongycastle.asn1.BERSet;
import org.spongycastle.asn1.DERNull;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.DERPrintableString;
import org.spongycastle.asn1.DERSequence;
import org.spongycastle.asn1.DERSet;
import org.spongycastle.asn1.DERUTCTime;
import org.spongycastle.asn1.cms.Attribute;
import org.spongycastle.asn1.cms.AttributeTable;
import org.spongycastle.asn1.cms.CMSAttributes;
import org.spongycastle.asn1.cms.EncryptedContentInfo;
import org.spongycastle.asn1.cms.IssuerAndSerialNumber;
import org.spongycastle.asn1.cms.KeyTransRecipientInfo;
import org.spongycastle.asn1.cms.OriginatorInfo;
import org.spongycastle.asn1.cms.RecipientIdentifier;
import org.spongycastle.asn1.cms.RecipientInfo;
import org.spongycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.spongycastle.asn1.x500.X500Name;
import org.spongycastle.asn1.x509.AlgorithmIdentifier;
import org.spongycastle.asn1.x509.Certificate;
import org.spongycastle.asn1.x509.TBSCertificate;

import es.gob.afirma.core.ciphers.AOCipherConfig;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherAlgorithm;
import es.gob.afirma.core.ciphers.CipherConstants.AOCipherBlockMode;

/** Funciones comunes para sobres digitales. */
final class Utils {

    private Utils() {
        // No permitimos la instancacion
    }

    private static final byte[] SALT = {
            (byte) 0xA2, (byte) 0x35, (byte) 0xDC, (byte) 0xA4, (byte) 0x11, (byte) 0x7C, (byte) 0x99, (byte) 0x4B
    };

    private static final int ITERATION_COUNT = 9;

    /** Vector de inicializaci&oacute;n de 8 bytes. Un vector de inicializaci&oacute;n
     * de 8 bytes es necesario para el uso de los algoritmos DES y DESede. */
    private static final byte[] IV_8 = {
            (byte) 0xC6, (byte) 0xBA, (byte) 0xDE, (byte) 0xA4, (byte) 0x76, (byte) 0x43, (byte) 0x32, (byte) 0x6B
    };

    /** Vector de inicializaci&oacute;n de 16 bytes. Un vector de inicializaci&oacute;n
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

    /** Comprueba que el archivo a tratar no es nulo e inicializa la clave de cifrado.
     * @param config Configuraci&oacute;n de cifrado.
     * @param keySize Tama&ntilde;o (en bits) de la clave de cifrado.
     * @return Clave secreta */
    static SecretKey initEnvelopedData(final AOCipherConfig config, final Integer keySize) {

        // Asignamos la clave de cifrado
        try {
            return assignKey(config, keySize);
        }
        catch (final Exception ex) {
            LOGGER.severe("Error durante el proceso de asignado de clave, se devolvera null: " + ex); //$NON-NLS-1$
        }

        return null;
    }

    /** Asigna la clave para firmar el contenido del fichero que queremos
     * envolver y que m&aacute;s tarde ser&aacute; cifrada con la clave
     * p&uacute;blica del usuario que hace la firma.
     * @param config configuraci&oacute;n necesaria para crear la clave.
     * @param keySize Tama&ntilde;o (en bits) de la clave.
     * @return Clave asignada.
     * @throws NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario. */
    private static SecretKey assignKey(final AOCipherConfig config,
    		                           final Integer keySize) throws NoSuchAlgorithmException {
        final KeyGenerator kg = KeyGenerator.getInstance(
    		config.getAlgorithm().getName()
		);
        if (keySize != null) {
        	kg.init(keySize.intValue(), new SecureRandom());
        }
        else {
        	kg.init(new SecureRandom());
        }
        return kg.generateKey();
    }

    /** Obtiene un listado de certificados.
     * @param signerCertificateChain Cadena de certificados del firmante.
     * @return ASN1Set Lista de certificados en formato ASN.1.
     * @throws IOException Cuando hay problemas de entrada / salida.
     * @throws CertificateEncodingException Cuando hay problemas relacionados con la
     *                                      codificaci&oacute;n de los certificados X.509.*/
    static ASN1Set fetchCertificatesList(final X509Certificate[] signerCertificateChain) throws IOException, CertificateEncodingException {
        if (signerCertificateChain.length != 0) {
            final List<ASN1Encodable> ce = new ArrayList<>();
            for (final X509Certificate element : signerCertificateChain) {
                ce.add(Certificate.getInstance(ASN1Primitive.fromByteArray(element.getEncoded())));
            }
            return EvelopUtils.createBerSetFromList(ce);
        }
        return null;
    }

    /** Crea la estructura interna para el ensobrado de datos.
     * @param data Datos que se desean ensobrar.
     * @param config Configraci&oacute;n para el cifrado.
     * @param certDest Certificados de los destinatarios del sobre.
     * @param cipherKey Clave para la identificaci&oacute;n del remitente..
     * @return Objeto con la informaci&oacute;n para la generaci&oacute;n del sobre.
     * @throws IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los
     *         datos.
     * @throws CertificateEncodingException Si se produce alguna excepci&oacute;n
     *         con los certificados de los usuarios.
     * @throws IllegalBlockSizeException Cuando hay problemas internos con los tama&ntilde;os de bloque de cifrado.
     * @throws InvalidAlgorithmParameterException Si no se soporta un par&aacute;metro necesario para un algoritmo.
     * @throws NoSuchPaddingException Cuando no se soporta un tipo de relleno necesario.
     * @throws NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario.
     * @throws InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave.
     * @throws BadPaddingException Cuando hay problemas con un relleno de datos. */
    static Info initVariables(final byte[] data,
    		                  final AOCipherConfig config,
    		                  final X509Certificate[] certDest,
    		                  final SecretKey cipherKey) throws CertificateEncodingException,
    		                                                    IOException,
    		                                                    InvalidKeyException,
    		                                                    NoSuchAlgorithmException,
    		                                                    NoSuchPaddingException,
    		                                                    InvalidAlgorithmParameterException,
    		                                                    IllegalBlockSizeException,
    		                                                    BadPaddingException {

        // Reiniciamos las dos variables
        final Info infos = new Info();

        final ASN1EncodableVector recipientInfos = new ASN1EncodableVector();

        for (final X509Certificate element : certDest) {

        	final TBSCertificate tbs = TBSCertificate.getInstance(ASN1Primitive.fromByteArray(element.getTBSCertificate()));

            // creamos el recipiente con los datos del destinatario.
            final KeyTransRecipientInfo keyTransRecipientInfo = new KeyTransRecipientInfo(
        		// Creamos el recipientInfo
        		new RecipientIdentifier(
                    // Obtenemos el issuer & serial number
        			new IssuerAndSerialNumber(X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue())
    			),
    			// obtenemos el algoritmo de cifrado (RSA / DSA).
    			tbs.getSubjectPublicKeyInfo().getAlgorithm(),
        		new DEROctetString(
    				cipherKey(
		        		// Obtenemos la clave publica
		        		element.getPublicKey(),
		        		cipherKey
		    		)
				)
    		);

            // Lo anadimos al recipiente de destinatarios.
            recipientInfos.add(new RecipientInfo(keyTransRecipientInfo));
        }

        // 3. ENCRIPTEDCONTENTINFO
        infos.setEncInfo(getEncryptedContentInfo(data, config, cipherKey));

        infos.setRecipientInfos(recipientInfos);

        return infos;
    }

    /** M&eacute;todo que obtiene el EncriptedContentInfo a partir del archivo a
     * cifrar. El contenido es el siguiente:
     * <pre>
     * <code>
     * EncryptedContentInfo ::= SEQUENCE {
     *     contentType ContentType,
     *     contentEncryptionAlgorithm ContentEncryptionAlgorithmIdentifier,
     *     encryptedContent [0] IMPLICIT EncryptedContent OPTIONAL
     * }
     * </code>
     * </pre>
     * @param file Archivo a cifrar.
     * @param config Configuraci&oacute;n de la clave de cifrado.
     * @param cipherKey Clave de cifrado
     * @return Un sistema EncryptedContentInfo.
     * @throws java.security.NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario.
     * @throws javax.crypto.NoSuchPaddingException Cuando el JRE no soporta alg&uacute;n tipo de relleno necesario.
     * @throws java.security.InvalidAlgorithmParameterException Si no se soporta un par&aacute;metro necesario para un algoritmo.
     * @throws java.security.InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave.
     * @throws java.io.IOException Cuando hay problemas de entrada / salida.
     * @throws BadPaddingException Cuando hay problemas con el relleno de datos.
     * @throws IllegalBlockSizeException Cuando hay problemas con un relleno de datos. */
    static EncryptedContentInfo getEncryptedContentInfo(final byte[] file,
    		                                            final AOCipherConfig config,
    		                                            final SecretKey cipherKey) throws NoSuchAlgorithmException,
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
     * <pre>
     * <code>
     * EncryptedContentInfo ::= SEQUENCE {
     *     contentType ContentType,
     *     contentEncryptionAlgorithm ContentEncryptionAlgorithmIdentifier,
     *     encryptedContent [0] IMPLICIT EncryptedContent OPTIONAL
     * }
     * </code>
     * </pre>
     * @param file Archivo a cifrar.
     * @param cipherKey Clave de cifrado.
     * @param config Configuraci&oacute;n de cifrado.
     * @return Un sistema EncryptedContentInfo.
     * @throws NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario.
     * @throws NoSuchPaddingException Cuando el JRE no soporta alg&uacute;n tipo de relleno necesario.
     * @throws InvalidAlgorithmParameterException Si no se soporta un par&aacute;metro necesario para un algoritmo.
     * @throws InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave.
     * @throws IOException Cuando hay problemas de entrada / salida.
     * @throws IllegalBlockSizeException Cuando hay problemas internos con los tama&ntilde;os de bloque de cifrado.
     * @throws BadPaddingException Cuando hay problemas con un relleno de datos. */
    static EncryptedContentInfo getEncryptedContentInfo(final byte[] file,
    		                                            final Key cipherKey,
    		                                            final AOCipherConfig config) throws NoSuchAlgorithmException,
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

    /** Obtiene el contenido de un archivo encriptado.
     * @param file Archivo con los datos.
     * @param config Configuracion de cifrado.
     * @param params Par&aacute;metros.
     * @param cipher Encriptador.
     * @return Contenido de un archivo encriptado.
     * @throws BadPaddingException Cuando hay problemas con un relleno de datos.
     * @throws IOException Cuando hay problemas con el tratamiento de datos.
     * @throws IllegalBlockSizeException Cuando hay problemas internos con los tama&ntilde;os de bloque de cifrado. */
    private static EncryptedContentInfo getEncryptedContentInfo(final byte[] file,
    		                                                    final AOCipherConfig config,
    		                                                    final AlgorithmParameterSpec params,
    		                                                    final Cipher cipher) throws IOException,
    		                                                                                IllegalBlockSizeException,
    		                                                                                BadPaddingException {
    	ASN1Encodable asn1Params;
        if (params != null) {
        	try (
    			final ASN1InputStream aIn = new ASN1InputStream(cipher.getParameters().getEncoded("ASN.1")); //$NON-NLS-1$
			) {
        		asn1Params = aIn.readObject();
        	}
        }
        else {
            asn1Params = DERNull.INSTANCE;
        }

        // obtenemos el OID del algoritmo de cifrado
        final AlgorithmIdentifier encAlgId = new AlgorithmIdentifier(new ASN1ObjectIdentifier(config.getAlgorithm().getOid()), asn1Params);

        // Obtenemos el identificador
        final ASN1ObjectIdentifier contentType = PKCSObjectIdentifiers.encryptedData;
        return new EncryptedContentInfo(
    		contentType,
    		encAlgId,
    		new DEROctetString(cipher.doFinal(file))
		);
    }

    /** Crea el cifrador usado para cifrar tanto el fichero como la clave usada
     * para cifrar dicho fichero.
     * @param algName algoritmo utilizado para cifrar.
     * @return Cifrador.
     * @throws NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario.
     * @throws NoSuchPaddingException Cuando el JRE no soporta alg&uacute;n tipo de relleno necesario. */
    private static Cipher createCipher(final String algName) throws NoSuchAlgorithmException,
                                                                    NoSuchPaddingException {
        return Cipher.getInstance(algName);
    }

    /** Genera los par&aacute;metros necesarios para poder operar con una
     * configuracion concreta de cifrado. Si no es necesario ning&uacute;n
     * par&aacute;metro especial, devolvemos <code>null</code>.
     * @param algorithmConfig Configuraci&oacute;n de cifrado que debemos parametrizar.
     * @return Par&aacute;metros para operar. */
    private static AlgorithmParameterSpec getParams(final AOCipherConfig algorithmConfig) {

        AlgorithmParameterSpec params = null;
        if (algorithmConfig.getAlgorithm().supportsPassword()) {
            params = new PBEParameterSpec(SALT, ITERATION_COUNT);
        }
        else {
            if (!algorithmConfig.getBlockMode().equals(AOCipherBlockMode.ECB)) {
                params = new IvParameterSpec(
            		algorithmConfig.getAlgorithm().equals(AOCipherAlgorithm.AES) ?
        				IV_16 :
        					IV_8
				);
            }
        }

        return params;
    }

    /** Cifra la clave usada para cifrar el archivo usando para ello la clave p&uacute;blica del
     * certificado del usuario.
     * @param pKey Clave p&uacute;blica del certificado.
     * @param cipherKey Clave de cifrado.
     * @return La clave cifrada en "WRAP_MODE".
     * @throws NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario.
     * @throws NoSuchPaddingException Cuando el JRE no soporta alg&uacute;n tipo de relleno necesario.
     * @throws InvalidKeyException Cuando hay problemas de adecuaci&oacute;n de la clave.
     * @throws IllegalBlockSizeException Cuando hay problemas internos con los tama&ntilde;os de
     *                                   bloque de cifrado. */
    private static byte[] cipherKey(final PublicKey pKey,
    		                        final SecretKey cipherKey) throws NoSuchAlgorithmException,
                                                                      NoSuchPaddingException,
                                                                      InvalidKeyException,
                                                                      IllegalBlockSizeException {
        final Cipher cipher = createCipher(pKey.getAlgorithm());
        cipher.init(Cipher.WRAP_MODE, pKey);
        return cipher.wrap(cipherKey);
    }

    /** Inicializa el contexto.
     * @param digestAlgorithm Algoritmo de huella digital.
     * @param datos Datos a firmar o envolver.
     * @param dataType Tipo de los datos a firmar o envolver.
     * @param messageDigest Huella digital de los datos a firmar o envolver.
     * @return ASN1EncodableVector Contexto codificado en ASN.1.
     * @throws NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario. */
    static ASN1EncodableVector initContexExpecific(final String digestAlgorithm,
    		                                       final byte[] datos,
    		                                       final String dataType,
    		                                       final byte[] messageDigest) throws NoSuchAlgorithmException {
        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = new ASN1EncodableVector();

        // tipo de contenido
        if (dataType != null) {
            contexExpecific.add(
        		new Attribute(
    				CMSAttributes.contentType,
    				new DERSet(
						new ASN1ObjectIdentifier(dataType)
					)
				)
    		);
        }

        // fecha de firma
        contexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));

        // MessageDigest
        contexExpecific.add(new Attribute(CMSAttributes.messageDigest,
            new DERSet(
        		new DEROctetString(
                    messageDigest != null ?
                            messageDigest :
                                MessageDigest.getInstance(digestAlgorithm).digest(datos))
        		)
			)
        );

        return contexExpecific;
    }

    /** Genera la parte que contiene la informaci&oacute;n del
     * Usuario. Se generan los atributos no firmados.
     * @param uatrib Lista de atributos no firmados que se insertar&aacute;n dentro
     *               del archivo de firma.
     * @return Atributos no firmados de la firma. */
    static ASN1Set generateUnsignedAtt(final Map<String, byte[]> uatrib) {

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
                    new ASN1ObjectIdentifier(e.getKey().toString()),
                    // el array de bytes en formato string
                    new DERSet(new DERPrintableString(new String(e.getValue()))))
                );
            }
        }
        else {
            return null;
        }

        return EvelopUtils.getAttributeSet(new AttributeTable(contexExpecific));
    }

    static byte[] genMac(final String encryptionAlg, final byte[] content, final SecretKey ciphKey) throws NoSuchAlgorithmException, InvalidKeyException {
        final Mac mac = encryptionAlg == null || encryptionAlg.equals("") ? Mac.getInstance(ENCRYPTION_ALG_DEFAULT) : Mac.getInstance(encryptionAlg); //$NON-NLS-1$
        mac.init(ciphKey);
        return mac.doFinal(content);
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
                final List<ASN1Encodable> ce = new ArrayList<>();
                for (final X509Certificate element : signerCertificateChain) {
                    if (element != null) {
                        ce.add(Certificate.getInstance(ASN1Primitive.fromByteArray(element.getEncoded())));
                    }
                }
                // se introducen la nueva cadena de certificados.
                if (ce.size() != 0) {
                    certificates = EvelopUtils.createBerSetFromList(ce);
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
                final List<ASN1Encodable> ce = new ArrayList<>();
                for (final X509Certificate element : signerCertificateChain) {
                    if (element != null) {
                        ce.add(Certificate.getInstance(ASN1Primitive.fromByteArray(element.getEncoded())));
                    }
                }
                // se introducen la nueva cadena de certificados.
                if (ce.size() != 0) {
                    certificates = EvelopUtils.createBerSetFromList(ce);
                    v.add(certificates);
                    origInfo = new OriginatorInfo(new BERSet(v), certrevlist);
                }
            }
        }
        return origInfo;
    }

    /** Obtiene los par&aacute;metros de los certificados.
     * @param userCert Certificado del usuario
     * @param elementRecipient Listado de destinatarios
     * @return Par&aacute;metros de los certificados (<code>EncryptedKeyDatas</code>).
     * @throws AOInvalidRecipientException Si el destinatario es inv&aacute;lido por cualquier motivo.
     * @throws IOException Cuando hay problemas de entrada / salida.
     * @throws CertificateEncodingException Si hay problemas con la codificaci&oacute;n de los certificados. */
    static EncryptedKeyDatas fetchEncryptedKeyDatas(final X509Certificate userCert,
    		                                        final Enumeration<?> elementRecipient) throws AOInvalidRecipientException,
                                                                                                  IOException,
                                                                                                  CertificateEncodingException {

        final EncryptedKeyDatas encryptedKeyDatas = new EncryptedKeyDatas();
        AlgorithmIdentifier algEncryptedKey = null;
        byte[] encryptedKey = null;

        // Obtenemos los datos del certificado destino
        final TBSCertificate tbs = TBSCertificate.getInstance(ASN1Primitive.fromByteArray(userCert.getTBSCertificate()));
        // Obtenemos el Isuer & serial number
        final IssuerAndSerialNumber isse = new IssuerAndSerialNumber(
    		X500Name.getInstance(tbs.getIssuer()), tbs.getSerialNumber().getValue()
		);

        // Obtenemos los recipientInfo
        RecipientInfo reci;
        while (elementRecipient.hasMoreElements()) {
            // obtengo los recipientInfo
            final ASN1Sequence intermedio = (ASN1Sequence) elementRecipient.nextElement();
            reci = RecipientInfo.getInstance(intermedio);
            final KeyTransRecipientInfo kri = KeyTransRecipientInfo.getInstance(reci.toASN1Primitive());
            final IssuerAndSerialNumber actual = IssuerAndSerialNumber.getInstance(kri.getRecipientIdentifier().toASN1Primitive());
            // Comparo el issuer y el serial number con el certificado que me
            // pasan para descifrar.
            if (actual.equals(isse)) {
                // Obtengo los datos para descifrar.
                encryptedKey = kri.getEncryptedKey().getOctets();
                algEncryptedKey = kri.getKeyEncryptionAlgorithm();
            }
        }

        // si no se encuentran coincidencias es tonteria continuar.
        if (encryptedKey == null || algEncryptedKey == null) {
            throw new AOInvalidRecipientException("El usuario indicado no es uno de los destinatarios del sobre digital"); //$NON-NLS-1$
        }

        encryptedKeyDatas.setAlgEncryptedKey(algEncryptedKey);
        encryptedKeyDatas.setEncryptedKey(encryptedKey);

        return encryptedKeyDatas;
    }

    /** Obtiene los datos envueltos
     * @param cmsData Datos CMS.
     * @return ASN1Sequence Secuencia con los datos envueltos.
     * @throws IOException Cuando hay problemas de entrada / salida. */
    static ASN1Sequence fetchWrappedData(final byte[] cmsData) throws IOException {

    	final ASN1Sequence dsq;
    	try (
	        // Leemos el fichero que contiene el envoltorio
	        final ASN1InputStream is = new ASN1InputStream(cmsData);
		) {
	        // Comenzamos a obtener los datos.
	        dsq = (ASN1Sequence) is.readObject();
    	}

        final Enumeration<?> e = dsq.getObjects();

        // Elementos que contienen los elementos OID EnvelopedData.
        e.nextElement();

        // Contenido de EnvelopedData
        return (ASN1Sequence) ((ASN1TaggedObject) e.nextElement()).getObject();

    }

    /** Descifra el contenido a partir de un fichero usando la clave del usuario.
     * @param file Contenido cifrado del sobre digital.
     * @param config Configuraci&oacute;n de descrifrado.
     * @param cipherKey Clave de cifrado.
     * @return Conteido descifrado.
     * @throws NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario.
     * @throws NoSuchPaddingException Cuando el JRE no soporta alg&uacute;n tipo de relleno necesario.
     * @throws InvalidAlgorithmParameterException Si no se soporta un par&aacute;metro necesario para un algoritmo.
     * @throws InvalidKeyException Cuando la clave proporcionada no es v&aacute;lida.
     * @throws IllegalBlockSizeException Cuando hay problemas internos con los tama&ntilde;os de bloque de cifrado.
     * @throws BadPaddingException Cuando hay problemas con el relleno de datos. */
    static byte[] deCipherContent(final byte[] file,
    		                      final AOCipherConfig config,
    		                      final SecretKey cipherKey) throws NoSuchAlgorithmException,
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

    /** Carga la clave de cifrado.
     * @param config Configuraci&oacute;n.
     * @param key Clave.
     * @return Clave secreta.
     * @throws InvalidKeySpecException Cuando hay problemas con la clave proporcionada.
     * @throws NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario. */
    static SecretKey loadCipherKey(final AOCipherConfig config, final String key) throws InvalidKeySpecException, NoSuchAlgorithmException {
        return SecretKeyFactory.getInstance(
    		config.getAlgorithm().getName()
		).generateSecret(
			new PBEKeySpec(key.toCharArray(), SALT, ITERATION_COUNT)
		);
    }

    /** Asigna la clave para firmar el contenido del fichero que queremos
     * envolver y que m&aacute;s tarde ser&aacute; cifrada con la clave
     * p&uacute;blica del usuario que hace la firma.
     * @param passCiphered Clave cifrada.
     * @param keyEntry Contrase&ntilde;a que se va a usar para descifrar.
     * @param algClave Algoritmo necesario para crear la clave.
     * @return Objeto con la configuracion y la clave de cifrado
     * @throws NoSuchPaddingException Cuando el JRE no soporta alg&uacute;n tipo de relleno necesario.
     * @throws NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario.
     * @throws InvalidKeyException Cuando la clave proporcionada no es v&aacute;lida.
     * @throws BadPaddingException Si el relleno a usar en la desenvoltura es distinto al que se us&oacute;
     *                             en la envoltura.
     * @throws IllegalBlockSizeException Si el tipo de bloque a usar en la desenvoltura es distinto al que se us&oacute;
     *                                   en la envoltura. */
    static KeyAsigned assignKey(final byte[] passCiphered,
    		                    final PrivateKeyEntry keyEntry,
    		                    final AlgorithmIdentifier algClave) throws NoSuchAlgorithmException,
    		                                                               NoSuchPaddingException,
    		                                                               InvalidKeyException,
    		                                                               IllegalBlockSizeException,
    		                                                               BadPaddingException {
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
            throw new NoSuchAlgorithmException("No se ha podido determinar el algoritmo de cifrado de la clave"); //$NON-NLS-1$
        }

        // establecemos como configuraci&oacute;n para descifrar el contenido
        // del paquete despu&eacute;s,
        keyAsigned.setConfig(new AOCipherConfig(algorithm, null, null));

        // Desenvolvemos la clave usada para cifrar el contenido
        // a partir de la clave privada del certificado del usuario.
        // Usamos un descifrado RSA en vez de una desenvoltura para evitar los
        // problemas de UNWRAPPING de ciertos controladores PKCS#11 (FNMT)
        final Cipher cipher2 = Cipher.getInstance("RSA/ECB/PKCS1Padding"); //$NON-NLS-1$
		cipher2.init(Cipher.DECRYPT_MODE, keyEntry.getPrivateKey());
		keyAsigned.setCipherKey(
			new SecretKeySpec(
				cipher2.doFinal(passCiphered),
				algorithm.getName()
			)
		);
        return keyAsigned;
    }

    /** Genera la parte que contiene la informaci&oacute;n del
     * usuario. Se generan los atributos que se necesitan para generar la firma.
     * @param digestAlgorithm Identifica el algoritmo utilizado firmado.
     * @param datos Datos firmados.
     * @param dataType Identifica el tipo del contenido a firmar.
     * @param uatrib Conjunto de atributos no firmados.
     * @return Los datos necesarios para generar la firma referente a los datos
     *         del usuario.
     * @throws NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario. */
    static ASN1Set generateSignerInfo(final String digestAlgorithm,
    		                          final byte[] datos,
    		                          final String dataType,
    		                          final Map<String, byte[]> uatrib) throws NoSuchAlgorithmException {

        // // ATRIBUTOS

        // authenticatedAttributes
        final ASN1EncodableVector contexExpecific = Utils.initContexExpecific(digestAlgorithm, datos, dataType, null);

        // agregamos la lista de atributos a mayores.
        if (uatrib.size() != 0) {
            final Iterator<Entry<String, byte[]>> it = uatrib.entrySet().iterator();
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
        else {
            return null;
        }

        return EvelopUtils.getAttributeSet(new AttributeTable(contexExpecific));
    }

    /** Obtiene la estructura ASN&#183;1 de firma usando los atributos del firmante.
     * @param signatureAlgorithm Algoritmo para la firma.
     * @param keyEntry Clave para firmar.
     * @param signedAttr2 Atributos firmados.
     * @return Firma de los atributos.
     * @throws NoSuchAlgorithmException Cuando el JRE no soporta alg&uacute;n algoritmo necesario.
     * @throws IOException Cuando hay problemas de entrada / salida.
     * @throws InvalidKeyException Cuando la clave proporcionada no es v&aacute;lida.
     * @throws SignatureException Cuando hay problemas con la firma PKCS#1. */
    static ASN1OctetString firma(final String signatureAlgorithm,
    		                     final PrivateKeyEntry keyEntry,
    		                     final ASN1Set signedAttr2) throws NoSuchAlgorithmException,
                                                                   IOException,
                                                                   InvalidKeyException,
                                                                   SignatureException {

        final Signature sig = Signature.getInstance(signatureAlgorithm);

        final byte[] tmp = signedAttr2.getEncoded(ASN1Encoding.DER);

        // Indicar clave privada para la firma
        sig.initSign(keyEntry.getPrivateKey());

        // Actualizamos la configuracion de firma
        if (tmp != null) {
            sig.update(tmp);
        }

        // firmamos.
        final byte[] realSig = sig.sign();

        return new DEROctetString(realSig);

    }
}

/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.signers.aobinarysignhelper;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.AlgorithmParameters;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.security.spec.AlgorithmParameterSpec;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.KeyGenerator;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEParameterSpec;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DERNull;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.DERUTCTime;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.EnvelopedData;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.KeyTransRecipientInfo;
import org.bouncycastle.asn1.cms.OriginatorInfo;
import org.bouncycastle.asn1.cms.RecipientIdentifier;
import org.bouncycastle.asn1.cms.RecipientInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.SubjectPublicKeyInfo;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;
import org.bouncycastle.asn1.x509.X509CertificateStructure;
import org.ietf.jgss.Oid;

import es.gob.afirma.ciphers.AOAlgorithmConfig;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;
import es.gob.afirma.misc.AOConstants.AOCipherBlockMode;

/**
 * Clase que implementa firma digital EnvelopedData en CADES
 * basada en PKCS#7/CMS EnvelopedData.
 * La Estructura del mensaje es la siguiente:<br>
 * <pre><code>

 * EnvelopedData ::= SEQUENCE {
 *     version CMSVersion,
 *     originatorInfo [0] IMPLICIT OriginatorInfo OPTIONAL,
 *     recipientInfos RecipientInfos,
 *     encryptedContentInfo EncryptedContentInfo,
 *     unprotectedAttrs [1] IMPLICIT UnprotectedAttributes OPTIONAL
 * }
 *
 *</code></pre>
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para crear un
 * mensaje Data de BouncyCastle: <a href="http://www.bouncycastle.org/">www.bouncycastle.org</a>
 */


public class CADESEnvelopedData extends SigUtils {

/**
	 * Clave de cifrado. La almacenamos internamente porque no hay forma de mostrarla
	 * directamente al usuario.
	 */
	private SecretKey cipherKey;


	private static final byte[] SALT = {
		(byte)0xA2, (byte)0x35, (byte)0xDC, (byte)0xA4,
		(byte)0x11, (byte)0x7C, (byte)0x99, (byte)0x4B
	};

    private static final int ITERATION_COUNT = 9;

    /**
	 * Vector de inicializacion de 8 bytes. Un vector de inicializaci&oacute;n
	 * de 8 bytes es necesario para el uso de los algoritmos DES y DESede.
	 */
	private static final byte[] IV_8 = {
		(byte)0xC6, (byte)0xBA, (byte)0xDE, (byte)0xA4,
		(byte)0x76, (byte)0x43, (byte)0x32, (byte)0x6B
	};

	/**
	 * Vector de inicializacion de 16 bytes. Un vector de inicializaci&oacute;n
	 * de 16 bytes es necesario para el uso de los algoritmos DES y DESede.
	 */
	private static final byte[] IV_16 = {
		(byte)0xB2, (byte)0xBA, (byte)0xDE, (byte)0xA4,
		(byte)0x41, (byte)0x7F, (byte)0x97, (byte)0x4B,
		(byte)0xAC, (byte)0x63, (byte)0xAC, (byte)0xAA,
		(byte)0x76, (byte)0x73, (byte)0x12, (byte)0x6B
	};

   /**
    * M&eacute;todo que genera la firma de tipo EnvelopedData.
    *
    * @param parameters  Par&aacute;metros necesarios para la generaci&oacute;n de este tipo.
    * @param config      Configuraci&oacute;n del algoritmo para firmar
    * @param certDest    Certificado del destino al cual va dirigido la firma.
    * @param dataType    Identifica el tipo del contenido a firmar.
    * 
    * @return            la firma de tipo EnvelopedData.
    * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
    * @throws java.security.cert.CertificateEncodingException Si se produce alguna excepci&oacute;n con los certificados de firma.
    * @throws java.security.NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella digital
    */
    public byte[] genEnvelopedData(P7ContentSignerParameters parameters,AOAlgorithmConfig config, X509Certificate[] certDest, Oid dataType) throws IOException, CertificateEncodingException, NoSuchAlgorithmException {

        //Comprobamos que el archivo a tratar no sea nulo.
		if (certDest==null || certDest.length == 0){
			throw new NullPointerException("No se pueden envolver datos sin certificados destino.");
		}
        
        // Asignamos la clave de cifrado
        // Generamos la clave necesaria para el cifrado

        try {
            assignKey(config);
        } 
        catch (Throwable ex) {
            Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de asignacion de clave: " + ex);
        }

         //Datos previos &uacute;tiles
        String digestAlgorithm = AOCryptoUtil.getDigestAlgorithmName(
        		parameters.getSignatureAlgorithm());
        
        // 1.  ORIGINATORINFO
        // obtenemos la lista de certificados
        ASN1Set certificates = null;
        ASN1Set certrevlist = null;
        X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();

        OriginatorInfo origInfo = null;
       
        if (signerCertificateChain.length != 0) {
            List<DEREncodable> ce = new ArrayList<DEREncodable>();
            for (int i=0; i<signerCertificateChain.length;i++)
                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[i].getEncoded())));
            certificates = createBerSetFromList(ce);

            origInfo = new OriginatorInfo(certificates, certrevlist);
        }
       

        // 2.   RECIPIENTINFOS

        //variables utilizadas
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
        //generamos el contenedor de cifrado
        EncryptedContentInfo encInfo = null;

        RecipientInfo recipient = null;

        for (int contCert=0;contCert<certDest.length;contCert++){
            cert = certDest[contCert];
            tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(cert.getTBSCertificate()));
            // Obtenemos el Isuer & serial number
            isse = new IssuerAndSerialNumber(tbs.getIssuer(), tbs.getSerialNumber().getValue());
            // Creamos el recipientInfo
            rid= new RecipientIdentifier(isse);
            // Obtenemos la clave publica
            pubKey = cert.getPublicKey();
            // obtenemos la información de la clave publica
            info = tbs.getSubjectPublicKeyInfo();
            // obtenemos el algoritmo de cifrado.
            keyEncAlg = info.getAlgorithmId();

            try {
                // ciframos la clave
                encryptedKey = cipherKey(pubKey);
                // 3.   ENCRIPTEDCONTENTINFO
                encInfo = getEncryptedContentInfo(parameters.getContent(), config);
            } catch (Throwable e) {
                Logger.getLogger("es.gob.afirma").severe("Error durante el proceso cifrado de la clave: " + e);
            }
            // creamos el recipiente con los datos del destinatario.
            KeyTransRecipientInfo keyTransRecipientInfo = new KeyTransRecipientInfo(
                                rid,
                                keyEncAlg,
                                new DEROctetString(encryptedKey));

            recipient = new RecipientInfo(keyTransRecipientInfo);
            // Lo a&ntilde;adimos al recipiente de destinatarios.
            recipientInfos.add(recipient);
        }

      // 4. ATRIBUTOS

        ASN1Set unprotectedAttrs = null;
        unprotectedAttrs = generateSignerInfo( digestAlgorithm, parameters.getContent(), dataType);

     // construimos el Enveloped Data y lo devolvemos
     return new ContentInfo(
        	PKCSObjectIdentifiers.envelopedData,
        	new EnvelopedData(
                origInfo,
                new DERSet(recipientInfos),
                encInfo,
                unprotectedAttrs
            )
        ).getDEREncoded();

     }

     /**
     *  M&eacute;todo que genera la parte que contiene la informaci&oacute;n del usuario.
     *  Se generan los atributos que se necesitan para generar la firma.
     *
     * @param digestAlgorithm Identifica el algoritmo utilizado firmado.
     *
     * @param datos             Datos firmados.
     *
     * @param dataType      Identifica el tipo del contenido a firmar.
     *
     * @return      Los datos necesarios para generar la firma referente a los
     *              datos del usuario.
     *
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.security.cert.CertificateException
     */
    private ASN1Set generateSignerInfo(final String digestAlgorithm, 
    		                           final byte[] datos, 
    		                           final Oid dataType) throws NoSuchAlgorithmException {

        //// ATRIBUTOS

        //authenticatedAttributes
        ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();

        //tipo de contenido
        ContexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new DERObjectIdentifier(dataType.toString()))));

        //fecha de firma
        ContexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));

        //MessageDigest
        ContexExpecific.add(
            new Attribute(
            	CMSAttributes.messageDigest,
                new DERSet(
                	new DEROctetString(
                		MessageDigest.getInstance(digestAlgorithm.toString()).digest(datos)
                	)
                )
            )
        );

     return getAttributeSet(new AttributeTable(ContexExpecific));

    }



     /**
     * M&eacute;todo que genera la firma de tipo EnvelopedData.
     *
     * @param file        file Datos binarios a firmar
     * @param digestAlg   Algoritmo de hash
     * @param config      Configuraci&oacute;n del algoritmo para firmar
     * @param certDest    Certificado del destino al cual va dirigido la firma.
     * @param dataType    Identifica el tipo del contenido a firmar.
     *
     * @return            la firma de tipo EnvelopedData.
     * @throws java.io.IOException Si hay problemas en la lectura de datos
     * @throws java.security.cert.CertificateEncodingException Cuando el certificado proporcionado no est&aacute; codificado adecuadamente
     * @throws java.security.NoSuchAlgorithmException Si no se soporta alguno de los algoritmos indicados
     */
    public byte[] genEnvelopedData(InputStream file, String digestAlg, AOAlgorithmConfig config, X509Certificate[] certDest, Oid dataType) throws IOException, CertificateEncodingException, NoSuchAlgorithmException {

        byte[] data = null;

        try {
            data = readData(file);
        } catch (AOException ex) {
            Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de lectura del fichero a cifrar: " + ex);
        }



        //Comprobamos que el archivo a tratar no sea nulo.
		if (certDest==null || certDest.length == 0){
			throw new NullPointerException("No se pueden envolver datos sin certificados destino.");
		}

        // Asignamos la clave de cifrado
        // Generamos la clave necesaria para el cifrado
        try {
            assignKey(config);
        } 
        catch (Throwable ex) {
            Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de asignacion de clave: " + ex);
        }

         //Datos previos &uacute;tiles
        String digestAlgorithm = AOCryptoUtil.getDigestAlgorithmName(digestAlg);

        // 1.  ORIGINATORINFO

        OriginatorInfo origInfo = null;


        // 2.   RECIPIENTINFOS

        //variables utilizadas
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
        //generamos el contenedor de cifrado
        EncryptedContentInfo encInfo = null;

        RecipientInfo recipient = null;

        for (int contCert=0;contCert<certDest.length;contCert++){
            cert = certDest[contCert];
            tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(cert.getTBSCertificate()));
            // Obtenemos el Isuer & serial number
            isse = new IssuerAndSerialNumber(tbs.getIssuer(), tbs.getSerialNumber().getValue());
            // Creamos el recipientInfo
            rid= new RecipientIdentifier(isse);
            // Obtenemos la clave publica
            pubKey = cert.getPublicKey();
            // obtenemos la información de la clave publica
            info = tbs.getSubjectPublicKeyInfo();
            // obtenemos el algoritmo de cifrado.
            keyEncAlg = info.getAlgorithmId();

            try {
                // ciframos la clave
                encryptedKey = cipherKey(pubKey);
                // 3.   ENCRIPTEDCONTENTINFO
                encInfo = getEncryptedContentInfo(data, config);
            } catch (Throwable e) {
                Logger.getLogger("es.gob.afirma").severe("Error durante el proceso cifrado de la clave: " + e);
            }
            // creamos el recipiente con los datos del destinatario.
            KeyTransRecipientInfo keyTransRecipientInfo = new KeyTransRecipientInfo(
                                rid,
                                keyEncAlg,
                                new DEROctetString(encryptedKey));

            recipient = new RecipientInfo(keyTransRecipientInfo);
            // Lo a&ntilde;adimos al recipiente de destinatarios.
            recipientInfos.add(recipient);
        }



      // 4. ATRIBUTOS

        ASN1Set unprotectedAttrs = null;
        unprotectedAttrs = generateSignerInfo( digestAlgorithm, data, dataType);

     // construimos el Enveloped Data y lo devolvemos
     return new ContentInfo(
        	PKCSObjectIdentifiers.envelopedData,
        	new EnvelopedData(
                origInfo,
                new DERSet(recipientInfos),
                encInfo,
                unprotectedAttrs
            )
        ).getDEREncoded();

     }


    /*************************************************************************/
    /**************** Metodos auxiliares de cifrado **************************/
    /*************************************************************************/

    /**
     * M&eacute;todo que obtiene el EncriptedContentInfo a partir del archivo
     * a cifrar. El contenido es el siguiente:
     * <pre><code>
     * EncryptedContentInfo ::= SEQUENCE {
     *     contentType ContentType,
     *     contentEncryptionAlgorithm ContentEncryptionAlgorithmIdentifier,
     *     encryptedContent [0] IMPLICIT EncryptedContent OPTIONAL
     * }
     * </code></pre>
     *
     * @param file Archivo a cifrar.
     * @return Un sistema EncryptedContentInfo.
     *
     * @throws java.security.NoSuchProviderException
     * @throws java.security.NoSuchAlgorithmException
     * @throws javax.crypto.NoSuchPaddingException
     * @throws java.security.InvalidAlgorithmParameterException
     * @throws java.security.InvalidKeyException
     * @throws java.io.IOException
     * @throws org.bouncycastle.cms.CMSException
     */
    private EncryptedContentInfo getEncryptedContentInfo(byte[] file, AOAlgorithmConfig config) throws NoSuchProviderException, NoSuchAlgorithmException, NoSuchPaddingException, InvalidAlgorithmParameterException, InvalidKeyException, IOException {


        AlgorithmParameterSpec params = this.getParams(config);
        Cipher cipher = createCipher(config.toString());
        cipher.init(Cipher.ENCRYPT_MODE, cipherKey, params);
        byte[] ciphered = null;
        try {
            ciphered = cipher.doFinal(file);
        } catch (IllegalBlockSizeException ex) {
            Logger.getLogger(CMSEnvelopedData.class.getName()).log(Level.SEVERE, null, ex);
        } catch (BadPaddingException ex) {
            Logger.getLogger(CMSEnvelopedData.class.getName()).log(Level.SEVERE, null, ex);
        }

        DEREncodable asn1Params;
        if (params != null){
            ASN1InputStream             aIn = new ASN1InputStream(cipher.getParameters().getEncoded("ASN.1"));
            asn1Params = aIn.readObject();
        }
        else{
            asn1Params = new DERNull();
        }
        // obtenemos el OID del algoritmo de cifrado
        AlgorithmIdentifier  encAlgId = new AlgorithmIdentifier(
                new DERObjectIdentifier(config.getAlgorithm().getOid()),
                asn1Params);
        // Obtenemos el identificador
        DERObjectIdentifier contentType = PKCSObjectIdentifiers.encryptedData;
        return new EncryptedContentInfo(
                        contentType,
                        encAlgId,
                        new DEROctetString(ciphered)
                );
    }

    /**
     * M&eacute;todo cifra la clave usada para cifrar el archivo usando para ello
     * la clave p&uacute;blica del certificado del usuario.
     *
     * @param pKey  Clave pública del certificado.
     * @return La clave cifrada en "WRAP_MODE".
     *
     * @throws java.security.NoSuchProviderException
     * @throws java.security.NoSuchAlgorithmException
     * @throws javax.crypto.NoSuchPaddingException
     * @throws java.security.InvalidKeyException
     * @throws java.security.InvalidAlgorithmParameterException
     * @throws javax.crypto.IllegalBlockSizeException
     * @throws javax.crypto.BadPaddingException
     */
    private byte[] cipherKey(PublicKey pKey) throws NoSuchProviderException, NoSuchAlgorithmException, NoSuchPaddingException, InvalidKeyException, InvalidAlgorithmParameterException, IllegalBlockSizeException {

        Cipher cipher = createCipher(pKey.getAlgorithm());
        AlgorithmParameters params = cipher.getParameters();
        cipher.init(Cipher.WRAP_MODE, pKey, params);
        byte [] ciphered = cipher.wrap(cipherKey);

        return ciphered;
    }

    /**
     * Asigna la clave para firmar el contenido del fichero que queremos envolver
     * y qeu m&aacute;s tarde ser&aacute; cifrada con la clave p&uacute;blica del usuario que
     * hace la firma.
     *
     * @param config configuraci&oacute;n necesaria para crear la clave.
     */
    private void assignKey(AOAlgorithmConfig config ) throws NoSuchAlgorithmException {
        SecureRandom rand = new SecureRandom();

        KeyGenerator kg = KeyGenerator.getInstance(config.getAlgorithm().getName());
        kg.init(rand);
        SecretKey encKey = kg.generateKey();
        this.cipherKey = encKey;

    }

   
    /**
     * Crea el cifrador usado para cifrar tanto el fichero como la clave usada para
     * cifrar dicho fichero.
     *
     * @param algName algoritmo utilizado para cifrar.
     * @param provider  Proveedor que se utiliza para cifrar.
     * @throws java.security.NoSuchAlgorithmException
     * @throws javax.crypto.NoSuchPaddingException
     */
     private Cipher createCipher(
        String algName)
        throws NoSuchAlgorithmException, NoSuchPaddingException
    {
        return Cipher.getInstance(algName);
    }

     /**
	 * Genera los par&aacute;metros necesarios para poder operar con una configuracion concreta de cifrado.
	 * Si no es necesario ning&uacute;n par&aacute;metro especial, devolvemos <code>null</code>.
	 * @param algorithmConfig Configuracion de cifrado que debemos parametrizar.
	 * @return Par&aacute;metros para operar.
	 */
	private AlgorithmParameterSpec getParams(AOAlgorithmConfig algorithmConfig) {

		AlgorithmParameterSpec params = null;
		if(algorithmConfig.getAlgorithm().supportsPassword()) {
			params = new PBEParameterSpec(SALT, ITERATION_COUNT);
		} else {
//			System.out.println("Cogemos los parametros de: "+algorithmConfig);
//			System.out.println(algorithmConfig.getAlgorithm());
//			System.out.println(algorithmConfig.getBlockMode());
//			System.out.println(algorithmConfig.getPadding());
//			System.out.println(!algorithmConfig.getBlockMode().equals(AOCipherBlockMode.ECB));
			if(!algorithmConfig.getBlockMode().equals(AOCipherBlockMode.ECB)) {
				params = new IvParameterSpec(
						algorithmConfig.getAlgorithm().equals(AOCipherAlgorithm.AES) ? IV_16 : IV_8
				);
			}
		}
		return params;
	}

    /**
     * M&eacute;todo que lee el fichero de entrada para transformarlo en un
     * array de bytes.
     * @param file  Fichero de entrada.
     * @return      Array de bytes.
     * @throws es.map.es.map.afirma.exceptions.AOException
     */
    private byte[] readData(InputStream file) throws AOException{

        BufferedInputStream bufin = new BufferedInputStream(file);
		byte[] buffer = new byte[1024];
		int len;
		ByteArrayOutputStream baos = new ByteArrayOutputStream();

		try {
			while (bufin.available() != 0) {
				len = bufin.read(buffer);
				baos.write(buffer, 0, len);
			}
		} catch (Exception e) {
			throw new AOException(
					"Error al leer los datos a firmar", e);
		}

		return baos.toByteArray();
    }

}


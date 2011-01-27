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
import java.security.Signature;
import java.security.SignatureException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.security.spec.AlgorithmParameterSpec;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
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

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.ASN1OctetString;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DEREncodable;
import org.bouncycastle.asn1.DERNull;
import org.bouncycastle.asn1.DERObjectIdentifier;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERPrintableString;
import org.bouncycastle.asn1.DERSet;
import org.bouncycastle.asn1.DERUTCTime;
import org.bouncycastle.asn1.cms.Attribute;
import org.bouncycastle.asn1.cms.AttributeTable;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.KeyTransRecipientInfo;
import org.bouncycastle.asn1.cms.RecipientIdentifier;
import org.bouncycastle.asn1.cms.RecipientInfo;
import org.bouncycastle.asn1.cms.SignerIdentifier;
import org.bouncycastle.asn1.cms.SignerInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.SubjectPublicKeyInfo;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;
import org.bouncycastle.asn1.x509.X509CertificateStructure;
import org.bouncycastle.asn1.x509.X509Name;
import org.ietf.jgss.Oid;

import sun.security.x509.AlgorithmId;
import es.gob.afirma.ciphers.AOAlgorithmConfig;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOCryptoUtil;
import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;
import es.gob.afirma.misc.AOConstants.AOCipherBlockMode;

/**
 * Clase que implementa firma digital PKCS#7/CMS SignedAndEnvelopedData basado
 * en las especificaciones de RFC-2315.
 *
 * La Estructura del mensaje es la siguiente:<br>
 *
 * <pre><code>
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
 * </code></pre>
 * 
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para crear un
 * mensaje SignedAndEnvelopedData de BouncyCastle: <a href="http://www.bouncycastle.org/">www.bouncycastle.org</a>
 */
public final class CMSSignedAndEnvelopedData extends SigUtils {

     /**
	 * Clave de cifrado. La almacenamos internamente.
	 */
	private SecretKey cipherKey;
    ASN1Set signedAttr2;

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
    * M&eacute;todo que genera la firma de tipo SignedAndEnvelopedData.
    *
    * @param parameters Par&aacute;metros necesarios para la generaci&oacute;n de este tipo.
    * @param config     Configuraci&oacute;n del algoritmo para firmar
    * @param certDest   Certificado del destino al cual va dirigido la firma.
    * @param dataType          Identifica el tipo del contenido a firmar.
    * @param keyEntry   Eatrada a la clave privada para firma
    * @param atrib      Conjunto de atributos firmados.
    * @param uatrib     Conjunto de atributos no firmados.
    *
    * @return           La firma de tipo SignedAndEnvelopedData.
    * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
    * @throws java.security.cert.CertificateEncodingException Si se produce alguna excepci&oacute;n con los certificados de firma.
    * @throws java.security.NoSuchAlgorithmException Si no se soporta alguno de los algoritmos de firma o huella digital
    */
    public byte[] genSignedAndEnvelopedData(P7ContentSignerParameters parameters,AOAlgorithmConfig config, X509Certificate[] certDest, Oid dataType , PrivateKeyEntry keyEntry, HashMap<Oid, byte[]> atrib, HashMap<Oid, byte[]> uatrib) throws IOException, CertificateEncodingException, NoSuchAlgorithmException {

        //Comprobamos que el archivo a tratar no sea nulo.
		if (certDest==null || certDest.length == 0){
			throw new NullPointerException("No se pueden envolver datos sin certificados destino.");
		}

        // Asignamos la clave de cifrado
        try {
            assignKey(config);
        } catch (Throwable ex) {
             Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de asignado de clave: " + ex);
        }

       // 1. VERSION
        // la version se mete en el constructor del signedAndEnvelopedData y es 1

        // 2. DIGESTALGORITM
        // buscamos que timo de algoritmo es y lo codificamos con su OID

        ASN1EncodableVector digestAlgs = new ASN1EncodableVector();
        AlgorithmIdentifier digAlgId;

        String signatureAlgorithm = parameters.getSignatureAlgorithm();
        String digestAlgorithm = null;
        String keyAlgorithm = null;
        int with = signatureAlgorithm.indexOf("with");
        if (with > 0) {
            digestAlgorithm = AOCryptoUtil.getDigestAlgorithmName(signatureAlgorithm);
            int and = signatureAlgorithm.indexOf("and", with + 4);
            if (and > 0) keyAlgorithm = signatureAlgorithm.substring(with + 4, and);
            else keyAlgorithm = signatureAlgorithm.substring(with + 4);
        }

        AlgorithmId digestAlgorithmId = AlgorithmId.get(digestAlgorithm);
        try {
            digAlgId = makeAlgId(digestAlgorithmId.getOID().toString(), digestAlgorithmId.getEncodedParams());
        }
        catch (Exception e) {
        	e.printStackTrace();
            throw new IOException("Error de codificacion: " + e);
        }

        digestAlgs.add(digAlgId);


        // LISTA DE CERTIFICADOS: obtenemos la lista de certificados
        ASN1Set certificates = null;
        ASN1Set certrevlist = null;
        X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();

        if (signerCertificateChain.length != 0) {
            List<DEREncodable> ce = new ArrayList<DEREncodable>();
            for (int i=0; i<signerCertificateChain.length;i++)
                ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[i].getEncoded())));
            certificates = createBerSetFromList(ce);
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
            } catch (Throwable ex) {
                Logger.getLogger("es.gob.afirma").severe("Error durante el proceso cifrado de la clave: " + ex);
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


        // 4. SIGNERINFO
        // raiz de la secuencia de SignerInfo
        ASN1EncodableVector signerInfos = new ASN1EncodableVector();

        TBSCertificateStructure tbs2 = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getTBSCertificate()));

        IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(tbs2.getIssuer(), tbs2.getSerialNumber().getValue());

        SignerIdentifier identifier = new SignerIdentifier(encSid);

        //AlgorithmIdentifier
        digAlgId = new AlgorithmIdentifier(new DERObjectIdentifier(digestAlgorithmId.getOID().toString()), new DERNull());

        //// ATRIBUTOS
        ASN1Set signedAttr = generateSignerInfo(signerCertificateChain[0],digestAlgorithm, parameters.getContent(), dataType, atrib);

        ASN1Set unSignedAttr = null;
        unSignedAttr = generateUnsignerInfo(uatrib);

        //digEncryptionAlgorithm
        AlgorithmId digestAlgorithmIdEnc = AlgorithmId.get(keyAlgorithm);
        AlgorithmIdentifier encAlgId;
        try {
            encAlgId = makeAlgId(digestAlgorithmIdEnc.getOID().toString(), digestAlgorithmIdEnc.getEncodedParams());
        }
        catch (Exception e) {
            throw new IOException("Error de codificacion: " + e);
        }

        ASN1OctetString sign2= null;
        try {
            sign2 = firma(signatureAlgorithm, keyEntry);
        } catch (AOException ex) {
            Logger.getLogger(GenSignedData.class.getName()).log(Level.SEVERE, null, ex);
        }

        signerInfos.add(
    		new SignerInfo(
    	        	identifier,
    	        	digAlgId,
    	        	signedAttr,
    	        	encAlgId,
    	        	sign2,
    	        	unSignedAttr//null //unsignedAttr
	        )
        );


     // construimos el Signed And Enveloped Data y lo devolvemos
     return new ContentInfo(
        	PKCSObjectIdentifiers.signedAndEnvelopedData,
        	new SignedAndEnvelopedData(
                new DERSet(recipientInfos),
                new DERSet(digestAlgs),
                encInfo,
                certificates,
                certrevlist,
                new DERSet(signerInfos)
            )
        ).getDEREncoded();
}

    /**
     *  M&eacute;todo que genera la parte que contiene la informaci&oacute;n del Usuario.
     *  Se generan los atributos que se necesitan para generar la firma.
     *
     * @param digestAlgorithm   Algoritmo Firmado.
     * @param datos             Datos firmados.
     * @param dataType          Identifica el tipo del contenido a firmar.
     * @param atrib             Conjunto de atributos firmados.
     *
     * @return      Los datos necesarios para generar la firma referente a los
     *              datos del usuario.
     *
     * @throws java.security.NoSuchAlgorithmException
     * @throws java.security.cert.CertificateException
     * @throws java.io.IOException
     */
    private ASN1Set generateSignerInfo(X509Certificate cert,
                            String digestAlgorithm,
                            byte[] datos,
                            Oid dataType,
                            HashMap<Oid, byte[]> atrib)
                        throws NoSuchAlgorithmException {

        
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

        //Serial Number
        // comentar lo de abajo para version del rfc 3852
        ContexExpecific.add(
    		new Attribute(
				X509Name.SERIALNUMBER,
                new DERSet(new DERPrintableString(cert.getSerialNumber().toString()))
			)
        );

        //agregamos la lista de atributos a mayores.
        if (atrib.size()!=0){
        	Iterator<Map.Entry<Oid, byte[]>> it = atrib.entrySet().iterator();
        	while (it.hasNext()) {
        	Map.Entry<Oid, byte[]> e = it.next();
        	ContexExpecific.add(
                    new Attribute(
                        // el oid
                        new DERObjectIdentifier((e.getKey()).toString()),
                        // el array de bytes en formato string
                        new DERSet(new DERPrintableString(e.getValue()))
                    )
                );
        	}
        }

        signedAttr2 = getAttributeSet(new AttributeTable(ContexExpecific));

        return getAttributeSet(new AttributeTable(ContexExpecific));

    }


    /**
     *  M&eacute;todo que genera la parte que contiene la informaci&oacute;n del Usuario.
     *  Se generan los atributos no firmados.
     *
     * @param uatrib    Lista de atributos no firmados que se insertar&aacute;n dentro del archivo de firma.
     *
     * @return      Los atributos no firmados de la firma.
     */
    private ASN1Set generateUnsignerInfo(HashMap<Oid, byte[]> uatrib){

        //// ATRIBUTOS

        //authenticatedAttributes
        ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();


        //agregamos la lista de atributos a mayores.
        if (uatrib.size()!=0){
        	Iterator<Map.Entry<Oid, byte[]>> it = uatrib.entrySet().iterator();
        	while (it.hasNext()) {
        	Map.Entry<Oid, byte[]> e = it.next();
        	ContexExpecific.add(
                    new Attribute(
                        // el oid
                        new DERObjectIdentifier((e.getKey()).toString()),
                        // el array de bytes en formato string
                        new DERSet(new DERPrintableString(e.getValue()))
                    )
                );
        	}
        }
        else{
            return null;
        }

        return getAttributeSet(new AttributeTable(ContexExpecific));

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
     */
    private EncryptedContentInfo getEncryptedContentInfo(byte[] file, AOAlgorithmConfig config)
                                            throws NoSuchProviderException,
                                            NoSuchAlgorithmException,
                                            NoSuchPaddingException,
                                            InvalidAlgorithmParameterException,
                                            InvalidKeyException,
                                            IOException {

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
     * @param pKey  Clave p&uacute;blica del certificado.
     * @return La clave cifrada en "WRAP_MODE".
     *
     * @throws java.security.NoSuchProviderException
     * @throws java.security.NoSuchAlgorithmException
     * @throws javax.crypto.NoSuchPaddingException
     * @throws java.security.InvalidKeyException
     * @throws java.security.InvalidAlgorithmParameterException
     * @throws javax.crypto.IllegalBlockSizeException
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
    private void assignKey(AOAlgorithmConfig config) throws NoSuchAlgorithmException {
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
     * @return Cifrador.
     * @throws java.security.NoSuchAlgorithmException
     * @throws javax.crypto.NoSuchPaddingException
     * @throws NoSuchProviderException 
     */
     private Cipher createCipher(
        String algName)
        throws NoSuchAlgorithmException, NoSuchPaddingException, NoSuchProviderException
    {
        return Cipher.getInstance(algName, "SunJCE");
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
			if(!algorithmConfig.getBlockMode().equals(AOCipherBlockMode.ECB)) {
				params = new IvParameterSpec(
						algorithmConfig.getAlgorithm().equals(AOCipherAlgorithm.AES) ? IV_16 : IV_8
				);
			}
		}

		return params;
	}


    /**
     * Realiza la firma usando los atributos del firmante.
     * @param signatureAlgorithm    Algoritmo para la firma
     * @param keyEntry              Clave para firmar.
     * @return                      Firma de los atributos.
     * @throws es.map.es.map.afirma.exceptions.AOException
     */
    private ASN1OctetString firma (String signatureAlgorithm, PrivateKeyEntry keyEntry) throws AOException{

        Signature sig = null;
		try {
			sig = Signature.getInstance(signatureAlgorithm);
		} catch (Exception e) {
            e.printStackTrace();
		}

        byte[] tmp= null;

        try {
            tmp = signedAttr2.getEncoded(ASN1Encodable.DER);
        } catch (IOException ex) {
            Logger.getLogger(GenSignedData.class.getName()).log(Level.SEVERE, null, ex);
        }

        //Indicar clave privada para la firma
		try {
			sig.initSign(keyEntry.getPrivateKey());
		} catch (final Throwable e) {
			throw new AOException(
					"Error al obtener la clave de firma para el algoritmo '" + signatureAlgorithm + "': " + e);
		}



        // Actualizamos la configuracion de firma
		try {
			sig.update(tmp);
		} catch (SignatureException e) {
			throw new AOException(
					"Error al configurar la informacion de firma: " + e);
		}


        //firmamos.
        byte[] realSig=null;
        try {
			realSig = sig.sign();
		} catch (Exception e) {
			throw new AOException("Error durante el proceso de firma: " + e);
		}

        ASN1OctetString encDigest = new DEROctetString(realSig);

        return encDigest;


    }
    
    /**
     * M&eacute;todo que inserta remitentes en el "OriginatorInfo" de un sobre de tipo AuthenticatedEnvelopedData.
     *
     * @param data 	fichero que tiene la firma.
     * @param signerCertificateChain Cadena de certificados a agregar.
     * @return  La nueva firma AuthenticatedEnvelopedData con los remitentes que ten&iacute;a (si los tuviera) 
     * 		 con la cadena de certificados nueva.
     */
    @SuppressWarnings("unchecked")
	public byte[] addOriginatorInfo(InputStream data, P7ContentSignerParameters parameters, PrivateKeyEntry keyEntry, Oid dataType, HashMap<Oid, byte[]> atrib, HashMap<Oid, byte[]> uatrib){
        //boolean isValid = false;
    	byte[] retorno = null;
        try {
            ASN1InputStream is = new ASN1InputStream(data);
            // LEEMOS EL FICHERO QUE NOS INTRODUCEN
            ASN1Sequence dsq = null;
            dsq=(ASN1Sequence)is.readObject();
            Enumeration<Object> e = dsq.getObjects();
            // Elementos que contienen los elementos OID Data
            DERObjectIdentifier doi = (DERObjectIdentifier)e.nextElement();
            if (doi.equals(PKCSObjectIdentifiers.signedAndEnvelopedData)){
                // Contenido de Data
	             ASN1TaggedObject doj =(ASN1TaggedObject) e.nextElement();
	
	             SignedAndEnvelopedData signEnv =new SignedAndEnvelopedData((ASN1Sequence)doj.getObject());
	             	             
	             //Obtenemos los originatorInfo
	             ASN1EncodableVector signerInfos = new ASN1EncodableVector();
	             Enumeration<?>  signers = signEnv.getSignerInfos().getObjects();
	             while(signers.hasMoreElements()){
	            	 signerInfos.add((ASN1Sequence)signers.nextElement());
	             }
	             
	             //certificado del remitente
                 X509Certificate[] signerCertificateChain = parameters.getSignerCertificateChain();
	             
                 ASN1EncodableVector signCerts = new ASN1EncodableVector();
                 
	             //Si no hay certificados, se deja como esta.
	             if (signerCertificateChain.length != 0) {
	            	 
	            	 //algoritmo
	            	 String signatureAlgorithm = parameters.getSignatureAlgorithm();
	                 String digestAlgorithm = null;
	                 String keyAlgorithm = null;
	                 AlgorithmIdentifier digAlgId;
	                 int with = signatureAlgorithm.indexOf("with");
	                 if (with > 0) {
	                     digestAlgorithm = AOCryptoUtil.getDigestAlgorithmName(signatureAlgorithm);
	                     int and = signatureAlgorithm.indexOf("and", with + 4);
	                     if (and > 0) keyAlgorithm = signatureAlgorithm.substring(with + 4, and);
	                     else keyAlgorithm = signatureAlgorithm.substring(with + 4);
	                 }

	                 AlgorithmId digestAlgorithmId = AlgorithmId.get(digestAlgorithm);
	                 try {
	                     digAlgId = makeAlgId(digestAlgorithmId.getOID().toString(), digestAlgorithmId.getEncodedParams());
	                 }
	                 catch (Exception e2) {
	                 	e2.printStackTrace();
	                     throw new IOException("Error de codificacion: " + e2);
	                 }
	                 
	                 TBSCertificateStructure tbs2 = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[0].getTBSCertificate()));

	                 IssuerAndSerialNumber encSid = new IssuerAndSerialNumber(tbs2.getIssuer(), tbs2.getSerialNumber().getValue());

	                 SignerIdentifier identifier = new SignerIdentifier(encSid);

	                 //AlgorithmIdentifier
	                 digAlgId = new AlgorithmIdentifier(new DERObjectIdentifier(digestAlgorithmId.getOID().toString()), new DERNull());

	                 //// ATRIBUTOS
	                 ASN1Set signedAttr = generateSignerInfo(signerCertificateChain[0],digestAlgorithm, parameters.getContent(), dataType, atrib);

	                 ASN1Set unSignedAttr = null;
	                 unSignedAttr = generateUnsignerInfo(uatrib);

	                 //digEncryptionAlgorithm
	                 AlgorithmId digestAlgorithmIdEnc = AlgorithmId.get(keyAlgorithm);
	                 AlgorithmIdentifier encAlgId;
	                 try {
	                     encAlgId = makeAlgId(digestAlgorithmIdEnc.getOID().toString(), digestAlgorithmIdEnc.getEncodedParams());
	                 }
	                 catch (Exception e3) {
	                     throw new IOException("Error de codificacion: " + e3);
	                 }

	                 ASN1OctetString sign2= null;
	                 try {
	                     sign2 = firma(signatureAlgorithm, keyEntry);
	                 } catch (AOException ex) {
	                     Logger.getLogger(GenSignedData.class.getName()).log(Level.SEVERE, null, ex);
	                 }

	                 //EN ESTE PUNTO YA TENEMOS EL NUEVO SIGNER
	                 SignerInfo nuevoSigner = new SignerInfo(identifier,
								             	         	 digAlgId,
								             	        	 signedAttr,
								             	        	 encAlgId,
								             	        	 sign2,
								             	         	 unSignedAttr//null //unsignedAttr
								         	         );
	                 
		             //introducimos el nuevo Signer
	                 signerInfos.add(nuevoSigner);
	                 
	                 // LISTA DE CERTIFICADOS: obtenemos la lista de certificados
	                 Enumeration<?>  cers = signEnv.getCertificates().getObjects();
	                 while(cers.hasMoreElements()){
	                	 signCerts.add((ASN1Sequence)cers.nextElement());
	                 }
	                 
	                 if (signerCertificateChain.length != 0) {
	                     for (int i=0; i<signerCertificateChain.length;i++)
	                    	 signCerts.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[i].getEncoded())));
	                 }
	            	 
		             
	             }
	             else{
	            	 Logger.getLogger("es.gob.afirma").warning("No se ha podido obtener el certificado del nuevo firmante ");
	             }

	            
	             ASN1Set certrevlist = null;
	             
	            // Se crea un nuevo AuthenticatedEnvelopedData a partir de los datos anteriores con los nuevos originantes.
	            retorno = new ContentInfo(
	                    	PKCSObjectIdentifiers.signedAndEnvelopedData,
	                    	new SignedAndEnvelopedData(
	                    		signEnv.getRecipientInfos(),//new DERSet(recipientInfos),
	                    		signEnv.getDigestAlgorithms(),//new DERSet(digestAlgs),
	                    		signEnv.getEncryptedContentInfo(),//encInfo,
	                    		new DERSet(signCerts),//certificates,
	                    		certrevlist,//certrevlist,
	                            new DERSet(signerInfos)
	                        )
	                    ).getDEREncoded();
            }
           
        } catch (Exception ex) {
            Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de insercion: " + ex);
            
        }
        return retorno;
    }

}

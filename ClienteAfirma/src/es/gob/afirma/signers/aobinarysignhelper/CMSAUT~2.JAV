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
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.PublicKey;
import java.security.SecureRandom;
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
import javax.crypto.Mac;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEParameterSpec;

import org.bouncycastle.asn1.ASN1EncodableVector;
import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Object;
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
import org.bouncycastle.asn1.cms.AuthEnvelopedData;
import org.bouncycastle.asn1.cms.CMSAttributes;
import org.bouncycastle.asn1.cms.ContentInfo;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.KeyTransRecipientInfo;
import org.bouncycastle.asn1.cms.OriginatorInfo;
import org.bouncycastle.asn1.cms.RecipientIdentifier;
import org.bouncycastle.asn1.cms.RecipientInfo;
import org.bouncycastle.asn1.pkcs.PKCSObjectIdentifiers;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;
import org.bouncycastle.asn1.x509.X509CertificateStructure;
import org.ietf.jgss.Oid;

import es.gob.afirma.ciphers.AOAlgorithmConfig;
import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;
import es.gob.afirma.misc.AOConstants.AOCipherBlockMode;


/**
 *  Clase que implementa firma digital PKCS#7/CMS AuthenticatedEnvelopedData (RFC 5083)
 *  La Estructura del mensaje es la siguiente:<br>
 *  <pre><code>
 *
 *  id-ct-authEnvelopedData OBJECT IDENTIFIER ::= { iso(1)
 *         member-body(2) us(840) rsadsi(113549) pkcs(1) pkcs-9(9)
 *         smime(16) ct(1) 23 }
 *
 *  The authenticated-data content type shall have ASN.1 type
 *  AuthenticatedEnvelopedData:
 *
 *      AuthEnvelopedData ::= SEQUENCE {
 *       version CMSVersion,
 *       originatorInfo [0] IMPLICIT OriginatorInfo OPTIONAL,
 *       recipientInfos RecipientInfos,
 *       authEncryptedContentInfo EncryptedContentInfo,
 *       authAttrs [1] IMPLICIT AuthAttributes OPTIONAL,
 *       mac MessageAuthenticationCode,
 *       unauthAttrs [2] IMPLICIT UnauthAttributes OPTIONAL }
 *
 *     AuthAttributes ::= SET SIZE (1..MAX) OF Attribute
 *
 *     UnauthAttributes ::= SET SIZE (1..MAX) OF Attribute
 *
 *     MessageAuthenticationCode ::= OCTET STRING
 *
 * </code></pre>
 * La implementaci&oacute;n del c&oacute;digo ha seguido los pasos necesarios para crear un
 * mensaje AuthenticatedEnvelopedData de BouncyCastle: <a href="http://www.bouncycastle.org/">www.bouncycastle.org</a>
 */

public class CMSAuthenticatedEnvelopedData extends SigUtils{

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
	 * 
	 * @param parameters		Par&aacute;metros necesarios que contienen tanto la firma
     *                      	del archivo a firmar como los datos del firmante.
	 * @param config			Configuraci&oacute;n del algoritmo para firmar
	 * @param certDest			Certificado del destino al cual va dirigido la firma.
	 * @param dataType			Identifica el tipo del contenido a firmar.
	 * @param applyTimestamp	Si se aplica el Timestamp o no.
	 * @param atrib				Atributos firmados opcionales. 
	 * @param uatrib			Atributos no autenticados firmados opcionales.
	 * @return					Firma de tipo AuthenticatedData.
	 * @throws IOException	Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
	 * @throws CertificateEncodingException Si se produce alguna excepci&oacute;n con los certificados de firma.
	 * @throws NoSuchAlgorithmException Si no se encuentra un algoritmo v&aacute;lido.
	 * @throws Exception	Cuando ocurre alguno error con contemplado por las otras excepciones declaradas
	 */
	public byte[] genAuthenticatedEnvelopedData(
						P7ContentSignerParameters parameters, 
						AOAlgorithmConfig config,
						X509Certificate[] certDest, 
						Oid dataType, 
			            boolean applyTimestamp,
                        HashMap<Oid, byte[]> atrib,
						HashMap<Oid,byte[]> uatrib
						) throws IOException, CertificateEncodingException, NoSuchAlgorithmException, Exception {
		
		
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
            
            //introducimos una lista vacía en los CRL ya que no podemos modificar el codigo de bc.
            List<DEREncodable> crl = new ArrayList<DEREncodable>();
            certrevlist = createBerSetFromList(crl);
            origInfo = new OriginatorInfo(certificates, certrevlist);
        }
        
// 2.   RECIPIENTINFOS

        
     // Asignamos la clave de cifrado
        // Generamos la clave necesaria para el cifrado
        try {
            assignKey(config);
        } catch (Throwable ex) {
            Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de asignacion de clave: " + ex);
        }
        
        //variables utilizadas
        ASN1EncodableVector recipientInfos = new ASN1EncodableVector();
        X509Certificate cert;
        TBSCertificateStructure tbs;
        IssuerAndSerialNumber isse;
        RecipientIdentifier rid;
        PublicKey pubKey;
         // Cifrado de la clave
        byte[] encryptedKey = null;
        RecipientInfo recipient = null;
        
        //generamos el contenedor de cifrado
        EncryptedContentInfo encInfo = null;

        for (int contCert=0;contCert<certDest.length;contCert++){
            cert = certDest[contCert];
            tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(cert.getTBSCertificate()));
            // Obtenemos el Isuer & serial number
            isse = new IssuerAndSerialNumber(tbs.getIssuer(), tbs.getSerialNumber().getValue());
            // Creamos el recipientInfo
            rid= new RecipientIdentifier(isse);
            // Obtenemos la clave publica
            pubKey = cert.getPublicKey();
            // obtenemos la informaciÃƒÂ³n de la clave publica. info = tbs.getSubjectPublicKeyInfo();
            // obtenemos el algoritmo de cifrado. keyEncAlg = info.getAlgorithmId();
            AlgorithmIdentifier keyalg = makeAlgId(config.getAlgorithm().getOid(),null);
            
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
                                keyalg,
                                new DEROctetString(encryptedKey));

            recipient = new RecipientInfo(keyTransRecipientInfo);
            // Lo a&ntilde;adimos al recipiente de destinatarios.
            recipientInfos.add(recipient);
        }
  
    
// 4. ATRIBUTOS FIRMADOS
        ASN1Set authAttr = null;        
        authAttr = generateSignedAtt(dataType, 
        					applyTimestamp, 
        					atrib);

// 5. MAC
        byte[] atributes = authAttr.getDEREncoded();
        byte[] content = parameters.getContent();
        
        byte [] pack = genPack(atributes,content);
        
        byte[] mac = null;
	    try{
	    	mac = genMac(config.getAlgorithm().getName(), pack, cipherKey);
	    }
        catch(Throwable e) {
            throw new Exception("Error de codificacion: " + e);
        }
        
// 6. ATRIBUTOS NO FIRMADOS.

        ASN1Set unAuthAttr = null;
        unAuthAttr = generateUnsignedAtt(uatrib);

		
// construimos el Authenticated data y lo devolvemos
	     return new ContentInfo(
	        	PKCSObjectIdentifiers.id_ct_authEnvelopedData,
	        	new AuthEnvelopedData(
	                origInfo,					// originatorInfo,
	                new DERSet(recipientInfos), // recipientInfos,
	                encInfo, 				    // authEncryptedContentInfo,
	                authAttr, 					// authAttrs
	                new DEROctetString(mac),	// mac
	                unAuthAttr 					// unauthAttrs
	            )
	        ).getDEREncoded();
	     
	}
	
	private byte[] genMac(String encryptionAlg, byte[] content, SecretKey cipherKey) throws Exception{
    	
		//Security.addProvider(new BouncyCastleProvider());
		
    	Mac mac = Mac.getInstance( "HmacSHA512" );
    	mac.init( cipherKey );
    	byte [] r  = mac.doFinal( content );
    	return r ;

    }
	
	private byte[] genPack(byte[] parte1, byte[] parte2){
		byte [] pack = new byte[parte1.length + parte2.length];
		
		for (int i = 0; i<parte1.length; i++){
			pack[i] = parte1[i];
		}
		
		for (int i = 0; i<parte2.length; i++){
			pack[i+parte1.length] = parte2[i];
		}
		
		return pack;		
	}
 	
	
	/**
	 *  M&eacute;todo que genera la parte que contiene la informaci&oacute;n del Usuario.
	 *  Se generan los atributos que se necesitan para generar la firma.
	 *
	 * @param cert              Certificado necesario para la firma.
	 * @param digestAlgorithm   Algoritmo Firmado.
	 * @param datos             Datos firmados.
	 * @param datatype          Identifica el tipo del contenido a firmar.
	 * @param timestamp			Introducir TimeStaming
	 * @param atrib             Lista de atributos firmados que se insertar&aacute;n dentro del archivo de firma.
	 *
	 * @return      Los atributos firmados de la firma.
	 *
	 * @throws java.security.NoSuchAlgorithmException Si no se encuentra un algoritmo v&aacute;lido.
	 */
	private ASN1Set generateSignedAtt(Oid datatype,
	                        boolean timestamp,
	                        HashMap<Oid, byte[]> atrib)
	                    throws NoSuchAlgorithmException {
	
	    //// ATRIBUTOS
	
	    //authenticatedAttributes
	    ASN1EncodableVector ContexExpecific = new ASN1EncodableVector();
	
	    //tipo de contenido
	    ContexExpecific.add(new Attribute(CMSAttributes.contentType, new DERSet(new DERObjectIdentifier(datatype.toString()))));
	
	    //fecha de firma
	    if (timestamp){
	    ContexExpecific.add(new Attribute(CMSAttributes.signingTime, new DERSet(new DERUTCTime(new Date()))));
	    }
	    
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
	private ASN1Set generateUnsignedAtt(HashMap<Oid, byte[]> uatrib){
	
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
            Logger.getLogger(CMSAuthenticatedEnvelopedData.class.getName()).log(Level.SEVERE, null, ex);
        } catch (BadPaddingException ex) {
            Logger.getLogger(CMSAuthenticatedEnvelopedData.class.getName()).log(Level.SEVERE, null, ex);
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
	    cipher.init(Cipher.WRAP_MODE, pKey); 
	    	return cipher.wrap(cipherKey);
	}
	
	/**
     * Crea el cifrador usado para cifrar tanto el fichero como la clave usada para
     * cifrar dicho fichero.
     *
     * @param algName algoritmo utilizado para cifrar.
     * @return Cifrador.
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
	 * Genera un estructura de tipo SET de formato ASN1.
	 * @param derObjects Una lista con los objetos a obtener el tipo SET
	 * @return  Un SET de ASN1 con los elementos de la lista introducida.
	 */
	@Override
	protected ASN1Set createBerSetFromList(List<DEREncodable> derObjects) {
	    ASN1EncodableVector v = new ASN1EncodableVector();
	    for (DEREncodable d : derObjects) v.add(d);
	    
	    return new BERSet(v);
	}
	
	/**
     * Asigna la clave para firmar el contenido del fichero que queremos envolver
     * y qeu m&aacute;s tarde ser&aacute; cifrada con la clave p&uacute;blica del usuario que
     * hace la firma.
     *
     * @param config configuraci&oacute;n necesaria para crear la clave.
     */
    private void assignKey(AOAlgorithmConfig config ) throws NoSuchAlgorithmException {
        KeyGenerator kg = KeyGenerator.getInstance(config.getAlgorithm().getName());
        kg.init(new SecureRandom());
        this.cipherKey = kg.generateKey();
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
     * M&eacute;todo que inserta remitentes en el "OriginatorInfo" de un sobre de tipo AuthenticatedEnvelopedData.
     *
     * @param data 	fichero que tiene la firma.
     * @param signerCertificateChain Cadena de certificados a agregar.
     * @return  La nueva firma AuthenticatedEnvelopedData con los remitentes que ten&iacute;a (si los tuviera) 
     * 		 con la cadena de certificados nueva.
     */
    @SuppressWarnings("unchecked")
	public byte[] addOriginatorInfo(InputStream data, X509Certificate[] signerCertificateChain){
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
            if (doi.equals(PKCSObjectIdentifiers.id_ct_authEnvelopedData)){
                // Contenido de Data
	             ASN1TaggedObject doj =(ASN1TaggedObject) e.nextElement();
	
	             AuthEnvelopedData authEnv =new AuthEnvelopedData((ASN1Sequence)doj.getObject());
	             
	             //Obtenemos los originatorInfo
	             OriginatorInfo origInfo = authEnv.getOriginatorInfo();
	             ASN1Set certs = null;
	             if(origInfo!=null){
	            	 certs = origInfo.getCertificates();
	             }
	             
	             //Si no hay certificados, se deja como esta.
	             if (signerCertificateChain.length != 0) {
		             //no tiene remitentes
		             if (certs==null){
	            		 ASN1Set certificates = null;
	            		 ASN1Set certrevlist = null;
	                     List<DEREncodable> ce = new ArrayList<DEREncodable>();
	                     for (int i=0; i<signerCertificateChain.length;i++)
	                    	 if(signerCertificateChain[i]!=null)
	                    		 ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[i].getEncoded())));
	                     //se introducen la nueva cadena de certificados.
	                     if(ce.size()!=0){
	                    	 certificates = createBerSetFromList(ce);
	                    	 origInfo = new OriginatorInfo(certificates, certrevlist);
	                     }
	                 }		    
		             //tiene remitentes
		             else{	
		            	 // Se obtienen los certificados que tenia la firma.
		            	 ASN1EncodableVector v = new ASN1EncodableVector();
		            	 if (certs.getObjectAt(0) instanceof DERSequence) {
		            		 ASN1EncodableVector subv = new ASN1EncodableVector();
		            		 for(int i=0;i<certs.size();i++){
		            			 subv.add(certs.getObjectAt(i));
			            	 }		            		 
		            		 v.add(new BERSet(subv));
						 }
		            	 else{
		            		 for(int i=0;i<certs.size();i++){
			            		 v.add(certs.getObjectAt(i));
			            	 }
		            	 }
		            	 
		            	 ASN1Set certificates = null;
	            		 ASN1Set certrevlist = new BERSet(new ASN1EncodableVector());
	                     List<DEREncodable> ce = new ArrayList<DEREncodable>();
	                     for (int i=0; i<signerCertificateChain.length;i++)
	                    	 if(signerCertificateChain[i]!=null)
	                    		 ce.add(X509CertificateStructure.getInstance(ASN1Object.fromByteArray(signerCertificateChain[i].getEncoded())));
	                     //se introducen la nueva cadena de certificados.
	                     if(ce.size()!=0){
	                    	 certificates = createBerSetFromList(ce);
	                    	 v.add(certificates);
	                    	 origInfo = new OriginatorInfo(new BERSet(v), certrevlist);
	                     }	                     		            	 
		             }
	             }
	             	             
	            // Se crea un nuevo AuthenticatedEnvelopedData a partir de los datos anteriores con los nuevos originantes.
	            retorno = new ContentInfo(
	            		PKCSObjectIdentifiers.id_ct_authEnvelopedData,
	            			new AuthEnvelopedData(
	            	                origInfo,					// OriginatorInfo
	            	                authEnv.getRecipientInfos(), // ASN1Set
	            	                authEnv.getAuthEncryptedContentInfo(),
	            	                authEnv.getAuthAttrs(),
	            	                authEnv.getMac(),
	            	                authEnv.getUnauthAttrs()
	            	            )
	                    ).getDEREncoded();
            }
           
        } catch (Exception ex) {
            Logger.getLogger("es.gob.afirma").severe("Error durante el proceso de insercion: " + ex);
            
        }
        return retorno;
    }
   	
}

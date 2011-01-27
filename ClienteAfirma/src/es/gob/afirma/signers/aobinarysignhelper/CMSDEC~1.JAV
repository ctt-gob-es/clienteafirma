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
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Enumeration;
import java.util.logging.Logger;

import javax.crypto.Cipher;
import javax.crypto.Mac;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1Set;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.cms.AuthenticatedData;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.KeyTransRecipientInfo;
import org.bouncycastle.asn1.cms.RecipientInfo;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOInvalidRecipientException;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;

/**
 * Clase que extrae el contenido de un fichero en formato AuthenticatedData.
 * de CMS.
 * 
 */
public class CMSDecipherAuthenticatedData {

	 /**
	 * Clave de cifrado. La almacenamos internamente porque no hay forma de mostrarla
	 * directamente al usuario.
	 */
	private SecretKey cipherKey;
	private AOCipherAlgorithm claveMac;
	
	/**
	 * 
	 * @param file			Flujo de lectura de los datos que contienen el tipo EnvelopedData
     *                      para obtener los datos cifrados.
     * @param userCert      Certificado del Usuario que quiere descifrar el contenido del EnvelopedData.
     * @param keyEntry      Clave privada del certificado usado para descifrar el contenido.
	 * @return				El contenido de una firma de tipo authenticatedData.
	 * @throws IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
	 * @throws CertificateEncodingException Si se produce alguna excepci&oacute;n con los certificados de firma.
	 * @throws AOException Cuando ocurre un error durante el proceso de descifrado (formato o clave incorrecto,...)
	 * @throws AOInvalidRecipientException Cuando se indica un certificado que no est&aacute; entre los destinatarios del sobre.
	 * @throws InvalidKeyException Cuando la clave almacenada en el sobre no es v&aacute;lida.
	 */
	public byte[] decipherAuthenticatedData(InputStream file, X509Certificate userCert, PrivateKeyEntry keyEntry) throws IOException, CertificateEncodingException, AOException, AOInvalidRecipientException, InvalidKeyException {
		byte[] contenido = new byte[0];

		AuthenticatedData authenticated = null;
		// Leemos el fichero que contiene la firma.
		byte[] codeFile = AOUtil.getDataFromInputStream(file);
		ASN1InputStream is = new ASN1InputStream(codeFile);

		try{
			// Comenzamos a obtener los datos.
			ASN1Sequence dsq = null;
			dsq = (ASN1Sequence) is.readObject();
			Enumeration<Object> e = dsq.getObjects();
			// Elementos que contienen los elementos OID EnvelopedData.
			e.nextElement();
			// Contenido de EnvelopedData
			ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
			ASN1Sequence authenticatedData = (ASN1Sequence) doj.getObject();

			authenticated = AuthenticatedData.getInstance(authenticatedData);
		}catch(Exception ex){
			Logger.getLogger("es.gob.afirma").severe("El fichero no contiene un tipo EnvelopedData:  " + ex);
			throw new AOException("El fichero no contiene un tipo EnvelopedData", ex);
		}

		//obtenemos los datos del certificado destino.
		IssuerAndSerialNumber isse;
		TBSCertificateStructure tbs;

		//los datos del descifrado.
		byte[] encryptedKey = null;
		AlgorithmIdentifier algEncryptedKey = null;

		tbs = TBSCertificateStructure.getInstance(ASN1Object.fromByteArray(userCert.getTBSCertificate()));
		// Obtenemos el Isuer & serial number
		isse = new IssuerAndSerialNumber(tbs.getIssuer(), tbs.getSerialNumber().getValue());

		//obtenesmos los recipientInfo.
		Enumeration<Object> elementRecipient = authenticated.getRecipientInfos().getObjects();
		RecipientInfo reci = null;
		while (elementRecipient.hasMoreElements()){
			// obtengo los recipientInfo
			ASN1Sequence intermedio = (ASN1Sequence) elementRecipient.nextElement();
			reci = RecipientInfo.getInstance(intermedio);
			KeyTransRecipientInfo kri = KeyTransRecipientInfo.getInstance(reci.getDERObject());
			IssuerAndSerialNumber actual = IssuerAndSerialNumber.getInstance(kri.getRecipientIdentifier().getDERObject());
			// Comparo el issuer y el serial number con el certificado que me pasan para descifrar.
			if (actual.equals(isse)){
				//Obtengo los datos para descifrar.
				encryptedKey = kri.getEncryptedKey().getOctets();
				algEncryptedKey = kri.getKeyEncryptionAlgorithm();
			}
		}


		// si no se encuentran coincidencias es tonteria continuar.
		if ((encryptedKey== null) || (algEncryptedKey == null)){
			throw new AOInvalidRecipientException(
					"El usuario indicado no es uno de los destinatarios del sobre digital.");
		}

		// Asignamos la clave de descifrado del contenido.
		assignKey(encryptedKey, keyEntry, algEncryptedKey);



		String macAlg = authenticated.getMacAlgorithm().getObjectId().toString();
		ASN1Set authAttr = authenticated.getAuthAttrs();


		byte[] macGenerada = null;
		try{
			macGenerada = genMac(macAlg, authAttr.getDEREncoded(),this.cipherKey);
		}
		catch(InvalidKeyException e) {
			throw e;
		}
		catch(Throwable e) {
			throw new AOException("Error de codificacion: " + e);
		}

		byte[] macObtenida = authenticated.getMac().getOctets();

		if (java.util.Arrays.equals(macGenerada, macObtenida)){
			contenido = ((DEROctetString) authenticated.getEncapsulatedContentInfo().getContent()).getOctets();
		}

		return contenido;		
	}
	
	private byte[] genMac(String encryptionAlg, byte[] content, SecretKey cipherKey) throws Exception {
    	//KeyGenerator kg = KeyGenerator.getInstance("HmacSHA512");
    	//SecretKey sk = kg.generateKey();
    		        
    	Mac mac = Mac.getInstance( claveMac.getName() );
    	mac.init( cipherKey );
    	byte [] r  = mac.doFinal( content );
    	return r ;
    }
	
	/**
     * Asigna la clave para firmar el contenido del fichero que queremos envolver
     * y que m&aacute;s tarde ser&aacute; cifrada con la clave p&uacute;blica del usuario que
     * hace la firma.
     *
     * @param passCiphered Clave cifrada.
     * @param keyEntry     Contrase&ntilde;a que se va a usar para descifrar.
     * @param algClave     Algoritmo necesario para crear la clave.
     * @throws AOException Cuando no se pudo descifrar la clave con el certificado de usuario.
     */
    private void assignKey(byte[] passCiphered, PrivateKeyEntry keyEntry, AlgorithmIdentifier algClave) throws AOException {

        AOCipherAlgorithm algorithm = null;

        // obtenemos el algoritmo usado para cifrar la pass
        for (AOCipherAlgorithm algo : AOCipherAlgorithm.values()){
            if (algo.getOid().equals(algClave.getObjectId().toString())){
                algorithm = algo;
                break;
            }
        }
        
        claveMac = algorithm;

        // Desembolvemos la clave usada para cifrar el contenido
        // a partir de la clave privada del certificado del usuario.
        try {
			byte[] encrypted = passCiphered;
			//final Cipher cipher2 = Cipher.getInstance("RSA/ECB/PKCS1Padding");
			Cipher cipher = createCipher(keyEntry.getPrivateKey().getAlgorithm());
			cipher.init(Cipher.UNWRAP_MODE, keyEntry.getPrivateKey());
			this.cipherKey = (SecretKey) cipher.unwrap(encrypted, algorithm.getName(), Cipher.SECRET_KEY);

//			System.out.println("Metodo alternativo");
//			cipher2.init(Cipher.DECRYPT_MODE, keyEntry.getPrivateKey());
//			this.cipherKey = new SecretKeySpec(cipher2.doFinal(encrypted), "AES");
			
		} catch (Throwable e) {
            Logger.getLogger("es.gob.afirma").severe("Ocurri\u00F3 un error al recuperar la clave de cifrado del sobre autenticado: " + e);
            throw new AOException("Ocurrio un error al recuperar la clave de cifrado del sobre autenticado: " + e);
		}
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
}

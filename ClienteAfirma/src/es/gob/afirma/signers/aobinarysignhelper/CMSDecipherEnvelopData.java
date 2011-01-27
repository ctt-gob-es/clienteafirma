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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.NoSuchProviderException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.security.spec.AlgorithmParameterSpec;
import java.util.Enumeration;
import java.util.logging.Logger;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEParameterSpec;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Object;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.EnvelopedData;
import org.bouncycastle.asn1.cms.IssuerAndSerialNumber;
import org.bouncycastle.asn1.cms.KeyTransRecipientInfo;
import org.bouncycastle.asn1.cms.RecipientInfo;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;
import org.bouncycastle.asn1.x509.TBSCertificateStructure;

import es.gob.afirma.ciphers.AOAlgorithmConfig;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOInvalidRecipientException;
import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;
import es.gob.afirma.misc.AOConstants.AOCipherBlockMode;
import es.gob.afirma.misc.AOConstants.AOCipherPadding;

/**
 * Clase que descifra el contenido de un fichero en formato EnvelopedData.
 * de CMS.
 *
 * Se usa para ello una clave del usuario.
 */
public class CMSDecipherEnvelopData {


  /**
	 * Clave de cifrado. La almacenamos internamente porque no hay forma de mostrarla
	 * directamente al usuario.
	 */
	private SecretKey cipherKey;

    private AOAlgorithmConfig config;

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
     * &Eacute;ste m&eacute;todo descifra el contenido de un CMS EnvelopedData.
     *
     * @param file           Flujo de lectura de los datos que contienen el tipo EnvelopedData
     *                       para obtener los datos cifrados.
     * @param userCert       Certificado del Usuario que quiere descifrar el contenido del EnvelopedData.
     * @param keyEntry       Clave privada del certificado usado para descifrar el contenido.
     * @return               El contenido descifrado del EnvelopedData.
     *
     * @throws java.io.IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
     * @throws java.security.cert.CertificateEncodingException Si se produce alguna excepci&oacute;n con los certificados de firma.
     * @throws AOException Cuando ocurre un error durante el proceso de descifrado (formato o clave incorrecto,...)
     * @throws AOInvalidRecipientException Cuando se indica un certificado que no est&aacute; entre los destinatarios del sobre.
     * @throws InvalidKeyException Cuando la clave almacenada en el sobre no es v&aacute;lida.
     */
    @SuppressWarnings("unchecked")
	public byte[] dechiperEnvelopData(InputStream file, X509Certificate userCert, PrivateKeyEntry keyEntry) throws IOException, CertificateEncodingException, AOException, AOInvalidRecipientException, InvalidKeyException {

        //Contendra el contenido a tratar.
        EnvelopedData enveloped = null;
        RecipientInfo reci = null;

        // Leemos el fichero que contiene la firma.
        byte[] codeFile = createArrayFromFile(file);
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
            ASN1Sequence contentEnvelopedData = (ASN1Sequence) doj.getObject();

            enveloped = EnvelopedData.getInstance(contentEnvelopedData);
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
        Enumeration<Object> elementRecipient = enveloped.getRecipientInfos().getObjects();
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

         //Obtenemos el contenido cifrado
        EncryptedContentInfo contenidoCifrado = enveloped.getEncryptedContentInfo();

        // Obtenemos el algoritmo usado para cifrar la clave generada.
        AlgorithmIdentifier algClave = contenidoCifrado.getContentEncryptionAlgorithm();

        // Asignamos la clave de descifrado del contenido.
        assignKey(encryptedKey, keyEntry, algClave);

        //Desciframos el contenido.
        final byte[] deciphered;
        byte[] contCifrado = contenidoCifrado.getEncryptedContent().getOctets();
        try {
            deciphered = deCipherContent(contCifrado);
        } 
        catch (InvalidKeyException ex) {
        	throw ex;
        } 
        catch (Throwable ex) {
            throw new AOException("Error al descifrar los contenidos del sobre digital", ex);
        }
        return deciphered;
     }


    /**
     * Descifra el contenido a partir de un fichero usando la clave del usuario.
     *
     * @param file  Contenido cifrado del sobre digital.
     * @return      Conteido descifrado.
     *
     * @throws java.security.NoSuchProviderException
     * @throws java.security.NoSuchAlgorithmException
     * @throws javax.crypto.NoSuchPaddingException
     * @throws java.security.InvalidAlgorithmParameterException
     * @throws java.security.InvalidKeyException
     * @throws java.io.IOException
     * @throws org.bouncycastle.cms.CMSException
     * @throws javax.crypto.IllegalBlockSizeException
     * @throws javax.crypto.BadPaddingException
     */
     private byte [] deCipherContent(byte[] file) throws NoSuchProviderException, NoSuchAlgorithmException, NoSuchPaddingException, InvalidAlgorithmParameterException, InvalidKeyException, IllegalBlockSizeException, BadPaddingException{

        //asignamos los par&aacute;metros
        AlgorithmParameterSpec params = this.getParams(config);
        //Creamos el cipher
        Cipher cipher = createCipher(config.toString());
        //inicializamos el cipher
        cipher.init(Cipher.DECRYPT_MODE, cipherKey, params);

        //desciframos.
        return cipher.doFinal(file);
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

        //establecemos como configuraci&oacute;n para descifrar el contenido del paquete despu&eacute;s,
        config = new AOAlgorithmConfig(
                algorithm,
				AOCipherBlockMode.CBC,
				AOCipherPadding.PKCS5PADDING
            );

        // Desembolvemos la clave usada para cifrar el contenido
        // a partir de la clave privada del certificado del usuario.
        try {
			byte[] encrypted = passCiphered;
			final Cipher cipher2 = Cipher.getInstance("RSA/ECB/PKCS1Padding");
			cipher2.init(Cipher.UNWRAP_MODE, keyEntry.getPrivateKey());
			this.cipherKey = (SecretKey) cipher2.unwrap(encrypted, algorithm.getName(), Cipher.SECRET_KEY);

//			System.out.println("Metodo alternativo");
//			cipher2.init(Cipher.DECRYPT_MODE, keyEntry.getPrivateKey());
//			this.cipherKey = new SecretKeySpec(cipher2.doFinal(encrypted), "AES");
			
		} catch (Throwable e) {
            Logger.getLogger("es.gob.afirma").severe("Ocurri\u00F3 un error al recuperar la clave de cifrado del sobre digital: " + e);
            throw new AOException("Ocurrio un error al recuperar la clave de cifrado del sobre digital: " + e);
		}
    }


    /**
     * Crea el cifrador usado para cifrar tanto el fichero como la clave usada para
     * cifrar dicho fichero.
     *
     * @param algName algoritmo utilizado para cifrar.
     * @return Cifrador adecuado
     * @throws java.security.NoSuchAlgorithmException
     * @throws javax.crypto.NoSuchPaddingException
     */
     private Cipher createCipher(String algName) throws NoSuchAlgorithmException, NoSuchPaddingException {
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
		} 
		else {
			if(!algorithmConfig.getBlockMode().equals(AOCipherBlockMode.ECB)) {
				params = new IvParameterSpec(
					algorithmConfig.getAlgorithm().equals(AOCipherAlgorithm.AES) ? IV_16 : IV_8
				);
			}
		}

		return params;
	}


    /**
     * M&eacute;todo que transforma un Inputstream en un array de bytes.
     * @param file  InputStream a ser transformado.
     * @return      Fichero en formato array de bytes.
     * @throws IOException Cuando no se pudieron leer los datos de la firma. 
     */
    private byte[] createArrayFromFile(InputStream file) throws IOException {

        ByteArrayOutputStream baos=null;
        try{

            InputStream dataReader = file;

            byte[] buffer = new byte[1024];
            int len;
            baos = new ByteArrayOutputStream();

            while (dataReader.available() != 0) {
                len = dataReader.read(buffer);
                baos.write(buffer, 0, len);
            }

        }
        catch (Throwable ex){
        	throw new IOException("Ocurrio un error durante el proceso de lectura del fichero: " + ex);
        }

        return baos.toByteArray();

    }

}


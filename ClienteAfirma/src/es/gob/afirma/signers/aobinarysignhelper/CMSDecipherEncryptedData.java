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
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.spec.AlgorithmParameterSpec;
import java.util.Enumeration;
import java.util.logging.Logger;

import javax.crypto.Cipher;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.PBEParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import org.bouncycastle.asn1.ASN1InputStream;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.ASN1TaggedObject;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.EncryptedData;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

import sun.misc.BASE64Decoder;
import es.gob.afirma.ciphers.AOAlgorithmConfig;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;
import es.gob.afirma.misc.AOConstants.AOCipherBlockMode;
import es.gob.afirma.misc.AOConstants.AOCipherPadding;

/**
 * Clase que descifra el contenido de un fichero en formato EncryptedData
 * de CMS.
 *
 * Se usa para ello una clave del usuario.
 */
public class CMSDecipherEncryptedData extends SigUtils {


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
     * M&eacute;todo principal que descifra datos del tipo de EncryptedData.
     *
     * @param file      Archivo que los datos cifrados.
     * @param pass      Contrase&ntilde;a o clave que se uso para cifrar los datos.
     *
     * @return          Datos sin encriptar.
     * @throws IOException Si ocurre alg&uacute;n problema leyendo o escribiendo los datos
     * @throws AOException Cuando ocurre un error durante el proceso de descifrado (formato o clave incorrecto,...)
     * @throws InvalidKeyException Cuando se proporciona una clave incorrecta para el descifrado.
     */
    @SuppressWarnings("unchecked")
	public byte[] dechiperEncryptedData(InputStream file, String pass) throws AOException, IOException, InvalidKeyException {

        AlgorithmIdentifier alg = null;
        EncryptedContentInfo eci = null;

        // donde se guardara el resultad.
        final byte[] deciphered;

        // leemos el fichero que contiene la firma.
        byte[] codeFile = createArrayFromFile(file);
        ASN1InputStream is = new ASN1InputStream(codeFile);

        try {
            // Comenzamos a obtener los datos.
            ASN1Sequence dsq = null;
            dsq = (ASN1Sequence) is.readObject();
            Enumeration<Object> e = dsq.getObjects();
            // Elementos que contienen los elementos OID EncryptedData.
            e.nextElement();
            // Contenido de EncryptedData
            ASN1TaggedObject doj = (ASN1TaggedObject) e.nextElement();
            ASN1Sequence contentEncryptedData = (ASN1Sequence) doj.getObject();

            //Obtenemos los datos del encryptedData.
            Enumeration<Object> e2 = contentEncryptedData.getObjects();
            // version
            e2.nextElement();
            // EncryptedContentInfo. donde está lo que necesitamos.
            eci = EncryptedContentInfo.getInstance(e2.nextElement());

            //Obtenemos el agoritmo de cifrado
            alg= eci.getContentEncryptionAlgorithm();

            // Se intenta obtener el encrypted data.
            // Si no puede convertirse, dara error.
            // "EncryptedData EncryptedData" no se usara. solo es para verificar que es de este tipo.
            new EncryptedData(eci);

         }catch (Exception ex){
            Logger.getLogger("es.gob.afirma").severe("El fichero no contiene un tipo EncryptedData:  " + ex);
            throw new AOException("El fichero no contiene un tipo EncryptedData", ex);
        }

        //asignamos la clave de descifrado a partir del algoritmo.
        assignKey(alg,pass);

        //Obtenemos el contenido cifrado.
        byte [] contCifrado = eci.getEncryptedContent().getOctets();

        //Desciframos.
        try {
            deciphered = deCipherContent(contCifrado);
        } catch (InvalidKeyException ex) {
        	throw ex;
        } catch (Exception ex) {
        	Logger.getLogger("es.gob.afirma").severe("El fichero no contiene un tipo EncryptedData:  " + ex);
        	throw new AOException("Error al descifrar los contenidos encriptados", ex);
        }

        return deciphered;
     }


    /**
     * Descifra el contenido a partir de un fichero usando la clave del usuario.
     * @param file  Contenido cifrado del sobre digital.
     * @return Contenido descifrado
     * @throws Exception
     */
     private byte [] deCipherContent(byte[] file) throws Exception {

        //asignamos los par&aacute;metros
        AlgorithmParameterSpec params = this.getParams(config);
        //Creamos el cipher
        Cipher cipher = createCipher(config.toString());
        //inicializamos el cipher
        cipher.init(Cipher.DECRYPT_MODE, cipherKey, params);
        //desciframos.
        byte [] ciphered = cipher.doFinal(file);

        return ciphered;
    }

    
    /**
     * Asigna la clave para firmar el contenido del fichero que queremos envolver
     * y qeu m&aacute;s tarde ser&aacute; cifrada con la clave p&uacute;blica del usuario que
     * hace la firma.
     *
     * @param alg    Algoritmo necesario para crear la clave.
     * @param key    Contrase&ntilde;a que se va a usar para cifrar.
     * @throws AOException Cuando la clave o password no son v&aacute;lidas.
     */
    private void assignKey(AlgorithmIdentifier alg, String key) throws AOException{

        // obtenemos el oid del algoritmo.
        String algoritmoOid = alg.getObjectId().toString();
        
        AOCipherAlgorithm algorithm = null;
        AOCipherAlgorithm aux = null;

        // A partir de los tipos de algoritmos, buscamos el que coincida
        // con el oid de cifrado obtenido del fichero de firma.
        AOCipherAlgorithm[] algoritmos = AOCipherAlgorithm.values();
        for (int i=0; i<algoritmos.length;i++){
            aux = algoritmos[i];
            if (aux.getOid().equals(algoritmoOid)){
                algorithm = aux;
            }
        }
        //Creamos una configuraci&oacute;n  partir del algoritmo.
        config = new AOAlgorithmConfig(
                algorithm,
				AOCipherBlockMode.CBC,
				AOCipherPadding.PKCS5PADDING
            );

        // Generamos la clave necesaria para el cifrado
        if ((config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHMD5ANDDES)) ||
                (config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHSHA1ANDDESEDE)) ||
                (config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHSHA1ANDRC2_40))){
            try {
             this.cipherKey = SecretKeyFactory.getInstance(config.getAlgorithm().getName())
				.generateSecret(new PBEKeySpec(key.toCharArray(), SALT, ITERATION_COUNT));
            } catch (Throwable ex) {
                throw new AOException("Ocurrio un error durante el proceso de asignacion de la clave (a partir de password)", ex);
            }
        }
        else{
            try {
                this.cipherKey = new SecretKeySpec(new BASE64Decoder().decodeBuffer(key), config.getAlgorithm().getName());
            } catch (Throwable ex) {
               throw new AOException("Ocurrio un error durante el proceso de asignacion de la clave (a partir de key)", ex);
            }
        }


    }

//    /**
//     * Genera el proveedor de cifrado.
//     *
//     * @param providerName  Nombre del proveedor.
//     * @return  El proveedor.
//     * @throws java.security.NoSuchProviderException
//     */
//    private static Provider getProvider(String providerName)
//        throws NoSuchProviderException
//    {
//        if (providerName != null)
//        {
//            Provider prov = Security.getProvider(providerName);
//            if (prov != null)
//            {
//                return prov;
//            }
//            throw new NoSuchProviderException("provider " + providerName + " not found.");
//        }
//        return null;
//    }

    /**
     * Crea el cifrador usado para cifrar tanto el fichero como la clave usada para
     * cifrar dicho fichero.
     *
     * @param algName algoritmo utilizado para cifrar.
     * @param provider  Proveedor que se utiliza para cifrar.
     * @return Cifrador adecuado
     * @throws java.security.NoSuchAlgorithmException
     * @throws javax.crypto.NoSuchPaddingException
     */
     private Cipher createCipher(String algName) throws NoSuchAlgorithmException, NoSuchPaddingException
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
     * @throws		IOException Cuando no se pudieron leer los datos de la firma. 
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

        }catch (Exception ex){
            throw new IOException("Ocurrio un error durante el proceso de lectura del fichero: " + ex);
        }

        return baos.toByteArray();

    }

//    /**
//     * M&eacute;todo principal para pruebas.
//     * @param args
//     */
//    public static void main(String[] args){
//        FileInputStream fis = null;
//
//        try {
//            File firma = new File("c:\\encrypted.p7s");
//            //File firma = new File("c:\\resultado.jpg");
//            fis = new FileInputStream(firma);
//
//            // clave de descifrado
//            String pass= "mipropiaclave";
//            byte[] a = new CMSDecipherEncryptedData().dechiperEncryptedData(fis, pass);
//
//            // Se abre el fichero donde se hará la copia
//			FileOutputStream fileOutput = new FileOutputStream ("c:\\resultado.jpg");
//			fileOutput.write(a);
//			fileOutput.close();
//
//
//
//            System.out.println(a);
//
//        }
//        catch (Exception ex) {
//            Logger.getLogger(ValidateCMS.class.getName()).log(Level.SEVERE, null, ex);
//
//        }
//
//    }

}


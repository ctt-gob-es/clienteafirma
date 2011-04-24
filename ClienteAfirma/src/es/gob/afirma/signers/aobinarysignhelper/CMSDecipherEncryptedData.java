/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */

package es.gob.afirma.signers.aobinarysignhelper;

import java.security.InvalidKeyException;
import java.util.Enumeration;
import java.util.logging.Logger;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;

import org.apache.commons.codec.binary.Base64;
import org.bouncycastle.asn1.ASN1Sequence;
import org.bouncycastle.asn1.cms.EncryptedContentInfo;
import org.bouncycastle.asn1.cms.EncryptedData;
import org.bouncycastle.asn1.x509.AlgorithmIdentifier;

import es.gob.afirma.ciphers.AOCipherConfig;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;

/**
 * Clase que descifra el contenido de un fichero en formato EncryptedData
 * de CMS.
 *
 * Se usa para ello una clave del usuario.
 */
public final class CMSDecipherEncryptedData  {


	/**
	 * Clave de cifrado. La almacenamos internamente porque no hay forma de mostrarla
	 * directamente al usuario.
	 */
	private SecretKey cipherKey;

	private AOCipherConfig config;

	/**
	 * M&eacute;todo principal que descifra datos del tipo de EncryptedData.
	 *
	 * @param encryptedData      Datos  del tipo CMS EncryptedData.
	 * @param pass      Contrase&ntilde;a o clave que se uso para cifrar los datos.
	 *
	 * @return          Datos sin encriptar.
	 * @throws AOException Cuando ocurre un error durante el proceso de descifrado (formato o clave incorrecto,...)
	 * @throws InvalidKeyException Cuando se proporciona una clave incorrecta para el descifrado.
	 */
	public byte[] dechiperEncryptedData(byte[] encryptedData, String pass) throws AOException, InvalidKeyException {

		AlgorithmIdentifier alg = null;
		EncryptedContentInfo eci = null;

		// donde se guardara el resultad.
		final byte[] deciphered;

		try {
			ASN1Sequence contentEncryptedData = Utils.fetchWrappedData(encryptedData);

			//Obtenemos los datos del encryptedData.
			Enumeration<?> e2 = contentEncryptedData.getObjects();
			// version
			e2.nextElement();
			// EncryptedContentInfo. donde estÃ¡ lo que necesitamos.
			eci = EncryptedContentInfo.getInstance(e2.nextElement());

			//Obtenemos el agoritmo de cifrado
			alg = eci.getContentEncryptionAlgorithm();

			// Se intenta obtener el encrypted data.
			// Si no puede convertirse, dara error.
			// "EncryptedData EncryptedData" no se usara. solo es para verificar que es de este tipo.
			new EncryptedData(eci);
		} 
		catch (final Throwable ex){
			throw new AOException("El fichero no contiene un tipo EncryptedData", ex);
		}

		//asignamos la clave de descifrado a partir del algoritmo.
		assignKey(alg,pass);

		//Obtenemos el contenido cifrado.
		byte [] contCifrado = eci.getEncryptedContent().getOctets();

		//Desciframos.
		try {
			deciphered = Utils.deCipherContent(contCifrado, config, cipherKey);
		} catch (InvalidKeyException ex) {
			throw ex;
		} catch (Exception ex) {
			Logger.getLogger("es.gob.afirma").severe("El fichero no contiene un tipo EncryptedData:  " + ex);
			throw new AOException("Error al descifrar los contenidos encriptados", ex);
		}

		return deciphered;
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
		String algoritmoOid = alg.getAlgorithm().toString();

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
		config = new AOCipherConfig(algorithm, null, null);

		// Generamos la clave necesaria para el cifrado
		if ((config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHMD5ANDDES)) ||
				(config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHSHA1ANDDESEDE)) ||
				(config.getAlgorithm().equals(AOCipherAlgorithm.PBEWITHSHA1ANDRC2_40))) {
			try {
				this.cipherKey = Utils.loadCipherKey(config,key);
			} catch (Throwable ex) {
				throw new AOException("Error durante el proceso de asignacion de la clave (a partir de password)", ex);
			}
		}
		else {
			try {
				this.cipherKey = new SecretKeySpec(Base64.decodeBase64(key), config.getAlgorithm().getName());
			} catch (Throwable ex) {
				throw new AOException("Error durante el proceso de asignacion de la clave (a partir de key)", ex);
			}
		}


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
	//            // Se abre el fichero donde se harÃ¡ la copia
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


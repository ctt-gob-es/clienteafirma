/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.ciphers;

import java.security.InvalidKeyException;
import java.security.Key;
import java.security.spec.AlgorithmParameterSpec;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.PBEParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import sun.misc.BASE64Decoder;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOInvalidKeyException;
import es.gob.afirma.misc.AOConstants.AOCipherAlgorithm;
import es.gob.afirma.misc.AOConstants.AOCipherBlockMode;
import es.gob.afirma.misc.AOConstants.AOCipherPadding;

/**
 * Cifrador seg&uacute;n las capacidades del proveedor JCE (<i>Java Cryptography Extension</i>). 
 */
public final class AOSunJCECipher implements AOCipher {

	/**
	 * Proveedor Java que da soporte a los algoritmos aqui descritos.
	 */
	private static final String PROVIDER = "SunJCE";
		
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
	
	/** Configuraciones de cifrado soportadas. */
    private static final AOAlgorithmConfig[] SUPPORTED_CONFIGS = new AOAlgorithmConfig[] {
    	new AOAlgorithmConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.ECB, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.ECB, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.CBC, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.CBC, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.PCBC, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.PCBC, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.CTR, AOCipherPadding.NOPADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.CTR, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.CFB, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.CFB, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.OFB, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.AES, AOCipherBlockMode.OFB, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.ARCFOUR, AOCipherBlockMode.ECB, AOCipherPadding.NOPADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.ECB, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.ECB, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.CBC, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.CBC, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.PCBC, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.PCBC, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.CTR, AOCipherPadding.NOPADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.CTR, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.CFB, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.CFB, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.OFB, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.BLOWFISH, AOCipherBlockMode.OFB, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.ECB, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.ECB, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.CBC, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.CBC, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.PCBC, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.PCBC, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.CTR, AOCipherPadding.NOPADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.CTR, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.CFB, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.CFB, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.OFB, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.DES, AOCipherBlockMode.OFB, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.ECB, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.ECB, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.CBC, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.CBC, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.PCBC, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.PCBC, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.CTR, AOCipherPadding.NOPADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.CTR, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.CFB, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.CFB, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.OFB, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.TRIPLEDES, AOCipherBlockMode.OFB, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.RC2, AOCipherBlockMode.ECB, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.RC2, AOCipherBlockMode.ECB, AOCipherPadding.ISO10126PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.PBEWITHSHA1ANDDESEDE, AOCipherBlockMode.CBC, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.PBEWITHSHA1ANDRC2_40, AOCipherBlockMode.CBC, AOCipherPadding.PKCS5PADDING),
    	new AOAlgorithmConfig(AOCipherAlgorithm.PBEWITHMD5ANDDES, AOCipherBlockMode.CBC, AOCipherPadding.PKCS5PADDING)
	};
	
	public AOAlgorithmConfig[] getSupportedConfigs() {
		return SUPPORTED_CONFIGS.clone();
	}
    
	public byte[] cipher(byte[] data, AOAlgorithmConfig algorithmConfig, Key cipherKey) throws AOException, AOInvalidKeyException {
	
		if(data == null || algorithmConfig == null || cipherKey == null || data.length == 0) {
			throw new AOException("Los parametros de la funcion de cifrado no pueden ser nulos o vacios");
		}
		
		// Tomamos el cipher para el algoritmo indicado
		final Cipher cipher;
		try {
			cipher = Cipher.getInstance(algorithmConfig.toString(), PROVIDER);
		}
		catch (Exception e) {
			e.printStackTrace();
			throw new AOException("Error al obtener el cifrador: " + e);
		}
		
		// Inicializamos el cipher
		try {
			cipher.init(Cipher.ENCRYPT_MODE, cipherKey, this.getParams(algorithmConfig));
		}
		catch (InvalidKeyException e) {
			throw new AOInvalidKeyException("La clave de cifrado introducida no es valida para el algoritmo '" + algorithmConfig.getAlgorithm().getName() + "': " + e);
		}
		catch (Exception e) {
			throw new AOException("Error al inicializar el cifrador: " + e);
		}

		// Realizamos el cifrado
		try {
			return cipher.doFinal(data);
		}
		catch (BadPaddingException e) {
			throw new AOInvalidKeyException(e.toString());
		}
		catch (Exception e) {
			e.printStackTrace();
			throw new AOException("Error cifrando los datos: " + e);
		}
	}

	public byte[] decipher(byte[] data, AOAlgorithmConfig algorithmConfig, Key decipherKey) throws AOException, AOInvalidKeyException {
		
		if(data == null || algorithmConfig == null || decipherKey == null) {
			throw new AOException("Los parametros de la funcion de descifrado no pueden ser nulos");
		}
		
		// Tomamos el cipher para el algoritmo indicado
		final Cipher cipher;
		try {
			cipher = Cipher.getInstance(algorithmConfig.toString(), PROVIDER);
		}
		catch (Exception e) {
			e.printStackTrace();
			throw new AOException("Error obteniendo el descifrador: " + e);
		}

		// Inicializamos el cipher
		try {
			cipher.init(
				Cipher.DECRYPT_MODE,
				decipherKey,
				this.getParams(algorithmConfig)
			);
		}
		catch (InvalidKeyException e) {
			throw new AOInvalidKeyException("La clave de descifrado introducida no es valida para el algoritmo '" + algorithmConfig.getAlgorithm().getName() + "': " + e);
		}
		catch (Exception e) {
			e.printStackTrace();
			throw new AOException("Error al inicializar el descifrador: " + e);
		}
		
		// Realizamos el descifrado
		try {
			return cipher.doFinal(data);
		}
		catch (BadPaddingException e) {
			throw new AOInvalidKeyException(e.toString());
		}
		catch (Exception e) {
			throw new AOException(e.toString());
		}
	}

	public Key decodeKey(String base64Key, AOAlgorithmConfig algorithmConfig, Object[] params) throws AOException {
		if(base64Key == null || base64Key.length() < 1) throw new NullPointerException("La clave a descodificar no puede ser nula ni vacia");
		if(algorithmConfig == null) throw new NullPointerException("La configuracion de cifrado no puede ser nula");

		try {
			return new SecretKeySpec(new BASE64Decoder().decodeBuffer(base64Key), algorithmConfig.getAlgorithm().getName());
		}
		catch (Exception e) {
			throw new AOException(e.toString());
		}
	}
	
	public Key decodePassphrase(String passphrase, AOAlgorithmConfig algorithmConfig, Object[] params) throws AOException {
		
		if(passphrase == null || passphrase.length() < 1) throw new NullPointerException("La contrase\u00F1a para la generacion de la clave no puede ser nula ni vacia");
		if(algorithmConfig == null) throw new NullPointerException("La configuracion de cifrado no puede ser nula");
		
		try {
			return SecretKeyFactory.getInstance(algorithmConfig.getAlgorithm().getName(), PROVIDER)
				.generateSecret(new PBEKeySpec(passphrase.toCharArray(), SALT, ITERATION_COUNT));
		}
		catch (Exception e) {
			throw new AOException(e.toString());
		}
	}
	
	public Key generateKey(AOAlgorithmConfig algorithmConfig) throws AOException {
		try {
			return KeyGenerator.getInstance(algorithmConfig.getAlgorithm().getName(), PROVIDER).generateKey();
		}
		catch (Exception e) {
			throw new AOException("No se pudo generar una clave compatible para la configuracion '"
					+ algorithmConfig + "': " + e);
		}
	}
	
	/**
	 * Genera los par&aacute;metros necesarios para poder operar con una configuracion concreta de cifrado.
	 * Si no es necesario ning&uacute;n par&aacute;metro especial, devolvemos <code>null</code>.
	 * @param algorithmConfig Configuracion de cifrado que debemos parametrizar.
	 * @return Par&aacute;metros para operar.
	 */
	private AlgorithmParameterSpec getParams(AOAlgorithmConfig algorithmConfig) {
		
		AlgorithmParameterSpec params = null;
		if(algorithmConfig.getAlgorithm().supportsPassword()) params = new PBEParameterSpec(SALT, ITERATION_COUNT);
		else {
			if(!algorithmConfig.getBlockMode().equals(AOCipherBlockMode.ECB)) {
				params = new IvParameterSpec(
						algorithmConfig.getAlgorithm().equals(AOCipherAlgorithm.AES) ? IV_16 : IV_8
				);
			}
		}
		
		return params;
	}
//	
//	private AlgorithmParameterSpec getParams(String algorithmConfig) {
//		AlgorithmParameterSpec params = null;
//		String algorithm = this.getAlgorithmName(algorithmConfig);
//		if(algorithm.equals("PBEWithMD5AndDES") ||
//				algorithm.equals("PBEWithMD5AndTripleDES") ||
//				algorithm.equals("PBEWithSHA1AndDESede") ||
//				algorithm.equals("PBEWithSHA1AndRC2_40")) {
//			params = new PBEParameterSpec(SALT, ITERATION_COUNT);
//		} else {
//			String mode = this.getAlgorithmMode(algorithmConfig);
//			if(mode.length() > 1 && !mode.equals("ECB")) {
//				params = new IvParameterSpec(algorithm.equals("AES") ? IV_16 : IV_8);
//			}
//		}
//		return params;
//	}
}

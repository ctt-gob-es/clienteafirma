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

import java.security.Key;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.exceptions.AOInvalidKeyException;


/**
 * Define los requerimientos de las clases capaces de efectuar cifrado de datos.
 * Una clase de cifrado puede efectuar cifrados y descifrado de datos en m&aacute;s de
 * un formato.
 * @version 1.0
 */
public interface AOCipher {

	/**
	 * Recupera las distintas configuraciones de algoritmos de cifrado (algoritmo-modo_de_bloque-padding)
	 * que soporta el proveedor.
	 * @return Configuraciones de cifrado.
	 */
	public AOAlgorithmConfig[] getSupportedConfigs();
	
	/**
	 * Cifra un mensaje. El algoritmo que deseamos utilizar para el descifrado puede ir
	 * acompa&ntilde;ado de una configuraci&oacute;n, seg&uacute;n lo requiera el proveedor de
	 * cifrado. Por ejemplo, podr&iacute;a utilizarse una de las siguientes cadenas:<br/>
	 * <ul>
	 * <li><i>Algoritmo</i></li>
	 * <li><i>Algoritmo</i>/<i>Modo</i>/<i>Padding</i></li>
	 * </ul>
	 * @param data Datos a cifrar.
	 * @param algorithmConfig Configuraci&oacute;n del algoritmo de cifrado.
	 * @param cipherKey Clave con la que se desea cifrar.
	 * @return Datos cifrados.
	 * @throws AOException Cuando ocurre un error durante la operaci&oacute;n
	 * @throws AOInvalidKeyException Cuando la clave de cifrado introducida no es compatible con este algoritmo.
	 */
	public byte[] cipher(byte[] data, AOAlgorithmConfig algorithmConfig, Key cipherKey) throws AOException, AOInvalidKeyException;
	
	/**
	 * Descifra un mensaje. El algoritmo que deseamos utilizar para el descifrado puede ir
	 * acompa&ntilde;ado de una configuraci&oacute;n, seg&uacute;n lo requiera el proveedor de
	 * cifrado. Por ejemplo, podr&iacute;a utilizarse una de las siguientes cadenas:<br/>
	 * <ul>
	 * <li><i>Algoritmo</i></li>
	 * <li><i>Algoritmo</i>/<i>Modo</i>/<i>Padding</i></li>
	 * </ul>
	 * @param data Datos a descifrar.
	 * @param algorithmConfig Configuraci&oacute;n del algoritmo de cifrado.
	 * @param decipherKey Clave para el descifrado de los datos.
	 * @return Datos descifrados.
	 * @throws AOException Cuando ocurre un error durante la operaci&oacute;n
	 * @throws AOInvalidKeyException Cuando la clave de cifrado introducida no es compatible con este algoritmo.
	 */
	public byte[] decipher(byte[] data, AOAlgorithmConfig algorithmConfig, Key decipherKey) throws AOException, AOInvalidKeyException;
	
	/**
	 * Obtiene una clave para el algoritmo seleccionado a partir de su codificaci&oacute;n.
	 * Seg&uacute;n el algoritmo puede ser necesario o no el uso de par&aacute;metros adicionales.
	 * @param base64Key Clave codificada en Base64. 
	 * @param algorithmConfig Algoritmo de cifrado.
	 * @param params Par&aacute;metros adicionales.
	 * @return Clave.
	 * @throws AOException Cuando se produce un error al generar la clave.
	 */
	public Key decodeKey(String base64Key, AOAlgorithmConfig algorithmConfig, Object[] params) throws AOException;
	
	/**
	 * Obtiene una clave para el algoritmo seleccionado a partir de la contrase&ntilde;a.
	 * Seg&uacute;n el algoritmo puede ser necesario o no el uso de par&aacute;metros adicionales.
	 * @param passphrase Contrase&ntilde;a para la generaci&oacute;n de la clave. 
	 * @param algorithmConfig Algoritmo de cifrado.
	 * @param params Par&aacute;metros adicionales.
	 * @return Clave.
	 * @throws AOException Cuando se produce un error al generar la clave.
	 */
	public Key decodePassphrase(String passphrase, AOAlgorithmConfig algorithmConfig, Object[] params) throws AOException;
	
	/**
	 * Genera una nueva clave para el algoritmo seleccionado.
	 * @param algorithmConfig Algoritmo de cifrado.
	 * @return Clave.
	 * @throws AOException Cuando se produce un error al generar la clave.
	 */
	public Key generateKey(AOAlgorithmConfig algorithmConfig) throws AOException;

}

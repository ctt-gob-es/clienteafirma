package es.gob.afirma.ciphers;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;

/**
 * Define los metodos que deben utilizar las clases encargadas de cifrar y descifrar datos.
 */
public interface ServerCipher {

	/**
	 * Descifra los datos pasados por par&aacute;metro.
	 * @param data Datos a descifrar.
	 * @return Datos descifrados.
	 * @throws InvalidKeyException Si la clave proporcionada no es v&aacute;lida para el algoritmo especificado.
	 * @throws NoSuchAlgorithmException Si el algoritmo de cifrado solicitado no est&aacute; disponible en el entorno.
	 * @throws NoSuchPaddingException Si el esquema de relleno (padding) especificado no está soportado.
	 * @throws InvalidAlgorithmParameterException Si los par&aacute;metros del algoritmo (por ejemplo, IV) son incorrectos.
	 * @throws IllegalBlockSizeException Si el tama&ntilde;o del bloque de datos no es compatible con el modo de cifrado.
	 * @throws BadPaddingException Si los datos tienen un padding incorrecto o han sido alterados.
	 * @throws GeneralSecurityException Si ocurre un error general de seguridad durante la operaci&oacute;n.
	 * @throws IOException Si ocurre un error de entrada/salida al procesar los datos.
	 */
	byte[] decipherData(final String data) throws InvalidKeyException, NoSuchAlgorithmException,
														NoSuchPaddingException, InvalidAlgorithmParameterException,
														IllegalBlockSizeException, BadPaddingException,
														GeneralSecurityException, IOException;

	/**
	 * Descifra los datos pasados por par&aacute;metro.
	 * @param data Datos a descifrar.
	 * @return Datos descifrados.
	 * @throws GeneralSecurityException Si ocurre un error general de seguridad durante la operaci&oacute;n.
	 * @throws NoSuchAlgorithmException Si el algoritmo de cifrado solicitado no est&aacute; disponible en el entorno.
	 * @throws NoSuchPaddingException Si el esquema de relleno (padding) especificado no está soportado.
	 * @throws InvalidKeyException Si la clave proporcionada no es v&aacute;lida para el algoritmo especificado.
	 * @throws InvalidAlgorithmParameterException Si los par&aacute;metros del algoritmo (por ejemplo, IV) son incorrectos.
	 * @throws IllegalBlockSizeException Si el tama&ntilde;o del bloque de datos no es compatible con el modo de cifrado.
	 * @throws BadPaddingException Si los datos tienen un padding incorrecto o han sido alterados.
	 * @throws IOException Si ocurre un error de entrada/salida al procesar los datos.
	 */
	byte[] decipherData(final byte[] data) throws GeneralSecurityException,
														NoSuchAlgorithmException, NoSuchPaddingException,
														InvalidKeyException, InvalidAlgorithmParameterException,
														IllegalBlockSizeException, BadPaddingException, IOException;

	/**
	 * Cifra los datos pasados por par&aacute;etro.
	 * @param data Datos a cifrar
	 * @return Datos cifrados.
	 * @throws IOException Si se produce un error de entrada/salida durante la operaci&oacute;n.
	 * @throws NoSuchAlgorithmException Si el algoritmo de cifrado solicitado no est&aacute; disponible.
	 * @throws NoSuchPaddingException Si el esquema de relleno especificado no est&aacute; soportado.
	 * @throws InvalidKeyException Si la clave proporcionada no es v&aacute;lida para el algoritmo seleccionado.
	 * @throws InvalidAlgorithmParameterException Si los par&aacute;metros del algoritmo son inv&aacute;lidos.
	 * @throws IllegalBlockSizeException Si el tama&ntilde;o del bloque de datos no es compatible con el modo de cifrado.
	 * @throws BadPaddingException Si el relleno (padding) de los datos es incorrecto o los datos han sido alterados.
	 * @throws GeneralSecurityException Si ocurre un error general de seguridad durante la ejecuci&oacute;n.
	 */
	String cipherData(final byte[] data) throws IOException,
													NoSuchAlgorithmException, NoSuchPaddingException,
													InvalidKeyException, InvalidAlgorithmParameterException,
													IllegalBlockSizeException, BadPaddingException, GeneralSecurityException;

}

package es.gob.afirma.standalone.crypto;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.InvalidKeyException;
import java.util.Arrays;

import es.gob.afirma.core.misc.Base64;

/** Gestor para el cifrado sim&eacute;trico de datos (para el servidor intermedio). */
public final class CypherDataManager {

	/** Car&aacute;cter utilizado para separar el padding agregado a los datos para cifrarlos y los propios datos
	 * cifrados en base64. */
	private static final char PADDING_CHAR_SEPARATOR = '.';

	/** Juego de carateres UTF-8. */
	private static final String DEFAULT_URL_ENCODING = "UTF-8"; //$NON-NLS-1$

	/** Descifra datos.
	 * @param cypheredDataB64 Datos cifrados (en Base64)
	 * @param cypherKey Clave de descifrado
	 * @return Datos descifrados
	 * @throws InvalidKeyException Si la clave de descifrado no es v&aacute;lida
	 * @throws GeneralSecurityException Cuando falla el proceso de cifrado
	 * @throws IOException Si hay problemas en el tratamiento de datos */
	public static byte[] decipherData(final byte[] cypheredDataB64,
			                          final byte[] cypherKey) throws InvalidKeyException,
			                                                         GeneralSecurityException,
			                                                         IOException {
		final String recoveredData = new String(cypheredDataB64, DEFAULT_URL_ENCODING).replace("_", "/").replace("-", "+"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		if (cypherKey != null) {
			return decipherData(recoveredData, cypherKey);
		}
		return Base64.decode(recoveredData, true);
	}

	/** Descifra una cadena de datos. Esta cadena viene precedida por el n&uacute;mero de caracteres de padding que
	 * se agregaron y separado por un punto (.) de la cadena base 64 con los datos cifrados.
	 * @param data Cadena de datos con la forma: PADDING.CIPHERDATAB64.
	 * @param cipherKey Clave de cifrado.
	 * @return Datos descifrados.
	 * @throws InvalidKeyException Cuando la clave no es v&aacute;lida.
	 * @throws GeneralSecurityException Cuando falla el proceso de cifrado.
	 * @throws IllegalArgumentException Si los datos no se corresponden con un Base64 v&aacute;lido.
	 * @throws IOException Cuando ocurre un error en la decodificaci&oacute;n de los datos. */
	private static byte[] decipherData(final String data,
			                           final byte[] cipherKey) throws InvalidKeyException,
			                                                          GeneralSecurityException,
			                                                          IllegalArgumentException,
			                                                          IOException {
		int padding = 0;
		final int dotPos = data.indexOf(PADDING_CHAR_SEPARATOR);
		if (dotPos != -1) {
			padding = Integer.parseInt(data.substring(0, dotPos));
		}

		final byte[] decipheredData = DesCipher.decipher(
				Base64.decode(data.substring(dotPos + 1).replace('+', '-').replace('/', '_'), true),
				cipherKey);

		return padding == 0 ? decipheredData : Arrays.copyOf(decipheredData, decipheredData.length - padding);
	}

	/** Genera una cadena con datos cifrados y codificados en base 64 antecedidos por el n&uacute;mero de
	 * caracteres que se han tenido que agregar como padding y separados por un car&aacute;cter separador.
	 * @param data Datos a cifrar.
	 * @param cipherKey Clave de cifrado.
	 * @return Cadena con el numero de caracteres agregados manualmente para cumplir la longitud requerida,
	 * el caracter separador y los datos cifrados y en base 64.
	 * @throws InvalidKeyException Cuando la clave no es v&aacute;lida.
	 * @throws GeneralSecurityException Cuando falla el proceso de cifrado. */
	public static String cipherData(final byte[] data, final byte[] cipherKey) throws InvalidKeyException, GeneralSecurityException {
		return new StringBuilder((int)(data.length * 1.2))
			.append(Integer.toString((DesCipher.getPaddingLength() - data.length % DesCipher.getPaddingLength()) % DesCipher.getPaddingLength()))
			.append(PADDING_CHAR_SEPARATOR)
			.append(Base64.encode(DesCipher.cipher(data, cipherKey), true)).toString();
	}
	
	/** Genera una cadena con datos cifrados y codificados en base 64 antecedidos por el n&uacute;mero de
	 * caracteres que se han tenido que agregar como padding y separados por un car&aacute;cter separador.
	 * Usado para multiples firmas
	 * @param data Datos a cifrar.
	 * @param cipherKey Clave de cifrado.
	 * @return Cadena con el numero de caracteres agregados manualmente para cumplir la longitud requerida,
	 * el caracter separador y los datos cifrados y en base 64.
	 * @throws InvalidKeyException Cuando la clave no es v&aacute;lida.
	 * @throws GeneralSecurityException Cuando falla el proceso de cifrado.
	 * @throws IOException En caso de errores en el tratamiento de datos. */
	public static String cipherDatas(final byte[][] data, final byte[] cipherKey) throws InvalidKeyException, GeneralSecurityException, IOException 
	{
		StringBuilder cadenaFinal = new StringBuilder ();
		
		for (int i = 0; i < data.length; i++)
		{
			System.out.println (Base64.encode(data[i]));
			if(i>0){
				cadenaFinal.append(":");
			}
			cadenaFinal.append(cipherData(data[i],cipherKey));
		}
				
		return cadenaFinal.toString();
	}
}

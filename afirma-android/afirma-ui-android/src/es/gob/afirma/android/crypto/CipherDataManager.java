package es.gob.afirma.android.crypto;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.InvalidKeyException;
import java.util.Arrays;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;

import android.util.Log;
import es.gob.afirma.core.misc.Base64;

/** Gestor para el cifrado sim&eacute;trico de datos (para el servidor intermedio). */
public final class CipherDataManager {

	/** Tag del log. */
	private static final String ES_GOB_AFIRMA = "es.gob.afirma"; //$NON-NLS-1$

    /** Padding requerido para el cifrado. */
    private static final int PADDING_LENGTH = 8;

	/** Car&aacute;cter utilizado para separar el padding agregado a los datos para cifrarlos y los propios datos
	 * cifrados en Base64. */
	private static final char PADDING_CHAR_SEPARATOR = '.';

	/** Juego de carateres UTF-8. */
	private static final String DEFAULT_URL_ENCODING = "UTF-8"; //$NON-NLS-1$

	/** Descifra datos, que deben proporcionarse como una cadena de texto (su codificaci&oacute;n binaria)
	 * que contenga los propios datos a descifrar en Base64 precedida por el n&uacute;mero de caracteres de
	 * relleno que se agregaron y separados por un punto (.).
	 * Por ejemplo, la cadena "3.dGV4dG8gY2lmcmFkbyAgIA==", donde "dGV4dG8gY2lmcmFkbyAgIA==" son los datos
	 * cifrados y se indica que a&ntilde;adieron en origen 3 octetos de relleno.
	 * @param cipheredDataB64 Datos cifrados, en el formato indicado.
	 * @param cipherKey Clave de descifrado.
	 * @return Datos descifrados y con el relleno eliminado.
	 * @throws InvalidKeyException Cuando la clave proporcionada no es una claves DES v&aacute;lida.
	 * @throws GeneralSecurityException Si hay errores de seguridad.
	 * @throws IOException Si hay problemas en el tratamiento de los datos, */
	public static byte[] decipherData(final byte[] cipheredDataB64,
			                          final byte[] cipherKey) throws InvalidKeyException,
			                                                         GeneralSecurityException,
			                                                         IOException {

		Log.i(ES_GOB_AFIRMA, "Componemos la cadena para descifrar"); //$NON-NLS-1$
		final String recoveredData = new String(cipheredDataB64, DEFAULT_URL_ENCODING).replace("_", "/").replace("-", "+"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		byte[] decipheredData;
		if (cipherKey != null) {
			Log.i(ES_GOB_AFIRMA, "Vamos a descifrar"); //$NON-NLS-1$
			decipheredData = decipherData(recoveredData, cipherKey);
			Log.i(ES_GOB_AFIRMA, "Descifrado"); //$NON-NLS-1$
		}
		else {
			Log.i(ES_GOB_AFIRMA, "No tenemos clave para descifrar. Consideramos los datos como descifrados"); //$NON-NLS-1$
			decipheredData = Base64.decode(recoveredData, true);
		}
		return decipheredData;
	}

	/** Descifra una cadena de datos. Esta cadena viene precedida por el n&uacute;mero de caracteres de relleno que
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

		final byte[] decipheredData = decipher(
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
	 * @throws GeneralSecurityException Cuando falla el proceso de cifrado.
	 * @throws IOException Si hay errores en el propio proceso de cifrado o en el tratamiento general de los datos. */
	public static String cipherData(final byte[] data, final byte[] cipherKey) throws InvalidKeyException, GeneralSecurityException, IOException {
		return getNeededPaddingAsString(data) + PADDING_CHAR_SEPARATOR + Base64.encode(cipher(data, cipherKey), true);
	}

	/** Devuelve, como n&uacute;mero pasado a cadena de texto, la cantidad de octetos que hay que a&ntilde;adir
	 * a un array de octetos para que estos sean m&uacute;ltiplos de 8.
	 * @param data Array de octetos de entrada.
	 * @return Cantidad de octetos que hay que a&ntilde;adir al array de octetos de entrada para que el
	 * n&uacute;mero total de octetos de este sea m&uacute;ltiplo de 8.*/
	private static String getNeededPaddingAsString(final byte[] data) {
	    return Integer.toString((getBlockSize() - data.length % getBlockSize()) % getBlockSize());
	}

    /** Cifra datos con un algoritmo DES (ECB sin relleno). Antes de cifrar los datos se les a&ntilde;ade
     * un relleno independiente del cifrado, f&aacute;cilmente eliminable cuando los datos de entrada
     * con Base64.
     * @param data Datos que se desean cifrar.
     * @param sk Contrase&ntilde;a para generar la clave secreta.
     * @return Resultado del proceso de cifrado.
     * @throws InvalidKeyException Cuando La clave no es valida para el algoritmo DES.
     * @throws GeneralSecurityException Cuando los datos introducidos no son validos o se produce un error en la operaci&oacute;n. */
    private static byte[] cipher(final byte[] data, final byte[] sk) throws InvalidKeyException, GeneralSecurityException {
        final Cipher desCipher;
        try {
            desCipher = Cipher.getInstance("DES/ECB/NoPadding"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            // Este caso no deberia ocurrir nunca
            throw new GeneralSecurityException("Algoritmo o formato no soportado por la maquina virtual: " + e, e); //$NON-NLS-1$
        }
        desCipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(sk, "DES")); //$NON-NLS-1$
        return desCipher.doFinal(padData(data, 8));
    }

    /** Descifra datos con un algoritmo DES (ECB sin relleno).
     * @param data Datos que se desean cifrar.
     * @param sk Contrase&ntilde;a para generar la clave secreta.
     * @return Resultado del proceso de descifrado.
     * @throws InvalidKeyException Cuando La clave no es valida para el algoritmo DES.
     * @throws GeneralSecurityException Cuando los datos introducidos no son validos o se produce un error en la operaci&oacute;n. */
    private static byte[] decipher(final byte[] data, final byte[] sk) throws InvalidKeyException, GeneralSecurityException {
        final Cipher desCipher;
        try {
            desCipher = Cipher.getInstance("DES/ECB/NoPadding"); //$NON-NLS-1$
        }
        catch (final Exception e) {
            // Este caso no deberia ocurrir nunca
        	Log.e(ES_GOB_AFIRMA, "Algoritmo o formato no soportado por la maquina virtual", e); //$NON-NLS-1$
            throw new GeneralSecurityException("Algoritmo o formato no soportado por la maquina virtual: " + e, e); //$NON-NLS-1$
        }
        desCipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(sk, "DES")); //$NON-NLS-1$

        return desCipher.doFinal(data);
    }

    /** Rellena un array de bytes para que sea m&uacute;ltiplo de la cantidad indicada.
     * @param data Datos de entrada
     * @param padding M&acute;ltiplo
     * @return Datos con el relleno a&ntilde;adido */
    private static byte[] padData(final byte[] data, final int padding) {
        if (data.length % padding == 0) {
            return data;
        }
        return Arrays.copyOf(data, (data.length / padding + 1) * padding);
    }

    /** Recupera la longitud del relleno requerido para el cifrado. Esto es, de que n&uacute;mero deber se
     * m&uacute;ltiplo la longitud de los datos.
     * @return Longitud del relleno. */
    private static int getBlockSize() {
        return PADDING_LENGTH;
    }

}

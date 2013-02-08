package es.gob.afirma.signfolder;

import java.io.IOException;

/** Utilidades de codificaci&oacute;n y dedecodificaci&oacute;n de datos en Base64.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public interface Base64Codec {

	/** Descodifica datos en Base64.
	 * @param base64EncodedData Datos codificados en Base64
	 * @return Datos binarios descodificados
	 * @throws IOException Cuando ocurre alg&uacute;n problema en la descodificaci&oacute;n */
	byte[] base64Decode(final String base64EncodedData) throws IOException;

	/** Codifica datos en Base64.
	 * @param data Datos a codificar
	 * @return Datos codificados en Base64
	 * @throws IOException Cuando ocurre alg&uacute;n problema en la codificaci&oacute;n */
	String base64EncodeData(final byte[] data) throws IOException;

	/** Convierte datos en Base64 en una cadena de texto susceptible de ser enviada en una URL (se convierten los
	 * caracteres '\n', '\r', '=', espacio, '+' y '/').
	 * @param base64Data Datos en Base64
	 * @return Texto compatible con codificaci&oacute;n URL */
	String base64ToUrlEncoding(final String base64Data);

}

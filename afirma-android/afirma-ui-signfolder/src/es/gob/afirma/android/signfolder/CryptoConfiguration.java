package es.gob.afirma.android.signfolder;

import java.security.KeyStore.PrivateKeyEntry;

/**
 * Clase para la referenciaci&oacute;n est&aacute;tica del certificado de autenticaci&oacute;n
 * y firma.
 * 
 * IMPORTANTE: Es obligatoria esta referencia para sortear un bug de Android introducido en la
 * versi&oacute;n 4.1.2 y solventado en la 4.2. Este bug provoca un error a nivel nativo en la
 * biblioteca "libcrypto.so" cuando se pierde la referencia a una clave extraida del almacen de
 * claves del usuario. Se puede encontrar m&aacute;s informacion de este error en:
 * 
 * 		https://code.google.com/p/android/issues/detail?id=62319
 * 
 * Los objetos almacenadados en esta clase no se utilizan, con estar referenciados ya deja de
 * producirse el error.   
 */
public class CryptoConfiguration {

	private static PrivateKeyEntry pke = null;
	
	private static String alias = null;
	
	/**
	 * Establece la referencia al certificado seleccionado y sus claves.
	 * @param keyEntry Referencia a los datos del certificado.
	 */
	public static void setCertificatePrivateKeyEntry(PrivateKeyEntry keyEntry) {
		pke = keyEntry;
	}

	/**
	 * Establece el alias del certificado seleccionado.
	 * @param certAlias Alias del certificado seleccionado.
	 */
	public static void setCertificateAlias(String certAlias) {
		alias = certAlias;
	}

	/**
	 * Recupera la referencia al certificado seleccionado y sus claves.
	 * @return Referencia a los datos del certificado.
	 */
	public static PrivateKeyEntry getCertificatePrivateKeyEntry() {
		return pke;
	}
	
	/**
	 * Recupera el alias del certificado seleccionado.
	 * @return Alias del certificado.
	 */
	public static String getCertificateAlias() {
		return alias;
	}
}

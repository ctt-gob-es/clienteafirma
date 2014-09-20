package es.gob.afirma.core.keystores;

import java.io.IOException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableEntryException;
import java.security.cert.X509Certificate;

import javax.security.auth.callback.PasswordCallback;

/** Interfaz que define las funciones b&aacute;sicas de un almacen de certificados.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public interface KeyStoreManager {

	/** Indica si el certificado contiene clave privada.
	 * @param alias Alias del certificado.
	 * @return <code>true</code> si el certificado contiene clave privada, <code>false</code> en caso contrario.
	 * @throws KeyStoreException Si no se puede comprobar la presencia de clave privada o no existe el certificado
	 *                           con el alias indicado. */
	boolean isKeyEntry(String alias) throws KeyStoreException;

	/** Obtiene todos los alias de los certificados del almac&eacute;n actual.
     * @return Todos los alias encontrados en el almac&eacute;n actual */
	String[] getAliases();

	/** Obtiene un certificado del keystore activo a partir de su alias.
     * @param alias
     *        Alias del certificado.
     * @return El certificado o {@code null} si no se pudo recuperar. */
    X509Certificate getCertificate(String alias);

    /** Obtiene la cadena de certificaci&oacute;n de un certificado del keystore activo a partir de su alias.
     * @param alias Alias del certificado.
     * @return Certificados de la cadena de certificaci&oacute;n o {@code null} si no se pudo recuperar. */
    X509Certificate[] getCertificateChain(String alias);

    /** Obtiene la clave privada de un certificado.
     * @param alias
     *        Alias del certificado
     * @param pssCallback
     *        <i>CallBback</i> para obtener la contrase&ntilde;a del
     *        certificado que contiene la clave
     * @return Clave privada del certificado correspondiente al alias
     * @throws KeyStoreException
     * 		   Cuando ocurren errores en el tratamiento del almac&eacute;n de claves
     * @throws NoSuchAlgorithmException
     * 		   Cuando no se puede identificar el algoritmo para la recuperaci&oacute;n de la clave.
     * @throws UnrecoverableEntryException
     * 		   Si la contrase&ntilde;a proporcionada no es v&aacute;lida para obtener la clave privada
     * @throws es.gob.afirma.core.AOCancelledOperationException
     * 		   Cuando el usuario cancela el proceso antes de que finalice
     */
    KeyStore.PrivateKeyEntry getKeyEntry(String alias,
             PasswordCallback pssCallback) throws KeyStoreException,
                                                  NoSuchAlgorithmException,
                                                  UnrecoverableEntryException;

	/** Refresca los certificados del almac&eacute;n actual.
	 * @throws IOException En caso de errores de entrada / salida */
	void refresh() throws IOException;

}

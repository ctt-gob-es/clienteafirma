package es.gob.afirma.core.ui;

import java.io.IOException;
import java.security.KeyStore.PrivateKeyEntry;
import java.security.cert.Certificate;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.core.keystores.NameCertificateBean;

/**
 * Interfaz que implementan los di&aacute;logos de selecciOacute;n de certificados.
 */
public interface KeyStoreDialogManager {

	/**
	 * Manda recargar al almac&eacute;n asociado actualmente al di&aacute;logo de
	 * selecci&oacute;n.
	 * @throws IOException En caso de errores de entrada / salida
	 */
	void refresh() throws IOException;
	
	/**
	 * Obtiene el listado de certificados con alias que deben mostrarse en el
	 * di&aacute;logo de selecci&oacute;n.
	 * @return Listado de certificados con alias.
	 */
	NameCertificateBean[] getNameCertificates();

	/**
	 * Cambia el almacen que gestiona internamente el di&aacute;logo.
	 * @param ksm Almac&eacute;n de certificados.
	 */
	void setKeyStoreManager(KeyStoreManager ksm);

	/**
	 * Devuelve la clave asociada a un alias.
	 * @param alias Alias de la clave que se desea recuperar.
	 * @throws AOException Cuando no se puede extraer la clave del almac&eacute;n.
	 * @return Clave.
	 */
	Object getKeyEntry(String alias) throws AOException;

	/** Muestra el di&aacute;logo de selecci&oacute;n y establece cual es la pareja clave-certificado seleccionado.
	 * Este m&eacute;todo no puede utilizarse cuando el di&aacute;logo se utilice para mostrar certificados sin
	 * clave privada.
	 * @throws AOCancelledOperationException Cuando no se selecciona ning&uacute;n certificado.
	 * @throws RuntimeException Cuando se produce un error en la extracci&oacute;n de la clave del almac&eacute;n. */
	void show();

	/**
	 * Recupera el alias del certificado seleccionado;
	 * @return Alias de certificado.
	 */
	String getSelectedAlias();

	/**
	 * Recupera el par Clave-Certificado del certificado seleccionado. Antes debe haberse invocado al m&eacute;todo
	 * {@link KeyStoreDialogManager#show()}.
	 * @return Par clave-certificado.
	 */
	PrivateKeyEntry getSelectedPrivateKeyEntry();
	
	/**
	 * Recupera la cadena de certificaci&oacute;n del certificado seleccionado. Antes debe haberse invocado al
	 * m&eacute;todo {@link KeyStoreDialogManager#show()}. 
	 * @return Par certificado.
	 */
	Certificate[] getSelectedCertificateChain();
}

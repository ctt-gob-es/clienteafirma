package es.gob.afirma.core.ui;

import java.security.KeyStore.PrivateKeyEntry;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.AOException;
import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.core.keystores.NameCertificateBean;

/**
 * Interfaz que implementan los di&aacute;logos de selecciOacute;n de certificados.
 */
public interface KeyStoreDialogManager {

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

	/** Muestra el di&aacute;logo de selecci&oacute;n y devuelve la clave seleccionada.
	 * @throws AOCancelledOperationException Cuando no se selecciona ning&uacute;n certificado. */
	void show();

	/**
	 * Recupera el alias del certificado seleccionado;
	 * @return Alias de certificado.
	 */
	String getSelectedAlias();

	/**
	 * Recupera el par Clave-Certificado del certificado seleccionado.
	 * @return Par clave-certificado.
	 */
	PrivateKeyEntry getSelectedPrivateKeyEntry();
}

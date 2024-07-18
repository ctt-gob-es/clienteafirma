/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.ui;

import java.io.IOException;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.keystores.CertificateContext;
import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.core.keystores.NameCertificateBean;

/** Interfaz que implementan los di&aacute;logos de selecci&oacute;n de certificados. */
public interface KeyStoreDialogManager {

	// Tipos de almacenes que se gestionan desde el dialogo

	/** Identificador de almac&eacute;n del sistema. */
	int KEYSTORE_ID_SYSTEM = 1;

	/** Identificador de almac&eacute;n de mozilla. */
	int KEYSTORE_ID_MOZILLA = 2;

	/** Identificador de almac&eacute;n externo PKCS#12. */
	int KEYSTORE_ID_PKCS12 = 3;

	/** Identificador de almac&eacute;n de DNIe. */
	int KEYSTORE_ID_DNIE = 4;

	/** Identificador de almac&eacute;n externo PKCS#11. */
	int KEYSTORE_ID_PKCS11 = 5;

	/** Manda recargar al almac&eacute;n asociado actualmente al di&aacute;logo de
	 * selecci&oacute;n.
	 * @throws IOException En caso de errores de entrada / salida. */
	void refresh() throws IOException;

	/** Obtiene el listado de certificados con alias que deben mostrarse en el
	 * di&aacute;logo de selecci&oacute;n.
	 * @return Listado de certificados con alias. */
	NameCertificateBean[] getNameCertificates();

	/** Cambia el almac&eacute;n que gestiona internamente el di&aacute;logo.
	 * @param ksm Almac&eacute;n de certificados. */
	void setKeyStoreManager(KeyStoreManager ksm);

	/**
	 * Cambia el almac&eacute;n cargado en el di&aacute;logo al correspondiente al que corresponde
	 * al indicado. Los tipos de almac&eacute;n a los que se puede cambiar se declaran en esta
	 * misma interfaz.
	 * @param keyStoreId Identificador de almac&eacute;n.
	 * @param parent Componente padre sobre el que mostrar cualquier di&aacute;logo gr&aacute;fico.
	 * @return {@code true} si se completa el cambio de almac&eacute;n, {@code false} en caso
	 * contrario.
	 */
	boolean changeKeyStoreManager(int keyStoreId, Object parent);

	/**
	 * Cambia el almac&eacute;n cargado a uno de tipo PKCS#11
	 * @param parent Componente padre sobre el que mostrar cualquier di&aacute;logo gr&aacute;fico.
	 * @param ksName Nombre del almac&eacute;n de claves PKCS#11.
	 * @param ksLibPath Ruta con el controlador del almac&eacute;n de claves PKCS#11.
	 * @return {@code true} si se completa el cambio de almac&eacute;n, {@code false} en caso
	 * contrario.
	 */
	boolean changeKeyStoreManagerToPKCS11(Object parent, String ksName, String ksLibPath);

	/**
	 * Indica entre qu&eacute; tipos de almacenes se permite cambiar desde el di&aacute;logo de selecci&oacute;n.
	 * @return Listado con el identificador de tipos de almac&eacute;n.
	 */
	int[] getAvailablesKeyStores();

	/** Devuelve la clave asociada a un alias.
	 * @param alias Alias de la clave que se desea recuperar.
	 * @throws AOException Cuando no se puede extraer la clave del almac&eacute;n.
	 * @return Clave. */
	Object getKeyEntry(String alias) throws AOException;

	/** Muestra el di&aacute;logo con el listado de certificados que se ajusta a los criterios establecidos
	 * para que el usuario seleccione uno de ellos.
	 * @return Alias del certifciado seleccionado.
	 * @throws es.gob.afirma.core.AOCancelledOperationException Cuando no se selecciona ning&uacute;n certificado.
	 * @throws RuntimeException Cuando se produce un error en la extracci&oacute;n de la clave del almac&eacute;n.
	 * @throws AOException Cuando no hay certificados en el almac&eacute;n acordes a los criterios establecidos. */
	String show() throws AOException;

	/** Recupera el alias del certificado seleccionado;. Este m&eacute;todo no deber&iacute;a
	 * usarse cuando se permite la carga de almacenes externos. En su lugar, se debe utilizar {@link #getSelectedCertificateContext()}
	 * @return Alias de certificado. */
	String getSelectedAlias();

	/**
	 * Recupera el contexto del certificado seleccionado. Desde este contexto se puede
	 * obtener el almac&eacute;n al que pertenece el certificado (que puede no ser el
	 * utilizado al iniciar el di&aacute;logo, ya que el usuario puede haber cambiado
	 * de almac&eacute;n) y el alias del certificado para su extracci&oacute;n del
	 * almac&eacute;n.
	 * @return Contexto del certificado.
	 */
	CertificateContext getSelectedCertificateContext();

	/** Permite o prohibe la apertura de almacenes externos al principal desde el UI del di&aacute;logo.
	 * @param showButton <code>true</code> para mostrar el bot&oacute;n de apertura de almacenes externos,
	 *                   <code>false</code> para ocultarlo. */
	void allowOpenExternalStores(boolean showButton);

	/** Indica si la apertura de almacenes externos al principal desde el UI del di&aacute;logo est&aacute;
	 * permitida o prohibida.
	 * @return <code>true</code> si se permite la apertura de almacenes externos,
	 *         <code>false</code> en caso contrario. */
	boolean isExternalStoresOpeningAllowed();

	/** Obtiene el nombre del tipo de almac&eacute;n del que se muestran los
	 * certificados en el di&aacute;logo.
	 * @return Nombre del tipo de almac&eacute;n o {@code null} si no se conoce. */
	String getKeyStoreName();

	/** Obtiene el nombre dde la librer&iacute;a en caso de que se utilice.
	 * @return Nombre de la librer&iacute;a o {@code null} si no se conoce. */
	String getLibName();

}

/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.core.ui;

import java.awt.Component;
import java.io.IOException;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.core.keystores.NameCertificateBean;

/** Interfaz que implementan los di&aacute;logos de selecci&oacute;n de certificados. */
public interface KeyStoreDialogManager {

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
	 * Cambia el almacen cargado en el di&aacute;logo al correspondiente al que corresponde
	 * al indicado. Tipos de almac&eacute;n:
	 * <ul>
	 * <li>1: Almac&eacute;n del sistema.</li>
	 * <li>2: Almac&eacute;n de Firefox.</li>
	 * <li>3: Almac&eacute;n en fichero (PFX/PKCS#12).</li>
	 * <li>4: DNI electr&oacute;nico.</li>
	 * </ul>
	 * @param keyStoreId Identificador de almac&eacute;n.
	 * @param parent Componente padre sobre el que mostrar cualquier di&aacute;logo gr&aacute;fico.
	 * @return {@code true} si se completa el cambio de almac&eacute;n, {@code false} en caso
	 * contrario.
	 */
	boolean changeKeyStoreManager(int keyStoreId, Component parent);

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

	/** Recupera el alias del certificado seleccionado;
	 * @return Alias de certificado. */
	String getSelectedAlias();

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
	 * @return nombre del tipo de almac&eacute;n o {@code null} si no se conoce. */
	String getKeyStoreName();
}

/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.keystores;

import es.gob.afirma.misc.AOConstants.AOKeyStore;

/**
 * Agrupaci&oacute;n de los tres atributos principales de un almac&eacute;n de
 * claves: tipo, descripci&oacute;n y biblioteca PKCS#11
 */
public final class KeyStoreConfiguration {

	final private AOKeyStore type;
	final private String description;
	final private String lib;

	/**
	 * Crea una configuraci&oacute;n para un almac&eacute;n de claves.
	 * 
	 * @param t
	 *            Typo de almac&eacute;n de claves
	 * @param d
	 *            Descripci&oacute;n del almac&eacute;n de claves
	 * @param l
	 *            Biblioteca PKCS#11 correspondiente al almac&eacute;n de claves
	 *            (&uacute;nicamente en almacenes tipo PKCS#11)
	 */
	public KeyStoreConfiguration(final AOKeyStore t, String d, final String l) {
		type = t;
		if (d == null)
			d = t.getDescription();
		description = d;
		lib = l;
	}

	/**
	 * Obtiene el tipo de almac&eacute;n de claves.
	 * 
	 * @return Tipo de almac&eacute;n de claves
	 */
	public AOKeyStore getType() {
		return type;
	}

	/**
	 * Obtiene la biblioteca PKCS#11 correspondiente al almac&eacute;n de
	 * claves. Este m&eacute;todo aplica &uacute;nicamente en almacenes tipo
	 * PKCS#11
	 * 
	 * @return Biblioteca PKCS#11 correspondiente al almac&eacute;n de claves
	 */
	public String getLib() {
		return lib;
	}

	@Override
	public String toString() {
		return description;
	}
}

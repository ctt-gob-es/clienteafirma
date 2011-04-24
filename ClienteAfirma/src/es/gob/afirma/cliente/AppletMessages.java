/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */

package es.gob.afirma.cliente;

import java.util.Locale;
import java.util.ResourceBundle;

final class AppletMessages {
	private static final String BUNDLE_NAME = "es.gob.afirma.cliente.appletmessages"; //$NON-NLS-1$

	private static ResourceBundle RESOURCE_BUNDLE;
	
	static {
		try {
			RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault());
		}
		catch(final Throwable e) {
			RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);
		}
	}
	

	private AppletMessages() {}

	static String getString(final String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		} 
		catch (final Exception e) {
			return '!' + key + '!';
		}
	}

	/**
	 * Recupera el texto identificado con la clave proporcionada y sustituye la subcadenas 
	 * "%0" por el texto proporcionado.
	 * @param key Clave del texto.
	 * @param text Texto que se desea insertar.
	 * @return Recuerso textual con la subcadena sustituida.
	 */
	static String getString(final String key, final String text) {
		try {
			return RESOURCE_BUNDLE.getString(key).replace("%0", text);
		} catch (Exception e) {
			return '!' + key + '!';
		}
	}
	
	/**
	 * Recupera el texto identificado con la clave proporcionada y sustituye las subcadenas de tipo
	 * "%i" por el texto en la posici&oacute;n 'i' del array proporcionado.
	 * @param key Clave del texto.
	 * @param params Par&aacute;metros que se desean insertar.
	 * @return Recuerso textual con las subcadenas sustituidas.
	 */
	static String getString(final String key, final String[] params) {
		
		String text;
		try {
			text = RESOURCE_BUNDLE.getString(key);
		} catch (Exception e) {
			return '!' + key + '!';
		}
		
		if (params != null) {
			for (int i = 0; i < params.length; i++) {
				text = text.replace("%" + i, params[i]);
			}
		}
		
		return text;
	}
}

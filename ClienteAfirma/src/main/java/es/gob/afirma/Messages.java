/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo las licencias EUPL version 1.1 y GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma;

import java.util.Locale;
import java.util.ResourceBundle;
import java.util.logging.Logger;

/**
 * Clase para la obtencion de los recursos textuales del n&uacute;cleo del cliente de firma.
 */ 
public final class Messages {
	
	private static final String BUNDLE_NAME = "messages"; //$NON-NLS-1$

	private static ResourceBundle RESOURCE_BUNDLE;
	
	static {
		try {
			RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault());
		}
		catch(final Throwable e) {
			try {
				RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);
			} 
			catch (final Throwable e1) {
				Logger.getLogger("es.gob.afirma").severe("No ha podido cargarse el fichero de mensajes localizados: " + e1);
			}
		}
	}

	private Messages() {}

	/**
	 * Recupera el texto identificado con la clave proporcionada.
	 * @param key Clave del texto.
	 * @return Recuerso textual.
	 */
	public static String getString(final String key) {
		try {
			return RESOURCE_BUNDLE.getString(key);
		} catch (Exception e) {
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
	public static String getString(final String key, final String text) {
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
	public static String getString(final String key, final String[] params) {
		
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

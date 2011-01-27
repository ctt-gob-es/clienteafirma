/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.cliente;

import java.util.Locale;
import java.util.MissingResourceException;
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
		catch (final MissingResourceException e) {
			return '!' + key + '!';
		}
	}
	
}

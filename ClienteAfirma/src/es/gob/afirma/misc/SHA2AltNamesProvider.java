/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.misc;

import java.security.AccessController;
import java.security.Provider;
import java.util.LinkedHashMap;
import java.util.Map;

import sun.security.action.PutAllAction;

/**
 * Proveedor para huellas digitales SHA2 con variantes de nombre no contempladas en
 * el proveedor Sun.
 */
public class SHA2AltNamesProvider extends Provider {

	/** Construye un nuevo proveedor de huellas digitales SHA-2 con nombres alternativos. */
	@SuppressWarnings("unchecked")
	public SHA2AltNamesProvider() {
		super("AOSHA2AltNamesProvider", 1.0, "Proveedor para huellas digitales SHA-2 con nombres alternativos");
		
		final Map map = (System.getSecurityManager() == null) ? (Map)this : new LinkedHashMap();
		
		map.put("MessageDigest.SHA256", "sun.security.provider.SHA2");
		map.put("MessageDigest.SHA384", "sun.security.provider.SHA5$SHA384");
		map.put("MessageDigest.SHA512", "sun.security.provider.SHA5$SHA512");
		
		if (map != this) {
		    AccessController.doPrivileged(new PutAllAction(this, map));
		}
	}

	private static final long serialVersionUID = -8651981256670188852L;
	
}

/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores,
 * según las condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se distribuyera
 * este fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.deploy.util;

import java.applet.Applet;
import java.security.AccessController;
import java.security.PrivilegedAction;

/**
 * Applet que permite comprobar si la JRE de que se utiliza es de 64bits. S&oacute;lo
 * v&aacute;lido con la JRE de Sun Microsystems / Oracle.
 */
public class JREArchDetector extends Applet {
	
	private static final long serialVersionUID = -4998465076121682756L;

	/**
	 * Comprueba si la m&aacute;quina virtual utilizada es de 64bits. S&oacute;lo
	 * v&aacute;lido con la JVM de Sun Microsystems / Oracle.
	 * @return Devuelve {@code true} si la JVM es de 64bits, {@code false} en caso contrario.
	 */
	public boolean isJRE64bits() {
		return AccessController.doPrivileged(new PrivilegedAction<Boolean>() {
			public Boolean run() {
				String jreArch = System.getProperty("sun.arch.data.model");
				return jreArch != null && jreArch.equals("64");
			}
		});
	}
}

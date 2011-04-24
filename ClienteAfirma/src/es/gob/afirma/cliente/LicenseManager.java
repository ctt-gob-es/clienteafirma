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

import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;

import es.gob.afirma.misc.AOUtil;

/**
 * Clase para la carga del acuerdo de licencia.
 */
final class LicenseManager {

	private static String LICENSE_FILE = "/resources/licenses";
	
	/**
	 * recupera el texto de la licencia del Cliente @firma. Si ocurre un error lanza una
	 * excepci&oacute;n.
	 * @return Texto de la licencia.
	 * @throws IOException Cuando no se ha podido recuperar el texto de la licencia.
	 */
	public static String getLicenceText() throws IOException {

		InputStream is = 
			Class.class.getResourceAsStream(
					LICENSE_FILE + "_" + Locale.getDefault()); //$NON-NLS-1$

		if (is == null) {
			is = Class.class.getResourceAsStream(
					LICENSE_FILE + "_" + Locale.getDefault().getLanguage()); //$NON-NLS-1$
		}
		
		if (is == null) {
			is = Class.class.getClass().getResourceAsStream(LICENSE_FILE);
		}

		try {
			return new String(AOUtil.getDataFromInputStream(is), "UTF-8"); //$NON-NLS-1$
		} 
		catch (final Throwable e) {
			throw new IOException("No se ha podido recuperar el texto de la licencia:" + e);
		} 
		finally {
			try { is.close(); } catch (Exception e) {}
		}
	}
}

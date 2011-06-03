/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.utils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import es.gob.afirma.ui.principal.PrincipalGUI;

/**
 * Clase con utilidades varias
 */
public class Utils {

	/**
	 * Abre un fichero en la aplicaci\u00F3n predefinida por el sistema operativo actual.
	 * @param filepath Ruta completa al fichero.
	 */
	public static void openFile(String filepath){
		String os = System.getProperty("os.name").toLowerCase();
		Runtime rt = Runtime.getRuntime();
		try {
			if (os.indexOf( "win" ) >= 0) 
				rt.exec("cmd.exe /C \""+filepath+"\"");
			else if (os.indexOf( "mac" ) >= 0)
				rt.exec( "open " + filepath);
			else {
				//prioritized 'guess' of users' preference
				List<String> browsers = new ArrayList<String>(Arrays.asList("epiphany", "firefox", "mozilla", "konqueror",
						"netscape","opera","links","lynx"));

				StringBuffer cmd = new StringBuffer();
				for (String browser : browsers)
					cmd.append( (browsers.get(0).equals(browser)  ? "" : " || " ) + browser +" \"" + filepath + "\" ");

				rt.exec(new String[] { "sh", "-c", cmd.toString() });
			}
		} catch (IOException e) {
			e.printStackTrace();
			PrincipalGUI.setNuevoEstado(Messages.getString("Validacion.error.valide"));
		}
	}
}

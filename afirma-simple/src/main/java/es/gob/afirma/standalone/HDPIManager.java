/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.logging.Logger;

/** Obtiene el modelo del dispositivo y verifica si tiene HDPI.
 * @author Sergio Mart&iacute;nez Rico */
final class HDPIManager {

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    private static String executeWmicCommand() {

    	final Runtime rt = Runtime.getRuntime();
    	final String[] commands = { "wmic", "csproduct", "get", "name" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

		try {
			final Process proc = rt.exec(commands);
			final BufferedReader stdInput = new BufferedReader(
				new InputStreamReader(proc.getInputStream())
			);
			String s = null;
			// Obtenemos la segunda linea que ser el nombre de modelo
			// (la primera es el nombre de la columna)
			while ((s = stdInput.readLine()) != null) {
			    if(!s.equals("Name") && s.length() > 0) { //$NON-NLS-1$
			    	return s.trim();
			    }
			}
		}
		catch (final IOException e) {
			LOGGER.warning(
               "Error en la ejecucion del comando " + commands.toString() + ": " + e //$NON-NLS-1$ //$NON-NLS-2$
            );
		}
		return null;
    }


	/** Indica si el dispositivo actual tiene pantalla HIDPI.
	 * @return <code>true</code> si se est&aacute; ejecutando sobre un dispositivo HIDPI,
	 *         <code>false</code> en caso contrario. */
	public static boolean isHDPIDevice() {
		final String modelName = executeWmicCommand();
		// En caso de ser una surface con HIDPI se utiliza el Look&Feel Metal.
    	// En caso contrario se utiliza Nimbus.
		final BufferedReader br = new BufferedReader(
			new InputStreamReader(
				HDPIManager.class.getResourceAsStream("/lookandfeel/hdpi_devices") //$NON-NLS-1$
			)
		);
	    try {
	    	String line = br.readLine();
	           while (line != null) {
	       		if(line.equals(modelName)) {
	           		return true;
	           	}
	       		line = br.readLine();
	       	}
	    }
	    catch(final IOException e) {
	    	LOGGER.warning(
                  "No se ha podido abrir el fichero de dispositivos HDPI: " + e //$NON-NLS-1$
			);
	    }
    	return false;
	}

}

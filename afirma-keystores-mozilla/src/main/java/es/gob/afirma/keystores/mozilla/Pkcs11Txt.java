/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.keystores.mozilla.AOSecMod.ModuleName;
import es.gob.afirma.keystores.mozilla.shared.SharedNssUtil;

/** Analizador del fichero "pkcs11.txt" para la configuraci&oacute;n de NSS en modo SQLite.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class Pkcs11Txt {

	private static final String NAME_SEARCH_TOKEN = "name=\""; //$NON-NLS-1$

	private static final String LIBRARY_SEARCH_TOKEN = "library="; //$NON-NLS-1$

	private static final String PKCS11TXT_FILENAME = "pkcs11.txt"; //$NON-NLS-1$

	private Pkcs11Txt() {
		// No instanciable
	}

	/** Obtiene los m&oacute;dulos PKCS#11 del fichero "pkcs11.txt" del perfil <b>global</b>
	 * (de sistema) de NSS.
	 * @return Lista de m&oacute;dulos PKCS#11 configurados en el fichero "pkcs11.txt".
	 * @throws IOException Si no se puede leer o analizar el fichero "pkcs11.txt". */
	public static List<ModuleName> getModules() throws IOException {
		final File f = new File(SharedNssUtil.getSharedUserProfileDirectory() + File.separator + PKCS11TXT_FILENAME);
		if (!f.isFile()) {
			return new ArrayList<>(0);
		}
		if (!f.canRead()) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"No hay permisos de lectura para 'pkcs11.txt' en " + f.getAbsolutePath() //$NON-NLS-1$
			);
			return new ArrayList<>(0);
		}
		final List<ModuleName> ret = getModules(f);
	    return ret;
	}

	static List<ModuleName> getModules(final File f)	throws  IOException {
		try (
			Reader fr = new FileReader(f);
		) {
			return getModules(fr);
		}
	}

	static List<ModuleName> getModules(final Reader fr) throws IOException {
		final List<ModuleName> ret = new ArrayList<>();

		try (
			final BufferedReader br = new BoundedBufferedReader(
				fr,
				512, // Maximo 512 lineas
				4096 // Maximo 4KB por linea
			);
		) {
		    String line;
		    String foundLib = null;
		    String foundName = null;
		    while ((line = br.readLine()) != null) {
		    	if (line.trim().isEmpty()) {
		    		// Una linea en blanco es una nueva seccion
		    		foundLib = null;
		    		foundName = null;
		    		continue;
		    	}

		    	// La documentacion de los modulos PKCS#11 establece que este fichero se compone de propiedades
		    	// y valores que pueden estar separados por un espacio en blanco
		    	// (https://developer.mozilla.org/en-US/docs/Mozilla/Projects/NSS/PKCS11/Module_Specs), pero en
		    	// la realidad no es asi. Se realiza una valoracion intermedia en la que se soportaria que la
		    	// clave "library" no estuviese al principio de una linea y que estuviese seguido por la clave
		    	// "name" en la misma linea. No se puede interpretar que un espacio en blanco separa las claves
		    	// porque impediria establecer librerias con rutas con espacios
		    	int libPos = -1;
		    	if (line.startsWith(LIBRARY_SEARCH_TOKEN)) {
		    		libPos = LIBRARY_SEARCH_TOKEN.length();
		    	}
		    	else if (line.indexOf(" " + LIBRARY_SEARCH_TOKEN) != -1) { //$NON-NLS-1$
		    		libPos = line.indexOf(" " + LIBRARY_SEARCH_TOKEN) + //$NON-NLS-1$
		    				(" " + LIBRARY_SEARCH_TOKEN).length(); //$NON-NLS-1$
		    	}
		    	if (libPos != -1) {
		    		int namePos = line.indexOf(" " + NAME_SEARCH_TOKEN, libPos); //$NON-NLS-1$
		    		if (namePos != -1) {
		    			foundLib = line.substring(libPos, namePos).trim();
		    			namePos += (" " + NAME_SEARCH_TOKEN).length(); //$NON-NLS-1$
		    			foundName = line.substring(namePos, line.indexOf('"', namePos));
		    		}
		    		else {
		    			foundLib = line.substring(libPos).trim();
		    		}
		    	}
		    	else if (line.startsWith("name=")){ //$NON-NLS-1$
		    		foundName = line.substring("name=".length()).trim(); //$NON-NLS-1$
		    	}

		    	if (foundLib != null && foundName != null && !foundLib.isEmpty() && !foundName.isEmpty()){
		    		ret.add(new ModuleName(foundLib, foundName));
		    		foundLib = null;
		    		foundName = null;
		    	}
		    }
		}
		return ret;
	}

}

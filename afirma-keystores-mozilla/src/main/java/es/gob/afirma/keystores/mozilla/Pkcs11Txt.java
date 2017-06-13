/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
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

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.BoundedBufferedReader;
import es.gob.afirma.keystores.mozilla.AOSecMod.ModuleName;
import es.gob.afirma.keystores.mozilla.shared.SharedNssUtil;

/** Analizador del fichero "pkcs11.txt" para la configuraci&oacute;n especial de NSS compartido.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class Pkcs11Txt {

	private static final String NAME_SEARCH_TOKEN = "name=\""; //$NON-NLS-1$

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
			BufferedReader br = new BoundedBufferedReader(
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
		    	final String lib = AOUtil.getRDNvalueFromLdapName("library", line.replace(" ", ",")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		    	if (lib != null && !lib.trim().isEmpty()) {
		    		if (line.contains(NAME_SEARCH_TOKEN)) {
			    		ret.add(
		    				new ModuleName(
								lib.trim(),
								line.substring(
									   line.indexOf(NAME_SEARCH_TOKEN) + NAME_SEARCH_TOKEN.length(),
									   line.indexOf(
										   '"',
										   line.indexOf(NAME_SEARCH_TOKEN) + NAME_SEARCH_TOKEN.length()
									   )
								)
							)
						);
			    		foundLib = null;
			    		foundName = null;
		    		}
		    		else {
		    			foundLib = lib.trim();
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

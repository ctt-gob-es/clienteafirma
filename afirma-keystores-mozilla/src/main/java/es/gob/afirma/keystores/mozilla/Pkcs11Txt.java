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
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.mozilla.AOSecMod.ModuleName;

/** Analizador del fichero "pkcs11.txt" para la configuraci&oacute;n especial de NSS compartido.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
final class Pkcs11Txt {

	private static final String PKCS11TXT_PATH_UNIX = " /etc/pki/nssdb"; //$NON-NLS-1$
	private static final String PKCS11TXT_FILENAME = "pkcs11.txt"; //$NON-NLS-1$

	private Pkcs11Txt() {
		// No instanciable
	}

	static List<ModuleName> getModules() throws IOException {
		return getModules(PKCS11TXT_PATH_UNIX);
	}

	static List<ModuleName> getModules(final String path) throws IOException {
		if (path == null) {
			return new ArrayList<ModuleName>(0);
		}
		File f = new File(path + (path.endsWith(File.separator) ? "" : File.separator) + PKCS11TXT_FILENAME); //$NON-NLS-1$
		if (!f.isFile() && !Platform.OS.WINDOWS.equals(Platform.getOS())) {
			f = new File(PKCS11TXT_PATH_UNIX + "/" + PKCS11TXT_FILENAME); //$NON-NLS-1$
			if (!f.isFile()) {
				return new ArrayList<ModuleName>(0);
			}
			Logger.getLogger("es.gob.afirma").info( //$NON-NLS-1$
				"Se usara la lista gloval de modulos PKCS#11 desde " + f.getAbsolutePath() //$NON-NLS-1$
			);
		}
		if (!f.canRead()) {
			Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
				"No hay permisos de lectura para 'pkcs11.txt' en " + f.getAbsolutePath() //$NON-NLS-1$
			);
			return new ArrayList<ModuleName>(0);
		}
		final List<ModuleName> ret = new ArrayList<ModuleName>();
		final Reader fr = new FileReader(f);
		final BufferedReader br = new BufferedReader(fr);
	    String line;
	    while ((line = br.readLine()) != null) {
	    	final String lib = AOUtil.getRDNvalueFromLdapName("library", line.replace(" ", ",")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	    	if (lib != null && !lib.trim().isEmpty()) {
	    		ret.add(
    				new ModuleName(
						lib.trim(),
						lib.trim().replace(" ", "_").replace(".", "_").replace("-", "_") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
					)
				);
	    	}
	    }
	    br.close();
	    fr.close();
	    return ret;
	}

}

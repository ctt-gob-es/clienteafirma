/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.keystores.mozilla.shared;

import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;

import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilities;

/** Utilidades para la gesti&oacute;n de almacenes SNN compartidos (de sistema).
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class SharedNssUtil {

	private static final String NSSDB_PATH_UNIX_GLOBAL = "/etc/pki/nssdb"; //$NON-NLS-1$
	private static final String NSSDB_PATH_UNIX_USER = System.getProperty("user.home") + "/.pki/nssdb"; //$NON-NLS-1$ //$NON-NLS-2$

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private SharedNssUtil() {
		// No instanciable
	}

	/** Obtiene el directorio de bases de datos de NSS compartido.
	 * El programa busca siempre primero el directorio del usuario, y solo si este no existe
	 * redirecciona al del sistema.
	 * @return Directorio de bases de datos de NSS compartido (<b>no</b> contiene el prefijo <i>sql:/</i>).
	 * @throws FileNotFoundException Si no se encuentra el directorio de bases de datos de NSS compartido. */
	public static String getSharedUserProfileDirectory() throws FileNotFoundException {
		File f = new File(NSSDB_PATH_UNIX_USER);
		if (!f.isDirectory()) {
			f = new File(NSSDB_PATH_UNIX_GLOBAL);
			if (!f.isDirectory()) {
				throw new FileNotFoundException("No se ha encontrado el directorio del almacen del sistema"); //$NON-NLS-1$
			}
		}
		if (f.listFiles(
			new FileFilter() {
				@Override
				public boolean accept(final File fi) {
					final String name = fi.getName();
					return name.startsWith("key") && name.endsWith(".db"); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
		).length > 0) {
			return f.getAbsolutePath();
		}
		throw new FileNotFoundException();
	}

	/** Obtiene las rutas completas hacia las bibliotecas (.so de UNIX) de los
	 * m&oacute;dulos de seguridad externos (PKCS#11) instalados en un NSS compartido de sistema,
	 * indexados por su descripci&oacute;n dentro de un <code>ConcurrentHashMap</code>.
	 * @param excludeDnie Si se establece a <code>true</code> excluye los m&oacute;dulos PKCS#11
	 *                    del DNIe, si se establece a <code>false</code> deja estos m&oacute;dulos en
	 *                    caso de que se encontrasen.
	 * @param includeKnownModules Si se establece a <code>true</code> se incluyen m&oacute;dulos PKCS#11 que
	 *                            est&eacute;n en el directorio de bibliotecas del sistema pero no en la
	 *                            base de datos de m&oacute;dulos de Mozilla (<i>secmod.db</i>), si se
	 *                            establece a <code>false</code> se devuelven &uacute;nicamente los
	 *                            m&oacute;dulos PKCS#11 de la base de datos.
	 * @return Nombres de las bibliotecas de los m&oacute;dulos de seguridad delNSS compartido de sistema. */
	static Map<String, String> getSharedNssPKCS11Modules(final boolean excludeDnie,
			                                             final boolean includeKnownModules) {

		if (!excludeDnie) {
			LOGGER.info("Se ha solicitado incluir los modulos nativos de DNIe"); //$NON-NLS-1$
		}

		final List<es.gob.afirma.keystores.mozilla.AOSecMod.ModuleName> modules;
		try {
			modules = Pkcs11Txt.getModules();
		}
		catch (final Exception e) {
			LOGGER.severe(
				"No se han podido obtener los modulos externos de Mozilla desde 'pkcs11.txt': " + e //$NON-NLS-1$
			);
			return new ConcurrentHashMap<>(0);
		}

		return MozillaKeyStoreUtilities.getPkcs11ModulesFromModuleNames(modules, includeKnownModules, excludeDnie);
	}

}

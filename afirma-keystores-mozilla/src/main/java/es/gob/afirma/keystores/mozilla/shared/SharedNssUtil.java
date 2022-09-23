/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla.shared;

import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import es.gob.afirma.keystores.mozilla.MozillaKeyStoreUtilities;
import es.gob.afirma.keystores.mozilla.Pkcs11Txt;

/** Utilidades para la gesti&oacute;n de almacenes NSS compartidos (de sistema).
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class SharedNssUtil {

	private static final String NSSDB_PATH_UNIX_GLOBAL = "/etc/pki/nssdb"; //$NON-NLS-1$
	private static final String NSSDB_PATH_UNIX_USER = System.getProperty("user.home") + "/.pki/nssdb"; //$NON-NLS-1$ //$NON-NLS-2$
	private static final String NSSDB_PATH_UNIX_USER_CHROMIUM = System.getProperty("user.home") + "/snap/chromium/current/.pki/nssdb"; //$NON-NLS-1$ //$NON-NLS-2$

	/** Lista de rutas de posibles directorios de perfil de NSS.
	 * Deben estar en el apropiado orden de b&uacute;squeda. */
	private static final String[] NSSDB_PATHS = new String[] {
			NSSDB_PATH_UNIX_USER,
			NSSDB_PATH_UNIX_USER_CHROMIUM,
			NSSDB_PATH_UNIX_GLOBAL
	};

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private SharedNssUtil() {
		// No instanciable
	}

	private static boolean isNssProfileDirectory(final String path) {
		if (path == null) {
			return false;
		}
		final File f = new File(path);
		if (!f.isDirectory()) {
			return false;
		}
		// Es un directorio de perfil de NSS si contiene al menos un fichero de claves (key*.db)
		return f.listFiles(
			new FileFilter() {
				@Override
				public boolean accept(final File fi) {
					final String name = fi.getName();
					return name.startsWith("key") && name.endsWith(".db"); //$NON-NLS-1$ //$NON-NLS-2$
				}
			}
		).length > 0;
	}

	/** Obtiene el directorio de bases de datos de NSS compartido (que usa Google Chrome).
	 * El programa busca siempre primero el directorio del usuario, y solo si este no existe
	 * redirecciona al del sistema.
	 * @return Directorio de bases de datos de NSS compartido (<b>no</b> contiene el prefijo <i>sql:/</i>).
	 * @throws FileNotFoundException Si no se encuentra el directorio de bases de datos de NSS compartido. */
	public static String getSharedUserProfileDirectory() throws FileNotFoundException {
		for (final String path : NSSDB_PATHS) {
			if (isNssProfileDirectory(path)) {
				LOGGER.info(
					"Detectado directorio del almacen NSS de claves: " + path.replace(System.getProperty("user.home"), "\u0334")  //$NON-NLS-1$ //$NON-NLS-2$//$NON-NLS-3$
				);
				return path;
			}
		}
		throw new FileNotFoundException("No se ha encontrado un directorio de perfil de NSS compartido"); //$NON-NLS-1$
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
	 * @return Nombres de las bibliotecas de los m&oacute;dulos de seguridad del NSS compartido de sistema. */
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
			LOGGER.log(
				Level.SEVERE,
				"No se han podido obtener los modulos externos de Mozilla desde 'pkcs11.txt': " + e, //$NON-NLS-1$
				e
			);
			return new ConcurrentHashMap<>(0);
		}
		return MozillaKeyStoreUtilities.getPkcs11ModulesFromModuleNames(modules, includeKnownModules, excludeDnie);
	}

}

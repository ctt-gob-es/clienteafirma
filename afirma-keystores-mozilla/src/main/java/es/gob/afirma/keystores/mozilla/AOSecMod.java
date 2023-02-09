/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.mozilla;

import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.keystores.KeyStoreUtilities;

/** Clase para la obtenci&oacute;n de los m&oacute;dulos PKCS#11 instalados en la
 * base de datos <i>secmod.db</i> de Mozilla / Firefox. */
public final class AOSecMod {

	private static final int FIRST_ASCII_PRINTABLE_CODE = 32;
	private static final int LAST_ASCII_PRINTABLE_CODE = 126;

    /** Listado de m&oacute;dulos almacenados en el fichero "Secmod.db". */
    private static List<ModuleName> modules = null;

    private AOSecMod() {
        // No permitimos la instanciacion
    }

    /** Limpia una cadena de texto eliminando los caracteres ASCII no imprimibles que pudiese
     * tener al comienzo.
     * @param commonName Cadena de texto de entrada.
     * @return Texto con los caracteres ASCII no imprimibles que pudiese
     * tener al comienzo eliminados. */
    private static String cleanModuleName(final String commonName) {
    	if (commonName == null) {
    		return ""; //$NON-NLS-1$
    	}
    	final char[] chars = commonName.toCharArray();
    	int nameOffset = 0;
    	for (int i=0; i<chars.length; i++) {
    		if (chars[i] < FIRST_ASCII_PRINTABLE_CODE || chars[i] > LAST_ASCII_PRINTABLE_CODE) {
    			nameOffset = i + 1;
    		}
    	}
    	return commonName.substring(nameOffset);
    }

    /** Obtiene un m&oacute;dulo de seguridad (PKCS#11) de la base de datos de Mozilla.
     * Cada registro de la base de datos tiene esta estructura:
     * <pre>
     * struct {
     *   BYTE  commonNameLen[ 2 ];
     *   BYTE  commonName   [ commonNameLen ];
     *   BTTE  libNameLen   [ 2 ];
     *   BYTE  libName      [ libNameLen ];
     * Si es un registro "extendido" tambien tendra estas entradas:
     *   BYTE  initStringLen[ 2 ];
     *   BYTE  initString   [ initStringLen ];
     * }
     * </pre>
     * @param secmoddb Base de datos de m&oacute;dulos de seguridad de Mozilla
     * @param namesOffset Punto de partida en la base de datos de m&oacute;dulos de seguridad
     * @return Primer m&oacute;dulo de seguridad encontrado en la base de datos a partir del punto
     *         de partida indicado */
    private static ModuleName processNames(final byte[] secmoddb, final int namesOffset) {

        int namesRunningOffset = namesOffset;

        int len = getShort(secmoddb, namesRunningOffset + 0);
        final String commonName = cleanModuleName(
    		new String(secmoddb, namesRunningOffset + 2, len)
		);

        namesRunningOffset += len + 2;

        len = getShort(secmoddb, namesRunningOffset);
        final String libName = new String(secmoddb, namesRunningOffset + 2, len);

        if (isWindowsLib(libName) || isUnixLib(libName)) {
            final String trueLibName = KeyStoreUtilities.searchPathForFile(
        		new String[] {
    				libName
        		}
    		);
            if (trueLibName != null) {
                return new ModuleName(trueLibName, commonName);
            }
        }

        throw new UnsupportedOperationException("Intento fallido: " + libName); //$NON-NLS-1$

    }

    private static boolean isUnixLib(final String libName) {
    	if (libName == null) {
    		return false;
    	}
    	return !Platform.OS.WINDOWS.equals(Platform.getOS()) && (libName.endsWith(".so") || libName.contains(".so.") || libName.endsWith(".dylib")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }

    private static boolean isWindowsLib(final String libName) {
    	if (libName == null) {
    		return false;
    	}
    	return Platform.OS.WINDOWS.equals(Platform.getOS()) && (libName.endsWith(".DLL") || libName.endsWith(".dll"));  //$NON-NLS-1$//$NON-NLS-2$
    }

    /** Obtiene los m&oacute;dulos de seguridad PKCS#11 instalados en la base de
     * datos <i>secmod.db</i>.
     * @param dir
     *        Directorio del perfil del usuario activo de Mozilla / Firefox
     * @return Vector con los m&oacute;dulos encontrados, el vector
     *         estar&aacute; vac&iacute;o si se encuentra alg&uacute;n problema
     *         durante la b&acute;squeda
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso */
    static List<ModuleName> getModules(final String dir) throws AOException {

        if (dir == null || dir.isEmpty()) {
            throw new IllegalArgumentException("El directorio del perfil de Mozilla no puede ser nulo"); //$NON-NLS-1$
        }

        String profileDir = dir;

        if (modules == null) {

            profileDir = profileDir.replace("\\ ", " "); //$NON-NLS-1$ //$NON-NLS-2$
            if (!profileDir.endsWith("/")) { //$NON-NLS-1$
                profileDir = profileDir + "/"; //$NON-NLS-1$
            }
            final File secmod = new File(profileDir, "secmod.db"); //$NON-NLS-1$
            if (!secmod.exists()) {
                throw new AOException("El directorio del perfil de Mozilla proporcionado no contiene una base de datos de modulos (secmod.db)"); //$NON-NLS-1$
            }
            final byte[] secMod;
            try (
        		final InputStream is = AOUtil.loadFile(AOUtil.createURI(secmod.getAbsolutePath()))
    		) {
                secMod = AOUtil.getDataFromInputStream(is);
            }
            catch (final Exception e) {
                throw new AOException("Error leyendo la base de datos de modulos (secmod.db)", e); //$NON-NLS-1$
            }

            // Obtenemos los modulos PKCS#11 asegurandonos de que no aparecen
            // mas de una vez
            modules = new ArrayList<>();
            final Set<String> libs = new HashSet<>();
            for (int i = 0; i < secMod.length; i++) {
                try {
                    final ModuleName module = processNames(secMod, i);
                    if (!libs.contains(module.getLib())) {
                        libs.add(module.getLib());
                        modules.add(module);
                    }
                }
                // Caso normal de error al identificar nombres
                catch (final IndexOutOfBoundsException e) {
                    continue;
				}
                // Caso normal de error al no detectar un nombre
                catch (final UnsupportedOperationException e) {
                    continue;
				}
                catch (final Exception e) {
                	Logger.getLogger("es.gob.afirma").warning( //$NON-NLS-1$
            			"Se omite un modulo PKCS#11 de Mozilla: " + e //$NON-NLS-1$
        			);
                    continue;
                }
            }
        }
        return modules != null ? new ArrayList<>(modules) : null;
    }

    /** M&oacute;dulo de seguridad (PKCS#11) de NSS. */
    public static final class ModuleName {

        private final String lib;
        private final String description;

        /** Construye un m&oacute;dulo de seguridad (PKCS#11) de NSS.
         * @param l Bibliotecas PKCS#11 (nombre incluyendo ruta completa).
         * @param d Descripci&oacute;n del m&oacute;dulo PKCS#11. */
        public ModuleName(final String l, final String d) {
            this.lib = l;
            this.description = d;
        }

        /** Obtiene el nombre de la biblioteca PKCS#11 del m&oacute;dulo.
         * @return Nombre de la biblioteca (con la ruta absoluta ioncluida) del
         *         m&oacute;dulo */
        String getLib() {
            return this.lib;
        }

        /** Obtiene la descripci&oacute;n (nombre com&uacute;n) del
         * m&oacute;dulo.
         * @return Descripci&oacute;n del m&oacute;dulo */
        String getDescription() {
            return this.description;
        }

        @Override
        public String toString() {
            return this.description + " (EXTERNAL, " + this.lib + ", slot 0)"; //$NON-NLS-1$ //$NON-NLS-2$
        }
    }

    /** Obtiene un n&uacute;mero de 16 bits a partir de dos posiciones de un
     * array de octetos.
     * @param src
     *        Array de octetos origen
     * @param offset
     *        Desplazamiento desde el origen para el comienzo del par de
     *        octetos
     * @return N&uacute;mero entero de 16 bits (sin signo) */
    private static int getShort(final byte[] src, final int offset) {
        return src[offset + 0] << 8 | src[offset + 1];
    }

}

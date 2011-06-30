/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.keystores;

import java.io.File;
import java.util.HashSet;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.Vector;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.Platform;

/** Clase para la obtenci&oacute;n de los m&oacute;dulos PKCS#11 instalados en la
 * base de datos <i>secmod.db</i> de Mozilla / Firefox. */

final class AOSecMod {

    /** Listado de m&oacute;dulos almacenados en el fichero "Secmod.db". */
    private static Vector<ModuleName> modules = null;

    private AOSecMod() {}

    /** <pre>
     * struct {
     *   BYTE  commonNameLen[ 2 ];
     *   BYTE  commonName   [ commonNameLen ];
     *   BTTE  libNameLen   [ 2 ];
     *   BYTE  libName      [ libNameLen ];
     * If it is "extended" it also has these members:
     *   BYTE  initStringLen[ 2 ];
     *   BYTE  initString   [ initStringLen ];
     * }
     * </pre> */
    private static ModuleName processNames(final byte[] secmoddb, final int namesOffset) {

        int namesRunningOffset = namesOffset;

        int len = getShort(secmoddb, namesRunningOffset + 0);
        String commonName = new String(secmoddb, namesRunningOffset + 2, len);

        namesRunningOffset += len + 2;

        len = getShort(secmoddb, namesRunningOffset);
        String libName = new String(secmoddb, namesRunningOffset + 2, len);

        if (libName.endsWith(".DLL") || libName.endsWith(".dll") || libName.endsWith(".so") || libName.endsWith(".dylib")) {

            // namesRunningOffset += len + 2;

            String trueLibName = searchPathForFile(new String[] {
                libName
            }, false);

            if (trueLibName != null) {
                return new ModuleName(trueLibName, commonName);
            }
        }

        throw new NullPointerException("Intento fallido: " + libName);

    }

    /** Obtiene los m&oacute;dulos de seguridad PKCS#11 instalados en la base de
     * datos <i>secmod.db</i>.
     * @param profileDir
     *        Directorio del perfil del usuario activo de Mozilla / Firefox
     * @return Vector con los m&oacute;dulos encontrados, el vector
     *         estar&aacute; vac&iacute;o si se encuentra alg&uacute;n problema
     *         durante la b&acute;squeda
     * @throws AOException
     *         Cuando ocurre cualquier problema durante el proceso */
    static Vector<ModuleName> getModules(String profileDir) throws AOException {
        if (profileDir == null || "".equals(profileDir)) {
            throw new AOException("El directorio del perfil de Mozilla era nulo");
        }

        if (modules == null) {

            profileDir = profileDir.replace("\\ ", " ");
            if (!profileDir.endsWith("/")) profileDir = profileDir + "/";
            final File secmod = new File(profileDir + "secmod.db");
            if (!secmod.exists()) {
                throw new AOException("El directorio del perfil de Mozilla proporcionado no contiene una base de datos de modulos (secmod.db)");
            }
            final byte[] secMod;
            try {
                secMod = AOUtil.getDataFromInputStream(AOUtil.loadFile(AOUtil.createURI(secmod.getAbsolutePath()), null, false));
            }
            catch (Exception e) {
                throw new AOException("Error leyendo la base de datos de modulos (secmod.db)");
            }

            // Obtenemos los modulos PKCS#11 asegurandonos de que no aparecen
            // mas de una vez
            modules = new Vector<ModuleName>();
            final Set<String> libs = new HashSet<String>();
            for (int i = 0; i < secMod.length; i++) {
                try {
                    ModuleName module = processNames(secMod, i);
                    if (!libs.contains(module.getLib())) {
                        libs.add(module.getLib());
                        modules.add(module);
                        // Logger.getLogger("es.gob.afirma").info("La busqueda manual sobre Mozilla secmod.db ha encontrado el siguiente modulo: "
                        // + module);
                    }
                }
                catch (Exception e) {
                    continue;
                }
            }
        }

        return modules;
    }

    /** M&oacute;dulo de seguridad (PKCS#11) de Mozilla / Firefox. */
    static class ModuleName {

        private String lib;
        private String description;

        ModuleName(String l, String d) {
            lib = l;
            description = d;
        }

        /** Obtiene el nombre de la biblioteca PKCS#11 del m&oacute;dulo.
         * @return Nombre de la biblioteca (con la ruta absoluta ioncluida) del
         *         m&oacute;dulo */
        String getLib() {
            return lib;
        }

        /** Obtiene la descripci&oacute;n (nombre com&uacute;n) del
         * m&oacute;dulo.
         * @return Descripci&oacute;n del m&oacute;dulo */
        String getDescription() {
            return description;
        }

        @Override
        public String toString() {
            // commonName + " (" + type + ", " + libraryName + ", slot " + slot
            // + ")";
            return description + " (EXTERNAL, " + lib + ", slot 0)";
        }
    }

    /** Obtiene un n&uacute;mero de 16 bits a partir de dos posiciones de un
     * array de octetos.
     * @param src
     *        Array de octetos origen
     * @param offset
     *        Desplazamiento desde el origen para el comienzo del par de
     *        octetos
     * @return N&ueacute;mero entero de 16 bits (sin signo) */
    private final static int getShort(final byte[] src, final int offset) {
        return (((src)[offset + 0] << 8) | (src)[offset + 1]);
    }

    /** Busca un fichero (o una serie de ficheros) en el PATH del sistema. Deja
     * de buscar en la primera ocurrencia
     * @param files
     *        Ficheros a buscar en el PATH
     * @param excludeAFirma
     *        Excluye el directorio de instalaci&oacute;n de AFirma de la
     *        b&uacute;squeda
     * @return Ruta completa del fichero encontrado en el PATH o <code>null</code> si no se encontr&oacute; nada */
    private final static String searchPathForFile(final String[] files, final boolean excludeAFirma) {
        if (files == null || files.length < 1) return null;

        // Si existe el primero con el PATH completo lo devolvemos sin mas
        if (new File(files[0]).exists()) return files[0];

        final StringTokenizer st = new StringTokenizer(Platform.getJavaLibraryPath(), File.pathSeparator);
        String libPath;
        while (st.hasMoreTokens()) {
            libPath = st.nextToken();
            if (!libPath.endsWith(File.separator)) libPath = libPath + File.separator;
            File tmpFile;
            for (String f : files) {
                tmpFile = new File(libPath + f);
                if (tmpFile.exists() && (!tmpFile.isDirectory())) return libPath + f;
            }
        }
        return null;
    }

}

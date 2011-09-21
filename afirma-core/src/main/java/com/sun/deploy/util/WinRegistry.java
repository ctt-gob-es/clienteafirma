/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package com.sun.deploy.util;

/** Clase para acceder al registro de Windows. Inspirada en la clase WinRegistry
 * de Sun Microsystems. */
public final class WinRegistry {

    static {
        initIDs();
    }

    private static final int KEY_READ = 0x20019;
    private static final int KEY_WRITE = 0x20006;

    private static final int REG_SZ = 0x1;
    private static final int REG_DWORD = 0x4;

    // Funciones de la biblioteca nativa para el acceso al registro de Windows

    private static native int sysOpenKey(int hKey, String subKey, int sam);

    private static native int sysCloseKey(int hKey);

    private static native KeyValue sysQueryKey(int hKey, String name);

    private static native int sysCreateKey(int hKey, String name, int sam);

    private static native boolean sysSetStringValue(int hKey, String name, String value);

    private static native void initIDs();

    /** Los valores del registro se componen de un tipo y de unos datos. */
    static class KeyValue {
        private final int type;
        private final byte[] data;

        KeyValue(final int type, final byte[] data) {
            this.type = type;
            this.data = data.clone();
        }

        /** Retorna el valor del par-valor
         * @return Valor de la clave */
        public byte[] getData() {
            return this.data.clone();
        }

        /** Retorna el valor tras realizar el casting al objeto java adecuado, ya
         * sea este un entero, una cadena o en su defecto un array de bytes.
         * @return Valor del objeto */
        public Object getValue() {
            switch (this.type) {
                case REG_SZ:
                    if (this.data.length <= 0) {
                        return null;
                    }
                    return new String(this.data, 0, this.data.length - 1);

                case REG_DWORD: {
                    int n = 0;
                    for (int i = 0; (i < 4) && (i < this.data.length); i++) {
                        n += this.data[i] << (i * 8);
                    }
                    return Integer.valueOf(n);
                }

                default:
                    return getData();
            }
        }
    }

    /** Realiza una b&uacute;squeda en el registro.
     * @param hKey
     *        Entero con la clave a buscar
     * @param path
     *        Ruta de inicio de la b&uacute;squeda
     * @param name
     *        Nombre de clave a buscar
     * @return Valor de la clave buscada o <code>null</code> si no se ha
     *         encontrado */
    public static Object get(final int hKey, final String path, final String name) {
        Object value = null;
        final int key = sysOpenKey(hKey, path, KEY_READ);
        if (key != 0) {
            final KeyValue keyValue = sysQueryKey(key, name);
            sysCloseKey(key);
            if (keyValue != null) {
                value = keyValue.getValue();
            }
        }
        return value;
    }

    /** Realiza la b&uacute;squeda de una clave en el registro, si el valor no es
     * de tipo cadena retorna un valor nulo.
     * @param hKey
     *        Tipo de clave a buscar
     * @param path
     *        Ruta de inicio de la b&uacute;squeda
     * @param name
     *        Nombre de la clave a buscar
     * @return Valor la de clave encontrada o <code>null</code> si no se
     *         encontr&oacute; */
    public static String getString(final int hKey, final String path, final String name) {
        final Object rv = get(hKey, path, name);
        return (rv instanceof String) ? (String) rv : null;
    }

    /** A&ntilde;ade una cadena como clave del registro, para lo cual se
     * especifica una ruta, un nombre, un valor y una clave.
     * @param hKey
     *        Tipo de dato a a&ntilde;adir
     * @param path
     *        Ruta donde a&ntilde;adir la clave
     * @param name
     *        Nombre de la clave a a&ntilde;adir
     * @param value
     *        Valor de la clave a a&ntilde;adir
     * @return <code>true</code> si se ha posido completar adecuadamente la
     *         operaci&oacute;n, <code>false</code> en caso contrario */
    public static boolean setStringValue(final int hKey, final String path, final String name, final String value) {
        boolean returnValue = false;
        final int key = sysCreateKey(hKey, path, KEY_WRITE);
        if (key != 0) {
            returnValue = sysSetStringValue(key, name, value);
            sysCloseKey(key);
        }
        return returnValue;
    }

}

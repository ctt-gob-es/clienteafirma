package com.sun.deploy.util;

/** Clase para acceder al registro de Windows. Inspirada en la clase WinRegistry de
 * Sun Microsystems. */
public final class WinRegistry {

    static {
        initIDs();
    }

    private WinRegistry() {}
    
    private static final int KEY_READ = 0x20019;

    private static final int REG_SZ = 0x1;
    private static final int REG_DWORD = 0x4;

    // Funciones de la biblioteca nativa para el acceso al registro de Windows

    static native int sysOpenKey(int hKey, String subKey, int sam);

    static native int sysCloseKey(int hKey);

    static native KeyValue sysQueryKey(int hKey, String name);

    static native int sysCreateKey(int hKey, String name, int sam);

    static native boolean sysSetStringValue(int hKey, String name, String value);

    static native void initIDs();

    /** Los valores del registro se componen de un tipo y de unos datos. */
    private final static class KeyValue {
        private final int type;
        private final byte[] data;

        private KeyValue(final int type, final byte[] data) {
            this.type = type;
            this.data = data.clone();
        }

        /** Retorna el valor del par-valor
         * @return Valor como array de bytes del par-valor */
        private byte[] getData() {
            return this.data;
        }

        /** Retorna el valor tras realizar el casting al objeto java adecuado, ya sea este un entero,
         * una cadena o en su defecto un array de bytes.
         * @return Valor actual */
        private Object getValue() {
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

    /** Realiza la b&uacute;squeda en el registro.
     * @param hKey Entero con la clave
     * @param path Ruta
     * @param name Nombre de clave a buscar
     * @return Valor de la clave del registro */
    private static Object get(final int hKey, final String path, final String name) {
        final int key = sysOpenKey(hKey, path, KEY_READ);
        if (key != 0) {
            final KeyValue keyValue = sysQueryKey(key, name);
            sysCloseKey(key);
            if (keyValue != null) {
                return keyValue.getValue();
            }
        }
        return null;
    }

    /** Realiza la b&uacute;squeda de una clave en el registro, si el valor no es de tipo cadena retorna
     * un valor nulo.
     * @param hKey Clave a buscar
     * @param path Ruta de la clave
     * @param name Nombre de la clave
     * @return Valor como cadena de texto de la clave */
    public static String getString(final int hKey, final String path, final String name) {
        final Object rv = get(hKey, path, name);
        return (rv instanceof String) ? (String) rv : null;
    }

}

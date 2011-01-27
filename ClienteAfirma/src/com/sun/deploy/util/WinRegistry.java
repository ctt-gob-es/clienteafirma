/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package com.sun.deploy.util;

/**
 * Clase para acceder al registro de Windows. Inspirada en la clase WinRegistry de
 * Sun Microsystems.
 */
public final class WinRegistry {

	static {
		initIDs();
	}
	
	private static final int KEY_READ = 0x20019;
	private static final int KEY_WRITE = 0x20006;

	private static final int REG_SZ = 0x1;
	private static final int REG_DWORD = 0x4;

	// Funciones de la biblioteca nativa para el acceso al registro de Windows
	
	static native int sysOpenKey(int hKey, String subKey, int sam);
	static native int sysCloseKey(int hKey);
	static native KeyValue sysQueryKey(int hKey, String name);
	static native int sysCreateKey(int hKey, String name, int sam);
	static native boolean sysSetStringValue(int hKey, String name, String value);
  static native void initIDs();

  
	/**
	 * Los valores del registro se componen de un tipo y de unos datos. 
	 */
	static class KeyValue {
		private final int type;
		private final byte[] data;

		KeyValue(int type, byte[] data) {
			this.type = type;
			this.data = data;
		}
		
		/**
		 * Retorna el tipo del valor del par-valor
		 */
		public int getType() {
			return type;
		}

		/**
		 * Retorna el valor del par-valor
		 */
		public byte[] getData() {
			return data;
		}

		/**
		 * Retorna el valor tras realizar el casting al objeto java adecuado, ya sea este un entero,
		 * una cadena o en su defecto un array de bytes. 
		 * @return
		 */
		public Object getValue() {
			switch (type) {
			case REG_SZ:
				if (data.length <= 0) return null;
				return new String(data, 0, data.length - 1);

			case REG_DWORD:
			{
				int n = 0;
				for(int i = 0; (i < 4) && (i < data.length); i++) {
					n += data[i] << (i * 8);
				}
				return new Integer(n);
			}

			default:
				return getData();
			}
		}
	}
	
	/**
	 * Realiza la b&uacute;squeda en el registro.
	 * @param hKey entero con la clave 
	 * @param path ruta
	 * @param name nombre de clave a buscar
	 * @return
	 */
	public static Object get(int hKey, String path, String name) {
		Object value = null;
		int key = sysOpenKey(hKey, path, KEY_READ);
		if (key != 0) {
			KeyValue keyValue = sysQueryKey(key, name);
			sysCloseKey(key);
			if (keyValue != null) {
				value = keyValue.getValue();
			}
		}
		
		return value;
	}

	/**
	 * Realiza la b&uacute;squeda de una clave en el registro, si el valor no es de tipo cadena retorna
	 * un valor nulo.
	 */
	public static String getString(int hKey, String path, String name) {
		Object rv = get(hKey, path, name);
		return (rv instanceof String) ? (String)rv : null;
	}

	/**
	 * A&ntilde;ade una cadena como valor del registro, para lo cual se especifica una ruta, un nombre,
	 * un valor y una clave.
	 * @return
	 */
	public static boolean setStringValue(int hKey, String path, String name, String value) {
		boolean returnValue = false;
		int key = sysCreateKey(hKey, path, KEY_WRITE);
		if (key != 0) {
			returnValue = sysSetStringValue(key, name, value);
			sysCloseKey(key);
		}
		
		return returnValue;
	}


}

/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.ciphers;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.security.Key;
import java.security.KeyStore;
import java.util.Enumeration;
import java.util.Vector;
import java.util.logging.Logger;

import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOUtil;

/**
 * Utilidades para el manejo de claves de cifrado en el almac&eacute;n privado de AFirma.
 */
public final class AOCipherKeyStoreHelper {

	/** Almac&eacute;n de claves de cifrado. */
    private KeyStore ks;
    
    /** Interfaz para la inserci&oacute;n de la contrase&ntilde;a del almac&eacute;n. */
    private char[] pss;
    
	/**
	 * Almacena una clave en el almac&eacute;n privado de AFirma.
	 * @param alias Alias con el que se almacenar&aacute; la clave 
	 * @param key Clave a almacenar
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso de almacenado
	 */
	public void storeKey(String alias, Key key) throws AOException {
		if (ks == null) {
			throw new AOException("No se puede almacenar una clave en un almacen no inicializado");
		}
		try {
			ks.setKeyEntry(alias, key, pss, null);
		}
		catch(Throwable e) {
			e.printStackTrace();
			throw new AOException("Error almacenando la clave en el almacen: " + e);
		}
		try {
			ks.store(new BufferedOutputStream(new FileOutputStream(new File(AOUtil.getCipherKeystore()))), pss);
		}
		catch(Throwable e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Obtiene los alias de todas las claves del almac&eacute;n privado de AFIrma.
	 * @return Alias de todas las claves del almac&eacute;n
	 */
	@SuppressWarnings("unchecked")
	public String[] getAliases() {
		final Enumeration aliases;
		try {
			aliases = ks.aliases();
		}
		catch(Throwable e) {
			Logger.getLogger("es.gob.afirma").severe(
				"Ocurrio un error obteniendo los alias del almacen, se devolvera una lista vacia: " + e 
			);
			return new String[0];
		}
		Vector<String> tmpRet = new Vector<String>();
		while (aliases.hasMoreElements()) tmpRet.add(aliases.nextElement().toString());
		return tmpRet.toArray(new String[0]);
	}
	
	private void createCipherKeyStore() throws AOException {
		if (ks == null) {
			try {
				ks = KeyStore.getInstance("JCEKS");
			}
			catch(Throwable e) {
				e.printStackTrace();
				throw new AOException("Error obteniendo una instancia de KeyStore JCE: " + e);
			}
		}
		if (new File(AOUtil.getCipherKeystore()).exists()) {
			Logger.getLogger("es.gob.afirma").warning(
				"Se ha pedido crear un almacen de claves, pero ya existia (" + 
				AOUtil.getCipherKeystore() + 
				"), se borrara el existente y se creara uno nuevo"
			);
			new File(AOUtil.getCipherKeystore()).delete();
		}
		try {
			ks.load(null, pss);
		}
		catch(Throwable e) {
			e.printStackTrace();
			throw new AOException("Error creando un KeyStore vacio: " + e);
		}
		try {
			ks.store(new FileOutputStream(new File(AOUtil.getCipherKeystore())), pss);
		} 
		catch (Throwable e) {
			e.printStackTrace();
			throw new AOException("Error guardando en disco el KeyStore vacio: " + e);
		}
		if (!new File(AOUtil.getCipherKeystore()).exists()) {
			throw new AOException(
				"Se creo el KeyStore sin errores, pero este no aparece en el disco"
			);
		}		
	}
	
	/**
	 * Carga el almac&eacute;n privado de claves de cifrado de AFirma.
	 * @throws AOException Cuando ocurre cualquier problema durante la carga
	 */
	public void loadCipherKeyStore() throws AOException {
		if (ks == null) {
			try {
				ks = KeyStore.getInstance("JCEKS");
			}
			catch(Throwable e) {
			    throw new AOException("Error al instalanciar un almacen de claves", e);
			}
		}
		if (!new File(AOUtil.getCipherKeystore()).exists()) {
			Logger.getLogger("es.gob.afirma").warning("El almacen no existe, se creara uno nuevo");
			createCipherKeyStore();
		}
		try {
			ks.load(new BufferedInputStream(new FileInputStream(new File(AOUtil.getCipherKeystore()))), pss);
		}
		catch(Throwable e) {
			throw new AOException("Error al cargar el almacen de claves de cifrado", e);
		}
	}
	
	
	/**
	 * Obtiene una clave del almac&eacute;n.
	 * @param alias Alias de la clave solicitada
	 * @return Clave de cifrado del alam&eacute;n privado de claves de cifrado de AFirma cuyo alias coincide
	 *         con el proporcionado
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso
	 */
	public Key getKey(String alias) throws AOException {
		try {
			return ks.getKey(alias, pss);
		}
		catch(Throwable e) {
			throw new AOException("Error recuperando la contrasena con alias '" + alias + "': " + e);
		}	
	}
	
	/**
	 * Crea un <code>AOCipherKeyStoreHelper</code>.
	 * @param p Contrase&ntilde;a del almac&eacute;n de claves
	 * @throws AOException Cuando ocurre cualquier problema durante el proceso
	 */
	public AOCipherKeyStoreHelper(char[] p) throws AOException {
		if (p == null) throw new NullPointerException("Se necesita una contrasena para instanciar la clase");
		pss = p;
		loadCipherKeyStore();
	}
	
	/**
	 * Indica si el almac&eacute;n privado de claves de cifrado de AFirma ha sido ya creado.
	 * @return <code>true</code> si el almac&eacute;n ya existe, <code>false</code> en caso contrario
	 */
	public static boolean storeExists() {
		File keystore = new File(AOUtil.getCipherKeystore());
		return keystore.exists() && keystore.isFile();
	}
	
	/**
	 * Elimina el almac&eacute;n de claves del usuario. No se realiza ninguna consulta de
	 * verificaci&oacute;n de la orden. 
	 * @return Devuelve <code>true</code> si el keystore exist&iacute;a y se borr&oacute;
	 * correctamente. 
	 */
	public static boolean removeStore() {
	    File storeFile = new File(AOUtil.getCipherKeystore());
	    return storeFile.exists() && storeFile.delete();
	}
}

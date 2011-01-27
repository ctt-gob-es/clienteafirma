/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.keystores;

import java.io.ByteArrayInputStream;
import java.security.KeyStore;
import java.security.Security;
import java.security.cert.Certificate;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import sun.security.pkcs11.SunPKCS11;
import es.gob.afirma.callbacks.UIPasswordCallback;
import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOUtil;

/**
 * Representa a un <i>AOKeyStoreManager</i> para acceso a almacenes de claves de Mozilla / Firefox
 * accedidos v&iacute;a NSS en el que se tratan de forma unificada los m&oacute;dulos internos y
 * externos.
 */
public final class MozillaUnifiedKeyStoreManager extends AOKeyStoreManager {
	
	private Hashtable<String, KeyStore> storesByAlias;
	
	private Vector<KeyStore> kss = new Vector<KeyStore>();
	
	/**
	 * PasswordCallback establecido de forma externa para el acceso al almac&eacute;n.
	 */
	private PasswordCallback externallPasswordCallback = null;
	
	/**
	 * Inicializa la clase gestora de almacenes de claves.
	 * @return Almac&eacute;n de claves de Mozilla / Firefox correspondiente &uacute;nicamente el m&oacute;dulo
	 *         interno principal
	 */
	public Vector<KeyStore> init() {
		
		// Por si el proveedor estubiese ya instalado por una ejecucion anterior intentamos obtenerlo directamente
		nssProvider = Security.getProvider("SunPKCS11-NSSCrypto-AFirma");
		try {
			if (nssProvider == null) {
				Logger.getLogger("es.gob.afirma").info("Inicializando almacen unificado de Mozilla Firefox (NSS + modulos PKCS#11)");
								
				String nssDirectory = KeyStoreUtilities.getSystemNSSLibDir();
				String p11NSSConfigFile = KeyStoreUtilities.createPKCS11NSSConfigFile(
						AOUtil.getMozillaUserProfileDirectory(), 
						nssDirectory
				);
				
				// Cargamos las dependencias necesarias para la correcta carga del almacen
				KeyStoreUtilities.loadNSSDependencies(nssDirectory);
				
				Logger.getLogger("es.gob.afimra").info("PKCS11 NSS configuration:\n" + p11NSSConfigFile);
				nssProvider = new SunPKCS11(new ByteArrayInputStream(p11NSSConfigFile.getBytes()));
				Security.addProvider(nssProvider);
				Logger.getLogger("es.gob.afirma").info("Proveedor PKCS#11 para Mozilla/Firefox anadido");
			}
		}
		catch(Throwable e) {
			Logger.getLogger("es.gob.afirma").severe(
				"Ocurrio un error inicializando NSS, se continuara con los almacenes externos de Mozilla, pero los certificados del almacen interno no estaran disponibles: " + e
			);
			e.printStackTrace();
		}
		
		Enumeration<String> tmpAlias = new Vector<String>(0).elements();
		storesByAlias = new Hashtable<String, KeyStore>();
		
		KeyStore ks = null;
		
		if (nssProvider!=null) {
			try {
				ks = KeyStore.getInstance(AOConstants.AOKeyStore.MOZILLA.getName(), nssProvider);
			} 
			catch (Throwable e) {
				//e.printStackTrace();
				Logger.getLogger("es.gob.afirma").warning(
					"No se ha podido obtener el KeyStore de nombre '" + AOConstants.AOKeyStore.MOZILLA.getName() +
					"' del Provider 'sun.security.pkcs11.SunPKCS11', se continuara con los almacenes externos: " + e
				);
				ks = null;
			}
		}
		if (ks !=null) {
		    try {
		        ks.load(
                    null,
                    new char[0]
                );
		    } 
		    catch (Throwable e) {
		        try {
	                ks.load(
	                    null,
	                    (externallPasswordCallback != null ?
	                    		externallPasswordCallback.getPassword()	:
	                    		new UIPasswordCallback("Contrase\u00F1a del almac\u00E9n de Mozilla.", null).getPassword())
	                );
	            }
	            catch (AOCancelledOperationException e1) {
	                ks = null;
	                throw e1;
	            }
	            catch(Throwable e2) {
	                //e.printStackTrace();
	                Logger.getLogger("es.gob.afirma").warning(
	                    "No se ha podido inicializar el KeyStore de nombre '" + AOConstants.AOKeyStore.MOZILLA.getName() +
	                    "' del Provider 'sun.security.pkcs11.SunPKCS11', se continuara con los almacenes externos: " + e2
	                );
	                ks = null;
	            }
            }
			
			if (ks != null) {
				try {
					tmpAlias = ks.aliases();
				}
				catch(Throwable e) {
					//e.printStackTrace();
					Logger.getLogger("es.gob.afirma").warning(
						"El almacen interno de Mozilla no devolvio certificados, se continuara con los externos: " + e
					);
					ks = null;
				}
				while(tmpAlias.hasMoreElements()) {
					storesByAlias.put(tmpAlias.nextElement().toString(), ks);
				}
			}
		}
		
		kss.add(ks);
		
		// Vamos ahora con los almacenes externos
		Hashtable<String, String> externalStores = KeyStoreUtilities.getMozillaPKCS11Modules();
		
		if (externalStores.size() > 0) {
    		StringBuilder logStr = new StringBuilder("Encontrados los siguientes modulos PKCS#11 externos instalados en Mozilla / Firefox: ");
    		for (String key : externalStores.keySet()) {
    		    logStr.append("'");
    		    logStr.append(externalStores.get(key));
    		    logStr.append("' ");
    		}
    		Logger.getLogger("es.gob.afirma").info(logStr.toString());
		}
		else Logger.getLogger("es.gob.afirma").info("No se han encontrado modulos PKCS#11 externos instalados en Mozilla / Firefox"); 
				
		KeyStore tmpStore = null;
		Object descr;
		for (Enumeration<String> e = externalStores.keys() ; e.hasMoreElements() ;) {
			descr = e.nextElement();
			try {
				tmpStore = new AOKeyStoreManager().init(
						AOConstants.AOKeyStore.PKCS11, 
						null,
						new UIPasswordCallback("Introduzca la contrase\u00F1a de " + KeyStoreUtilities.getMozModuleName(descr.toString()), null), 
						new String[] { externalStores.get(descr), descr.toString() }
				).get(0);
			}
			catch(AOCancelledOperationException ex) {
				Logger.getLogger("es.gob.afirma").warning(
						"Se cancelo el acceso al almacen externo  '" + descr + "', se continuara con el siguiente: " + ex
				);
				continue;
			}
			catch (Throwable ex) {
				Logger.getLogger("es.gob.afirma").severe("No se ha podido inicializar el PKCS#11 '"+descr+"': "+ex);
				continue;
			}
			
			Logger.getLogger("es.gob.afirma").info(
				"El almacen externo '" + descr + "' ha podido inicializarse, se anadiran sus entradas"
			);
			
			if (ks == null) ks = tmpStore;
			
			tmpAlias = new Vector<String>(0).elements();
			try {
				tmpAlias = tmpStore.aliases();
			}
			catch(Throwable ex) {
				//ex.printStackTrace();
				Logger.getLogger("es.gob.afirma").warning(
					"Se encontro un error obteniendo los alias del almacen externo '" + descr + "', se continuara con el siguiente: " + ex
				);
				continue;
			}
			String alias;
			while (tmpAlias.hasMoreElements()) {
			    alias = tmpAlias.nextElement().toString();
			    storesByAlias.put(alias, tmpStore);
		          Logger.getLogger("es.gob.afirma").info(
	                  "Anadida la entrada '" + alias + "' del almacen externo '" + descr + "'"
	              );
			}
			kss.add(tmpStore);
		}
		
		return kss;
	}

	/**
	 * Establece la interfaz de entrada de la contrase&ntilde;a del almac&eacute;n interno de
	 * Mozilla. Si no se indica o se establece a <code>null</code> se utilizar&aacute; el por
	 * defecto. 
	 * @param externallPC Interfaz de entrada de contrase&ntilde;a.
	 */
	public void setPasswordCallback(PasswordCallback externallPC) {
		this.externallPasswordCallback = externallPC;
	}
	
	@Override
	public String[] getAliases() {
		if (kss == null) {
			Logger.getLogger("es.gob.afirma").warning(
				"Se han pedido los alias de un almacen sin inicializar, se intentara inicializar primero"
			);
			try {
				init();
			} 
			catch (Throwable e) {
				//e.printStackTrace();
				Logger.getLogger("es.gob.afirma").severe(
					"No se ha podido inicializar el almacen, se devolvera una lista de alias vacia: " + e
				);
				return new String[0];
			}
		}
		String[] tmpAlias = new String[storesByAlias.size()];
		int i=0;
		for (Enumeration<String> e = storesByAlias.keys();e.hasMoreElements();) {
			tmpAlias[i] = e.nextElement().toString();
			i++;
		}
		return tmpAlias.clone();
	}
	
	@Override
	public KeyStore.PrivateKeyEntry getKeyEntry(String alias, PasswordCallback pssCallback) throws AOCancelledOperationException {
		KeyStore tmpStore = storesByAlias.get(alias);
		if (tmpStore == null) throw new NullPointerException(
			"No hay ninguna almacen de Mozilla que contenga un certificado con el alias '" + alias + "'" 
		);
		KeyStore.PrivateKeyEntry keyEntry = null;
		try {
			keyEntry = (KeyStore.PrivateKeyEntry) tmpStore.getEntry(
				alias,
				new KeyStore.PasswordProtection(pssCallback.getPassword())
			);
		}
		catch (AOCancelledOperationException e) {
			throw e;
		}
		catch (Throwable e) {
			//e.printStackTrace();
			Logger.getLogger("es.gob.afirma").severe("No se ha podido obtener el puntero a la clave privada del certicado "+
				"con el alias '" + alias + "', se devolvera null: " + e);
			return null;
		}
		return keyEntry;
	}
	
	@Override
	public Vector<KeyStore> getKeyStores() {
		//System.out.println("Se ha solicitado KeyStore: " + ks);
		return kss;
	}
	
	@Override
	public String toString() {
		return "Almacen de claves de tipo Mozilla unificado";
	}
	
	/**
	 * Obtiene un certificado del keystore activo a partir de su alias.
	 * @param alias Alias del certificado.
	 * @return Certificado.
	 */
	@Override
	public Certificate getCertificate(String alias) {
	    if (kss == null) {
           Logger.getLogger("es.gob.afirma").warning(
               "El KeyStore actual no esta inicializado, por lo que no se pudo recuperar el certificado para el alias '" + alias + "'"
           );
	        return null;
	    }
	    for(KeyStore ks : kss) {
	    	try {
	    		if (ks.containsAlias(alias)) return ks.getCertificate(alias);
    	    }
    	    catch(Throwable e) {
    	        Logger.getLogger("es.gob.afirma").info(
    	            "El KeyStore '" + ks + "' no contenia o no pudo recuperar el certificado para el alias '" + alias + "', se probara con el siguiente: " + e
    	        );
	    	}
	    }
        Logger.getLogger("es.gob.afirma").warning(
            "Ningun KeyStore de Mozilla/Firefox contenia el certificado para el alias '" + alias + "', se devolvera null"
        );
        return null;
	}
}

/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo las licencias EUPL version 1.1 y GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.keystores;

import java.awt.Component;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.logging.Logger;

import javax.security.auth.callback.PasswordCallback;

import es.atosorigin.exe.PEParser;
import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOUtil;
import es.gob.afirma.misc.Platform;
import es.gob.afirma.ui.AOUIManager;

/**
 * Obtiene clases de tipo AOKeyStoreManager seg&uacute;n se necesiten, proporcionando adem&aacute;s ciertos
 * m&eacute;todos de utilidad.
 * Contiene fragmentos de las clases <code>com.sun.deploy.config.UnixConfig</code> y <code>com.sun.deploy.config.WinConfig</code>
 * @version 0.3
 */
public final class AOKeyStoreManagerFactory {
    
	/**
	 * Obtiene el <code>KeyStoreManager</code> del tipo indicado.
	 * @param store Almac&eacute;n de claves
	 * @param lib Biblioteca del KeyStore (solo para KeyStoreManager de tipo BR_PKCS11) o fichero de almac&eacute;n de claves
	 *            (para PKCS#12, Java KeyStore, JCE KeyStore, X.509 y PKCS#7)
	 * @param description Descripci&oacute;n del KeyStoreManager que se desea obtener, necesario para
	 *                    obtener el n&uacute;mero de z&oacute;calo de los modulos PKCS#11 obtenidos
	 *                    del Secmod de Mozilla / Firefox. Debe seguir el formato definido en el m&eacute;todo
	 *                    <code>toString()</code> de la clase <code>sun.security.pkcs11.Secmod.Module</code>
	 * @param pssCallback Callback que solicita la password del repositorio que deseamos recuperar.
	 * @param parentComponent Componente padre sobre el que mostrar los di&aacute;logos modales de ser necesario. 
	 * @return KeyStoreManager del tipo indicado
	 * @throws AOCancelledOperationException Cuando el usuario cancela el proceso (por ejemplo, al introducir la contrase&ntilde;a) 
	 * @throws AOKeystoreAlternativeException Cuando ocurre cualquier otro problema durante el proceso
	 */
	public static AOKeyStoreManager getAOKeyStoreManager(final AOConstants.AOKeyStore store, 
			                                             final String lib, 
			                                             final String description, 
			                                             final PasswordCallback pssCallback, 
			                                             final Component parentComponent) throws AOCancelledOperationException, 
			                                                                                     AOKeystoreAlternativeException {

		final AOKeyStoreManager ksm = new AOKeyStoreManager();
		
		
		
		// Fichero P7, X509, P12/PFX o Java JKS, en cualquier sistema operativo
		if (store == AOConstants.AOKeyStore.PKCS12 || 
			store == AOConstants.AOKeyStore.JAVA || 
			store == AOConstants.AOKeyStore.SINGLE ||
			store == AOConstants.AOKeyStore.JAVACE ||
			store == AOConstants.AOKeyStore.JCEKS
		) {
			String storeFilename = null;
			if (lib != null && !"".equals(lib) && new File(lib).exists()) storeFilename = lib;
			if (storeFilename == null) {
			    
			    String desc = null;
			    String[] exts = null;
			    if (store == AOConstants.AOKeyStore.PKCS12) {
			        exts = new String[] { "pfx", "p12" };
			        desc = "Almacenes PKCS#12 (*.p12, *.pfx)";
			    }
			    if (store == AOConstants.AOKeyStore.JAVA) {
			        exts = new String[] { "jks" };
			        desc = "Java KeyStore (*.jks)";
			    }
			    if (store == AOConstants.AOKeyStore.SINGLE) {
			        exts = new String[] { "cer", "p7b" };
			        desc = "Certificados PKCS#7 / X.509 (*.cer, *.p7b)";
			    }
			    if (store == AOConstants.AOKeyStore.JCEKS) {
                    exts = new String[] { "jceks", "jks" };
                    desc = "Java Cryptography Extension KeyStore (*.jceks, *.jks)";
                }
			    
                storeFilename = AOUIManager.getLoadFileName("Abrir repositorio " + store.getDescription(), exts, desc, parentComponent);
			    if(storeFilename == null) throw new AOCancelledOperationException("No se ha seleccionado el almac\u00E9n de certificados");
			}
			final InputStream is;
			try {
			    is = new FileInputStream(storeFilename);
				ksm.init(store, is, pssCallback, null);
			}
			catch(final Throwable e) {
			    throw new AOKeystoreAlternativeException(
			    	getAlternateKeyStoreType(store),
			    	"No se ha podido abrir el almacen de tipo " + store.getDescription(),
			    	e
	    		);
			}
			return ksm;
		}
		// Token PKCS#11, en cualquier sistema operativo
		else if (store == AOConstants.AOKeyStore.PKCS11) {
		    String p11Lib = null; 
		    if (lib != null && !"".equals(lib) && new File(lib).exists()) p11Lib = lib;
            if (p11Lib == null) p11Lib = AOUIManager.getLoadFileName("Seleccionar biblioteca PKCS#11", new String[] { "dll", "so"}, "Bibliotecas PKCS#11 (*.dll, *.so)", parentComponent);
            if (p11Lib == null) throw new AOCancelledOperationException("No se ha seleccionado el controlador PKCS#11");
            if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
            	try {
	            	final InputStream is = new FileInputStream(new File(p11Lib));
	            	Logger.getLogger("es.gob.afirma").info(
            			"Informacion sobre el modulo PKCS11 '" + p11Lib + "':" 
        			);
	            	new PEParser().parse(
	        			AOUtil.getDataFromInputStream(is), 
	        			Logger.getLogger("es.gob.afirma")
	    			);
	            	try { is.close(); } catch(final Throwable e) {}
            	}
            	catch(final Throwable e) {
            		Logger.getLogger("es.gob.afirma").warning(
        				"No se ha podido obtener informacion sobre el modulo PKCS11 '" + p11Lib + "':" + e
    				);
            	}
            }
		    try {
		    	ksm.init(
		    			store, 
		    			null,  
		    			pssCallback,
		    			new String[] { p11Lib, description }				
		    	);
		    } 
		    catch (final Throwable e) {
		    	throw new AOKeystoreAlternativeException(
	    			getAlternateKeyStoreType(store),
	    			"Error al inicializar el modulo PKCS#11", 
	    			e
    			);
		    }
			return ksm;
		}
		
		// Internet Explorer en Windows (descartamos Internet Explorer en Solaris, HP-UX o Mac OS X)
		// o Google Chrome en Windows, que tambien usa el almacen de CAPI
		else if (Platform.getOS().equals(Platform.OS.WINDOWS) &&
				 (store == AOConstants.AOKeyStore.WINDOWS || 
				  store == AOConstants.AOKeyStore.WINROOT ||
				  store == AOConstants.AOKeyStore.WINADDRESSBOOK ||
				  store == AOConstants.AOKeyStore.WINCA /* ||
				  store == AOConstants.AOKeyStore.WINDEPLOY*/)) {
			try {
				ksm.init(store, null, pssCallback, null);
			}
			catch(final Throwable e) {
		    	throw new AOKeystoreAlternativeException(
	    			getAlternateKeyStoreType(store),
	    			"Error al inicializar el almacen " + store.getDescription(), 
	    			e
    			);
			}
			return ksm;
		}
				
		// Mozilla / Firefox en cualquier plataforma, via PKCS#11 y NSS
		else if (store == AOConstants.AOKeyStore.MOZILLA) {
			
			final String nssLibDir;
			try {
				nssLibDir = MozillaKeyStoreUtilities.getSystemNSSLibDir();
				ksm.init(
					store,  
					null,  
					pssCallback,
					new String[] {
						MozillaKeyStoreUtilities.getMozillaUserProfileDirectory(),
						nssLibDir	
					}
				);
			} 
			catch (final Throwable e) {
		    	throw new AOKeystoreAlternativeException(
	    			getAlternateKeyStoreType(store),
	    			"Error al inicializar el almacen NSS de Mozilla Firefox", 
	    			e
    			);
			}
			return ksm;
		}

		// Se soporta aqui el almacen unificado de Mozilla unicamente para facilitar las pruebas
		else if (store == AOConstants.AOKeyStore.MOZ_UNI) {
			final MozillaUnifiedKeyStoreManager ksmUni = new MozillaUnifiedKeyStoreManager();
			ksmUni.setParentComponent(parentComponent);
			ksmUni.setPasswordCallback(pssCallback);
			ksmUni.init();
			return ksmUni;
		}
		
		// Apple Safari sobre Mac OS X
		// Identificacion del sistema operativo segun (anadiendo mayusculas donde se necesitaba)
		// http://developer.apple.com/technotes/tn2002/tn2110.html
		else if (Platform.getOS().equals(Platform.OS.MACOSX) && store == AOConstants.AOKeyStore.APPLE) {
			try {
				ksm.init(store, null, pssCallback, null);
			}
			catch(final Throwable e) {
		    	throw new AOKeystoreAlternativeException(
	    			getAlternateKeyStoreType(store),
	    			"Error al inicializar el Llavero de Mac OS X", 
	    			e
    			);
			}
			return ksm;
		}
		
		throw new AOKeystoreAlternativeException(
			getAlternateKeyStoreType(store),
			"La plataforma de navegador '" + store.getDescription() + "' mas sistema operativo '" +
				Platform.getOS() + " (" + Platform.getOsVersion() + ")' mas Java '" +
				Platform.getJavaVersion() + "' no esta soportada"
		);
	}
	
	/**
	 * @return <code>AOConstants.AOKeyStore</code> alternativo o <code>null</code> si no hay alternativo
	 */
	private static AOConstants.AOKeyStore getAlternateKeyStoreType(final AOConstants.AOKeyStore currentStore) {
		if (AOConstants.AOKeyStore.PKCS12.equals(currentStore)) return null;
		if (Platform.OS.WINDOWS.equals(Platform.getOS()) && (!AOConstants.AOKeyStore.WINDOWS.equals(currentStore))) return AOConstants.AOKeyStore.WINDOWS;
		if (Platform.OS.MACOSX.equals(Platform.getOS()) && (!AOConstants.AOKeyStore.APPLE.equals(currentStore))) return AOConstants.AOKeyStore.APPLE;
		return AOConstants.AOKeyStore.PKCS12;
	}
}

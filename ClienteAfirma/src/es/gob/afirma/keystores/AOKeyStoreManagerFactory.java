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

import java.awt.Component;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;

import javax.security.auth.callback.PasswordCallback;

import es.gob.afirma.exceptions.AOCancelledOperationException;
import es.gob.afirma.exceptions.AOException;
import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.misc.AOUtil;
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
	 * @throws AOException Cuando ocurre cualquier otro problema durante el proceso
	 */
	public static AOKeyStoreManager getAOKeyStoreManager(AOConstants.AOKeyStore store, String lib, String description, PasswordCallback pssCallback, Component parentComponent) throws AOCancelledOperationException, AOException {

		final AOKeyStoreManager ksm = new AOKeyStoreManager();
		
		final String osname = System.getProperty("os.name");
		
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
			InputStream is;
			try {
			    is = new FileInputStream(storeFilename);
			}
			catch(Throwable e) {
			    e.printStackTrace();
			    throw new AOException("No se ha podido abrir el almacen de tipo "+store.getDescription());
			}
			ksm.init(store, is, pssCallback, null);
			return ksm;
		}
		// Token PKCS#11, en cualquier sistema operativo
		else if (store == AOConstants.AOKeyStore.PKCS11) {
		    String p11Lib = null; 
		    if (lib != null && !"".equals(lib) && new File(lib).exists()) p11Lib = lib;
            if (p11Lib == null) p11Lib = AOUIManager.getLoadFileName("Seleccionar biblioteca PKCS#11", new String[] { "dll", "so"}, "Bibliotecas PKCS#11 (*.dll, *.so)", parentComponent);
            if (p11Lib == null) throw new AOCancelledOperationException("No se ha seleccionado el controlador PKCS#11");
		    try {
		    	ksm.init(
		    			store, 
		    			null,  
		    			pssCallback,
		    			new String[] { p11Lib, description }				
		    	);
		    } catch (Throwable e) {
		    	throw new AOException("Ocurrio un error al inicializar el modulo PKCS#11", e);
		    }
			return ksm;
		}
		
		// Internet Explorer en Windows (decartamos Internet Explorer en Solaris, HP-UX o Mac OS X)
		// o Google Chrome en Windows, que tambien usa el almacen de CAPI
		else if ((store == AOConstants.AOKeyStore.WINDOWS || 
				  store == AOConstants.AOKeyStore.WINROOT ||
				  store == AOConstants.AOKeyStore.WINADDRESSBOOK ||
				  store == AOConstants.AOKeyStore.WINCA) && 
				  osname.contains("indows") /*&&
				  !(System.getProperty("java.version").compareTo("1.6") < 0)*/) {
			ksm.init(
				store, 
				null, 
				pssCallback,
				null
			);
			return ksm;
		}
				
		// Mozilla / Firefox en cualquier plataforma, via PKCS#11 y NSS
		else if (store == AOConstants.AOKeyStore.MOZILLA) {
			
			// Nos aseguramos de que el directorio de Firefox esta en el path y esta el primero.
			// Es posible que los cambios no surtan efecto hasta despues de reiniciar el navegador  
			KeyStoreUtilities.fixFirefoxNSSPath();
						
			ksm.init(
				store,  
				null,  
				pssCallback,
				new String[] {
						AOUtil.getMozillaUserProfileDirectory(), // MozillaUserProfileDirectory
						KeyStoreUtilities.getSystemNSSLibDir()	
				}
			);
			return ksm;
		}
		
		// Apple Safari sobre Mac OS X
		// Identificacion del sistema operativo segun (anadiendo mayusculas donde se necesitaba)
		// http://developer.apple.com/technotes/tn2002/tn2110.html
		else if (store == AOConstants.AOKeyStore.APPLE && osname.startsWith("Mac OS X")) {
			ksm.init(
				store,
				null,
				pssCallback,
				null
			);
			return ksm;
		}
		
		// Se soporta aqui el almacen unificado de Mozilla unicamente para facilitar las pruebas
		else if (store == AOConstants.AOKeyStore.MOZ_UNI) {
			
			// Nos aseguramos de que el directorio de Firefox esta en el path y esta el primero.
			// Es posible que los cambios no surtan efecto hasta despues de reiniciar el navegador  
			KeyStoreUtilities.fixFirefoxNSSPath();
			
			MozillaUnifiedKeyStoreManager ksmUni = new MozillaUnifiedKeyStoreManager();
			ksmUni.setPasswordCallback(pssCallback);
			ksmUni.init();
			return ksmUni;
		}
		
		throw new AOException(
			"La plataforma de navegador '" + store.getDescription() + "' mas sistema operativo '" +
			System.getProperty("os.name") + "' mas Java '"+System.getProperty("java.version")+
			"' no esta soportada"
		);
	}
}

/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.misc;

import java.io.File;
import java.security.AccessController;
import java.util.logging.Logger;

/**
 * Constantes de utilidad en toda la aplicaci&oacute;n.
 * @version 0.3.
 */
public final class AOInstallParameters {
		
	/** Directorio de usuario. */
	public static final String USER_HOME;
	static {
	    USER_HOME = AccessController.doPrivileged(new java.security.PrivilegedAction<String>() {
	        public String run() {
	            try {
	                return System.getProperty("user.home") + File.separator;
	            } catch (Throwable e) {
	                Logger.getLogger("es.gob.afirma").severe("No ha podido determinarse el directorio de usuario para la configuracion del cliente");
	                return "";
	            }
	        }
	    });
	}
	
	/** Directorio de las bibliotecas de despliegue. */
	private static final String DEPLOY_DLL_HOME = File.separator + "deploy";
	
	/** Directorio de las bibliotecas de utilidad. */
	private static final String AOUTIL_DLL_HOME = File.separator + "aoutil";
	
	/** Directorio de instalacion por defecto de las librer&iacute;as del applet. */
	public static final String DEFAULT_INSTALLATION_DIR = "afirma.5";
	
	/** Directorio de instalaci&oacute;n. */
	private static String installationDir = DEFAULT_INSTALLATION_DIR;
	
	/**
	 * Establece el nombre del directorio de instalacion para poder parametrizar el resto de directorios.
	 * Por ejemplo, si el directorio de instalacion fuese: "C:/Documents and Settings/User1/afirma.5"<br/>
	 * deber&iacute;amos utilizar como par&aacute;metro "afirma.5". Si se establece null, se tomara el
	 * directorio de instalacion por defecto.
	 * @param installationDirectory Directorio de instalaci&oacute;n del applet.
	 */
	public static void setInstallationDirectory(String installationDirectory) {
		if(installationDirectory != null) {
			installationDir = installationDirectory;
		} else {
			installationDir = DEFAULT_INSTALLATION_DIR;
		}
	}
	
	//************************************************************
	//************* DIRECTORIOS DE INSTALACION *******************
	//************************************************************
	
	/**
	 * Recupera el directorio de instalaci&oacute;n del cliente (s&oacute;lo el nombre del directorio).
	 * @return Directorio de instalaci&oacute;n.
	 */
	public static String getInstallationDirectory() {
		return installationDir;
	}
	
	/**
	 * Recupera el directorio de instalaci&oacute;n del cliente con el path completo.
	 * @return Directorio de instalaci&oacute;n.
	 */
	public static String getHomeApplication() {
		return USER_HOME + installationDir;
	}
	
	/**
	 * Recupera el directorio de instalaci&oacute;n de las librer&iacute;as de despliegue
	 * que utiliza el cliente.
	 * @return Directorio en donde hemos instalado las librer&iacute;as de despliegue.
	 */
	public static final String getDeployDllDir() {
		return USER_HOME + installationDir + DEPLOY_DLL_HOME;
	}
		
	/**
	 * Recupera el directorio de instalaci&oacute;n de las librer&iacute;as de utilidad
	 * que utiliza el cliente.
	 * @return Directorio con las librer&iacute;as de utilidad.
	 */
	public static final String getAOUtilDllDir() {
		return USER_HOME + installationDir + AOUTIL_DLL_HOME;
	}

	/**
	 * Recupera el directorio del usuario activo (terminado en el car&acute;cter separador).
	 * @return Directorio del usuario activo del sistema.
	 */
	public static final String getUserHome() {
		return USER_HOME;
	}
	
	   /** Preguntar al usuario sobre la acci&oacute;n a realizar. */
    public final static int ACTION_ASK = 1;
    
    /** Respetar instalaciones antiguas del cliente. */
    public final static int ACTION_RESPECT = 2;
    
    /** Eliminar instalaciones antiguas del cliente. */
    public final static int ACTION_DELETE = 3;
    
    /** Acci&oacute;n a realizar con respecto a las versiones antiguas encontradas del cliente. */
    public int oldVersionsAction = ACTION_ASK;
}

/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */



package es.gob.afirma.misc.mozilla.utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Vector;


/**
 * M&eacute;todos de utilidad para Mozilla Firefox y Nestcape
 * 
 * 
 * Inspirada en la clase com.sun.deploy.net.proxy.NSPreferences de Sun Microsystems. 
 */
public final class NSPreferences {
	
    /**
     * Return the location of the user profile directory in the
     * netscape registry or null if we can't figure that out.  This method
     * should work with versions 6 of Navigator.
     * @param registryFile Netscape / Mozilla registry file
     * @return Location of the Netscape / Mozilla user profile directory
     * @throws IOException When there's an error opening or reading the registry file
     */
    public static String getNS6UserProfileDirectory(File registryFile) throws IOException {
        final NSRegistry reg = new NSRegistry().open(registryFile);
        String path = null;
        String currProfileName = null;

        // Get current user profile directory
        if (reg != null) {
            currProfileName = reg.get("Common/Profiles/CurrentProfile");
            if (currProfileName != null) path = reg.get("Common/Profiles/" + currProfileName + "/directory");
            reg.close();
        }

        if (path == null) throw new IOException();
        return path;
    }
   
    /**
     * Devuelve el directorio del perfil activo de Firefox. Si no hubiese perfil activo,
     * devolver&iacute;a el directorio del perfil por defecto y si tampoco lo hubiese
     * el del primer perfil encontrado. Si no hubiese perfiles configurados,
     * devolver&iacute;a {@code null}.
     * @param iniFile Fichero con la informaci&oacute;n de los perfiles de Firefox.
     * @return Directorio con la informaci&oacute;n del perfil.
     * @throws IOException Cuando ocurre un error abriendo o leyendo el fichero.
     */
    public static String getFireFoxUserProfileDirectory(File iniFile) throws IOException {

    	if (iniFile == null) throw new NullPointerException(
    			"El fichero INI nulo y no se podra determinar el directorio del usuario de firefox"
    	);

    	if (!iniFile.exists() || !iniFile.isFile()) {
    		throw new IOException("No se ha encontrado el fichero con los perfiles de firefox");
    	}

    	String currentProfilePath = null;
    	
    	// Leemos el fichero con la informacion de los perfiles y buscamos el activo(el que esta bloqueado)
    	FirefoxProfile[] profiles = readProfiles(iniFile);
    	for (FirefoxProfile profile : profiles) {
    		if (isProfileLocked(profile)) {
    			currentProfilePath = profile.absolutePath;
    			break;
    		}	
    	}

    	// Si no hay ninguno actualmente activo, tomamos el por defecto
    	if (currentProfilePath == null) {
    		for (FirefoxProfile profile : profiles) {
    			if (profile.isDefault) {
      			currentProfilePath = profile.absolutePath;
      			break;
    			}
    		}
    	}

    	// Si no hay ninguno por defecto, se toma el primero
    	if (profiles.length > 0) {
    		currentProfilePath = profiles[0].absolutePath;
    	}

    	return currentProfilePath;
    }

    /**
     * Parsea la informacion de los perfiles declarada en el fichero "profiles.ini".
     * Para identificar correctamente los perfiles es necesario que haya al menos una
     * l&iacute;nea de separaci&oacute;n entre los bloques de informaci&oacute;n de
     * cada perfil.
     * @param iniFile Fichero con lainformaci&oacute;n de los perfiles.
     * @return Listado de perfiles completos encontrados.
     * @throws IOException Cuando se produce un error durante la lectura de la
     * configuraci&oacute;n.
     */
    private static FirefoxProfile[] readProfiles(File iniFile) throws IOException {
    	
    	final String NAME_ATR = "name=";
    	final String IS_RELATIVE_ATR = "isrelative=";
    	final String PATH_PROFILES_ATR = "path=";
    	final String IS_DEFAULT_ATR = "default=";
    	
    	String line = null;
    	Vector<FirefoxProfile> profiles = new Vector<FirefoxProfile>();
    	BufferedReader in = new BufferedReader(new FileReader(iniFile));
    	try {
    		while ((line = in.readLine()) != null) {
    			
    			// Buscamos un nuevo bloque de perfil
    			if (!line.trim().toLowerCase().startsWith("[profile"))
    				continue;
    			
    			FirefoxProfile profile = new FirefoxProfile();
    			while((line = in.readLine()) != null && line.trim().length() > 0 && !line.trim().toLowerCase().startsWith("[profile")) {
    				if(line.trim().toLowerCase().startsWith(NAME_ATR))
    					profile.name = line.trim().substring(NAME_ATR.length());
    				else if(line.trim().toLowerCase().startsWith(IS_RELATIVE_ATR))
    					profile.isRelative = line.trim().substring(IS_RELATIVE_ATR.length()).equals("1");
    				else if(line.trim().toLowerCase().startsWith(PATH_PROFILES_ATR))
    					profile.path = line.trim().substring(PATH_PROFILES_ATR.length());
    				else if(line.trim().toLowerCase().startsWith(IS_DEFAULT_ATR))
    					profile.isDefault = line.trim().substring(IS_DEFAULT_ATR.length()).equals("1");
    				else
    					break;
    			}
    			
    			// Debemos encontrar al menos el nombre y la ruta del perfil
    			if (profile.name != null || profile.path != null) {
    				profile.absolutePath =
    					profile.isRelative ? 
    						new File(iniFile.getParent(), profile.path).toString() :
    						profile.path;
    						
    				profiles.add(profile);
    			}
    		}
    	} catch (Exception e) {
    		throw new IOException("Ocurrio un error al leer la configuracion de los perfiles de Firefox: " + e);
    	} finally {
    		try { in.close(); } catch (Exception e) {}
    	}
    	
    	return profiles.toArray(new FirefoxProfile[profiles.size()]);
		}
    
    /**
     * Comprueba que un perfil de Firefox est&eacute; bloqueado. Un perfil esta
     * bloqueado cuando en su directorio se encuentra el fichero "parent.lock".
     * @param profile Informaci&oacute;n del perfil de Firefox.
     * @return Devuelve {@code true} si el perfil esta bloqueado, {@code false}
     * en caso contrario.
     */
    private static boolean isProfileLocked(FirefoxProfile profile) {
    	return 	new File(profile.absolutePath, "parent.lock").exists() ||	// En Windows
    					new File(profile.absolutePath, "lock").exists();					// En Linux
    }

    /**
     * Clase que almacena la configuraci&oacute;n para la identificacion de un perfil
     * de Mozilla Firefox. 
     */
    private static class FirefoxProfile {
    	String name = null;
    	boolean isRelative = true;
    	String path = null;
    	String absolutePath = null;
    	boolean isDefault = false;
    }
}

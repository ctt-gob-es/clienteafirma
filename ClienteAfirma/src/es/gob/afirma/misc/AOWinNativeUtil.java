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

import es.gob.afirma.misc.AOInstallParameters;
import es.gob.afirma.misc.AOUtil;

/** 
 * M&eacute;todos de utilidad dependientes (biblioteca nativa) de MS-Windows para toda la aplicaci&oacute;n.
 * @version 0.1 
 */
public final class AOWinNativeUtil {
	
	/**
	 * Obtiene el nombre corto de un nombre largo de sistema de ficheros en Windows.
	 * Puede no funcionar si el usuario tiene deshabilitada la creaci&oacute;n de alternativas
	 * 8+3 a los nombres largos en NTFS
	 * @param longName Nombre "largo" de un fichero o un directorio
	 * @return Equivalente "corto" (8+3) al nombre "largo"
	 */
	public static native String getShortPathName(String longName);
	
	/**
	 * Obtiene un nombre largo de Windows a partir de su equivalente corto.
	 * No deber&iacute;a usarse, una canonicalizaci&oacute;n de Java es m&aacute;s efectiva,
	 * compatible y r&aacute;pida
	 * @param shortName Nombre corto del fichero (incluyendo ruta)
	 * @return Nombre largo del fichero (incluyendo ruta)
	 */
	public static native String getLongPathName(String shortName);
	
	static {
		if(new File(AOInstallParameters.getAOUtilDllDir() + File.separator + "ShortPathName.dll").exists())
	    AOUtil.loadNativeLibrary(AOInstallParameters.getAOUtilDllDir() + File.separator + "ShortPathName.dll");
	}

	
}

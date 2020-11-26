package es.gob.afirma.plugin.hash;

/**
 * Propiedades de configuraci&oacute;n que admitidas por el plugin de c&aacute;lculo
 * y comprobaci&oacute;n de huellas digitales.
 */
public class HashPreferences {

	/** Algoritmo de la huella digital para la creaci&oacute;n de huellas digitales de ficheros.*/
	public static final String PREFERENCE_CREATE_HASH_ALGORITHM = "createHashAlgorithm"; //$NON-NLS-1$

	/** Formato de la huella digital para la creaci&oacute;n de huellas digitales de ficheros.*/
	public static final String PREFERENCE_CREATE_HASH_FORMAT = "createHashFormat"; //$NON-NLS-1$

	/** Si est&aacute; establecido a <code>true</code> se copiara la huella digital de fichero al portapapeles.*/
	public static final String PREFERENCE_CREATE_HASH_CLIPBOARD = "createHashCopyToClipBoard"; //$NON-NLS-1$

	/** Algoritmo de la huella digital para la creaci&oacute;n de huellas digitales de directorio. */
	public static final String PREFERENCE_CREATE_HASH_DIRECTORY_ALGORITHM = "createHashDirectoryAlgorithm"; //$NON-NLS-1$

	/** Si se debe calcular la huella digital de los ficheros en los subdirectorios del directorio seleccionado. */
	public static final String PREFERENCE_CREATE_HASH_DIRECTORY_RECURSIVE = "createHashDirectoryRecursive"; //$NON-NLS-1$
}

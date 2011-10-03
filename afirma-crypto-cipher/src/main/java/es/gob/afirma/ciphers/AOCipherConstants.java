/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.ciphers;

/**
 * Constantes para el uso de las funcionalidades de cifrado.
 */
public class AOCipherConstants {

    /** Modo de generaci&oacute;n autom&aacute;tica de clave sim&eacute;trica
     * aleatoria. */
    public static final String KEY_MODE_GENERATEKEY = "GENERATEKEY"; //$NON-NLS-1$

    /** Modo de inserci&oacute;n directa de clave por parte del usuario. */
    public static final String KEY_MODE_USERINPUT = "USERINPUT"; //$NON-NLS-1$

    /** Modo de generaci&oacute;n de clave a partir de una password. */
    public static final String KEY_MODE_PASSWORD = "PASSWORD"; //$NON-NLS-1$

    /** Algoritmo de cifrado que se usa por defecto. */
    public static final String DEFAULT_KEY_MODE = KEY_MODE_GENERATEKEY;
}

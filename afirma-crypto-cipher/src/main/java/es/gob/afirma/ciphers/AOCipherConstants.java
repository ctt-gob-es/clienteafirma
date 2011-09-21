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

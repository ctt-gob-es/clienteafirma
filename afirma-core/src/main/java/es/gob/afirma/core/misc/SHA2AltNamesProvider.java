/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.core.misc;

import java.security.MessageDigest;
import java.security.Provider;
import java.security.Security;

/** Proveedor para huellas digitales SHA2 con variantes de nombre no contempladas
 * en el proveedor Sun. */
public final class SHA2AltNamesProvider extends Provider {

    /** Comprueba si es necesario un proveedor para nombre de huella digital SHA2 sin gui&oacute;n
     *  y lo instala en ese caso. */
    public static void install() {
        try {
            MessageDigest.getInstance("SHA256"); //$NON-NLS-1$
            sha256Class = null;
        }
        catch(final Exception e) {
            // Se ignora el error
        }
        try {
            MessageDigest.getInstance("SHA384"); //$NON-NLS-1$
            sha384Class = null;
        }
        catch(final Exception e) {
            // Se ignora el error
        }
        try {
            MessageDigest.getInstance("SHA512"); //$NON-NLS-1$
            sha512Class = null;
        }
        catch(final Exception e) {
            // Se ignora el error
        }
        try {
            AOUtil.classForName(sha256Class);
        }
        catch(final Throwable e) {
            sha256Class = null;
        }
        try {
            AOUtil.classForName(sha384Class);
        }
        catch(final Throwable e) {
            sha384Class = null;
        }
        try {
            AOUtil.classForName(sha512Class);
        }
        catch(final Throwable e) {
            sha512Class = null;
        }
        
        if (sha256Class != null || sha384Class != null || sha512Class != null) {
            Security.addProvider(new SHA2AltNamesProvider());
        }
        
    }
    
    private static String sha256Class = "sun.security.provider.SHA2"; //$NON-NLS-1$
    private static String sha384Class = "sun.security.provider.SHA5$SHA384"; //$NON-NLS-1$
    private static String sha512Class = "sun.security.provider.SHA5$SHA512"; //$NON-NLS-1$
    
    private static final long serialVersionUID = 2361025283448164750L;

    /** Construye un nuevo proveedor de huellas digitales SHA-2 con nombres
     * alternativos. */
    public SHA2AltNamesProvider() {
        super("AOSHA2AltNamesProvider", 1.0, "Proveedor para huellas digitales SHA-2 con nombres alternativos");  //$NON-NLS-1$//$NON-NLS-2$
        if (sha256Class != null) {
            put("MessageDigest.SHA256", sha256Class); //$NON-NLS-1$
        }
        if (sha384Class != null) {
            put("MessageDigest.SHA384", sha384Class); //$NON-NLS-1$
        }
        if (sha512Class != null) {
            put("MessageDigest.SHA512", sha512Class); //$NON-NLS-1$
        }
    }


}

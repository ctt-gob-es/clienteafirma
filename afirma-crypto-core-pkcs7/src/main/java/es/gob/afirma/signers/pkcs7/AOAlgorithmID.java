/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.signers.pkcs7;

import java.util.Dictionary;
import java.util.Hashtable;

/** Identificadores (OID) comunes de algoritmos usados en CMS/CAdES
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s
 */
public final class AOAlgorithmID {
    
    private AOAlgorithmID() {
        // No permitimos la instanciacion
    }
    
    private static final Dictionary<String, String> OIDS = new Hashtable<String, String>();
    static {
        OIDS.put("SHA1", "1.3.14.3.2.26"); //$NON-NLS-1$ //$NON-NLS-2$
        OIDS.put("SHA-1", "1.3.14.3.2.26"); //$NON-NLS-1$ //$NON-NLS-2$
        OIDS.put("SHA", "1.3.14.3.2.26"); //$NON-NLS-1$ //$NON-NLS-2$
        OIDS.put("1.3.14.3.2.26", "1.3.14.3.2.26"); //$NON-NLS-1$ //$NON-NLS-2$
        
        OIDS.put("SHA-512", "2.16.840.1.101.3.4.2.3"); //$NON-NLS-1$ //$NON-NLS-2$
        OIDS.put("SHA512", "2.16.840.1.101.3.4.2.3"); //$NON-NLS-1$ //$NON-NLS-2$
        OIDS.put("2.16.840.1.101.3.4.2.3", "2.16.840.1.101.3.4.2.3"); //$NON-NLS-1$ //$NON-NLS-2$
        
        OIDS.put("MD2", "1.2.840.113549.2.2"); //$NON-NLS-1$ //$NON-NLS-2$
        OIDS.put("1.2.840.113549.2.2", "1.2.840.113549.2.2"); //$NON-NLS-1$ //$NON-NLS-2$
        
        OIDS.put("MD5", "1.2.840.113549.2.5"); //$NON-NLS-1$ //$NON-NLS-2$
        OIDS.put("1.2.840.113549.2.5", "1.2.840.113549.2.5"); //$NON-NLS-1$ //$NON-NLS-2$
        
        OIDS.put("SHA-256", "2.16.840.1.101.3.4.2.1"); //$NON-NLS-1$ //$NON-NLS-2$
        OIDS.put("SHA256", "2.16.840.1.101.3.4.2.1"); //$NON-NLS-1$ //$NON-NLS-2$
        OIDS.put("2.16.840.1.101.3.4.2.1", "2.16.840.1.101.3.4.2.1"); //$NON-NLS-1$ //$NON-NLS-2$
        
        OIDS.put("SHA-384", "2.16.840.1.101.3.4.2.2"); //$NON-NLS-1$ //$NON-NLS-2$
        OIDS.put("SHA384", "2.16.840.1.101.3.4.2.2"); //$NON-NLS-1$ //$NON-NLS-2$
        OIDS.put("2.16.840.1.101.3.4.2.2", "2.16.840.1.101.3.4.2.2"); //$NON-NLS-1$ //$NON-NLS-2$
        
        OIDS.put("RSA", "1.2.840.113549.1.1.1"); //$NON-NLS-1$ //$NON-NLS-2$
        OIDS.put("1.2.840.113549.1.1.1", "1.2.840.113549.1.1.1"); //$NON-NLS-1$ //$NON-NLS-2$
    }
    
    /** Obtiene el OID del algoritmo indicado.
     * @param name Nombre del algoritmo
     * @return OID del algoritmo
     */
    public static String getOID(final String name) {
        if (name == null) {
            return null;
        }
        final String res = OIDS.get(name.toUpperCase());
        if (res == null) {
            throw new IllegalArgumentException("Se deconoce el algoritmo '" + name + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return res;
    }

}

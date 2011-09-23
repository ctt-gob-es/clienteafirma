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
    
    private static final Dictionary<String, String> oids = new Hashtable<String, String>();
    static {
        oids.put("SHA1", "1.3.14.3.2.26"); //$NON-NLS-1$ //$NON-NLS-2$
        oids.put("SHA-1", "1.3.14.3.2.26"); //$NON-NLS-1$ //$NON-NLS-2$
        oids.put("SHA", "1.3.14.3.2.26"); //$NON-NLS-1$ //$NON-NLS-2$
        oids.put("1.3.14.3.2.26", "1.3.14.3.2.26"); //$NON-NLS-1$ //$NON-NLS-2$
        
        oids.put("SHA-512", "2.16.840.1.101.3.4.2.3"); //$NON-NLS-1$ //$NON-NLS-2$
        oids.put("SHA512", "2.16.840.1.101.3.4.2.3"); //$NON-NLS-1$ //$NON-NLS-2$
        oids.put("2.16.840.1.101.3.4.2.3", "2.16.840.1.101.3.4.2.3"); //$NON-NLS-1$ //$NON-NLS-2$
        
        oids.put("MD2", "1.2.840.113549.2.2"); //$NON-NLS-1$ //$NON-NLS-2$
        oids.put("1.2.840.113549.2.2", "1.2.840.113549.2.2"); //$NON-NLS-1$ //$NON-NLS-2$
        
        oids.put("MD5", "1.2.840.113549.2.5"); //$NON-NLS-1$ //$NON-NLS-2$
        oids.put("1.2.840.113549.2.5", "1.2.840.113549.2.5"); //$NON-NLS-1$ //$NON-NLS-2$
        
        oids.put("SHA-256", "2.16.840.1.101.3.4.2.1"); //$NON-NLS-1$ //$NON-NLS-2$
        oids.put("SHA256", "2.16.840.1.101.3.4.2.1"); //$NON-NLS-1$ //$NON-NLS-2$
        oids.put("2.16.840.1.101.3.4.2.1", "2.16.840.1.101.3.4.2.1"); //$NON-NLS-1$ //$NON-NLS-2$
        
        oids.put("SHA-384", "2.16.840.1.101.3.4.2.2"); //$NON-NLS-1$ //$NON-NLS-2$
        oids.put("SHA384", "2.16.840.1.101.3.4.2.2"); //$NON-NLS-1$ //$NON-NLS-2$
        oids.put("2.16.840.1.101.3.4.2.2", "2.16.840.1.101.3.4.2.2"); //$NON-NLS-1$ //$NON-NLS-2$
        
        oids.put("RSA", "1.2.840.113549.1.1.1"); //$NON-NLS-1$ //$NON-NLS-2$
        oids.put("1.2.840.113549.1.1.1", "1.2.840.113549.1.1.1"); //$NON-NLS-1$ //$NON-NLS-2$
    }
    
    /** Obtiene el OID del algoritmo indicado.
     * @param name Nombre del algoritmo
     * @return OID del algoritmo
     */
    public static String getOID(final String name) {
        if (name == null) {
            return null;
        }
        final String res = oids.get(name.toUpperCase());
        if (res == null) {
            throw new IllegalArgumentException("Se deconoce el algoritmo '" + name + "'"); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return res;
    }

}

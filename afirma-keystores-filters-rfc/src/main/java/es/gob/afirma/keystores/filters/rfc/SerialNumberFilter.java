package es.gob.afirma.keystores.filters.rfc;

import java.math.BigInteger;
import java.security.cert.X509Certificate;

import es.gob.afirma.keystores.filters.CertificateFilter;

/** Filtro de certificados por su n&uacute;mero de serie.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class SerialNumberFilter implements CertificateFilter {
    
    private final BigInteger serialNumber;
    
    /** Construye un filtro de certificados por n&uacute;mero de serie.
     * @param serial N&uacute;mero de serie que debe tener el certificado para pasar el filtro
     */
    public SerialNumberFilter(final BigInteger serial) {
        if (serial == null) {
            throw new IllegalArgumentException("El numero de serie clave del filtro no puede ser nulo"); //$NON-NLS-1$
        }
        this.serialNumber = serial;
    }

    public boolean matches(X509Certificate cert) {
        if (cert == null) {
            return false;
        }
        return cert.getSerialNumber().equals(this.serialNumber);
    }

}

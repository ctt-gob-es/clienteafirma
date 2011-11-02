/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.keystores.filters.rfc;

import java.math.BigInteger;
import java.security.cert.X509Certificate;

import es.gob.afirma.keystores.filters.CertificateFilter;

/** Filtro de certificados por su n&uacute;mero de serie.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public class SerialNumberFilter extends CertificateFilter {
    
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

    @Override
	public boolean matches(X509Certificate cert) {
        if (cert == null) {
            return false;
        }
        return cert.getSerialNumber().equals(this.serialNumber);
    }

}

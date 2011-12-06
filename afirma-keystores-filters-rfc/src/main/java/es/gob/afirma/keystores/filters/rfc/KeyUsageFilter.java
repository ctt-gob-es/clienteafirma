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

import java.security.cert.X509Certificate;

import es.gob.afirma.keystores.main.filters.CertificateFilter;

/** Clase que representa un filtro de certificados por los atributos KeyUsage para
 * el di&aacute;logo de selecci&oacute;n. */
public class KeyUsageFilter extends CertificateFilter {
    
    private static final int KEYUSAGE_NBITS = 9;
    
    /** El KeyUsage m&iacute;nimos que debe cumplir el certificado. */
    private final Boolean[] keyUsageFilter;

    @Override
	public boolean matches(final X509Certificate cert) {
        if (cert == null) {
            return false;
        }
        if (this.keyUsageFilter.length == KEYUSAGE_NBITS) {
            final boolean[] certUsage = cert.getKeyUsage();
            if (certUsage != null) {
                for (int j = 0; j < certUsage.length; j++) {
                    if (this.keyUsageFilter[j] != null && this.keyUsageFilter[j].booleanValue() != certUsage[j]) {
                        return false;
                    }
                }
                return true;
            }
        }
        return false;
    }
    
    /** Construye un filtro para certificados.
     * @param keyUsage M&aacute;scara de bits para filtro por <i>KeyUsage</i><br/>
     *        Cada certificado puede permitir simult&aacute;neamente cualquiera de
     *        estos 8 usos:<br/>
     *        <ol>
     *        <li><b>digitalSignature</b></li>
     *        <li><b>nonRepudiation</b></li>
     *        <li><b>keyEncipherment</b></li>
     *        <li><b>dataEncipherment</b></li>
     *        <li><b>keyAgreement</b></li>
     *        <li><b>keyCertSign</b></li>
     *        <li><b>cRLSign</b></li>
     *        <li><b>encipherOnly</b></li>
     *        <li><b>decipherOnly</b></li>
     *        </ol>
     *        Cada uno de los elementos del array designan en orden a uno de estos 8
     *        usos. El valor de cada elemento puede ser:
     *        <ul>
     *        <li>{@code null}: No establece un filtro para ese uso del certificado.</li>
     *        <li>{@code false}: El certificado no debe tener permitido ese uso.</li>
     *        <li>{@code true}: El certificado debe tener permitido ese uso.</li>
     *        </ul> */
    public KeyUsageFilter(final Boolean[] keyUsage) {
        if (keyUsage == null) {
            throw new IllegalArgumentException("El criterio de filtrado no puede ser nulo"); //$NON-NLS-1$
        }
        this.keyUsageFilter = keyUsage.clone();
    }
    
    /** <i>KeyUsage</i> t&iacute;pico de un certificado v&aacute;lido para firmas
     * digitales. */
    public static final Boolean[] SIGN_CERT_USAGE = {
            null, // digitalSignature
            Boolean.TRUE, // nonRepudiation
            null, // keyEncipherment
            null, // dataEncipherment
            null, // keyAgreement
            null, // keyCertSign
            null, // cRLSign
            null, // encipherOnly
            null  // decipherOnly
    };

}

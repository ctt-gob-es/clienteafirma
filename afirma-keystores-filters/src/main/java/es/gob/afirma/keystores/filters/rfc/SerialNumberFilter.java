/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.filters.rfc;

import java.math.BigInteger;
import java.security.cert.X509Certificate;

import es.gob.afirma.keystores.CertificateFilter;

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
	public boolean matches(final X509Certificate cert) {
        if (cert == null) {
            return false;
        }
        return this.serialNumber.equals(cert.getSerialNumber());
    }

}

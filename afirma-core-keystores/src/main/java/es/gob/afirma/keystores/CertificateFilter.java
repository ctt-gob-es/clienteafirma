/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores;

import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;

import es.gob.afirma.core.keystores.KeyStoreManager;

/** Filtro para certificados. Debe autocontener toda la l&oacute;gica que indique si un
 * certificado cumple o no las condiciones del filtro.
 * El establecimiento de los datos encesarios para las condiciones de filtrado queda fuera
 * del interfaz y debe ser espec&iacute;fico para cada implementaci&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public abstract class CertificateFilter {

    /** Comprueba si un certificado se adec&uacute;a al filtro.
     * @param cert Certificado a comprobar.
     * @return <code>true</code> si el certificado se adec&uacute;a al filtro, <code>false</code> en caso contrario. */
    public abstract boolean matches(final X509Certificate cert);

    /** Obtiene del listado proporcionado los alias de certificados que cumplen con un determinado criterio.
     * Por defecto, el establecido en el m&eacute;todo {@link #matches(X509Certificate)}.
     * @param aliases Listado de alias de certificados.
     * @param ksm <code>AOKeyStoreManager</code> que contiene los certificados cuyos
     *            alias se indican.
     * @return Alias de certificados que cumplen el criterio. */
    public String[] matches(final String[] aliases, final KeyStoreManager ksm) {
        final List<String> filteredAliases = new ArrayList<>();
        for (final String alias : aliases) {
            if(matches(ksm.getCertificate(alias))) {
                filteredAliases.add(alias);
            }
        }
        return filteredAliases.toArray(new String[filteredAliases.size()]);
    }
}

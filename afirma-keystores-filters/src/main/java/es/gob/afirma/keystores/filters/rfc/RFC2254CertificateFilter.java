/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.filters.rfc;

import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.naming.directory.Attributes;
import javax.naming.directory.BasicAttributes;
import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;

import es.gob.afirma.core.keystores.KeyStoreManager;
import es.gob.afirma.keystores.CertificateFilter;

/** Clase que representa un filtro de certificados para
 * el di&aacute;logo de selecci&oacute;n. */
public final class RFC2254CertificateFilter extends CertificateFilter {

    private static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

    /** Filtro RFC2254 para el emisor del certificado. */
    private final String rfc2254IssuerFilter;

    /** Filtro RFC2254 para el emisor del certificado. */
    private final String rfc2254SubjectFilter;

    private final boolean recurseIssuers;

    /** Construye un filtro para certificados mediante expresi&oacute;n RFC2254.
     * @param subjectFilter Cadena seg&uacute;n la RFC2254 para filtro por el campo del titular (<i>subject</i>).
     *                      Si se indica <code>null</code> se acepta cualquier titular.
     * @param issuerFilter Cadena seg&uacute;n la RFC2254 para filtro por el campo del emisor (<i>issuer</i>).
     *                     Si se indica <code>null</code> se acepta cualquier emisor.
     * @param recurse Si se establece a <code>true</code>, el filtro para el emisor se aplica a cada uno de
     *                los certificados de la cadena de confianza de emisi&oacute;n, d&aacute;ndose el filtro
     *                por positivo si al menos uno de los certificados de esta cadena pasa el fltro establecido
     *                para el titular. */
    public RFC2254CertificateFilter(final String subjectFilter, final String issuerFilter, final boolean recurse) {
        if ((subjectFilter == null || subjectFilter.isEmpty()) && (issuerFilter == null || issuerFilter.isEmpty())) {
            throw new IllegalArgumentException("Al menos uno de los criterios de filtrado debe no ser nulo"); //$NON-NLS-1$
        }
        this.rfc2254IssuerFilter = issuerFilter != null && !issuerFilter.isEmpty() ? issuerFilter : null;
        this.rfc2254SubjectFilter = subjectFilter != null && !subjectFilter.isEmpty() ? subjectFilter : null;
        this.recurseIssuers = recurse;
    }

    /** Construye un filtro para certificados.
     * @param subjectFilter Cadena seg&uacute;n la RFC2254 para filtro por el campo del titular (<i>subject</i>).
     *                      Si se indica <code>null</code> se acepta cualquier titular.
     * @param issuerFilter Cadena seg&uacute;n la RFC2254 para filtro por el campo del emisor (<i>issuer</i>).
     *                     Si se indica <code>null</code> se acepta cualquier titular. */
    public RFC2254CertificateFilter(final String subjectFilter, final String issuerFilter) {
        this(subjectFilter, issuerFilter, false);
    }

    @Override
	public String[] matches(final String[] aliases, final KeyStoreManager ksm) {
    	if (!this.recurseIssuers) {
    		return super.matches(aliases, ksm);
    	}
        final List<String> filteredAliases = new ArrayList<>();
        for (final String alias : aliases) {
        	if (filterSubjectByRFC2254(this.rfc2254SubjectFilter, ksm.getCertificate(alias)) && matchesIssuersRecursivelly(ksm.getCertificateChain(alias))) {
                filteredAliases.add(alias);
            }
        }
        return filteredAliases.toArray(new String[filteredAliases.size()]);
    }

    @Override
	public boolean matches(final X509Certificate cert) {
    	if (this.recurseIssuers) {
    		LOGGER.warning(
				"No se dispone de la cadena de certificacion completa, el filtro solo se aplicara al emisor inmediato" //$NON-NLS-1$
			);
    	}
        return filterSubjectByRFC2254(this.rfc2254SubjectFilter, cert)
            && filterIssuerByRFC2254(this.rfc2254IssuerFilter, cert);
    }

    private boolean matchesIssuersRecursivelly(final X509Certificate[] certs) {
    	if (certs == null) {
    		return false;
    	}
    	for (final X509Certificate cert : certs) {
    		if (filterSubjectByRFC2254(this.rfc2254IssuerFilter, cert)) {
    			return true;
    		}
    	}
    	return false;
    }

    private static boolean filterSubjectByRFC2254(final String filter, final X509Certificate cert) {
        if (cert == null || filter == null) {
            return true;
        }
        return filterRFC2254(filter, cert.getSubjectDN().toString());
    }

    private static boolean filterIssuerByRFC2254(final String filter, final X509Certificate cert) {
        if (cert == null || filter == null) {
            return true;
        }
        return filterRFC2254(filter, cert.getIssuerDN().toString());
    }

    /** Indica si un nombre LDAP se ajusta a los requisitos de un filtro.
     * @param f Filtro seg&uacute;n la RFC2254.
     * @param name Nombre LDAP al que se debe aplicar el filtro.
     * @return <code>true</code> si el nombre LDAP es nulo o se adec&uacute;a al
     *         filtro o este &uacute;ltimo es nulo, <code>false</code> en caso
     *         contrario */
    private static boolean filterRFC2254(final String f, final String name) {
        try {
            return filterRFC2254(f, new LdapName(name));
        }
        catch (final Exception e) {
            LOGGER.log(
            	Level.WARNING,
        		"No ha sido posible filtrar el certificado (filtro: '" + f + "', nombre: '" + name + "'), no se eliminara del listado: "  + e, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        		e
    		);
            return true;
        }
    }

    /** Indica si un nombres LDAP se ajusta a los requisitos de un filtro.
     * @param f Filtro seg&uacute;n la RFC2254.
     * @param name Nombre LDAP al que se debe aplicar el filtro.
     * @return <code>true</code> si el nombre LDAP es nulo o se adec&uacute;a al
     *         filtro o este &uacute;ltimo es nulo, <code>false</code> en caso
     *         contrario */
	private static boolean filterRFC2254(final String f, final LdapName name) {
        if (f == null || name == null) {
            return true;
        }
        try {
            final List<Rdn> rdns = name.getRdns();
            if (rdns == null || rdns.isEmpty()) {
                LOGGER.warning(
            		"El nombre proporcionado para filtrar no contiene atributos, no se mostrara el certificado en el listado" //$NON-NLS-1$
        		);
                return false;
            }
            final Attributes attrs = new BasicAttributes(true);
            for (final Rdn rdn : rdns) {
                attrs.put(rdn.getType(), rdn.getValue());
            }
            return new SearchFilter(f).check(attrs);
        }
        catch (final Exception e) {
            LOGGER.log(
        		Level.WARNING,
        		"No ha sido posible filtrar el certificado (filtro: '" + f + "', nombre: '" + name + "'), no se eliminara del listado: " + e, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        		e
    		);
            return true;
        }
    }

}

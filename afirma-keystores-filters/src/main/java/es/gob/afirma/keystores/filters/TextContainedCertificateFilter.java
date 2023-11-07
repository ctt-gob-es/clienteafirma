/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.filters;

import java.security.cert.X509Certificate;

import es.gob.afirma.keystores.CertificateFilter;

/** Filtro de certificados que busca cadenas de texto concretas dentro del Principal
 * del Subject y/o Issuer de un certificado. Las comprobaciones realizadas no tienen
 * en cuenta may&uacute;sculas y min&uacute;sculas. */
public final class TextContainedCertificateFilter extends CertificateFilter {

	private final String subjectTexts[];
	private final String issuerTexts[];

    /** Construye el filtro de certificados.
     * @param subjectTexts Listado de cadenas de texto que deben aparecer en el Principal del Subject del certificado.
     * @param issuerTexts Listado de cadenas de texto que deben aparecer en el Principal del Issuer del certificado. */
    public TextContainedCertificateFilter(final String[] subjectTexts, final String[] issuerTexts) {
        if (subjectTexts == null && issuerTexts == null) {
            throw new IllegalArgumentException("Al menos uno de los criterios de filtrado debe no ser nulo"); //$NON-NLS-1$
        }
        this.subjectTexts = subjectTexts != null ? subjectTexts.clone() : null;
        this.issuerTexts = issuerTexts != null ? issuerTexts.clone() : null;
    }

    @Override
	public boolean matches(final X509Certificate cert) {

    	if (this.subjectTexts != null) {
    		final String subjectPrincipal = cert.getSubjectX500Principal().toString().toLowerCase();
    		for (final String fragment : this.subjectTexts) {
    			if (fragment != null && !subjectPrincipal.contains(fragment.toLowerCase())) {
    				return false;
    			}
    		}
    	}

    	if (this.issuerTexts != null) {
    		final String issuerPrincipal = cert.getIssuerX500Principal().toString().toLowerCase();
    		for (final String fragment : this.issuerTexts) {
    			if (fragment != null && !issuerPrincipal.contains(fragment.toLowerCase())) {
    				return false;
    			}
    		}
    	}

        return true;
    }


}

/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation; 
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either versión 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.miniapplet;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import es.gob.afirma.keystores.main.filters.CertificateFilter;
import es.gob.afirma.miniapplet.keystores.filters.SSLFilter;
import es.gob.afirma.miniapplet.keystores.filters.SignatureDNIeFilter;

/**
 * Identifica y obtiene los filtros de certificados definidos en las propiedades
 * de una operaci&oacute;n de firma/multifirma electr&oacute;nica.
 *  
 * @author Carlos Gamuci Mill&aacute;n
 */
final class CertFilterManager {

	private static final String FILTER_PREFIX_KEY = "filter"; //$NON-NLS-1$
	private static final String FILTER_TYPE_DNIE = "dnie:"; //$NON-NLS-1$
	private static final String FILTER_TYPE_SSL = "ssl:"; //$NON-NLS-1$
	
	private boolean mandatoryCertificate = false;
	
	private final List<CertificateFilter> filters = new ArrayList<CertificateFilter>();
	
	/**
	 * Identifica los filtros que deben aplicarse sobre una serie de certificados para
	 * comprobar cuales de ellos se ajustan a nuestra necesidades. 
	 * @param propertyFilters Listado de propiedades entre las que identificar las que
	 * establecen los criterios de filtrado.
	 */
	CertFilterManager(final Properties propertyFilters) {
		
		final String filterValue = propertyFilters.getProperty(FILTER_PREFIX_KEY);
		if (filterValue == null) {
			return;
		}
		
		final CertificateFilter filter;
		if (filterValue.toLowerCase().startsWith(FILTER_TYPE_DNIE)) {
			filter = new SignatureDNIeFilter();
		} 
		else if (filterValue.toLowerCase().startsWith(FILTER_TYPE_SSL)) {
			filter = new SSLFilter(filterValue.substring(FILTER_TYPE_SSL.length()));
		} 
		else {
			return;
		}
		
		this.filters.add(filter);
		this.mandatoryCertificate = true;
	}
	
	/**
	 * Devuelve la lista de certificados definidos.
	 * @return Listado de certificados.
	 */
	List<CertificateFilter> getFilters() {
		return (this.filters != null ? new ArrayList<CertificateFilter>(this.filters) : null);
	}
	
	/**
	 * Indica si se debe seleccionar autom&aacute;ticamente un certificado si es el &uacute;nico que
	 * cumple los filtros. 
	 * @return {@code true} si debe seleccionarse autom&aacute;ticamente el &uacute;nico certificado
	 * que supera el filtrado, {@code false} en caso contrario.
	 */
	boolean isMandatoryCertificate() {
		return this.mandatoryCertificate;
	}
	
	
}

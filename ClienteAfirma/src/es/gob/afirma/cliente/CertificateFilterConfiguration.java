/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */

package es.gob.afirma.cliente;


/**
 * Clase para almacenar la configuraci&oacute;n para el filtrado de
 * certificados en el di&aacute;logo de selecci&oacute;n.
 */
class CertificateFilterConfiguration {

	/** 
	 * Filtro aplicado a los Principal de los certificados. 
	 * @deprecated 
	 */
	@Deprecated
	private es.gob.afirma.cliente.AOCertFilter nameFilter = null; 
	
	/** El KeyUsage m&iacute;nimos que debe cumplir el certificado. */
	private Boolean[] keyUsageFilter = null;
	
	 /** Indicador para que s&oacute;lo se muestren los certificados sin caducar. */ 
	private boolean showExpiratedCertificates = true;
	
	/** Filtro RFC2254 para el emisor del certificado. */
	private String rfc2254IssuerFilter = null;
	
	/** Filtro RFC2254 para el emisor del certificado. */
	private String rfc2254SubjectFilter = null;
	
	/** Indicador para que s&oacute;lo se muestre 1 certificado. */
	private boolean mandatoryCert = false;


	/**
	 * Inicializa el filtro de certificados. 
	 */
	void initialize() {
		nameFilter = null; 
		keyUsageFilter = null;
		rfc2254IssuerFilter = null;
		rfc2254SubjectFilter = null;
		showExpiratedCertificates = true;
		mandatoryCert = false;
	}
	
	@Deprecated
	es.gob.afirma.cliente.AOCertFilter getNameFilter() {
		return nameFilter;
	}
	
	@Deprecated
	void setNameFilter(es.gob.afirma.cliente.AOCertFilter nameFilter) {
		this.nameFilter = nameFilter;
	}

	Boolean[] getKeyUsageFilter() {
		return keyUsageFilter;
	}

	void setKeyUsageFilter(Boolean[] keyUsageFilter) {
		this.keyUsageFilter = keyUsageFilter;
	}

	boolean isShowExpiratedCertificates() {
		return showExpiratedCertificates;
	}

	void setShowExpiratedCertificates(boolean showExpiratedCertificates) {
		this.showExpiratedCertificates = showExpiratedCertificates;
	}

	String getRfc2254IssuerFilter() {
		return rfc2254IssuerFilter;
	}

	void setRfc2254IssuerFilter(String rfc2254IssuerFilter) {
		this.rfc2254IssuerFilter = rfc2254IssuerFilter;
	}

	String getRfc2254SubjectFilter() {
		return rfc2254SubjectFilter;
	}

	void setRfc2254SubjectFilter(String rfc2254SubjectFilter) {
		this.rfc2254SubjectFilter = rfc2254SubjectFilter;
	}

	boolean isMandatoryCert() {
		return mandatoryCert;
	}

	void setMandatoryCert(boolean mandatoryCert) {
		this.mandatoryCert = mandatoryCert;
	}
}

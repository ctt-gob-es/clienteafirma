/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.cliente;

import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import es.gob.afirma.cliente.utilidades.exp.ComplexCondition;

/**
 * Filtro de certificados. Se encarga de filtrar los certificados que se le
 * indiquen en base a un criterio basado en expresiones regulares.
 * 
 * @deprecated
 */
@Deprecated
final class AOCertFilter {

	/** Expresi&oacute;n del filtro de certificados. **/
	private String filter = null;

	/**
	 * Crea el filtro de certificados.
	 * 
	 * @param filter
	 *            Expresi&oacute;n del filtro.
	 */
	AOCertFilter(final String filter) {
		this.setFilter(filter);
	}

	/**
	 * Establece el filtro de los certificados.
	 * 
	 * @param filter
	 *            Expresi&oacute;n del filtro.
	 */
	void setFilter(String filter) {
		if (filter != null && filter.trim().length() < 1)
			filter = null; // Un filtro vacio no es filtro
		this.filter = filter;
	}

	/**
	 * Obtiene el filtro actual para los certificados.
	 * 
	 * @return Filtro actualmente establecido para los certificados
	 */
	String getFilter() {
		return this.filter;
	}

	/**
	 * Filtra los certificados indicados para obtener s&oacute;lo aquellos que
	 * se ajustan a la expresi&oacute;n del filtro.
	 * 
	 * @param certs
	 *            Listado de certificados sobre el que se debe aplicar el
	 *            filtro.
	 * @return Certificados que se ajustan al filtro
	 */
	X509Certificate[] filter(final X509Certificate[] certs) {

		if (certs == null) {
			Logger.getLogger("es.gob.afirma")
					.warning(
							"Se ha introducido nulo en el filtro de certificados, se devolvera una lista vacia de certificados");
			return new X509Certificate[0];
		}

		// Si no hay filtro, se entiende que todos los certificados de entrada
		// cumplen la condicion de filtrado
		if (this.filter == null)
			return certs;

		// Realizamos el filtrado y, en caso de error, devolvemos todos los
		// certificados
		try {
			return new ComplexCondition(this.filter).eval(certs);
		} 
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").warning(
				"Error al aplicar el filtro de certificados, se aceptaran todos los certificados: " + e
			);
		}
		return certs;
	}
}

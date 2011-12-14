/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.keystores.main.filters;

import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.List;

import es.gob.afirma.keystores.main.common.AOKeyStoreManager;

/** Filtro para certificados. Debe autocontener toda la l&oacute;gica que indique si un
 * certificado cumple o no las condiciones del filtro.
 * El establecimiento de los datos encesarios para las condiciones de filtrado queda fuera
 * del interfaz y debe ser espec&iacute;fico para cada implementaci&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public abstract class CertificateFilter {
    
    /** Comprueba si un certificado se adec&uacute;a al filtro
     * @param cert Certificado a comprobar
     * @return <code>true</code> si el certificado se adec&uacute;a al filtro, <code>false</code> en caso contrario
     */
    public abstract boolean matches(final X509Certificate cert);
    
    /** Obtiene los certificados de un listado que cumplen con un determinado criterio. Por defecto,
     * el establecido en el m&eacute;todo {@link #matches(X509Certificate)}. 
     * @param certs Listado de certificados.
     * @return Certificados que cumplen el criterio.
     */
    public X509Certificate[] matches(final X509Certificate[] certs) {
    	final List<X509Certificate> filteredCerts = new ArrayList<X509Certificate>();
    	for (final X509Certificate cert : certs) {
    		if (matches(cert)) {
    			filteredCerts.add(cert);
    		}
    	}
    	return filteredCerts.toArray(new X509Certificate[filteredCerts.size()]);
    }
    
    /** Obtiene los alias de certificados de un listado que cumplen con un determinado criterio. Por defecto,
     * el establecido en el m&eacute;todo {@link #matches(X509Certificate)}. 
     * @param aliases Listado de alias de certificados.
     * @param ksm <code>AOKeyStoreManager</code> que contiene los certificados cuyos
     *            alias se indican.
     * @return Alias de certificados que cumplen el criterio. */
    public String[] matches(final String[] aliases, final AOKeyStoreManager ksm) {
        final List<String> filteredAliases = new ArrayList<String>();
        for (final String alias : aliases) {
            if(matches(ksm.getCertificate(alias))) {
                filteredAliases.add(alias);
            }
        }
        return filteredAliases.toArray(new String[filteredAliases.size()]);
    }
}

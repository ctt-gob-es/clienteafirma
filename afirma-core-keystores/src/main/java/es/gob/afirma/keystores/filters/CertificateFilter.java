/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.keystores.filters;

import java.security.cert.X509Certificate;

/** Filtro para certificados. Debe autocontener toda la l&oacute;gica que indique si un
 * certificado cumple o no las condiciones del filtro.
 * El establecimiento de los datos encesarios para las condiciones de filtrado queda fuera
 * del interfaz y debe ser espec&iacute;fico para cada implementaci&oacute;n.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public interface CertificateFilter {
    
    /** Comprueba si un certificado se adec&uacute;a al filtro
     * @param cert Certificado a comprobar
     * @return <code>true</code> si el certificado se adec&uacute;a al filtro, <code>false</code> en caso contrario
     */
    boolean matches(final X509Certificate cert);
}

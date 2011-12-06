/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.keystores.filters.old;

import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import es.gob.afirma.core.misc.AOUtil;
import es.gob.afirma.keystores.main.filters.CertificateFilter;

/**
 * Filtro de certificados. Se encarga de filtrar los certificados que se le indiquen en base a un
 * criterio basado en expresiones regulares.
 * @deprecated
 */
@Deprecated
public class OldFilter extends CertificateFilter {
	
	/** Expresi&oacute;n del filtro de certificados. **/
	private final String filter;
	
	/**
	 * Crea el filtro de certificados.
	 * @param f Expresi&oacute;n del filtro.
	 */
	public OldFilter(String f) {
	    this.filter = f;
	}


    @Override
    public boolean matches(X509Certificate cert) {
        if (cert == null) {
            return false;
        }
        if (this.filter == null) {
            return true;
        }
        try {
            return new ComplexCondition(this.filter).eval(cert);
        }
        catch(final Exception e) {
            Logger.getLogger("es.gob.afirma").severe("Ha fallado la aplicacion del filtro (filtro='" + this.filter + "', certificado='" + AOUtil.getCN(cert) + "', no se filtrara el certificado: " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
            return true;
        }
    }
}

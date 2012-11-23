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
import java.util.ArrayList;
import java.util.Collection;

import es.gob.afirma.core.AOException;

/**
 * @deprecated Usar filtros compatibles RFC2254
 */
@Deprecated
abstract class AClause implements Clause {

    private Clause[] clauses;
    private Nexus[] nexus;

    protected void setClauses(final Clause[] c) {
        this.clauses = c.clone();
    }

    protected void setNexus(final Nexus[] n) {
        this.nexus = n.clone();
    }

    @Override
	public boolean eval(final X509Certificate cert) throws AOException {
        boolean eval;
        eval = this.clauses[0].eval(cert);
        for (int i = 0; i < this.nexus.length; i++) {
            final boolean aux = this.clauses[i + 1].eval(cert);
            eval = this.nexus[i].eval(eval, aux);
        }

        return eval;
    }

    @Override
	public X509Certificate[] eval(final X509Certificate[] certs) throws AOException {
        final Collection<X509Certificate> c = new ArrayList<X509Certificate>();
        for (final X509Certificate cert : certs) {
            if (eval(cert))
            {
                c.add(cert);
            }
        }
        return c.toArray(new X509Certificate[c.size()]);
    }

}

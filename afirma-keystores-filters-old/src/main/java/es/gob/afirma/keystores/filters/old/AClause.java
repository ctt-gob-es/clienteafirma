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
    protected Clause[] clauses;

    protected Nexus[] nexus;

    public boolean eval(X509Certificate cert) throws AOException {
        boolean eval;
        eval = this.clauses[0].eval(cert);
        for (int i = 0; i < this.nexus.length; i++) {
            boolean aux = this.clauses[i + 1].eval(cert);
            eval = this.nexus[i].eval(eval, aux);
        }

        return eval;
    }

    public X509Certificate[] eval(X509Certificate[] certs) throws AOException {
        Collection<X509Certificate> c = new ArrayList<X509Certificate>();
        for (int i = 0; i < certs.length; i++)
        {
            if (eval(certs[i]))
            {
                c.add(certs[i]);
            }
        }
        return c.toArray(new X509Certificate[c.size()]);
    }

}

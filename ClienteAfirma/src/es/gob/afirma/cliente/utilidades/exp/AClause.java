package es.gob.afirma.cliente.utilidades.exp;

import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Collection;

import es.gob.afirma.exceptions.AOException;

/** 
 * @deprecated Usar filtros compatibles RFC2254
 */
@Deprecated
abstract class AClause implements Clause
{
    protected Clause[] clauses;

    protected Nexus[] nexus;

    public boolean eval(X509Certificate cert) throws AOException
    {
        boolean eval;
        eval = clauses[0].eval(cert);
        for (int i = 0; i < nexus.length; i++)
        {
            boolean aux = clauses[i + 1].eval(cert);
            eval = nexus[i].eval(eval, aux);
        }

        return eval;
    }

    public X509Certificate[] eval(X509Certificate[] certs) throws AOException
    {
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

package es.gob.afirma.cliente.utilidades.exp;

import java.security.cert.X509Certificate;

import es.gob.afirma.exceptions.AOException;

/** 
 * @deprecated Usar filtros compatibles RFC2254
 */
@Deprecated
interface Clause
{
    public boolean eval(X509Certificate cert) throws AOException;

    public X509Certificate[] eval(X509Certificate[] certs) throws AOException;
}

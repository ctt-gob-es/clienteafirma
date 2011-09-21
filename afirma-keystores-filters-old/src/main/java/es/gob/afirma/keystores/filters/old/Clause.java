package es.gob.afirma.keystores.filters.old;

import java.security.cert.X509Certificate;

import es.gob.afirma.core.AOException;


/** @deprecated */
@Deprecated
interface Clause {
    boolean eval(X509Certificate cert) throws AOException;
    X509Certificate[] eval(X509Certificate[] certs) throws AOException;
}

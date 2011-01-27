/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


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

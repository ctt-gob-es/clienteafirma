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

import java.util.regex.Pattern;

/** 
 * @deprecated Usar filtros compatibles RFC2254
 */
@Deprecated
interface Operator
{
    public static Operator EQ = new Operator()
    {
        public boolean eval(Object o1, Object o2)
        {
            return o1.equals(o2);
        }
    };

    public static Operator MATCHES = new Operator()
    {
        public boolean eval(Object str, Object pattern)
        {
            boolean matches;
            matches = Pattern.matches((String) pattern, (String) str);
            return matches;
        }
    };

    public static Operator NOT_MATCHES = new Operator()
    {
        public boolean eval(Object str, Object pattern)
        {
            boolean matches;
            matches = Pattern.matches((String) pattern, (String) str);
            return !matches;
        }
    };

    public boolean eval(Object o1, Object o2);
}

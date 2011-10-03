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

import java.util.regex.Pattern;

/** @deprecated */
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

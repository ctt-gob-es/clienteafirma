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
    Operator EQ = new Operator()
    {
    	/** Evalua la operaci&oacute;n.
    	 * @param o1 Primer operando
    	 * @param o2 Segundo operando
    	 * @return Resultado de la evaluaci&oacute;n */
        public boolean eval(final Object o1, final Object o2)
        {
            return o1.equals(o2);
        }
    };

    Operator MATCHES = new Operator()
    {
    	/** Evalua la operaci&oacute;n.
    	 * @param str Cadena
    	 * @param pattern Patr&oacute;n
    	 * @return Resultado de la evaluaci&oacute;n */
        public boolean eval(final Object str, final Object pattern)
        {
            boolean matches;
            matches = Pattern.matches((String) pattern, (String) str);
            return matches;
        }
    };

    Operator NOT_MATCHES = new Operator()
    {
    	/** Evalua la operaci&oacute;n.
    	 * @param str Cadena
    	 * @param pattern Patr&oacute;n
    	 * @return Resultado de la evaluaci&oacute;n */
        public boolean eval(final Object str, final Object pattern)
        {
            boolean matches;
            matches = Pattern.matches((String) pattern, (String) str);
            return !matches;
        }
    };

    /** Evalua la operaci&oacute;n.
	 * @param o1 Primer operando
	 * @param o2 Segundo operando
	 * @return Resultado de la evaluaci&oacute;n */
    boolean eval(Object o1, Object o2);
}

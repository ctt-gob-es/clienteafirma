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

/** 
 * @deprecated Usar filtros compatibles RFC2254
 */
@Deprecated
interface Nexus
{
    public static final Nexus AND = new Nexus()
    {
        public boolean eval(boolean b1, boolean b2)
        {
            return b1 && b2;
        }
    };

    public static final Nexus OR = new Nexus()
    {
        public boolean eval(boolean b1, boolean b2)
        {
            return b1 || b2;
        }
    };

    public boolean eval(boolean b1, boolean b2);
}

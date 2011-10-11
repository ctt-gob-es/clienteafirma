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

/** @deprecated  */
@Deprecated
interface Nexus {
    static final Nexus AND = new Nexus() {
        public boolean eval(boolean b1, boolean b2) {
            return b1 && b2;
        }
    };

    static final Nexus OR = new Nexus() {
        public boolean eval(boolean b1, boolean b2) {
            return b1 || b2;
        }
    };

    boolean eval(boolean b1, boolean b2);
}

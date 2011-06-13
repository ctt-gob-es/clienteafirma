/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este 
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.standalone.dnie;

/** Excepci&oacute;n en la gesti&oacute;n de almac&eacute;n DNIe v&iacute;a PKCS#11 y JSR-268.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class DNIeManagerException extends Exception {

    private static final long serialVersionUID = 9198656551956236883L;

    DNIeManagerException(final String msg, final Throwable t) {
        super(msg, t);
    }

    DNIeManagerException(final String msg) {
        super(msg);
    }

}

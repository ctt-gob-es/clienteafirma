/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.applet;

import es.gob.afirma.core.misc.Platform;

/** Inicializa la variable est&aacute;tica que mantiene la configuraci&oacute;n
 * de la plataforma de ejecuci&oacute;n. */
public final class InitializePlatformAction extends BasicPrivilegedAction<Void, Void> {

    public Void run() {
        Platform.init();
        return null;
    }

    /** Establece el UserAgent del navegador actual. Esta configuraci&oacute;n no
     * puede obtenerse directamente desde el applet.
     * @param userAgent
     *        Denominaci&oacute; del navegador. */
    public void setUserAgent(final String userAgent) {
        Platform.setUserAgent(userAgent);
    }
}

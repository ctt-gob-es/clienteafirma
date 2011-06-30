package es.gob.afirma.cliente.actions;

import es.gob.afirma.misc.Platform;

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

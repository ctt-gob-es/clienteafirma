package es.gob.afirma.crypto.handwritten.data;

import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;

/** Manejador del protocolo <code>data:</code> (Base64) para las imagenes dentro de los HTML.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s. */
public final class Handler extends URLStreamHandler {

    @Override
    protected URLConnection openConnection(final URL u) {
        return new DataConnection(u);
    }

    /** instala el manejador del protocolo <code>data:</code> (Base64) para las imagenes dentro de los HTML. */
    public static void install() {
    	final String pkgName = Handler.class.getPackage().getName();
        final String pkg = pkgName.substring(0, pkgName.lastIndexOf('.'));
        String protocolHandlers = System.getProperty("java.protocol.handler.pkgs", ""); //$NON-NLS-1$ //$NON-NLS-2$
        if (!protocolHandlers.contains(pkg)) {
            if (!protocolHandlers.isEmpty()) {
                protocolHandlers += "|"; //$NON-NLS-1$
            }
            protocolHandlers += pkg;
            System.setProperty("java.protocol.handler.pkgs", protocolHandlers); //$NON-NLS-1$
        }
    }
}
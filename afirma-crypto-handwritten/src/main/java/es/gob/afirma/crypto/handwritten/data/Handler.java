package es.gob.afirma.crypto.handwritten.data;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;

public final class Handler extends URLStreamHandler {

    @Override
    protected URLConnection openConnection(final URL u) throws IOException {
    	System.out.println("OPEN_CONNECTION DATA");
        return new DataConnection(u);
    }

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
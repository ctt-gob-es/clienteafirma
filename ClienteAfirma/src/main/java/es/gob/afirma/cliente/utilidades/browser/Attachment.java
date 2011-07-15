/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.cliente.utilidades.browser;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.logging.Logger;

class Attachment {
    final String url;

    // public final byte[] bytes;

    Attachment(final String url) {
        this.url = url;
        // this.bytes= bytes;
    }

    String getName() {
        final int p1 = url.lastIndexOf(File.pathSeparatorChar);
        return p1 < url.length() ? url.substring(p1 + 1) : "";
    }

    File getFile() {
        return new File(url);
    }

    GraphicalFileInputStream getContentInputStream() throws FileNotFoundException {
        Logger.getLogger("es.gob.afirma").info(url);
        return new GraphicalFileInputStream(new File(url));
    }
}

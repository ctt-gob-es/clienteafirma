/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.applet.old.websign;

import java.io.File;
import java.io.FileNotFoundException;

final class Attachment {
    
    private final String url;

    Attachment(final String url) {
        this.url = url;
        // this.bytes= bytes;
    }
    
    String getURL() {
        return this.url;
    }

    String getName() {
        final int p1 = this.url.lastIndexOf(File.pathSeparatorChar);
        return p1 < this.url.length() ? this.url.substring(p1 + 1) : ""; //$NON-NLS-1$
    }

    File getFile() {
        return new File(this.url);
    }

    GraphicalFileInputStream getContentInputStream() throws FileNotFoundException {
        return new GraphicalFileInputStream(new File(this.url));
    }
}

/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.cliente.utilidades.browser;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

import es.gob.afirma.cliente.interfaz.ProgressWindow;

/** <code>InputStream</code> con di&aacute;logo gr&aacute;fico de espera en caso
 * de operaciones que tarden mucho. */
final class GraphicalFileInputStream extends FileInputStream {
    private final ProgressWindow progressWindow;
    private final File file;

    /** Construye el <code>InputStream</code> con di&aacute;logo gr&aacute;fico
     * de espera.
     * @param file
     *        Fichero sobre el cual se abre el <code>InputStream</code>
     * @throws FileNotFoundException
     *         Si no se encuentra el fichero */
    GraphicalFileInputStream(final File file) throws FileNotFoundException {
        super(file);
        this.file = file;
        this.progressWindow = new ProgressWindow("Leyendo: " + file.getName(), "", file.length());
        progressWindow.setVisible(true);
    }

    @Override
    public void close() {
        progressWindow.dispose();
    }

    /** Obtiene el tama&ntilde;o del fichero.
     * @return Tama&ntilde;o del fichero */
    long getFileSize() {
        return file.length();
    }

    @Override
    public int read() throws IOException {
        final int c = super.read();
        if (c > 0) {
            progressWindow.inc(1);
        }
        return c;
    }

    @Override
    public int read(final byte[] buff) throws IOException {
        final int c = super.read(buff);
        if (c > 0) {
            progressWindow.inc(c);
        }
        return c;
    }

    @Override
    public int read(final byte[] buff, final int off, final int len) throws IOException {
        final int c = super.read(buff, off, len);
        if (c > 0) {
            progressWindow.inc(c);
        }
        return c;
    }
}

package es.gob.afirma.signers.ooxml;

import java.io.FilterInputStream;
import java.io.InputStream;

final class NoCloseInputStream extends FilterInputStream {

    NoCloseInputStream(final InputStream proxy) {
        super(proxy);
    }

    @Override
    public void close() {
        // Se ignoran los errores al cerrar
    }
}

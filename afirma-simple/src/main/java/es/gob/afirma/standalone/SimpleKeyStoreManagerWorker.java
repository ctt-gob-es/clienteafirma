/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.standalone;

import java.awt.Component;
import java.util.logging.Logger;

import javax.swing.SwingWorker;

import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerException;

final class SimpleKeyStoreManagerWorker extends SwingWorker<Void, String> {

    private final SimpleAfirma simpleAFirma;
    private final Component parent;
    private final boolean dnie;

    private AOKeyStoreManager ksm;

    SimpleKeyStoreManagerWorker(final SimpleAfirma saf, final Component p, final boolean dni) {
        this.simpleAFirma = saf;
        this.parent = p;
        this.dnie = dni;
    }

    @Override
    protected Void doInBackground() throws AOKeyStoreManagerException {
        if (SimpleAfirma.DEBUG) {
            Logger.getLogger("es.gob.afirma").info("Solicitado establecimiento de KeyStore (DNIe=" + this.dnie + ")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
        this.ksm = SimpleKeyStoreManager.getKeyStore(this.dnie, this.parent);
        return null;
    }

    @Override
    protected void done() {
        if (this.simpleAFirma != null) {
            this.simpleAFirma.setKeyStoreManager(this.ksm);
        }
    }

}

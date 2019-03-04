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
    private final boolean forced;

    private AOKeyStoreManager ksm;

    SimpleKeyStoreManagerWorker(final SimpleAfirma saf, final Component p, final boolean dni, final boolean forced) {
        this.simpleAFirma = saf;
        this.parent = p;
        this.dnie = dni;
        this.forced = forced;
    }

    @Override
    protected Void doInBackground() throws AOKeyStoreManagerException {
        if (SimpleAfirma.DEBUG) {
            Logger.getLogger("es.gob.afirma").info("Solicitado establecimiento de KeyStore (DNIe=" + this.dnie + ")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }

        try {
        	this.ksm = SimpleKeyStoreManager.getKeyStore(this.dnie, this.forced, this.parent);
        }
        catch (final NoDnieFoundException e) {
        	// No se pudo cargar el DNIe y era obligatorio su uso. En ese caso, no cargamos ningun almacen
        	// y sera la aplicacion la que se encargue de hacerlo. Este es un caso tipico de Linux en donde
        	// el almacen se precarga y el intentar cargarlo a posteriori falla, por lo que se debe utilizar
        	// la version anteriormente cargada
        	this.ksm = null;
        }
        return null;
    }

    @Override
    protected void done() {
        if (this.simpleAFirma != null && this.ksm != null) {
            this.simpleAFirma.setKeyStoreManager(this.ksm);
        }
    }

}

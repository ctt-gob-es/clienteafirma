/*******************************************************************************
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde http://forja-ctt.administracionelectronica.gob.es/
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 ******************************************************************************/

package es.gob.afirma.standalone;

import java.awt.Component;

import javax.swing.SwingWorker;

import es.gob.afirma.keystores.common.AOKeyStoreManager;

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
    protected Void doInBackground() throws Exception {
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

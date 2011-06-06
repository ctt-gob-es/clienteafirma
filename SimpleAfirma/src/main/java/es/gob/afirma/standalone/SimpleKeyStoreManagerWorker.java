package es.gob.afirma.standalone;

import java.awt.Component;

import javax.swing.SwingWorker;

import es.gob.afirma.keystores.AOKeyStoreManager;

final class SimpleKeyStoreManagerWorker extends SwingWorker<Void, String> {

	private final SimpleAfirma simpleAFirma;
	private final Component parent;
	private final boolean dnie;
	
	private AOKeyStoreManager ksm;
	
	SimpleKeyStoreManagerWorker(final SimpleAfirma saf, final Component p, final boolean dni) {
		simpleAFirma = saf;
		parent = p;
		dnie = dni;
	}
	
	@Override
	protected Void doInBackground() throws Exception {
		ksm = SimpleKeyStoreManager.getKeyStore(dnie, parent);
		return null;
	}

	@Override
    protected void done() {
		if (simpleAFirma != null) simpleAFirma.setKeyStoreManager(ksm);
	}
	
}

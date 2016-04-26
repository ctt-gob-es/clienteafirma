package es.gob.afirma.standalone.ui;

import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_KEYSTORE_ALIAS_ONLY_CERTS;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_KEYSTORE_CYPH_ONLY_CERTS;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.KeyListener;

import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import es.gob.afirma.standalone.PreferencesManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

final class PreferencesPanelKeyStores extends JPanel {

	private static final long serialVersionUID = 3255071607793273334L;

	JCheckBox onlySignature = new JCheckBox(
		SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.0"), //$NON-NLS-1$
		Boolean.parseBoolean(PreferencesManager.get(PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS, "false")) //$NON-NLS-1$
	);

	JCheckBox onlyEncipherment = new JCheckBox(
		SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.1"), //$NON-NLS-1$
		Boolean.parseBoolean(PreferencesManager.get(PREFERENCE_KEYSTORE_CYPH_ONLY_CERTS, "false")) //$NON-NLS-1$
	);

	JCheckBox onlyAlias = new JCheckBox(
		SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.4"), //$NON-NLS-1$
		Boolean.parseBoolean(PreferencesManager.get(PREFERENCE_KEYSTORE_ALIAS_ONLY_CERTS, "false")) //$NON-NLS-1$
	);

	public PreferencesPanelKeyStores(final KeyListener keyListener, final ModificationListener modificationListener) {
    	SwingUtilities.invokeLater(() ->  createUI(keyListener, modificationListener));
	}

	void createUI(final KeyListener keyListener, final ModificationListener modificationListener) {

        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;

	    this.onlySignature.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.2") //$NON-NLS-1$
		);
	    this.onlySignature.setMnemonic('i');
	    this.onlySignature.addItemListener(modificationListener);
	    this.onlySignature.addKeyListener(keyListener);
        add(this.onlySignature, c);

        c.gridy++;

	    this.onlyEncipherment.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.3") //$NON-NLS-1$
		);
	    this.onlyEncipherment.setMnemonic('r');
	    this.onlyEncipherment.addItemListener(modificationListener);
	    this.onlyEncipherment.addKeyListener(keyListener);
        add(this.onlyEncipherment, c);

        c.gridy++;

	    this.onlyAlias.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.5") //$NON-NLS-1$
		);
	    this.onlyAlias.setMnemonic('s');
	    this.onlyAlias.addItemListener(modificationListener);
	    this.onlyAlias.addKeyListener(keyListener);
        add(this.onlyAlias, c);

	}

	void savePreferences() {
		PreferencesManager.putBoolean(PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS, this.onlySignature.isSelected());
		PreferencesManager.putBoolean(PREFERENCE_KEYSTORE_CYPH_ONLY_CERTS, this.onlyEncipherment.isSelected());
		PreferencesManager.putBoolean(PREFERENCE_KEYSTORE_ALIAS_ONLY_CERTS, this.onlyAlias.isSelected());
	}

}

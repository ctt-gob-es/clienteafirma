package es.gob.afirma.standalone.ui.preferences;

import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_KEYSTORE_ALIAS_ONLY_CERTS;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_KEYSTORE_DEFAULT_STORE;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.KeyListener;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.keystores.AOKeyStore;
import es.gob.afirma.keystores.AOKeyStoreDialog;
import es.gob.afirma.keystores.AOKeyStoreManager;
import es.gob.afirma.keystores.AOKeyStoreManagerFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.SimpleKeyStoreManager;
import es.gob.afirma.ui.core.jse.certificateselection.CertificateSelectionDialog;

final class PreferencesPanelKeyStores extends JPanel {

	private static final long serialVersionUID = 3255071607793273334L;

	private final JCheckBox onlySignature = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.0")); //$NON-NLS-1$

	private final JCheckBox onlyAlias = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.4")); //$NON-NLS-1$

	private final JComboBox<AOKeyStore> defaultStore;
	AOKeyStore getDefaultStore() {
		return this.defaultStore.getItemAt(this.defaultStore.getSelectedIndex());
	}

	private final JButton contentButton = new JButton(
		SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.9") //$NON-NLS-1$
	);

	JButton getContentButton() {
		return this.contentButton;
	}

	PreferencesPanelKeyStores(final KeyListener keyListener,
							  final ModificationListener modificationListener,
							  final boolean unprotected) {

		// Obtenemos primero la lista de almacenes disponibles para asignarla al JComboBox
		final AOKeyStore[] defaultStores;
		final List<AOKeyStore> stores = new ArrayList<>();
		final Platform.OS os = Platform.getOS();
		if (Platform.OS.WINDOWS.equals(os)) {
			stores.add(AOKeyStore.WINDOWS);
		}
		else if (Platform.OS.MACOSX.equals(os)) {
			stores.add(AOKeyStore.APPLE);
		}
		else {
			stores.add(AOKeyStore.SHARED_NSS);
		}
		if (SimpleKeyStoreManager.isFirefoxAvailable()) {
			stores.add(AOKeyStore.MOZ_UNI);
		}
		defaultStores = stores.toArray(new AOKeyStore[0]);
		this.defaultStore = new JComboBox<>(defaultStores);

		// Creamos el UI
		createUI(keyListener, modificationListener);
	}

	void createUI(final KeyListener keyListener,
				  final ModificationListener modificationListener) {

        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;

        loadPreferences();
        final JPanel keysFilerPanel = new JPanel(new GridBagLayout());
        keysFilerPanel.setBorder(
			BorderFactory.createTitledBorder(
				BorderFactory.createTitledBorder(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.6")) //$NON-NLS-1$
			)
		);

		final GridBagConstraints kfc = new GridBagConstraints();
		kfc.fill = GridBagConstraints.HORIZONTAL;
		kfc.weightx = 1.0;
		kfc.gridy = 0;
		kfc.insets = new Insets(5, 7, 5, 7);

	    this.onlySignature.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.2") //$NON-NLS-1$
		);
	    this.onlySignature.setMnemonic('i');
	    this.onlySignature.addItemListener(modificationListener);
	    this.onlySignature.addKeyListener(keyListener);
	    keysFilerPanel.add(this.onlySignature, kfc);

        kfc.gridy++;

	    this.onlyAlias.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.5") //$NON-NLS-1$
		);
	    this.onlyAlias.setMnemonic('s');
	    this.onlyAlias.addItemListener(modificationListener);
	    this.onlyAlias.addKeyListener(keyListener);
	    keysFilerPanel.add(this.onlyAlias, kfc);

	    final JPanel trustPanel = new JPanel();
	    trustPanel.setLayout(new FlowLayout(FlowLayout.LEFT));

	    kfc.insets = new Insets(0,2,0,0);
	    keysFilerPanel.add(trustPanel, kfc);
	    kfc.insets = new Insets(5, 7, 5, 7);

        final JPanel keysStorePanel = new JPanel(new GridBagLayout());
        keysStorePanel.setBorder(
			BorderFactory.createTitledBorder(
				BorderFactory.createTitledBorder(SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.7")) //$NON-NLS-1$
			)
		);

		final GridBagConstraints ksc = new GridBagConstraints();
		ksc.anchor = GridBagConstraints.LINE_START;
		ksc.gridy = 0;
		ksc.insets = new Insets(5, 7, 5, 7);

		this.defaultStore.addItemListener(
			e -> {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					SwingUtilities.invokeLater(() -> AOUIFactory.showMessageDialog(
						PreferencesPanelKeyStores.this,
						SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.16"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.17"), //$NON-NLS-1$
						JOptionPane.WARNING_MESSAGE
					));
				}
			}
		);

		this.defaultStore.addItemListener(modificationListener);
		this.defaultStore.addKeyListener(keyListener);

		keysStorePanel.add(this.defaultStore, ksc);

		this.contentButton.setMnemonic('V');
		this.contentButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent ae) {
					final AOKeyStoreManager ksm;
					try {
						ksm = AOKeyStoreManagerFactory.getAOKeyStoreManager(
							getDefaultStore(),
							null,
							"default", //$NON-NLS-1$
							getDefaultStore().getStorePasswordCallback(this),
							this
						);

						final CertificateSelectionDialog csd = new CertificateSelectionDialog(
							PreferencesPanelKeyStores.this,
							new AOKeyStoreDialog(
								ksm,
								this,
								true,
								true,
								false
							),
							SimpleAfirmaMessages.getString(
								"PreferencesPanelKeyStores.10", //$NON-NLS-1$
								getDefaultStore().toString()
							),
							SimpleAfirmaMessages.getString(
								"PreferencesPanelKeyStores.15", //$NON-NLS-1$
								getDefaultStore().toString()
							),
							false,
							true
						);
						csd.showDialog();
					}
					catch (final Exception e) {
						AOUIFactory.showErrorMessage(
							this,
							SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.11"), //$NON-NLS-1$
							SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.10", getDefaultStore().toString()), //$NON-NLS-1$
							JOptionPane.ERROR_MESSAGE
						);
						Logger.getLogger("es.gob.afirma").warning("Error al recuperar el almacen por defecto seleccionado: " + e); //$NON-NLS-1$ //$NON-NLS-2$
					}

				}
			}
		);
		this.contentButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanelKeyStores.8") //$NON-NLS-1$
		);
		this.contentButton.addKeyListener(keyListener);

		keysStorePanel.add(this.contentButton, ksc);

		ksc.weightx = 1.0;
		keysStorePanel.add(new JPanel(), ksc);

	    add(keysFilerPanel, c);
	    c.gridy++;

	    add(keysStorePanel, c);

	    c.weighty = 1.0;
	    c.gridy++;
		add(new JPanel(), c);
	}

	void savePreferences() {
		PreferencesManager.putBoolean(PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS, this.onlySignature.isSelected());
		PreferencesManager.putBoolean(PREFERENCE_KEYSTORE_ALIAS_ONLY_CERTS, this.onlyAlias.isSelected());
		PreferencesManager.put(
			PREFERENCE_KEYSTORE_DEFAULT_STORE,
			getDefaultStore().name()
		);
	}

	void loadPreferences() {
		this.onlySignature.setSelected(PreferencesManager.getBooleanPreference(PREFERENCE_KEYSTORE_SIGN_ONLY_CERTS, false));
		this.onlyAlias.setSelected(PreferencesManager.getBooleanPreference(PREFERENCE_KEYSTORE_ALIAS_ONLY_CERTS, false));
		this.defaultStore.setSelectedItem(
			SimpleKeyStoreManager.getDefaultKeyStoreType()
		);
	}
}

package es.gob.afirma.standalone.ui.preferences;

import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_IMPLICIT;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_POLICY_HASH;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_POLICY_HASH_ALGORITHM;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_POLICY_IDENTIFIER;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_CADES_POLICY_QUALIFIER;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.KeyListener;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JPanel;

import org.ietf.jgss.GSSException;
import org.ietf.jgss.Oid;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.preferences.PolicyPanel.PolicyItem;

final class PreferencesPanelCades extends JPanel {

	private static final long serialVersionUID = -2410844527428138817L;

	private static final String SIGN_FORMAT_CADES = "CAdES"; //$NON-NLS-1$

	private boolean unprotected = true;

	private static final AdESPolicy POLICY_CADES_PADES_AGE_1_9 = new AdESPolicy(
		"2.16.724.1.3.1.1.2.1.9", //$NON-NLS-1$
		"G7roucf600+f03r/o0bAOQ6WAs0=", //$NON-NLS-1$
		"SHA1", //$NON-NLS-1$
		"https://sede.060.gob.es/politica_de_firma_anexo_1.pdf" //$NON-NLS-1$
	);

	private final JPanel panelPolicies = new JPanel();
	private PolicyPanel cadesPolicyPanel;

	private final JCheckBox cadesImplicit = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.1")); //$NON-NLS-1$

	PreferencesPanelCades(final KeyListener keyListener,
						  final ModificationListener modificationListener,
						  final boolean unprotected) {

		this.unprotected = unprotected;
		createUI(keyListener, modificationListener, unprotected);
	}

	void createUI(final KeyListener keyListener,
				  final ModificationListener modificationListener,
				  final boolean unprotected) {

        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;

        loadPreferences();

        this.cadesPolicyPanel.setModificationListener(modificationListener);
        this.cadesPolicyPanel.setKeyListener(keyListener);
        this.panelPolicies.setLayout(new GridBagLayout());
        this.panelPolicies.add(this.cadesPolicyPanel, c);
        add(this.panelPolicies, c);

	    final FlowLayout fLayout = new FlowLayout(FlowLayout.LEADING);
	    final JPanel signatureMode = new JPanel(fLayout);
	    signatureMode.setBorder(BorderFactory.createTitledBorder(
				BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.69")) //$NON-NLS-1$
			)
		);
	    this.cadesImplicit.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanel.45") //$NON-NLS-1$
		);
	    this.cadesImplicit.setMnemonic('i');
	    this.cadesImplicit.addItemListener(modificationListener);
	    this.cadesImplicit.addKeyListener(keyListener);
	    this.cadesImplicit.setEnabled(unprotected);
	    signatureMode.add(this.cadesImplicit);

	    c.gridy++;
	    add(signatureMode, c);

	    c.gridy++;
	    c.weighty = 1.0;
	    add(new JPanel(), c);

	}

	@SuppressWarnings("unused")
	void checkPreferences() throws AOException {
		final AdESPolicy p = this.cadesPolicyPanel.getCurrentPolicy();
		if (p != null) {
			// No nos interesa el resultado, solo si construye sin excepciones
			try {
				new Oid(p.getPolicyIdentifier().replace("urn:oid:", "")); //$NON-NLS-1$ //$NON-NLS-2$
			}
			catch (final GSSException e) {
				throw new AOException("El identificador debe ser un OID", e); //$NON-NLS-1$
			}
		}
	}

	void savePreferences() {
		PreferencesManager.put(PREFERENCE_CADES_IMPLICIT, Boolean.valueOf(this.cadesImplicit.isSelected()).toString());
		final AdESPolicy cadesPolicy = this.cadesPolicyPanel.getCurrentPolicy();
		if (cadesPolicy != null) {
			PreferencesManager.put(PREFERENCE_CADES_POLICY_IDENTIFIER, cadesPolicy.getPolicyIdentifier());
			PreferencesManager.put(PREFERENCE_CADES_POLICY_HASH, cadesPolicy.getPolicyIdentifierHash());
			PreferencesManager.put(PREFERENCE_CADES_POLICY_HASH_ALGORITHM, cadesPolicy.getPolicyIdentifierHashAlgorithm());
			if (cadesPolicy.getPolicyQualifier() != null) {
				PreferencesManager.put(PREFERENCE_CADES_POLICY_QUALIFIER, cadesPolicy.getPolicyQualifier().toString());
			}
			else {
				PreferencesManager.remove(PREFERENCE_CADES_POLICY_QUALIFIER);
			}
		}
		else {
			PreferencesManager.remove(PREFERENCE_CADES_POLICY_IDENTIFIER);
			PreferencesManager.remove(PREFERENCE_CADES_POLICY_HASH);
			PreferencesManager.remove(PREFERENCE_CADES_POLICY_HASH_ALGORITHM);
			PreferencesManager.remove(PREFERENCE_CADES_POLICY_QUALIFIER);
		}
		this.cadesPolicyPanel.saveCurrentPolicy();
	}

	void loadPreferences() {
		this.cadesImplicit.setSelected(Boolean.parseBoolean(PreferencesManager.get(PREFERENCE_CADES_IMPLICIT, "true"))); //$NON-NLS-1$)

        final List<PolicyPanel.PolicyItem> cadesPolicies = new ArrayList<>();
        cadesPolicies.add(
    		new PolicyItem(
				SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
				POLICY_CADES_PADES_AGE_1_9
			)
		);

        this.panelPolicies.removeAll();
        this.cadesPolicyPanel = new PolicyPanel(
    		SIGN_FORMAT_CADES,
    		cadesPolicies,
    		getCadesPreferedPolicy(),
    		null,
    		this.unprotected
        );

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;
        this.panelPolicies.add(this.cadesPolicyPanel, c);
        revalidate();
        repaint();
	}

	/** Obtiene la configuraci&oacute;n de politica de firma CAdES establecida actualmente.
	 * @return Pol&iacute;tica de firma configurada. */
	private static AdESPolicy getCadesPreferedPolicy() {

		if (PreferencesManager.get(PREFERENCE_CADES_POLICY_IDENTIFIER, null) == null) {
			return null;
		}
		try {
			return new AdESPolicy(
					PreferencesManager.get(PREFERENCE_CADES_POLICY_IDENTIFIER, null),
					PreferencesManager.get(PREFERENCE_CADES_POLICY_HASH, null),
					PreferencesManager.get(PREFERENCE_CADES_POLICY_HASH_ALGORITHM, null),
					PreferencesManager.get(PREFERENCE_CADES_POLICY_QUALIFIER, null)
					);
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("Error al recuperar la politica CAdES guardada en preferencias: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
	}

}

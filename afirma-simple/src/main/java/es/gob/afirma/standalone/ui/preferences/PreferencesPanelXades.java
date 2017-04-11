package es.gob.afirma.standalone.ui.preferences;

import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_POLICY_IDENTIFIER;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_POLICY_IDENTIFIER_HASH;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_POLICY_QUALIFIER;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_SIGNER_CLAIMED_ROLE;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_XADES_SIGN_FORMAT;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyListener;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import java.util.prefs.BackingStoreException;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.preferences.PolicyPanel.PolicyItem;

final class PreferencesPanelXades extends JPanel {
	
	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final long serialVersionUID = 1424468022677956783L;

	private static final String SIGN_FORMAT_XADES = "XAdES"; //$NON-NLS-1$

	private static final AdESPolicy POLICY_XADES_AGE_1_9 = new AdESPolicy(
		"urn:oid:2.16.724.1.3.1.1.2.1.9", //$NON-NLS-1$
		"G7roucf600+f03r/o0bAOQ6WAs0=", //$NON-NLS-1$
		"SHA1", //$NON-NLS-1$
		"https://sede.060.gob.es/politica_de_firma_anexo_1.pdf" //$NON-NLS-1$
	);

	private final JTextField xadesSignatureProductionCity = new JTextField();
	private final JTextField xadesSignatureProductionProvince = new JTextField();
	private final JTextField xadesSignatureProductionPostalCode = new JTextField();
	private final JTextField xadesSignatureProductionCountry = new JTextField();
	private final JTextField xadesSignerClaimedRole = new JTextField();
	private final JComboBox<Object> xadesSignFormat = new JComboBox<>(
		new Object[] {
			AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED,
			AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING,
			AOSignConstants.SIGN_FORMAT_XADES_DETACHED
		}
	);

	private final JPanel panelPolicies = new JPanel();
	private PolicyPanel xadesPolicyPanel;
	
	/**
	 * Atributo que permite gestionar el bloqueo de preferencias.
	 */
	private boolean unprotected = true;

	PreferencesPanelXades(final KeyListener keyListener,
						  final ModificationListener modificationListener,
						  final boolean unprotected) {

		this.unprotected = unprotected;
		createUI(keyListener, modificationListener);
	}

	void createUI(final KeyListener keyListener,
				  final ModificationListener modificationListener
				  ) {
		
        setLayout(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.gridy = 0;
        
		// Panel para el boton de restaurar la configuracion
		final JPanel panelGeneral = new JPanel(new FlowLayout(FlowLayout.LEADING));
		panelGeneral.setBorder(BorderFactory.createTitledBorder(SimpleAfirmaMessages.getString("PreferencesPanel.108")) //$NON-NLS-1$
		);

		final JButton restoreConfigButton = new JButton(SimpleAfirmaMessages.getString("PreferencesPanel.147") //$NON-NLS-1$
		);

		restoreConfigButton.setMnemonic('R');
		restoreConfigButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(final ActionEvent ae) {
				if (AOUIFactory.showConfirmDialog(getParent(), SimpleAfirmaMessages.getString("PreferencesPanel.140"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("PreferencesPanel.139"), //$NON-NLS-1$
						JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {
					try {
						PreferencesManager.clearAll();
						loadDefaultPreferences();
					} catch (final BackingStoreException e) {
						LOGGER.severe("Error eliminando las preferencias de la aplicacion: " + e); //$NON-NLS-1$
						AOUIFactory.showErrorMessage(getParent(),
								SimpleAfirmaMessages.getString("PreferencesPanel.141"), //$NON-NLS-1$
								SimpleAfirmaMessages.getString("PreferencesPanel.117"), //$NON-NLS-1$
								JOptionPane.ERROR_MESSAGE);
					}
				}
			}
		});
		restoreConfigButton.getAccessibleContext()
				.setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.136") //$NON-NLS-1$
		);

		final JLabel restoreConfigLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.148")); //$NON-NLS-1$
		restoreConfigLabel.setLabelFor(restoreConfigButton);

		final GridBagConstraints gbcGeneral = new GridBagConstraints();
		gbcGeneral.fill = GridBagConstraints.HORIZONTAL;

		panelGeneral.add(restoreConfigLabel);
		panelGeneral.add(restoreConfigButton);

		//add(panelGeneral, gbcGeneral);

        loadPreferences();

        this.xadesPolicyPanel.setModificationListener(modificationListener);
        this.xadesPolicyPanel.setKeyListener(keyListener);
        this.panelPolicies.setLayout(new GridBagLayout());
        this.panelPolicies.add(this.xadesPolicyPanel, gbc);
        
       // gbc.gridy++;
        
        add(this.panelPolicies, gbc);

        final JPanel metadata = new JPanel();
        metadata.setBorder(BorderFactory.createTitledBorder(SimpleAfirmaMessages.getString("PreferencesPanel.8"))); //$NON-NLS-1$
        metadata.setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;

        final JLabel xadesSignatureProductionCityLabel = new JLabel(
    		SimpleAfirmaMessages.getString("PreferencesPanel.11") //$NON-NLS-1$
		);
        xadesSignatureProductionCityLabel.setLabelFor(this.xadesSignatureProductionCity);
        c.gridy++;
        metadata.add(xadesSignatureProductionCityLabel, c);
        this.xadesSignatureProductionCity.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanel.66") //$NON-NLS-1$
		);
        this.xadesSignatureProductionCity.addKeyListener(modificationListener);
        this.xadesSignatureProductionCity.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.xadesSignatureProductionCity, c);

        final JLabel xadesSignatureProductionProvinceLabel = new JLabel(
    		SimpleAfirmaMessages.getString("PreferencesPanel.14") //$NON-NLS-1$
		);
        xadesSignatureProductionProvinceLabel.setLabelFor(this.xadesSignatureProductionProvince);
        c.gridy++;
        metadata.add(xadesSignatureProductionProvinceLabel, c);
        this.xadesSignatureProductionProvince.addKeyListener(modificationListener);
        this.xadesSignatureProductionProvince.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.xadesSignatureProductionProvince, c);

        final JLabel xadesSignatureProductionPostalCodeLabel = new JLabel(
    		SimpleAfirmaMessages.getString("PreferencesPanel.102") //$NON-NLS-1$
		);
        xadesSignatureProductionPostalCodeLabel.setLabelFor(this.xadesSignatureProductionPostalCode);
        c.gridy++;
        metadata.add(xadesSignatureProductionPostalCodeLabel, c);
        this.xadesSignatureProductionPostalCode.addKeyListener(modificationListener);
        this.xadesSignatureProductionPostalCode.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.xadesSignatureProductionPostalCode, c);

        final JLabel xadesSignatureProductionCountryLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.12")); //$NON-NLS-1$
        xadesSignatureProductionCountryLabel.setLabelFor(this.xadesSignatureProductionCountry);
        c.gridy++;
        metadata.add(xadesSignatureProductionCountryLabel, c);
        this.xadesSignatureProductionCountry.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.67")); //$NON-NLS-1$
        this.xadesSignatureProductionCountry.addKeyListener(modificationListener);
        this.xadesSignatureProductionCountry.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.xadesSignatureProductionCountry, c);

        final JLabel xadesSignerCertifiedRoleLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanelXades.0")); //$NON-NLS-1$
        xadesSignerCertifiedRoleLabel.setLabelFor(this.xadesSignerClaimedRole);
        c.gridy++;
        metadata.add(xadesSignerCertifiedRoleLabel, c);
        this.xadesSignerClaimedRole.addKeyListener(modificationListener);
        this.xadesSignerClaimedRole.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.xadesSignerClaimedRole, c);

        final FlowLayout fLayout = new FlowLayout(FlowLayout.LEADING);
	    final JPanel signOptions = new JPanel(fLayout);
        signOptions.setBorder(BorderFactory.createTitledBorder(
				BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.69")) //$NON-NLS-1$
			)
		);

        final JPanel panelFirm = new JPanel();
		panelFirm.setBorder(
    		BorderFactory.createEmptyBorder()
		);
		panelFirm.setLayout(new GridBagLayout());

        final GridBagConstraints cf = new GridBagConstraints();
        cf.fill = GridBagConstraints.HORIZONTAL;
        cf.weightx = 1.0;

        this.xadesSignFormat.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.53")); //$NON-NLS-1$
        this.xadesSignFormat.addItemListener(modificationListener);
        this.xadesSignFormat.addKeyListener(keyListener);
        this.xadesSignFormat.setEnabled(!this.unprotected);

        final JLabel xadesFormatLabel = new JLabel(
				SimpleAfirmaMessages.getString("PreferencesPanel.15") //$NON-NLS-1$
		);
        xadesFormatLabel.addKeyListener(keyListener);
        xadesFormatLabel.setLabelFor(this.xadesSignFormat);

        panelFirm.add(xadesFormatLabel, cf);
        cf.gridy++;
        cf.gridy++;
        panelFirm.add(this.xadesSignFormat, cf);
        signOptions.add(panelFirm);

        gbc.gridy++;
        add(metadata, gbc);

        gbc.gridy++;
        add(signOptions, gbc);

        gbc.gridy++;
        gbc.weighty = 1.0;
        add(new JPanel(), gbc);

	}

	void savePreferences() {
		PreferencesManager.put(PREFERENCE_XADES_SIGN_FORMAT, this.xadesSignFormat.getSelectedItem().toString());

		if ("".equals(this.xadesSignatureProductionCity.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY);
		}
		else {
			PreferencesManager.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY, this.xadesSignatureProductionCity.getText());
		}
		if ("".equals(this.xadesSignatureProductionCountry.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY);
		}
		else {
			PreferencesManager.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY, this.xadesSignatureProductionCountry.getText());
		}
		if ("".equals(this.xadesSignatureProductionPostalCode.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE);
		}
		else {
			PreferencesManager.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE, this.xadesSignatureProductionPostalCode.getText());
		}
		if ("".equals(this.xadesSignatureProductionProvince.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE);
		}
		else {
			PreferencesManager.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE, this.xadesSignatureProductionProvince.getText());
		}
		if ("".equals(this.xadesSignerClaimedRole.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE);
		}
		else {
			PreferencesManager.put(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE, this.xadesSignerClaimedRole.getText());
		}
		if ("".equals(this.xadesSignerClaimedRole.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE);
		}
		else {
			PreferencesManager.put(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE, this.xadesSignerClaimedRole.getText());
		}

		final AdESPolicy xadesPolicy = this.xadesPolicyPanel.getCurrentPolicy();
		if (xadesPolicy != null) {
			PreferencesManager.put(PREFERENCE_XADES_POLICY_IDENTIFIER, xadesPolicy.getPolicyIdentifier());
			PreferencesManager.put(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH, xadesPolicy.getPolicyIdentifierHash());
			PreferencesManager.put(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM, xadesPolicy.getPolicyIdentifierHashAlgorithm());
			if (xadesPolicy.getPolicyQualifier() != null) {
				PreferencesManager.put(PREFERENCE_XADES_POLICY_QUALIFIER, xadesPolicy.getPolicyQualifier().toString());
			}
			else {
				PreferencesManager.remove(PREFERENCE_XADES_POLICY_QUALIFIER);
			}
		}
		else {
			PreferencesManager.remove(PREFERENCE_XADES_POLICY_IDENTIFIER);
			PreferencesManager.remove(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH);
			PreferencesManager.remove(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM);
			PreferencesManager.remove(PREFERENCE_XADES_POLICY_QUALIFIER);
		}
		this.xadesPolicyPanel.saveCurrentPolicy();

	}

	void loadPreferences() {
		this.xadesSignatureProductionCity.setText(PreferencesManager.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY, "")); //$NON-NLS-1$
		this.xadesSignatureProductionProvince.setText(
			PreferencesManager.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE, "") //$NON-NLS-1$
		);
		this.xadesSignatureProductionPostalCode.setText(
			PreferencesManager.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE, "") //$NON-NLS-1$
		);
		this.xadesSignatureProductionCountry.setText(
			PreferencesManager.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY, "") //$NON-NLS-1$
		);
		this.xadesSignerClaimedRole.setText(PreferencesManager.get(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE, "")); //$NON-NLS-1$

		this.xadesSignFormat.setSelectedItem(
    		PreferencesManager.get(
				PREFERENCE_XADES_SIGN_FORMAT,
				AOSignConstants.SIGN_FORMAT_XADES_DETACHED
			)
		);

		final List<PolicyPanel.PolicyItem> xadesPolicies = new ArrayList<>();
        xadesPolicies.add(
    		new PolicyItem(
        		SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
        		POLICY_XADES_AGE_1_9
    		)
		);

        this.panelPolicies.removeAll();
        this.xadesPolicyPanel = new PolicyPanel(
    		SIGN_FORMAT_XADES,
    		xadesPolicies,
    		getXadesPreferedPolicy(),
    		this.xadesSignFormat,
    		isUnprotected()
		);

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;
        this.panelPolicies.add(this.xadesPolicyPanel, c);
        revalidate();
        repaint();
	}
	
	void loadDefaultPreferences() {
		this.xadesSignatureProductionCity.setText(PreferencesManager.getPreference(PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY, "")); //$NON-NLS-1$
		this.xadesSignatureProductionProvince.setText(
			PreferencesManager.getPreference(PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE, "") //$NON-NLS-1$
		);
		this.xadesSignatureProductionPostalCode.setText(
			PreferencesManager.getPreference(PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE, "") //$NON-NLS-1$
		);
		this.xadesSignatureProductionCountry.setText(
			PreferencesManager.getPreference(PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY, "") //$NON-NLS-1$
		);
		this.xadesSignerClaimedRole.setText(PreferencesManager.getPreference(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE, "")); //$NON-NLS-1$

		// unprotected: true -> no puedo modificarla, cargo la que estaba
		if (isUnprotected()) {
			this.xadesSignFormat.setSelectedItem(PreferencesManager.getPreference(PREFERENCE_XADES_SIGN_FORMAT,
					AOSignConstants.SIGN_FORMAT_XADES_DETACHED));
		}

		final List<PolicyPanel.PolicyItem> xadesPolicies = new ArrayList<>();
        xadesPolicies.add(
    		new PolicyItem(
        		SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
        		POLICY_XADES_AGE_1_9
    		)
		);

        this.panelPolicies.removeAll();
        this.xadesPolicyPanel = new PolicyPanel(
    		SIGN_FORMAT_XADES,
    		xadesPolicies,
    		getXadesDefaultPolicy(),
    		this.xadesSignFormat,
    		isUnprotected()
		);

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;
        this.panelPolicies.add(this.xadesPolicyPanel, c);
        
        this.xadesSignFormat.setSelectedItem(AOSignConstants.SIGN_FORMAT_XADES_DETACHED);
        //this.xadesSignFormat.removeItemAt(1);
		this.xadesSignFormat.setEnabled(!this.unprotected);
        
        revalidate();
        repaint();
	}


	/** Obtiene la configuraci&oacute;n de politica de firma XAdES establecida actualmente.
	 * @return Pol&iacute;tica de firma configurada. */
	private static AdESPolicy getXadesPreferedPolicy() {

		if (PreferencesManager.get(PREFERENCE_XADES_POLICY_IDENTIFIER, null) == null) {
			return null;
		}
		try {
			return new AdESPolicy(
				PreferencesManager.get(PREFERENCE_XADES_POLICY_IDENTIFIER, null),
				PreferencesManager.get(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH, null),
				PreferencesManager.get(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM, null),
				PreferencesManager.get(PREFERENCE_XADES_POLICY_QUALIFIER, null)
				);
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("Error al recuperar la politica XAdES guardada en preferencias: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
	}
	
	/** Obtiene la configuraci&oacute;n de politica de firma XAdES establecida por defecto.
	 * @return Pol&iacute;tica de firma configurada. */
	private AdESPolicy getXadesDefaultPolicy() {

		AdESPolicy adesPolicy = null;

		if (isUnprotected()) {

			// unprotected = true, luego no pueden alterarse las
			// propiedades:
			// devolvemos las preferencias almacenadas actualmente

			adesPolicy = this.xadesPolicyPanel.getCurrentPolicy();

		} else {

			// unprotected = false, luego se pueden alterar las propiedades:
			// devolvemos las preferencias por defecto
			try {
				return new AdESPolicy(PreferencesManager.get(PREFERENCE_XADES_POLICY_IDENTIFIER, null),
						PreferencesManager.get(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH, null),
						PreferencesManager.get(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM, null),
						PreferencesManager.get(PREFERENCE_XADES_POLICY_QUALIFIER, null));
			} catch (final Exception e) {
				Logger.getLogger("es.gob.afirma") //$NON-NLS-1$
						.severe("Error al recuperar la politica XAdES guardada en preferencias: " + e); //$NON-NLS-1$

			}
		}

		return adesPolicy;
	}

	void checkPreferences() throws AOException {
		final AdESPolicy p = this.xadesPolicyPanel.getCurrentPolicy();
		if (p != null) {
			// No nos interesa el resultado, solo si construye sin excepciones
			try {
				new URI(p.getPolicyIdentifier());
			}
			catch (final URISyntaxException e) {
				throw new AOException("El identificador debe ser una URI", e); //$NON-NLS-1$
			}
		}

	}
	
	/**
	 * M&eacute;todo getter del atributo unprotected
	 * @return the unprotected
	 */
	public boolean isUnprotected() {
		return this.unprotected;
	}

	/**
	 * M&eacute;todo setter del atributo unprotected
	 * @param unprotected the unprotected to set
	 */
	public void setUnprotected(boolean unprotected) {
		this.unprotected = unprotected;
	}

}

package es.gob.afirma.standalone.ui.preferences;

import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_FACTURAE_POLICY_IDENTIFIER;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_FACTURAE_POLICY_IDENTIFIER_HASH;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_FACTURAE_POLICY_IDENTIFIER_HASH_ALGORITHM;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_FACTURAE_POLICY_QUALIFIER;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_CITY;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_COUNTRY;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_POSTAL_CODE;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_PROVINCE;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_FACTURAE_SIGNER_ROLE;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.KeyListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.ietf.jgss.GSSException;
import org.ietf.jgss.Oid;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.preferences.PolicyPanel.PolicyItem;

/** Pesta&ntilde;a de configuraci&oacute;n de las preferencias de facturaE.
 * @author Mariano Mart&iacute;nez. */
final class PreferencesPanelFacturaE extends JPanel {

	private static final long serialVersionUID = 4299378019540627483L;

	private static final String FACTURAE_ROL_EMISOR = "Emisor"; //$NON-NLS-1$
	private static final String FACTURAE_ROL_RECEPTOR = "Receptor"; //$NON-NLS-1$
	private static final String FACTURAE_ROL_TERCERO = "Tercero"; //$NON-NLS-1$

	private static final String SIGN_FORMAT_FACTURAE = AOSignConstants.SIGN_FORMAT_FACTURAE;

	private static final String POLICY_FACTURAE_31_NAME = "3.1"; //$NON-NLS-1$
	private static final AdESPolicy POLICY_FACTURAE_31 = new AdESPolicy(
		"http://www.facturae.es/politica_de_firma_formato_facturae/politica_de_firma_formato_facturae_v3_1.pdf", //$NON-NLS-1$
		"Ohixl6upD6av8N7pEvDABhEL6hM=", //$NON-NLS-1$
		"SHA1", //$NON-NLS-1$
		null
	);

	private static final String POLICY_FACTURAE_30_NAME = "3.0"; //$NON-NLS-1$
	private static final AdESPolicy POLICY_FACTURAE_30 = new AdESPolicy(
		"http://www.facturae.es/politica de firma formato facturae/politica de firma formato facturae v3_0.pdf", //$NON-NLS-1$
		"xmfh8D/Ec/hHeE1IB4zPd61zHIY=", //$NON-NLS-1$
		"SHA1", //$NON-NLS-1$
		null
	);

	private final JTextField facturaeSignatureProductionCity = new JTextField();
	private final JTextField facturaeSignatureProductionProvince = new JTextField();
	private final JTextField facturaeSignatureProductionPostalCode = new JTextField();
	private final JTextField facturaeSignatureProductionCountry = new JTextField();

	private final JComboBox<Object> facturaeRol = new JComboBox<>(
		new Object[] {
			FACTURAE_ROL_EMISOR,
			FACTURAE_ROL_RECEPTOR,
			FACTURAE_ROL_TERCERO
		}
	);

	private final JPanel panelPolicies = new JPanel();
	private PolicyPanel facturaePolicyPanel;

	PreferencesPanelFacturaE(final KeyListener keyListener,
							 final ModificationListener modificationListener) {

		createUI(keyListener, modificationListener);
	}

	void createUI(final KeyListener keyListener,
				  final ModificationListener modificationListener) {

        setLayout(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.gridy = 0;

        loadPreferences();

        this.panelPolicies.setLayout(new GridBagLayout());
        this.panelPolicies.add(this.facturaePolicyPanel, gbc);
        add(this.panelPolicies, gbc);

        this.facturaePolicyPanel.setModificationListener(modificationListener);
        this.facturaePolicyPanel.setKeyListener(keyListener);

        final JPanel metadata = new JPanel();
        metadata.setBorder(
    		BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanelFacturaE.4") //$NON-NLS-1$
			)
		);
        metadata.setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;

        final JLabel facturaeSignatureProductionCityLabel = new JLabel(
    		SimpleAfirmaMessages.getString("PreferencesPanel.11") //$NON-NLS-1$
		);
        facturaeSignatureProductionCityLabel.setLabelFor(this.facturaeSignatureProductionCity);
        c.gridy++;
        metadata.add(facturaeSignatureProductionCityLabel, c);
        this.facturaeSignatureProductionCity.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanel.66") //$NON-NLS-1$
		);
        this.facturaeSignatureProductionCity.addKeyListener(modificationListener);
        this.facturaeSignatureProductionCity.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.facturaeSignatureProductionCity, c);

        final JLabel facturaeSignatureProductionProvinceLabel = new JLabel(
    		SimpleAfirmaMessages.getString("PreferencesPanel.14") //$NON-NLS-1$
		);
        facturaeSignatureProductionProvinceLabel.setLabelFor(this.facturaeSignatureProductionProvince);
        c.gridy++;
        metadata.add(facturaeSignatureProductionProvinceLabel, c);
        this.facturaeSignatureProductionProvince.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanel.68") //$NON-NLS-1$
		);
        this.facturaeSignatureProductionProvince.addKeyListener(modificationListener);
        this.facturaeSignatureProductionProvince.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.facturaeSignatureProductionProvince, c);

        final JLabel facturaeSignatureProductionPostalCodeLabel = new JLabel(
    		SimpleAfirmaMessages.getString("PreferencesPanel.102") //$NON-NLS-1$
		);
        facturaeSignatureProductionPostalCodeLabel.setLabelFor(this.facturaeSignatureProductionPostalCode);
        c.gridy++;
        metadata.add(facturaeSignatureProductionPostalCodeLabel, c);
        this.facturaeSignatureProductionPostalCode.addKeyListener(modificationListener);
        this.facturaeSignatureProductionPostalCode.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.facturaeSignatureProductionPostalCode, c);

        final JLabel facturaeSignatureProductionCountryLabel = new JLabel(
    		SimpleAfirmaMessages.getString("PreferencesPanel.12") //$NON-NLS-1$
		);
        facturaeSignatureProductionCountryLabel.setLabelFor(this.facturaeSignatureProductionCountry);
        c.gridy++;
        metadata.add(facturaeSignatureProductionCountryLabel, c);
        this.facturaeSignatureProductionCountry.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanel.67") //$NON-NLS-1$
		);
        this.facturaeSignatureProductionCountry.addKeyListener(modificationListener);
        this.facturaeSignatureProductionCountry.addKeyListener(keyListener);
        c.gridy++;
        metadata.add(this.facturaeSignatureProductionCountry, c);

        final FlowLayout fLayout = new FlowLayout(FlowLayout.LEADING);
	    final JPanel signOptions = new JPanel(fLayout);
        signOptions.setBorder(BorderFactory.createTitledBorder(
				BorderFactory.createTitledBorder(
				SimpleAfirmaMessages.getString("PreferencesPanel.69")) //$NON-NLS-1$
			)
		);

	    final JPanel format = new JPanel();
        format.setBorder(
    		BorderFactory.createEmptyBorder()
		);

        format.setLayout(new GridBagLayout());

        final GridBagConstraints cf = new GridBagConstraints();
        cf.fill = GridBagConstraints.HORIZONTAL;
        cf.weightx = 1.0;

        this.facturaeRol.getAccessibleContext().setAccessibleDescription(
    		SimpleAfirmaMessages.getString("PreferencesPanel.53") //$NON-NLS-1$
		);
        this.facturaeRol.addItemListener(modificationListener);

        final JLabel facturaeRolLabel = new JLabel(
				SimpleAfirmaMessages.getString("PreferencesPanelFacturaE.3") //$NON-NLS-1$
		);
        facturaeRolLabel.setLabelFor(this.facturaeRol);

        format.add(facturaeRolLabel, cf);
        cf.gridy++;
        cf.gridy++;
        format.add(this.facturaeRol, cf);

        signOptions.add(format);

        gbc.gridy++;
        add(metadata, gbc);

        gbc.gridy++;
        add(signOptions, gbc);

        gbc.gridy++;
        gbc.weighty = 1.0;
        add(new JPanel(), gbc);
	}

	/** Guarda las preferencias de FacturaE. */
	void savePreferences() {

		PreferencesManager.put(PREFERENCE_FACTURAE_SIGNER_ROLE, this.facturaeRol.getSelectedItem().toString());

		if ("".equals(this.facturaeSignatureProductionCity.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_CITY);
		}
		else {
			PreferencesManager.put(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_CITY, this.facturaeSignatureProductionCity.getText());
		}

		if ("".equals(this.facturaeSignatureProductionCountry.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_COUNTRY);
		}
		else {
			PreferencesManager.put(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_COUNTRY, this.facturaeSignatureProductionCountry.getText());
		}

		if ("".equals(this.facturaeSignatureProductionPostalCode.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_POSTAL_CODE);
		}
		else {
			PreferencesManager.put(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_POSTAL_CODE, this.facturaeSignatureProductionPostalCode.getText());
		}

		if ("".equals(this.facturaeSignatureProductionProvince.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_PROVINCE);
		}
		else {
			PreferencesManager.put(PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_PROVINCE, this.facturaeSignatureProductionProvince.getText());
		}

		final AdESPolicy facturaePolicy = this.facturaePolicyPanel.getCurrentPolicy();
		if (facturaePolicy != null) {
			if (this.facturaePolicyPanel.getCurrentPolicyItem().toString().equals(SimpleAfirmaMessages.getString("PreferencesPanelFacturaE.1"))) { //$NON-NLS-1$
					PreferencesManager.put(PreferencesManager.PREFERENCE_FACTURAE_POLICY, POLICY_FACTURAE_30_NAME);
			}
			else if (this.facturaePolicyPanel.getCurrentPolicyItem().toString().equals(SimpleAfirmaMessages.getString("PreferencesPanelFacturaE.2"))) { //$NON-NLS-1$
				PreferencesManager.put(PreferencesManager.PREFERENCE_FACTURAE_POLICY, POLICY_FACTURAE_31_NAME);
			}
			PreferencesManager.put(PREFERENCE_FACTURAE_POLICY_IDENTIFIER, facturaePolicy.getPolicyIdentifier());
			PreferencesManager.put(PREFERENCE_FACTURAE_POLICY_IDENTIFIER_HASH, facturaePolicy.getPolicyIdentifierHash());
			PreferencesManager.put(PREFERENCE_FACTURAE_POLICY_IDENTIFIER_HASH_ALGORITHM, facturaePolicy.getPolicyIdentifierHashAlgorithm());
			if (facturaePolicy.getPolicyQualifier() != null) {
				PreferencesManager.put(PREFERENCE_FACTURAE_POLICY_QUALIFIER, facturaePolicy.getPolicyQualifier().toString());
			}
			else {
				PreferencesManager.remove(PREFERENCE_FACTURAE_POLICY_QUALIFIER);
			}
		}
		else {
			PreferencesManager.remove(PREFERENCE_FACTURAE_POLICY_IDENTIFIER);
			PreferencesManager.remove(PREFERENCE_FACTURAE_POLICY_IDENTIFIER_HASH);
			PreferencesManager.remove(PREFERENCE_FACTURAE_POLICY_IDENTIFIER_HASH_ALGORITHM);
			PreferencesManager.remove(PREFERENCE_FACTURAE_POLICY_QUALIFIER);
		}
		this.facturaePolicyPanel.saveCurrentPolicy();
	}

	void loadPreferences() {
		this.facturaeRol.setSelectedItem(
			PreferencesManager.get(
				PREFERENCE_FACTURAE_SIGNER_ROLE,
				FACTURAE_ROL_EMISOR
			)
		);

		this.facturaeSignatureProductionCity.setText(
			PreferencesManager.get(
				PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_CITY,
				"" //$NON-NLS-1$
			)
		);

		this.facturaeSignatureProductionProvince.setText(
			PreferencesManager.get(
				PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_PROVINCE,
				"" //$NON-NLS-1$
			)
		);

		this.facturaeSignatureProductionPostalCode.setText(
			PreferencesManager.get(
				PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_POSTAL_CODE,
				"" //$NON-NLS-1$
			)
		);

		this.facturaeSignatureProductionCountry.setText(
			PreferencesManager.get(
				PREFERENCE_FACTURAE_SIGNATURE_PRODUCTION_COUNTRY,
				"" //$NON-NLS-1$
			)
		);
		final List<PolicyPanel.PolicyItem> facturaePolicies = new ArrayList<>();

		facturaePolicies.add(
    		new PolicyItem(
    			SimpleAfirmaMessages.getString("PreferencesPanelFacturaE.2"), //$NON-NLS-1$
        		POLICY_FACTURAE_31
    		)
		);

		facturaePolicies.add(
    		new PolicyItem(
    			SimpleAfirmaMessages.getString("PreferencesPanelFacturaE.1"), //$NON-NLS-1$
        		POLICY_FACTURAE_30
    		)
		);

        this.panelPolicies.removeAll();
        this.facturaePolicyPanel = new PolicyPanel(
    		SIGN_FORMAT_FACTURAE,
    		facturaePolicies,
    		getFacturaEPreferedPolicy(),
    		null,
    		false,
    		false,
    		false
		);

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;
        this.panelPolicies.add(this.facturaePolicyPanel, c);
        revalidate();
        repaint();

	}

	/** Obtiene la configuraci&oacute;n de politica de firma FacturaE establecida actualmente.
	 * @return Pol&iacute;tica de firma configurada. */
	private static AdESPolicy getFacturaEPreferedPolicy() {

		if (PreferencesManager.get(PreferencesManager.PREFERENCE_FACTURAE_POLICY, "").equals(POLICY_FACTURAE_30_NAME)) { //$NON-NLS-1$
			return POLICY_FACTURAE_30;
		}
		else if (PreferencesManager.get(PreferencesManager.PREFERENCE_FACTURAE_POLICY, "").equals(POLICY_FACTURAE_31_NAME)) { //$NON-NLS-1$
			return POLICY_FACTURAE_31;
		}
		else {
			return null;
		}
	}

	@SuppressWarnings("unused")
	void checkPreferences() throws AOException {
		final AdESPolicy p = this.facturaePolicyPanel.getCurrentPolicy();
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
}

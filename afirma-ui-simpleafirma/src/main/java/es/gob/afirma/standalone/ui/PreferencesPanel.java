/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * Date: 11/01/11
 * You may contact the copyright holder at: soporte.afirma5@mpt.es
 */

package es.gob.afirma.standalone.ui;

import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_CADES_IMPLICIT;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_CADES_POLICY_IDENTIFIER;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_CADES_POLICY_IDENTIFIER_HASH;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_CADES_POLICY_IDENTIFIER_HASH_ALGORITHM;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_CADES_POLICY_QUALIFIER;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_OMIT_ASKONCLOSE;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_PADES_POLICY_IDENTIFIER;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_PADES_POLICY_IDENTIFIER_HASH;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_PADES_POLICY_QUALIFIER;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_PADES_SIGNER_CONTACT;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_PADES_SIGN_PRODUCTION_CITY;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_PADES_SIGN_REASON;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_SIGNATURE_ALGORITHM;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_POLICY_IDENTIFIER;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_POLICY_IDENTIFIER_HASH;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_POLICY_QUALIFIER;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_SIGNER_CERTIFIED_ROLE;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_SIGNER_CLAIMED_ROLE;
import static es.gob.afirma.standalone.PreferencesNames.PREFERENCE_XADES_SIGN_FORMAT;

import java.awt.GridLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import java.util.prefs.Preferences;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

import org.ietf.jgss.GSSException;
import org.ietf.jgss.Oid;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.standalone.Messages;

final class PreferencesPanel extends JPanel {

    final JButton applyButton = new JButton(Messages.getString("PreferencesPanel.0")); //$NON-NLS-1$

    private final Window window;
    Window getParentWindow() {
        return this.window;
    }

    private final ModificationListener modificationListener;

	private static final long serialVersionUID = -3168095095548385291L;

	private static final Preferences PREFERENCES = Preferences.userRoot();

	private static final AdESPolicy POLICY_XADES_AGE = new AdESPolicy(
		"urn:oid:2.16.724.1.3.1.1.2.1.8", //$NON-NLS-1$
		"7SxX3erFuH31TvAw9LZ70N7p1vA=", //$NON-NLS-1$
		"SHA1", //$NON-NLS-1$
		"http://administracionelectronica.gob.es/es/ctt/politicafirma/politica_firma_AGE_v1_8.pdf" //$NON-NLS-1$
	);

	private static final AdESPolicy POLICY_CADES_AGE = new AdESPolicy(
		"2.16.724.1.3.1.1.2.1.8", //$NON-NLS-1$
		"V8lVVNGDCPen6VELRD1Ja8HARFk=", //$NON-NLS-1$
		"SHA1", //$NON-NLS-1$
		"http://administracionelectronica.gob.es/es/ctt/politicafirma/politica_firma_AGE_v1_8.pdf" //$NON-NLS-1$
	);

	private static final int POLICY_INDEX_AGE = 1;

	private static final int POLICY_INDEX_NONE = 0;

	private static final AdESPolicy POLICY_XADES_CUSTOM;
	static {
		AdESPolicy customPolicy = null;
		try {
			customPolicy = new AdESPolicy(
				PREFERENCES.get(PREFERENCE_XADES_POLICY_IDENTIFIER, null),
				PREFERENCES.get(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH, null),
				PREFERENCES.get(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM, null),
				PREFERENCES.get(PREFERENCE_XADES_POLICY_QUALIFIER, null)
			);
		}
		catch(final Exception e) {
			// Se ignora
		}
		POLICY_XADES_CUSTOM = customPolicy;
	}

	private static final AdESPolicy POLICY_PADES_CUSTOM;
	static {
		AdESPolicy customPolicy = null;
		try {
			customPolicy = new AdESPolicy(
				PREFERENCES.get(PREFERENCE_PADES_POLICY_IDENTIFIER, null),
				PREFERENCES.get(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH, null),
				PREFERENCES.get(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM, null),
				PREFERENCES.get(PREFERENCE_PADES_POLICY_QUALIFIER, null)
			);
		}
		catch(final Exception e) {
			// Se ignora
		}
		POLICY_PADES_CUSTOM = customPolicy;
	}

	private static final AdESPolicy POLICY_CADES_CUSTOM;
	static {
		AdESPolicy customPolicy = null;
		try {
			customPolicy = new AdESPolicy(
				PREFERENCES.get(PREFERENCE_CADES_POLICY_IDENTIFIER, null),
				PREFERENCES.get(PREFERENCE_CADES_POLICY_IDENTIFIER_HASH, null),
				PREFERENCES.get(PREFERENCE_CADES_POLICY_IDENTIFIER_HASH_ALGORITHM, null),
				PREFERENCES.get(PREFERENCE_CADES_POLICY_QUALIFIER, null)
			);
		}
		catch(final Exception e) {
			// Se ignora
		}
		POLICY_CADES_CUSTOM = customPolicy;
	}

	private static final List<AdESPolicy> XADES_PRELOADED_POLICIES = new ArrayList<AdESPolicy>(1);
	static {
		XADES_PRELOADED_POLICIES.add(
			POLICY_INDEX_NONE, // Por posicion, en el 0 ninguna politica
			null
		);
		XADES_PRELOADED_POLICIES.add(
			POLICY_INDEX_AGE, // Por posicion, en el 1 la politica de la AGE
			POLICY_XADES_AGE
		);
		XADES_PRELOADED_POLICIES.add(
			2, // Por posicion, en el ultimo lugar una politica a medida
			POLICY_XADES_CUSTOM
		);
	}
	static List<AdESPolicy> getXadesPreloadedPolicies() {
		return XADES_PRELOADED_POLICIES;
	}

	private static final List<AdESPolicy> PADES_PRELOADED_POLICIES = new ArrayList<AdESPolicy>(1);
	static {
		PADES_PRELOADED_POLICIES.add(
			POLICY_INDEX_NONE, // Por posicion, en el 0 ninguna politica
			null
		);
		PADES_PRELOADED_POLICIES.add(
			POLICY_INDEX_AGE, // Por posicion, en el 1 la politica de la AGE
			POLICY_CADES_AGE
		);
		PADES_PRELOADED_POLICIES.add(
			2, // Por posicion, en el ultimo lugar una politica a medida
			POLICY_PADES_CUSTOM
		);
	}
	static List<AdESPolicy> getPadesPreloadedPolicies() {
		return PADES_PRELOADED_POLICIES;
	}

	private static final List<AdESPolicy> CADES_PRELOADED_POLICIES = new ArrayList<AdESPolicy>(1);
	static {
		CADES_PRELOADED_POLICIES.add(
			POLICY_INDEX_NONE, // Por posicion, en el 0 ninguna politica
			null
		);
		CADES_PRELOADED_POLICIES.add(
			POLICY_INDEX_AGE, // Por posicion, en el 1 la politica de la AGE
			POLICY_CADES_AGE
		);
		CADES_PRELOADED_POLICIES.add(
			2, // Por posicion, en el ultimo lugar una politica a medida
			POLICY_CADES_CUSTOM
		);
	}
	static List<AdESPolicy> getCadesPreloadedPolicies() {
		return CADES_PRELOADED_POLICIES;
	}

	private final JComboBox signarureAlgorithms = new JComboBox();

	private final JComboBox xadesPolicies = new JComboBox();
	JComboBox getXadesPolicies() {
		return this.xadesPolicies;
	}

	private final JComboBox padesPolicies = new JComboBox();
	JComboBox getPadesPolicies() {
		return this.padesPolicies;
	}

	private final JComboBox cadesPolicies = new JComboBox();
	JComboBox getCadesPolicies() {
		return this.cadesPolicies;
	}

	/** Algoritmos de huella digital admitidos para las pol&iacute;ticas de firma. */
	private static final String[] POLICY_HASH_ALGORITHMS = new String[] {
		"SHA1", //$NON-NLS-1$
		"SHA-512", //$NON-NLS-1$
		"SHA-384", //$NON-NLS-1$
		"SHA-256" //$NON-NLS-1$
	};

	//*********** CAMPOS POLITICA XADES ***************************************************************
	//*************************************************************************************************
	private final JTextField xadesPolicyIdentifier = new JTextField();
	JTextField getXadesPolicyIdentifier() {
		return this.xadesPolicyIdentifier;
	}
	private final JTextField xadesPolicyIdentifierHash = new JTextField();
	JTextField getXadesPolicyIdentifierHash() {
		return this.xadesPolicyIdentifierHash;
	}
	private final JComboBox xadesPolicyIdentifierHashAlgorithm = new JComboBox(POLICY_HASH_ALGORITHMS);
	JComboBox getXadesPolicyIdentifierHashAlgorithm() {
		return this.xadesPolicyIdentifierHashAlgorithm;
	}
	private final JTextField xadesPolicyQualifier =new JTextField();
	JTextField getXadesPolicyQualifier() {
		return this.xadesPolicyQualifier;
	}

	//*********** CAMPOS POLITICA CADES ***************************************************************
	//*************************************************************************************************
	private final JTextField cadesPolicyIdentifier = new JTextField();
	JTextField getCadesPolicyIdentifier() {
		return this.cadesPolicyIdentifier;
	}
	private final JTextField cadesPolicyIdentifierHash = new JTextField();
	JTextField getCadesPolicyIdentifierHash() {
		return this.cadesPolicyIdentifierHash;
	}
	private final JComboBox cadesPolicyIdentifierHashAlgorithm = new JComboBox(POLICY_HASH_ALGORITHMS);
	JComboBox getCadesPolicyIdentifierHashAlgorithm() {
		return this.cadesPolicyIdentifierHashAlgorithm;
	}
	private final JTextField cadesPolicyQualifier =new JTextField();
	JTextField getCadesPolicyQualifier() {
		return this.cadesPolicyQualifier;
	}

	//*********** CAMPOS POLITICA PADES ***************************************************************
	//*************************************************************************************************
	private final JTextField padesPolicyIdentifier = new JTextField();
	JTextField getPadesPolicyIdentifier() {
		return this.padesPolicyIdentifier;
	}
	private final JTextField padesPolicyIdentifierHash = new JTextField();
	JTextField getPadesPolicyIdentifierHash() {
		return this.padesPolicyIdentifierHash;
	}
	private final JComboBox padesPolicyIdentifierHashAlgorithm = new JComboBox(POLICY_HASH_ALGORITHMS);
	JComboBox getPadesPolicyIdentifierHashAlgorithm() {
		return this.padesPolicyIdentifierHashAlgorithm;
	}
	private final JTextField padesPolicyQualifier =new JTextField();
	JTextField getPadesPolicyQualifier() {
		return this.padesPolicyQualifier;
	}

	private final JCheckBox avoidAskForClose = new JCheckBox(
		Messages.getString("PreferencesPanel.36"), //$NON-NLS-1$
		PREFERENCES.getBoolean(PREFERENCE_OMIT_ASKONCLOSE, false)
	);

	private final JTextField padesSignReason = new JTextField(
		PREFERENCES.get(PREFERENCE_PADES_SIGN_REASON, "") //$NON-NLS-1$
	);
	private final JTextField padesSignProductionCity = new JTextField(
		PREFERENCES.get(PREFERENCE_PADES_SIGN_PRODUCTION_CITY, "") //$NON-NLS-1$
	);
	private final JTextField padesSignerContact = new JTextField(
		PREFERENCES.get(PREFERENCE_PADES_SIGNER_CONTACT, "") //$NON-NLS-1$
	);

	private final JCheckBox cadesImplicit = new JCheckBox(
		Messages.getString("PreferencesPanel.1"), //$NON-NLS-1$
		Boolean.parseBoolean(PREFERENCES.get(PREFERENCE_CADES_IMPLICIT, "true")) //$NON-NLS-1$
	);

	private final JTextField xadesSignatureProductionCity = new JTextField(
		PREFERENCES.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY, "") //$NON-NLS-1$
	);
	private final JTextField xadesSignatureProductionProvince = new JTextField(
		PREFERENCES.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE, "") //$NON-NLS-1$
	);
	private final JTextField xadesSignatureProductionPostalCode = new JTextField(
		PREFERENCES.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE, "") //$NON-NLS-1$
	);
	private final JTextField xadesSignatureProductionCountry = new JTextField(
		PREFERENCES.get(PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY, "") //$NON-NLS-1$
	);
	private final JTextField xadesSignerClaimedRole = new JTextField(
		PREFERENCES.get(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE, "") //$NON-NLS-1$
	);
	private final JTextField xadesSignerCertifiedRole = new JTextField(
		PREFERENCES.get(PREFERENCE_XADES_SIGNER_CERTIFIED_ROLE, "") //$NON-NLS-1$
	);
	private final JComboBox xadesSignFormat = new JComboBox(new String[] {
	  AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING,
      AOSignConstants.SIGN_FORMAT_XADES_DETACHED,
      AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED
	});

	void createUI() {
		final JTabbedPane tabbedPane = new JTabbedPane();
		tabbedPane.addTab(Messages.getString("PreferencesPanel.2"), createGeneralPanel()); //$NON-NLS-1$
		tabbedPane.addTab(Messages.getString("PreferencesPanel.3"), createPadesPanel()); //$NON-NLS-1$
		tabbedPane.addTab(Messages.getString("PreferencesPanel.4"), createCadesPanel()); //$NON-NLS-1$
		tabbedPane.addTab(Messages.getString("PreferencesPanel.5"), createXadesPanel()); //$NON-NLS-1$

		add(tabbedPane);
		add(createButtonsPanel());
	}

	@SuppressWarnings("unused")
    boolean savePreferences() {
		// primero comprobamos si hay alguna politica mal introducida
		if (this.xadesPolicies.getSelectedIndex() != POLICY_INDEX_NONE) {
			try {
				// Construimos el objeto para ver si los datos son correctos, ya que se comprueban en el constructor
				new AdESPolicy(
					PreferencesPanel.this.getXadesPolicyIdentifier().getText(),
					PreferencesPanel.this.getXadesPolicyIdentifierHash().getText(),
					PreferencesPanel.this.getXadesPolicyIdentifierHashAlgorithm().getSelectedItem().toString(),
					PreferencesPanel.this.getXadesPolicyQualifier().getText()
				);
			}
			catch(final Exception e) {
				JOptionPane.showMessageDialog(
					this,
					"<html><p>" + Messages.getString("PreferencesPanel.6") + ":<br>" + e.getLocalizedMessage() + "</p></html>", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
					Messages.getString("SimpleAfirma.7"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
				return false;
			}
		}
		if (this.padesPolicies.getSelectedIndex() != POLICY_INDEX_NONE) {
			try {
				// Construimos el objeto para ver si los datos son correctos, ya que se comprueban en el constructor
				new AdESPolicy(
					PreferencesPanel.this.getPadesPolicyIdentifier().getText(),
					PreferencesPanel.this.getPadesPolicyIdentifierHash().getText(),
					PreferencesPanel.this.getPadesPolicyIdentifierHashAlgorithm().getSelectedItem().toString(),
					PreferencesPanel.this.getPadesPolicyQualifier().getText()
				);
        		// No nos interesa el resultado, solo si construye sin excepciones
				try {
					new Oid(PreferencesPanel.this.getXadesPolicyIdentifier().getText().replace("urn:oid:", "")); //$NON-NLS-1$ //$NON-NLS-2$
				}
				catch (final GSSException e) {
					throw new AOException("El identificador debe ser un OID"); //$NON-NLS-1$
				}
			}
			catch(final Exception e) {
				JOptionPane.showMessageDialog(
					this,
					"<html><p>" + Messages.getString("PreferencesPanel.7") + ":<br>" + e.getLocalizedMessage() + "</p></html>", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
					Messages.getString("SimpleAfirma.7"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
				return false;
			}
		}
		if (this.cadesPolicies.getSelectedIndex() != POLICY_INDEX_NONE) {
			try {
				// Construimos el objeto para ver si los datos son correctos, ya que se comprueban en el constructor
				new AdESPolicy(
					PreferencesPanel.this.getCadesPolicyIdentifier().getText(),
					PreferencesPanel.this.getCadesPolicyIdentifierHash().getText(),
					PreferencesPanel.this.getCadesPolicyIdentifierHashAlgorithm().getSelectedItem().toString(),
					PreferencesPanel.this.getCadesPolicyQualifier().getText()
				);
        		// No nos interesa el resultado, solo si construye sin excepciones
				try {
					new Oid(PreferencesPanel.this.getXadesPolicyIdentifier().getText().replace("urn:oid:", "")); //$NON-NLS-1$ //$NON-NLS-2$
				}
				catch (final GSSException e) {
					throw new AOException("El identificador debe ser un OID"); //$NON-NLS-1$
				}
			}
			catch(final Exception e) {
				JOptionPane.showMessageDialog(
					this,
					"<html><p>" + Messages.getString("PreferencesPanel.38") + ":<br>" + e.getLocalizedMessage() + "</p></html>", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
					Messages.getString("SimpleAfirma.7"), //$NON-NLS-1$
					JOptionPane.ERROR_MESSAGE
				);
				return false;
			}
		}



		//****************************************************************************
		//**** PREFERENCIAS GENERALES ************************************************
		//****************************************************************************
		PreferencesPanel.PREFERENCES.put(PREFERENCE_SIGNATURE_ALGORITHM, this.signarureAlgorithms.getSelectedItem().toString());
		PreferencesPanel.PREFERENCES.putBoolean(PREFERENCE_OMIT_ASKONCLOSE, this.avoidAskForClose.isSelected());

		//****************************************************************************
		//**** PREFERENCIAS CADES ****************************************************
		//****************************************************************************
		PreferencesPanel.PREFERENCES.put(PREFERENCE_CADES_IMPLICIT, Boolean.valueOf(this.cadesImplicit.isSelected()).toString());
		if (this.cadesPolicies.getSelectedIndex() != POLICY_INDEX_NONE) {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_CADES_POLICY_IDENTIFIER, this.cadesPolicyIdentifier.getText());
			PreferencesPanel.PREFERENCES.put(PREFERENCE_CADES_POLICY_IDENTIFIER_HASH, this.cadesPolicyIdentifierHash.getText());
			PreferencesPanel.PREFERENCES.put(PREFERENCE_CADES_POLICY_IDENTIFIER_HASH_ALGORITHM, this.cadesPolicyIdentifierHashAlgorithm.getSelectedItem().toString());
			PreferencesPanel.PREFERENCES.put(PREFERENCE_CADES_POLICY_QUALIFIER, this.cadesPolicyQualifier.getText());
		}
		else {
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_CADES_POLICY_IDENTIFIER);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_CADES_POLICY_IDENTIFIER_HASH);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_CADES_POLICY_IDENTIFIER_HASH_ALGORITHM);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_CADES_POLICY_QUALIFIER);
		}

		//****************************************************************************
		//**** PREFERENCIAS PADES ****************************************************
		//****************************************************************************
		if ("".equals(this.padesSignerContact.getText())) { //$NON-NLS-1$
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_PADES_SIGNER_CONTACT);
		}
		else {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_PADES_SIGNER_CONTACT, this.padesSignerContact.getText());
		}
		if ("".equals(this.padesSignProductionCity.getText())) { //$NON-NLS-1$
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_PADES_SIGN_PRODUCTION_CITY);
		}
		else {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_PADES_SIGN_PRODUCTION_CITY, this.padesSignProductionCity.getText());
		}
		if ("".equals(this.padesSignReason.getText())) { //$NON-NLS-1$
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_PADES_SIGN_REASON);
		}
		else {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_PADES_SIGN_REASON, this.padesSignReason.getText());
		}
		if (this.padesPolicies.getSelectedIndex() != POLICY_INDEX_NONE) {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_PADES_POLICY_IDENTIFIER, this.padesPolicyIdentifier.getText());
			PreferencesPanel.PREFERENCES.put(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH, this.padesPolicyIdentifierHash.getText());
			PreferencesPanel.PREFERENCES.put(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM, this.padesPolicyIdentifierHashAlgorithm.getSelectedItem().toString());
			PreferencesPanel.PREFERENCES.put(PREFERENCE_PADES_POLICY_QUALIFIER, this.padesPolicyQualifier.getText());
		}
		else {
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_PADES_POLICY_IDENTIFIER);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_PADES_POLICY_QUALIFIER);
		}

		//****************************************************************************
		//**** PREFERENCIAS XADES ****************************************************
		//****************************************************************************
		PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_SIGN_FORMAT, this.xadesSignFormat.getSelectedItem().toString());
		if ("".equals(this.xadesSignatureProductionCity.getText())) { //$NON-NLS-1$
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY);
		}
		else {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_CITY, this.xadesSignatureProductionCity.getText());
		}
		if ("".equals(this.xadesSignatureProductionCountry.getText())) { //$NON-NLS-1$
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY);
		}
		else {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_COUNTRY, this.xadesSignatureProductionCountry.getText());
		}
		if ("".equals(this.xadesSignatureProductionPostalCode.getText())) { //$NON-NLS-1$
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE);
		}
		else {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_POSTAL_CODE, this.xadesSignatureProductionPostalCode.getText());
		}
		if ("".equals(this.xadesSignatureProductionProvince.getText())) { //$NON-NLS-1$
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE);
		}
		else {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_SIGNATURE_PRODUCTION_PROVINCE, this.xadesSignatureProductionProvince.getText());
		}
		if ("".equals(this.xadesSignerCertifiedRole.getText())) { //$NON-NLS-1$
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_SIGNER_CERTIFIED_ROLE);
		}
		else {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_SIGNER_CERTIFIED_ROLE, this.xadesSignerCertifiedRole.getText());
		}
		if ("".equals(this.xadesSignerClaimedRole.getText())) { //$NON-NLS-1$
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE);
		}
		else {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_SIGNER_CLAIMED_ROLE, this.xadesSignerClaimedRole.getText());
		}

		try {
			PreferencesPanel.PREFERENCES.flush();
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("Error al guardar las preferencias de firma: " + e); //$NON-NLS-1$ //$NON-NLS-2$
		}
		if (this.xadesPolicies.getSelectedIndex() != POLICY_INDEX_NONE) {
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_POLICY_IDENTIFIER, this.xadesPolicyIdentifier.getText());
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH, this.xadesPolicyIdentifierHash.getText());
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM, this.xadesPolicyIdentifierHashAlgorithm.getSelectedItem().toString());
			PreferencesPanel.PREFERENCES.put(PREFERENCE_XADES_POLICY_QUALIFIER, this.xadesPolicyQualifier.getText());
		}
		else {
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_POLICY_IDENTIFIER);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_POLICY_IDENTIFIER_HASH_ALGORITHM);
			PreferencesPanel.PREFERENCES.remove(PREFERENCE_XADES_POLICY_QUALIFIER);
		}


	    return true;

	}

	private JPanel createXadesPanel() {
        final JPanel panel = new JPanel();

        panel.add(createXadesPolicyPanel());

        final JPanel metadata = new JPanel();
        metadata.setBorder(BorderFactory.createTitledBorder(Messages.getString("PreferencesPanel.8"))); //$NON-NLS-1$
        metadata.setLayout(new GridLayout(0,1));

        final JLabel xadesSignatureProductionProvinceLabel = new JLabel(Messages.getString("PreferencesPanel.9")); //$NON-NLS-1$
        xadesSignatureProductionProvinceLabel.setLabelFor(this.xadesSignatureProductionProvince);
        metadata.add(xadesSignatureProductionProvinceLabel);
        this.xadesSignatureProductionProvince.addKeyListener(this.modificationListener);
        metadata.add(this.xadesSignatureProductionProvince);

        final JLabel xadesSignatureProductionPostalCodeLabel = new JLabel(Messages.getString("PreferencesPanel.10")); //$NON-NLS-1$
        xadesSignatureProductionPostalCodeLabel.setLabelFor(this.xadesSignatureProductionPostalCode);
        metadata.add(xadesSignatureProductionPostalCodeLabel);
        this.xadesSignatureProductionPostalCode.addKeyListener(this.modificationListener);
        metadata.add(this.xadesSignatureProductionPostalCode);

        final JLabel xadesSignatureProductionCityLabel = new JLabel(Messages.getString("PreferencesPanel.11")); //$NON-NLS-1$
        xadesSignatureProductionCityLabel.setLabelFor(this.xadesSignatureProductionCity);
        metadata.add(xadesSignatureProductionCityLabel);
        this.xadesSignatureProductionCity.addKeyListener(this.modificationListener);
        metadata.add(this.xadesSignatureProductionCity);

        final JLabel xadesSignatureProductionCountryLabel = new JLabel(Messages.getString("PreferencesPanel.12")); //$NON-NLS-1$
        xadesSignatureProductionCountryLabel.setLabelFor(this.xadesSignatureProductionCountry);
        metadata.add(xadesSignatureProductionCountryLabel);
        this.xadesSignatureProductionCountry.addKeyListener(this.modificationListener);
        metadata.add(this.xadesSignatureProductionCountry);

        final JLabel xadesSignerClaimedRoleLabel = new JLabel(Messages.getString("PreferencesPanel.13")); //$NON-NLS-1$
        xadesSignerClaimedRoleLabel.setLabelFor(this.xadesSignerClaimedRole);
        metadata.add(xadesSignerClaimedRoleLabel);
        this.xadesSignerClaimedRole.addKeyListener(this.modificationListener);
        metadata.add(this.xadesSignerClaimedRole);

        final JLabel xadesSignerCertifiedRoleLabel = new JLabel(Messages.getString("PreferencesPanel.14")); //$NON-NLS-1$
        xadesSignerCertifiedRoleLabel.setLabelFor(this.xadesSignerCertifiedRole);
        metadata.add(xadesSignerCertifiedRoleLabel);
        this.xadesSignerCertifiedRole.addKeyListener(this.modificationListener);
        metadata.add(this.xadesSignerCertifiedRole);

        final JPanel format = new JPanel();
        format.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), Messages.getString("PreferencesPanel.15"))); //$NON-NLS-1$
        this.xadesSignFormat.setSelectedItem(
    		PREFERENCES.get(PREFERENCE_XADES_SIGN_FORMAT, AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING)
		);
        this.xadesSignFormat.addItemListener(this.modificationListener);
        format.add(this.xadesSignFormat);

        panel.add(metadata);
        panel.add(format);
        return panel;
	}

	private JPanel createCadesPanel() {
	    final JPanel panel = new JPanel();

	    panel.add(createCadesPolicyPanel());

	    final JPanel signatureMode = new JPanel();
	    signatureMode.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), Messages.getString("PreferencesPanel.16"))); //$NON-NLS-1$
	    this.cadesImplicit.addItemListener(this.modificationListener);
	    signatureMode.add(this.cadesImplicit);
	    panel.add(signatureMode);
	    return panel;
	}

	private JPanel createGeneralPanel() {
		final JPanel panel = new JPanel();
		panel.setBorder(BorderFactory.createTitledBorder(Messages.getString("PreferencesPanel.17"))); //$NON-NLS-1$
		final JPanel signatureAgorithmPanel = new JPanel();
		signatureAgorithmPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), Messages.getString("PreferencesPanel.18"))); //$NON-NLS-1$
		this.signarureAlgorithms.addItemListener(this.modificationListener);
		this.signarureAlgorithms.setModel(new DefaultComboBoxModel(new String[] {
			"SHA1withRSA", //$NON-NLS-1$
			"SHA512withRSA", //$NON-NLS-1$
			"SHA384withRSA", //$NON-NLS-1$
			"SHA256withRSA" //$NON-NLS-1$
		}));
		this.signarureAlgorithms.setSelectedItem(PreferencesPanel.PREFERENCES.get(PREFERENCE_SIGNATURE_ALGORITHM, "SHA1withRSA")); //$NON-NLS-1$
		signatureAgorithmPanel.add(this.signarureAlgorithms);
		panel.add(signatureAgorithmPanel);

		final JPanel generalPreferencesPanel = new JPanel();
		generalPreferencesPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), Messages.getString("PreferencesPanel.37"))); //$NON-NLS-1$
		this.avoidAskForClose.addItemListener(this.modificationListener);
		generalPreferencesPanel.add(this.avoidAskForClose);
		panel.add(generalPreferencesPanel);

		return panel;
	}

	private JPanel createPadesPanel() {
	    final JPanel panel = new JPanel();

        panel.setLayout(new GridLayout(0,1));

        panel.add(createPadesPolicyPanel());

	    panel.setBorder(BorderFactory.createTitledBorder(Messages.getString("PreferencesPanel.19"))); //$NON-NLS-1$

	    final JLabel padesSignReasonLabel = new JLabel(Messages.getString("PreferencesPanel.20")); //$NON-NLS-1$
	    padesSignReasonLabel.setLabelFor(this.padesSignReason);
	    panel.add(padesSignReasonLabel);
	    this.padesSignReason.addKeyListener(this.modificationListener);
	    panel.add(this.padesSignReason);

	    final JLabel padesSignProductionCityLabel = new JLabel(Messages.getString("PreferencesPanel.21")); //$NON-NLS-1$
	    padesSignProductionCityLabel.setLabelFor(this.padesSignProductionCity);
	    panel.add(padesSignProductionCityLabel);
	    this.padesSignProductionCity.addKeyListener(this.modificationListener);
	    panel.add(this.padesSignProductionCity);

	    final JLabel padesSignerContactLabel = new JLabel(Messages.getString("PreferencesPanel.22")); //$NON-NLS-1$
	    padesSignerContactLabel.setLabelFor(this.padesSignerContact);
	    panel.add(padesSignerContactLabel);
	    this.padesSignerContact.addKeyListener(this.modificationListener);
	    panel.add(this.padesSignerContact);

	    return panel;
	}

	private JPanel createXadesPolicyPanel() {
		final JPanel panel = new JPanel();
		panel.setLayout(new GridLayout(0,1));
		panel.setBorder(BorderFactory.createTitledBorder(Messages.getString("PreferencesPanel.23"))); //$NON-NLS-1$

		// Los elementos del menu desplegable se identifican por su orden
		this.xadesPolicies.setModel(new DefaultComboBoxModel(new String[] {
			Messages.getString("PreferencesPanel.24"),	// Ninguna politica, debe ser el primer elemento //$NON-NLS-1$
			Messages.getString("PreferencesPanel.25"),	// Politica de la AGE, debe ser el segundo elemento //$NON-NLS-1$
			Messages.getString("PreferencesPanel.26")	// Politica a medida, debe ser el ultimo elemento //$NON-NLS-1$
		}));
		panel.add(this.xadesPolicies);
		this.xadesPolicies.addItemListener(this.modificationListener);
		this.xadesPolicies.addItemListener(new ItemListener() {
			/** {@inheritDoc} */
			@Override
			public void itemStateChanged(final ItemEvent ie) {

				final boolean enabled = PreferencesPanel.this.getXadesPolicies().getSelectedIndex() != POLICY_INDEX_NONE;
				final boolean editable = PreferencesPanel.this.getXadesPolicies().getSelectedIndex() != POLICY_INDEX_AGE;

				PreferencesPanel.this.getXadesPolicyIdentifier().setEnabled(enabled);
				PreferencesPanel.this.getXadesPolicyIdentifierHash().setEnabled(enabled);
				PreferencesPanel.this.getXadesPolicyIdentifierHashAlgorithm().setEnabled(enabled);
				PreferencesPanel.this.getXadesPolicyQualifier().setEnabled(enabled);

				PreferencesPanel.this.getXadesPolicyIdentifier().setEditable(editable);
				PreferencesPanel.this.getXadesPolicyIdentifierHash().setEditable(editable);
				PreferencesPanel.this.getXadesPolicyIdentifierHashAlgorithm().setEnabled(editable);
				PreferencesPanel.this.getXadesPolicyQualifier().setEditable(editable);

				loadXadesPolicy(getXadesPreloadedPolicies().get(PreferencesPanel.this.getXadesPolicies().getSelectedIndex()));
			}
		});

		final boolean enableTextFields = this.xadesPolicies.getSelectedIndex() != POLICY_INDEX_NONE;
		final boolean editableTextFields = this.xadesPolicies.getSelectedIndex() != POLICY_INDEX_AGE;

		this.xadesPolicyIdentifier.setEnabled(enableTextFields);
		this.xadesPolicyIdentifier.setEditable(editableTextFields);
		this.xadesPolicyIdentifier.addKeyListener(this.modificationListener);
		final JLabel policyIdentifierLabel = new JLabel(Messages.getString("PreferencesPanel.27")); //$NON-NLS-1$
		policyIdentifierLabel.setLabelFor(this.xadesPolicyIdentifier);
		panel.add(policyIdentifierLabel);
		panel.add(this.xadesPolicyIdentifier);

		this.xadesPolicyIdentifierHash.setEnabled(enableTextFields);
		this.xadesPolicyIdentifierHash.setEditable(editableTextFields);
		this.xadesPolicyIdentifierHash.addKeyListener(this.modificationListener);
		final JLabel policyIdentifierHashLabel = new JLabel(Messages.getString("PreferencesPanel.28")); //$NON-NLS-1$
		policyIdentifierHashLabel.setLabelFor(this.xadesPolicyIdentifierHash);
		panel.add(policyIdentifierHashLabel);
		panel.add(this.xadesPolicyIdentifierHash);

		this.xadesPolicyIdentifierHashAlgorithm.setEnabled(enableTextFields);
		this.xadesPolicyIdentifierHashAlgorithm.addItemListener(this.modificationListener);
		final JLabel policyIdentifierHashAlgorithmLabel = new JLabel(Messages.getString("PreferencesPanel.29")); //$NON-NLS-1$
		policyIdentifierHashAlgorithmLabel.setLabelFor(this.xadesPolicyIdentifierHashAlgorithm);
		panel.add(policyIdentifierHashAlgorithmLabel);
		panel.add(this.xadesPolicyIdentifierHashAlgorithm);

		this.xadesPolicyQualifier.setEnabled(enableTextFields);
		this.xadesPolicyQualifier.setEditable(editableTextFields);
		this.xadesPolicyQualifier.addKeyListener(this.modificationListener);
		final JLabel policyQualifierLabel = new JLabel(Messages.getString("PreferencesPanel.30")); //$NON-NLS-1$
		policyQualifierLabel.setLabelFor(this.xadesPolicyQualifier);
		panel.add(policyQualifierLabel);
		panel.add(this.xadesPolicyQualifier);

		// Cargamos la politica de las preferencias cambiado el Combo si es preciso
		final AdESPolicy savedPolicy = XADES_PRELOADED_POLICIES.get(this.xadesPolicies.getItemCount()-1);
		if (savedPolicy != null) {
			if (POLICY_XADES_AGE.equals(savedPolicy)) {
				this.xadesPolicies.setSelectedIndex(POLICY_INDEX_AGE);
			}
			else {
				this.xadesPolicies.setSelectedIndex(this.xadesPolicies.getItemCount()-1);
			}
		}

		return panel;
	}

	private JPanel createPadesPolicyPanel() {
		final JPanel panel = new JPanel();
		panel.setLayout(new GridLayout(0,1));
		panel.setBorder(BorderFactory.createTitledBorder(Messages.getString("PreferencesPanel.23"))); //$NON-NLS-1$

		// Los elementos del menu desplegable se identifican por su orden
		this.padesPolicies.setModel(new DefaultComboBoxModel(new String[] {
			Messages.getString("PreferencesPanel.24"),	// Ninguna politica, debe ser el primer elemento //$NON-NLS-1$
			Messages.getString("PreferencesPanel.25"),	// Politica de la AGE, debe ser el segundo elemento //$NON-NLS-1$
			Messages.getString("PreferencesPanel.26")	// Politica a medida, debe ser el ultimo elemento //$NON-NLS-1$
		}));
		panel.add(this.padesPolicies);
		this.padesPolicies.addItemListener(this.modificationListener);
		this.padesPolicies.addItemListener(new ItemListener() {
			/** {@inheritDoc} */
			@Override
			public void itemStateChanged(final ItemEvent ie) {

				final boolean enabled = PreferencesPanel.this.getPadesPolicies().getSelectedIndex() != POLICY_INDEX_NONE;
				final boolean editable = PreferencesPanel.this.getPadesPolicies().getSelectedIndex() != POLICY_INDEX_AGE;

				PreferencesPanel.this.getPadesPolicyIdentifier().setEnabled(enabled);
				PreferencesPanel.this.getPadesPolicyIdentifierHash().setEnabled(enabled);
				PreferencesPanel.this.getPadesPolicyIdentifierHashAlgorithm().setEnabled(enabled);
				PreferencesPanel.this.getPadesPolicyQualifier().setEnabled(enabled);

				PreferencesPanel.this.getPadesPolicyIdentifier().setEditable(editable);
				PreferencesPanel.this.getPadesPolicyIdentifierHash().setEditable(editable);
				PreferencesPanel.this.getPadesPolicyIdentifierHashAlgorithm().setEnabled(editable);
				PreferencesPanel.this.getPadesPolicyQualifier().setEditable(editable);

				loadPadesPolicy(getPadesPreloadedPolicies().get(PreferencesPanel.this.getPadesPolicies().getSelectedIndex()));
			}
		});

		final boolean enableTextFields = this.padesPolicies.getSelectedIndex() != POLICY_INDEX_NONE;
		final boolean editableTextFields = this.padesPolicies.getSelectedIndex() != POLICY_INDEX_AGE;

		this.padesPolicyIdentifier.setEnabled(enableTextFields);
		this.padesPolicyIdentifier.setEditable(editableTextFields);
		this.padesPolicyIdentifier.addKeyListener(this.modificationListener);
		final JLabel policyIdentifierLabel = new JLabel(Messages.getString("PreferencesPanel.39")); //$NON-NLS-1$
		policyIdentifierLabel.setLabelFor(this.padesPolicyIdentifier);
		panel.add(policyIdentifierLabel);
		panel.add(this.padesPolicyIdentifier);

		this.padesPolicyIdentifierHash.setEnabled(enableTextFields);
		this.padesPolicyIdentifierHash.setEditable(editableTextFields);
		this.padesPolicyIdentifierHash.addKeyListener(this.modificationListener);
		final JLabel policyIdentifierHashLabel = new JLabel(Messages.getString("PreferencesPanel.28")); //$NON-NLS-1$
		policyIdentifierHashLabel.setLabelFor(this.padesPolicyIdentifierHash);
		panel.add(policyIdentifierHashLabel);
		panel.add(this.padesPolicyIdentifierHash);

		this.padesPolicyIdentifierHashAlgorithm.setEnabled(enableTextFields);
		this.padesPolicyIdentifierHashAlgorithm.addItemListener(this.modificationListener);
		final JLabel policyIdentifierHashAlgorithmLabel = new JLabel(Messages.getString("PreferencesPanel.29")); //$NON-NLS-1$
		policyIdentifierHashAlgorithmLabel.setLabelFor(this.padesPolicyIdentifierHashAlgorithm);
		panel.add(policyIdentifierHashAlgorithmLabel);
		panel.add(this.padesPolicyIdentifierHashAlgorithm);

		this.padesPolicyQualifier.setEnabled(enableTextFields);
		this.padesPolicyQualifier.setEditable(editableTextFields);
		this.padesPolicyQualifier.addKeyListener(this.modificationListener);
		final JLabel policyQualifierLabel = new JLabel(Messages.getString("PreferencesPanel.30")); //$NON-NLS-1$
		policyQualifierLabel.setLabelFor(this.padesPolicyQualifier);
		panel.add(policyQualifierLabel);
		panel.add(this.padesPolicyQualifier);

		// Cargamos la politica de las preferencias cambiado el Combo si es preciso
		final AdESPolicy savedPolicy = PADES_PRELOADED_POLICIES.get(this.padesPolicies.getItemCount()-1);
		if (savedPolicy != null) {
			if (POLICY_CADES_AGE.equals(savedPolicy)) {
				this.padesPolicies.setSelectedIndex(POLICY_INDEX_AGE);
			}
			else {
				this.padesPolicies.setSelectedIndex(this.padesPolicies.getItemCount()-1);
			}
		}

		return panel;
	}

	private JPanel createCadesPolicyPanel() {
		final JPanel panel = new JPanel();
		panel.setLayout(new GridLayout(0,1));
		panel.setBorder(BorderFactory.createTitledBorder(Messages.getString("PreferencesPanel.23"))); //$NON-NLS-1$

		// Los elementos del menu desplegable se identifican por su orden
		this.cadesPolicies.setModel(new DefaultComboBoxModel(new String[] {
			Messages.getString("PreferencesPanel.24"),	// Ninguna politica, debe ser el primer elemento //$NON-NLS-1$
			Messages.getString("PreferencesPanel.25"),	// Politica de la AGE, debe ser el segundo elemento //$NON-NLS-1$
			Messages.getString("PreferencesPanel.26")	// Politica a medida, debe ser el ultimo elemento //$NON-NLS-1$
		}));
		panel.add(this.cadesPolicies);
		this.cadesPolicies.addItemListener(this.modificationListener);
		this.cadesPolicies.addItemListener(new ItemListener() {
			/** {@inheritDoc} */
			@Override
			public void itemStateChanged(final ItemEvent ie) {

				final boolean enabled = PreferencesPanel.this.getCadesPolicies().getSelectedIndex() != POLICY_INDEX_NONE;
				final boolean editable = PreferencesPanel.this.getCadesPolicies().getSelectedIndex() != POLICY_INDEX_AGE;

				PreferencesPanel.this.getCadesPolicyIdentifier().setEnabled(enabled);
				PreferencesPanel.this.getCadesPolicyIdentifierHash().setEnabled(enabled);
				PreferencesPanel.this.getCadesPolicyIdentifierHashAlgorithm().setEnabled(enabled);
				PreferencesPanel.this.getCadesPolicyQualifier().setEnabled(enabled);

				PreferencesPanel.this.getCadesPolicyIdentifier().setEditable(editable);
				PreferencesPanel.this.getCadesPolicyIdentifierHash().setEditable(editable);
				PreferencesPanel.this.getCadesPolicyIdentifierHashAlgorithm().setEnabled(editable);
				PreferencesPanel.this.getCadesPolicyQualifier().setEditable(editable);

				loadCadesPolicy(getCadesPreloadedPolicies().get(PreferencesPanel.this.getCadesPolicies().getSelectedIndex()));
			}
		});

		final boolean enableTextFields = this.cadesPolicies.getSelectedIndex() != POLICY_INDEX_NONE;
		final boolean editableTextFields = this.cadesPolicies.getSelectedIndex() != POLICY_INDEX_AGE;

		this.cadesPolicyIdentifier.setEnabled(enableTextFields);
		this.cadesPolicyIdentifier.setEditable(editableTextFields);
		this.cadesPolicyIdentifier.addKeyListener(this.modificationListener);
		final JLabel policyIdentifierLabel = new JLabel(Messages.getString("PreferencesPanel.39")); //$NON-NLS-1$
		policyIdentifierLabel.setLabelFor(this.cadesPolicyIdentifier);
		panel.add(policyIdentifierLabel);
		panel.add(this.cadesPolicyIdentifier);

		this.cadesPolicyIdentifierHash.setEnabled(enableTextFields);
		this.cadesPolicyIdentifierHash.setEditable(editableTextFields);
		this.cadesPolicyIdentifierHash.addKeyListener(this.modificationListener);
		final JLabel policyIdentifierHashLabel = new JLabel(Messages.getString("PreferencesPanel.28")); //$NON-NLS-1$
		policyIdentifierHashLabel.setLabelFor(this.cadesPolicyIdentifierHash);
		panel.add(policyIdentifierHashLabel);
		panel.add(this.cadesPolicyIdentifierHash);

		this.cadesPolicyIdentifierHashAlgorithm.setEnabled(enableTextFields);
		this.cadesPolicyIdentifierHashAlgorithm.addItemListener(this.modificationListener);
		final JLabel policyIdentifierHashAlgorithmLabel = new JLabel(Messages.getString("PreferencesPanel.29")); //$NON-NLS-1$
		policyIdentifierHashAlgorithmLabel.setLabelFor(this.cadesPolicyIdentifierHashAlgorithm);
		panel.add(policyIdentifierHashAlgorithmLabel);
		panel.add(this.cadesPolicyIdentifierHashAlgorithm);

		this.cadesPolicyQualifier.setEnabled(enableTextFields);
		this.cadesPolicyQualifier.setEditable(editableTextFields);
		this.cadesPolicyQualifier.addKeyListener(this.modificationListener);
		final JLabel policyQualifierLabel = new JLabel(Messages.getString("PreferencesPanel.30")); //$NON-NLS-1$
		policyQualifierLabel.setLabelFor(this.cadesPolicyQualifier);
		panel.add(policyQualifierLabel);
		panel.add(this.cadesPolicyQualifier);

		// Cargamos la politica de las preferencias cambiado el Combo si es preciso
		final AdESPolicy savedPolicy = CADES_PRELOADED_POLICIES.get(this.cadesPolicies.getItemCount()-1);
		if (savedPolicy != null) {
			if (POLICY_CADES_AGE.equals(savedPolicy)) {
				this.cadesPolicies.setSelectedIndex(POLICY_INDEX_AGE);
			}
			else {
				this.cadesPolicies.setSelectedIndex(this.cadesPolicies.getItemCount()-1);
			}
		}

		return panel;
	}

	private JPanel createButtonsPanel() {
		final JPanel panel = new JPanel();
		final JButton cancelButton = new JButton(Messages.getString("PreferencesPanel.31")); //$NON-NLS-1$
		cancelButton.setMnemonic('C');
		cancelButton.getAccessibleContext().setAccessibleDescription(
			Messages.getString("PreferencesPanel.32") //$NON-NLS-1$
		);
		cancelButton.addActionListener(new ActionListener() {
		    /** {@inheritDoc} */
            @Override
            public void actionPerformed(final ActionEvent ae) {
                PreferencesPanel.this.getParentWindow().dispose();
            }
        });
		panel.add(cancelButton);
		final JButton acceptButton = new JButton(Messages.getString("PreferencesPanel.33")); //$NON-NLS-1$
		acceptButton.setMnemonic('A');
		acceptButton.getAccessibleContext().setAccessibleDescription(
			Messages.getString("PreferencesPanel.34") //$NON-NLS-1$
		);
		acceptButton.addActionListener(new ActionListener() {
			/** {@inheritDoc} */
			@Override
			public void actionPerformed(final ActionEvent ae) {
				if (savePreferences()) {
				    PreferencesPanel.this.getParentWindow().dispose();
				}
			}
		});
		panel.add(acceptButton);
		this.applyButton.setMnemonic('p');
		this.applyButton.getAccessibleContext().setAccessibleDescription(
			Messages.getString("PreferencesPanel.35") //$NON-NLS-1$
		);
		this.applyButton.addActionListener(new ActionListener() {
			/** {@inheritDoc} */
			@Override
			public void actionPerformed(final ActionEvent ae) {
				if (savePreferences()) {
				    setModified(false);
				}
			}
		});
		this.applyButton.setEnabled(false);
		panel.add(this.applyButton);
		return panel;
	}

	PreferencesPanel(final Window w) {
	    if (w==null) {
	        throw new IllegalArgumentException("Es necesario proporcionar una referencia a la ventana contenedora"); //$NON-NLS-1$
	    }
	    this.window = w;
	    this.modificationListener = new ModificationListener(this);
	    createUI();
	}

	/** Indica si se ha modificado algun dato desde el &ucuate;ltimo guardado, y por lo tanto si hay algo nuevo para guardar. */
	void setModified(final boolean mod) {
	    this.applyButton.setEnabled(mod);
	}

	void loadXadesPolicy(final AdESPolicy policy) {
		if (policy != null) {
			PreferencesPanel.this.getXadesPolicyIdentifier().setText(policy.getPolicyIdentifier());
			PreferencesPanel.this.getXadesPolicyIdentifierHash().setText(policy.getPolicyIdentifierHash());
			PreferencesPanel.this.getXadesPolicyIdentifierHashAlgorithm().setSelectedItem(policy.getPolicyIdentifierHashAlgorithm());
			PreferencesPanel.this.getXadesPolicyQualifier().setText(policy.getPolicyQualifier().toString());
		}
		else {
			PreferencesPanel.this.getXadesPolicyIdentifier().setText(""); //$NON-NLS-1$
			PreferencesPanel.this.getXadesPolicyIdentifierHash().setText(""); //$NON-NLS-1$
			PreferencesPanel.this.getXadesPolicyQualifier().setText(""); //$NON-NLS-1$
		}
	}

	void loadPadesPolicy(final AdESPolicy policy) {
		if (policy != null) {
			PreferencesPanel.this.getPadesPolicyIdentifier().setText(policy.getPolicyIdentifier());
			PreferencesPanel.this.getPadesPolicyIdentifierHash().setText(policy.getPolicyIdentifierHash());
			PreferencesPanel.this.getPadesPolicyIdentifierHashAlgorithm().setSelectedItem(policy.getPolicyIdentifierHashAlgorithm());
			PreferencesPanel.this.getPadesPolicyQualifier().setText(policy.getPolicyQualifier().toString());
		}
		else {
			PreferencesPanel.this.getPadesPolicyIdentifier().setText(""); //$NON-NLS-1$
			PreferencesPanel.this.getPadesPolicyIdentifierHash().setText(""); //$NON-NLS-1$
			PreferencesPanel.this.getPadesPolicyQualifier().setText(""); //$NON-NLS-1$
		}
	}

	void loadCadesPolicy(final AdESPolicy policy) {
		if (policy != null) {
			PreferencesPanel.this.getCadesPolicyIdentifier().setText(policy.getPolicyIdentifier());
			PreferencesPanel.this.getCadesPolicyIdentifierHash().setText(policy.getPolicyIdentifierHash());
			PreferencesPanel.this.getCadesPolicyIdentifierHashAlgorithm().setSelectedItem(policy.getPolicyIdentifierHashAlgorithm());
			PreferencesPanel.this.getCadesPolicyQualifier().setText(policy.getPolicyQualifier().toString());
		}
		else {
			PreferencesPanel.this.getCadesPolicyIdentifier().setText(""); //$NON-NLS-1$
			PreferencesPanel.this.getCadesPolicyIdentifierHash().setText(""); //$NON-NLS-1$
			PreferencesPanel.this.getCadesPolicyQualifier().setText(""); //$NON-NLS-1$
		}
	}

	private static final class ModificationListener extends KeyAdapter implements ItemListener {

	    private final PreferencesPanel prefPanel;

        ModificationListener(final PreferencesPanel pp) {
            if (pp == null) {
                throw new IllegalArgumentException(
            		"Se necesita un ModificationListener para indicar que ha habido modificaciones, no puede ser nulo" //$NON-NLS-1$
        		);
            }
            this.prefPanel = pp;
        }

        /** {@inheritDoc} */
        @Override
        public void keyReleased(final KeyEvent arg0) {
            this.prefPanel.setModified(true);
        }

        /** {@inheritDoc} */
		@Override
		public void itemStateChanged(final ItemEvent arg0) {
			this.prefPanel.setModified(true);
		}

	}

}
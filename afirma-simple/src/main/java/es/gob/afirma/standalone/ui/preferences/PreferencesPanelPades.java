package es.gob.afirma.standalone.ui.preferences;

import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_FORMAT;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_POLICY_IDENTIFIER;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_POLICY_IDENTIFIER_HASH;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_POLICY_QUALIFIER;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_SIGNER_CONTACT;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_SIGN_PRODUCTION_CITY;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_SIGN_REASON;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_PADES_VISIBLE;

import java.awt.Container;
import java.awt.Cursor;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyListener;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.ietf.jgss.GSSException;
import org.ietf.jgss.Oid;

import es.gob.afirma.core.AOException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.signers.AdESPolicy;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.ui.preferences.PolicyPanel.PolicyItem;
import es.gob.afirma.standalone.ui.preferences.PreferencesPanel.ValueTextPair;

final class PreferencesPanelPades extends JPanel {
	
	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private static final long serialVersionUID = 4299378019540627483L;

	private final JPanel panelPolicies = new JPanel();
	
	private PolicyPanel padesPolicyDlg;
	
	/**
	 * Atributo que representa la etiqueta de la pol&iacute;tica seleccionada en
	 * el di&aacute;logo.
	 */
	private JLabel policyLabel;

	private static final AdESPolicy POLICY_CADES_PADES_AGE_1_9 = new AdESPolicy(
		"2.16.724.1.3.1.1.2.1.9", //$NON-NLS-1$
		"G7roucf600+f03r/o0bAOQ6WAs0=", //$NON-NLS-1$
		"SHA1", //$NON-NLS-1$
		"https://sede.060.gob.es/politica_de_firma_anexo_1.pdf" //$NON-NLS-1$
	);
	
	/**
	 * Atributo para gestionar el bloqueo de propiedades.
	 */
	private boolean unprotected = true;

	private final JComboBox<Object> padesBasicFormat = new JComboBox<>();
	JComboBox<Object> getBasicPadesFormat() {
		return this.padesBasicFormat;
	}

	private final JTextField padesSignReason = new JTextField();

	private final JTextField padesSignProductionCity = new JTextField();

	private final JTextField padesSignerContact = new JTextField();

	private final JCheckBox visiblePdfSignature = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.79")); //$NON-NLS-1$
	
	

	private static final String PADES_FORMAT_BASIC_TEXT = SimpleAfirmaMessages.getString("PreferencesPanel.71"); //$NON-NLS-1$
	private static final String PADES_FORMAT_BES_TEXT = SimpleAfirmaMessages.getString("PreferencesPanel.72"); //$NON-NLS-1$

	private static final String SIGN_FORMAT_PADES = "PAdES"; //$NON-NLS-1$

	PreferencesPanelPades(final KeyListener keyListener,
						  final ModificationListener modificationListener,
						  final boolean unprotected) {

		this.unprotected = unprotected;
		createUI(keyListener, modificationListener);
	}

	void createUI(final KeyListener keyListener,
				  final ModificationListener modificationListener) {

		setLayout(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1.0;
        gbc.gridy = 0;
       
		this.padesBasicFormat.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.70") //$NON-NLS-1$
		);
		final DefaultComboBoxModel<Object> padesFormatModel = new DefaultComboBoxModel<>(
			new Object[] {
				new ValueTextPair(AOSignConstants.PADES_SUBFILTER_BES, PADES_FORMAT_BES_TEXT),
				new ValueTextPair(AOSignConstants.PADES_SUBFILTER_BASIC, PADES_FORMAT_BASIC_TEXT)
			}
		);
		
		// Panel para el boton de restaurar la configuracion
		final JPanel panelGeneral = new JPanel(new FlowLayout(FlowLayout.LEADING));
		panelGeneral.setBorder(BorderFactory.createTitledBorder(
		    		SimpleAfirmaMessages.getString("PreferencesPanel.108")) //$NON-NLS-1$
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

					loadDefaultPreferences();

				}
			}
		});
		restoreConfigButton.getAccessibleContext()
				.setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.136") //$NON-NLS-1$
		);
		
		final JLabel restoreConfigLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.146")); //$NON-NLS-1$
		restoreConfigLabel.setLabelFor(restoreConfigButton);		
		
		final GridBagConstraints gbcGeneral = new GridBagConstraints();
		gbcGeneral.fill = GridBagConstraints.HORIZONTAL;

		panelGeneral.add(restoreConfigLabel);
		panelGeneral.add(restoreConfigButton);
		
		add(panelGeneral, gbcGeneral);
				
		this.padesBasicFormat.setModel(padesFormatModel);
		this.padesBasicFormat.addItemListener(modificationListener);
		this.padesBasicFormat.addKeyListener(keyListener);
		this.padesBasicFormat.setEnabled(!this.unprotected);

        loadPreferences();
        
        final List<PolicyPanel.PolicyItem> padesPolicies = new ArrayList<>();
        padesPolicies.add(
    		new PolicyItem(
        		SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
        		POLICY_CADES_PADES_AGE_1_9
    		)
		);
        
        this.padesPolicyDlg = new PolicyPanel(
        		SIGN_FORMAT_PADES,
        		padesPolicies,
        		getPadesPreferedPolicy(),
        		this.padesBasicFormat,
        		this.unprotected
    		);
    	
    	this.padesPolicyDlg.setModificationListener(modificationListener);
    	this.padesPolicyDlg.setKeyListener(keyListener);
        
        ///////////// Panel Policy ////////////////
        
        final JPanel policyConfigPanel = new JPanel(new FlowLayout(FlowLayout.LEADING));
		policyConfigPanel.setBorder(
			BorderFactory.createTitledBorder(
				BorderFactory.createTitledBorder(SimpleAfirmaMessages.getString("PreferencesPanel.153")) //$NON-NLS-1$
			)
		);

		final JButton policyConfigButton = new JButton(
			SimpleAfirmaMessages.getString("PreferencesPanel.150") //$NON-NLS-1$
		);
		
		this.policyLabel = new JLabel(this.padesPolicyDlg.getSelectedPolicy());
		this.policyLabel.setLabelFor(policyConfigButton);

		policyConfigButton.setMnemonic('P');
		policyConfigButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent ae) {
					changePadesPolicyDlg(getParent());
				}
			}
		);
		policyConfigButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.151") //$NON-NLS-1$
		);
		
		policyConfigButton.setEnabled(!isUnprotected());
		policyConfigPanel.add(this.policyLabel);
		policyConfigPanel.add(policyConfigButton);
        
        ///////////// Fin Panel Policy ////////////////
		
        gbc.gridy++;
              
        add(policyConfigPanel, gbc);
        
	    final JPanel metadataPanel = new JPanel();
        metadataPanel.setBorder(BorderFactory.createTitledBorder(
    		SimpleAfirmaMessages.getString("PreferencesPanel.19")) //$NON-NLS-1$
		);
        metadataPanel.setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;

	    final JLabel padesSignReasonLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.20")); //$NON-NLS-1$
	    padesSignReasonLabel.setLabelFor(this.padesSignReason);
	    metadataPanel.add(padesSignReasonLabel, c);

	    c.gridy++;

	    this.padesSignReason.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.63")); //$NON-NLS-1$
	    this.padesSignReason.addKeyListener(modificationListener);
	    this.padesSignReason.addKeyListener(keyListener);
	    metadataPanel.add(this.padesSignReason, c);

	    c.gridy++;

	    final JLabel padesSignProductionCityLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.21")); //$NON-NLS-1$
	    padesSignProductionCityLabel.setLabelFor(this.padesSignProductionCity);
	    metadataPanel.add(padesSignProductionCityLabel, c);

	    c.gridy++;

	    this.padesSignProductionCity.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.64")); //$NON-NLS-1$
	    this.padesSignProductionCity.addKeyListener(modificationListener);
	    this.padesSignProductionCity.addKeyListener(keyListener);
	    metadataPanel.add(this.padesSignProductionCity, c);

	    c.gridy++;

	    final JLabel padesSignerContactLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.22")); //$NON-NLS-1$
	    padesSignerContactLabel.setLabelFor(this.padesSignerContact);
	    metadataPanel.add(padesSignerContactLabel, c);

	    c.gridy++;

	    this.padesSignerContact.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("PreferencesPanel.65")); //$NON-NLS-1$
	    this.padesSignerContact.addKeyListener(modificationListener);
	    this.padesSignerContact.addKeyListener(keyListener);
	    metadataPanel.add(this.padesSignerContact, c);

	    c.gridy++;
	    c.weighty = 1.0;
	    metadataPanel.add(new JPanel(), c);

	    gbc.gridy++;

	    add(metadataPanel, gbc);

		final FlowLayout fLayout = new FlowLayout(FlowLayout.LEADING);
		final JPanel padesPreferencesPanel = new JPanel(fLayout);
		padesPreferencesPanel.setBorder(BorderFactory.createTitledBorder(
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
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weightx = 1.0;

		final JLabel fileFormatLabel = new JLabel(
				SimpleAfirmaMessages.getString("PreferencesPanel.115") //$NON-NLS-1$
		);
		fileFormatLabel.addKeyListener(keyListener);

		cf.anchor = GridBagConstraints.LINE_START;
		panelFirm.add(fileFormatLabel, cf);
		cf.gridy = 1;
		panelFirm.add(this.padesBasicFormat, cf);
		cf.gridy = 0;
		
		padesPreferencesPanel.setLayout(new GridBagLayout());
		final GridBagConstraints fc = new GridBagConstraints();
		fc.weightx = 1.0;
		fc.anchor = GridBagConstraints.LINE_START;

		padesPreferencesPanel.add(panelFirm, fc);
		padesPreferencesPanel.add(createVisiblePdfPanel(keyListener, modificationListener), fc);

		gbc.gridy++;
		add(padesPreferencesPanel, gbc);

	    gbc.gridy++;
	    gbc.weighty = 1.0;
	    add(new JPanel(), gbc); // Panel de relleno
	}

	private JPanel createVisiblePdfPanel(final KeyListener keyListener, final ModificationListener modificationListener) {
		final JPanel panel = new JPanel();
        panel.setBorder(
    		BorderFactory.createEmptyBorder()

		);
        panel.setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weightx = 1.0;

    	final JLabel visiblePdfSignatureLabel = new JLabel(
				SimpleAfirmaMessages.getString("PreferencesPanel.80") //$NON-NLS-1$
		);
    	visiblePdfSignatureLabel.setLabelFor(this.visiblePdfSignature);
        this.visiblePdfSignature.setMnemonic('i');
        panel.add(visiblePdfSignatureLabel, c);
        c.gridy = 1;
        panel.add(this.visiblePdfSignature, c);


    	this.visiblePdfSignature.addItemListener(modificationListener);
    	this.visiblePdfSignature.addKeyListener(keyListener);

        return panel;
	}

	void checkPreferences() throws AOException {
		final AdESPolicy p = this.padesPolicyDlg.getCurrentPolicy();
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
		// Firma PDF visible
		PreferencesManager.put(PREFERENCE_PADES_VISIBLE, Boolean.toString(this.visiblePdfSignature.isSelected()));

		if ("".equals(this.padesSignerContact.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_PADES_SIGNER_CONTACT);
		}
		else {
			PreferencesManager.put(PREFERENCE_PADES_SIGNER_CONTACT, this.padesSignerContact.getText());
		}
		if ("".equals(this.padesSignProductionCity.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_PADES_SIGN_PRODUCTION_CITY);
		}
		else {
			PreferencesManager.put(PREFERENCE_PADES_SIGN_PRODUCTION_CITY, this.padesSignProductionCity.getText());
		}
		if ("".equals(this.padesSignReason.getText())) { //$NON-NLS-1$
			PreferencesManager.remove(PREFERENCE_PADES_SIGN_REASON);
		}
		else {
			PreferencesManager.put(PREFERENCE_PADES_SIGN_REASON, this.padesSignReason.getText());
		}

		PreferencesManager.put(PREFERENCE_PADES_FORMAT, ((ValueTextPair) this.padesBasicFormat.getSelectedItem()).getValue());
		
		final AdESPolicy padesPolicy = this.padesPolicyDlg.getCurrentPolicy();
		if (padesPolicy != null) {
			PreferencesManager.put(PREFERENCE_PADES_POLICY_IDENTIFIER, padesPolicy.getPolicyIdentifier());
			PreferencesManager.put(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH, padesPolicy.getPolicyIdentifierHash());
			PreferencesManager.put(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM, padesPolicy.getPolicyIdentifierHashAlgorithm());
			if (padesPolicy.getPolicyQualifier() != null) {
				PreferencesManager.put(PREFERENCE_PADES_POLICY_QUALIFIER, padesPolicy.getPolicyQualifier().toString());
			}
			else {
				PreferencesManager.remove(PREFERENCE_PADES_POLICY_QUALIFIER);
			}
		}
		else {
			PreferencesManager.remove(PREFERENCE_PADES_POLICY_IDENTIFIER);
			PreferencesManager.remove(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH);
			PreferencesManager.remove(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM);
			PreferencesManager.remove(PREFERENCE_PADES_POLICY_QUALIFIER);
		}
		this.padesPolicyDlg.saveCurrentPolicy();
	}

	void loadPreferences() {
		this.padesSignReason.setText(PreferencesManager.get(PREFERENCE_PADES_SIGN_REASON, "")); //$NON-NLS-1$
		this.padesSignProductionCity.setText(PreferencesManager.get(PREFERENCE_PADES_SIGN_PRODUCTION_CITY, "")); //$NON-NLS-1$
		this.padesSignerContact.setText(PreferencesManager.get(PREFERENCE_PADES_SIGNER_CONTACT, "")); //$NON-NLS-1$
		this.visiblePdfSignature.setSelected(PreferencesManager.getBoolean(PREFERENCE_PADES_VISIBLE, false));

        final ComboBoxModel<Object> padesFormatModel = this.padesBasicFormat.getModel();
        final String selectedValue = PreferencesManager.get(
			PREFERENCE_PADES_FORMAT,
			AOSignConstants.PADES_SUBFILTER_BASIC
		);
		for (int i = 0; i < padesFormatModel.getSize(); i++) {
			if (padesFormatModel.getElementAt(i).equals(selectedValue)) {
				this.padesBasicFormat.setSelectedIndex(i);
				break;
			}
		}

		final List<PolicyPanel.PolicyItem> padesPolicies = new ArrayList<>();
        padesPolicies.add(
    		new PolicyItem(
        		SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
        		POLICY_CADES_PADES_AGE_1_9
    		)
		);
        this.panelPolicies.removeAll();
        this.padesPolicyDlg = new PolicyPanel(
    		SIGN_FORMAT_PADES,
    		padesPolicies,
    		getPadesPreferedPolicy(),
    		this.padesBasicFormat,
    		this.unprotected
		);

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;
        this.panelPolicies.add(this.padesPolicyDlg, c);
        revalidate();
        repaint();
	}
	
	/**
	 * Carga las opciones de configuraci&oacute;n por defecto del panel de
	 * firmas PAdES desde un fichero externo de preferencias.
	 * 
	 * @param unprotected
	 *            {@code true} Si las opciones de configuraci&oacute;n sensibles
	 *            a ello est&aacute;n protegidas y no pueden ser modificadas,
	 *            {@code false} en caso contrario
	 */
	void loadDefaultPreferences() {
		
		this.padesSignReason.setText(PreferencesManager.getPreference(PREFERENCE_PADES_SIGN_REASON, "")); //$NON-NLS-1$
		this.padesSignProductionCity.setText(PreferencesManager.getPreference(PREFERENCE_PADES_SIGN_PRODUCTION_CITY, "")); //$NON-NLS-1$
		this.padesSignerContact.setText(PreferencesManager.getPreference(PREFERENCE_PADES_SIGNER_CONTACT, "")); //$NON-NLS-1$
		this.visiblePdfSignature.setSelected(PreferencesManager.getBooleanPreference(PREFERENCE_PADES_VISIBLE, false));

        final ComboBoxModel<Object> padesFormatModel = this.padesBasicFormat.getModel();
        
        // unprotected: true -> no puedo modificarla, cargo la que estaba
		if (isUnprotected()) {
			final String selectedValue = PreferencesManager.get(PREFERENCE_PADES_FORMAT,
					AOSignConstants.PADES_SUBFILTER_BASIC);
			
			for (int i = 0; i < padesFormatModel.getSize(); i++) {
				if (padesFormatModel.getElementAt(i).equals(selectedValue)) {
					this.padesBasicFormat.setSelectedIndex(i);
					break;
				}
			}
		}        
		
		final List<PolicyPanel.PolicyItem> padesPolicies = new ArrayList<>();
        padesPolicies.add(
    		new PolicyItem(
        		SimpleAfirmaMessages.getString("PreferencesPanel.73"), //$NON-NLS-1$
        		POLICY_CADES_PADES_AGE_1_9
    		)
		);
        this.panelPolicies.removeAll();
        
        this.padesPolicyDlg = new PolicyPanel(
    		SIGN_FORMAT_PADES,
    		padesPolicies,
    		getPadesDefaultPolicy(),
    		this.padesBasicFormat,
    		this.unprotected
		);

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.gridy = 0;
        this.panelPolicies.add(this.padesPolicyDlg, c);
        
        if (this.padesBasicFormat.getItemCount() > 0) {
			this.padesBasicFormat.setSelectedIndex(0);
		}
		this.padesBasicFormat.setEnabled(!this.unprotected);
		
		this.policyLabel.setText(this.padesPolicyDlg.getSelectedPolicy());
        
        revalidate();
        repaint();
	}

	/** Obtiene la configuraci&oacute;n de pol&iacute;tica de firma PAdES establecida actualmente.
	 * @return Pol&iacute;tica de firma configurada. */
	private static AdESPolicy getPadesPreferedPolicy() {

		if (PreferencesManager.get(PREFERENCE_PADES_POLICY_IDENTIFIER, null) == null) {
			return null;
		}
		try {
			return new AdESPolicy(
				PreferencesManager.get(PREFERENCE_PADES_POLICY_IDENTIFIER, null),
				PreferencesManager.get(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH, null),
				PreferencesManager.get(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM, null),
				PreferencesManager.get(PREFERENCE_PADES_POLICY_QUALIFIER, null)
			);
		}
		catch (final Exception e) {
			Logger.getLogger("es.gob.afirma").severe("Error al recuperar la politica PAdES guardada en preferencias: " + e); //$NON-NLS-1$ //$NON-NLS-2$
			return null;
		}
	}
	
	/**
	 * Obtiene la configuraci&oacute;n de pol&iacute;tica de firma PAdES por
	 * defecto desde el fichero de preferencias.
	 * @param unprotected
	 *            {@code true} Si las opciones de configuraci&oacute;n sensibles
	 *            a ello est&aacute;n protegidas y no pueden ser modificadas,
	 *            {@code false} en caso contrario
	 * @return Pol&iacute;tica de firma configurada.
	 */
	private AdESPolicy getPadesDefaultPolicy() {

		AdESPolicy adesPolicy = null;

		if (isUnprotected()) {

			// unprotected = true, luego no pueden alterarse las
			// propiedades:
			// devolvemos las preferencias almacenadas actualmente

			adesPolicy = this.padesPolicyDlg.getCurrentPolicy();

		} else {
			try {
				// unprotected = false, luego se pueden alterar las propiedades:
				// devolvemos las preferencias por defecto
								
				if (PreferencesManager.getPreference(PREFERENCE_PADES_POLICY_IDENTIFIER, null) == null
						|| "".equals(PreferencesManager.getPreference(PREFERENCE_PADES_POLICY_IDENTIFIER, null))) { //$NON-NLS-1$
					this.padesPolicyDlg.loadPolicy(null);
				} else {
					this.padesPolicyDlg.loadPolicy(
							new AdESPolicy(PreferencesManager.getPreference(PREFERENCE_PADES_POLICY_IDENTIFIER, null),
									PreferencesManager.getPreference(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH, null),
									PreferencesManager.getPreference(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM,
											null),
									PreferencesManager.getPreference(PREFERENCE_PADES_POLICY_QUALIFIER, null)));
				}	
				
			} catch (final Exception e) {
				Logger.getLogger("es.gob.afirma") //$NON-NLS-1$
						.severe("Error al recuperar la politica PAdES guardada en fichero de preferencias: " + e); //$NON-NLS-1$

			}
		}

		return adesPolicy;
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
	
	
	
	/** Di&aacute;logo para cambair la configuracion de la pol&iacute;tica
	 * @param container Contenedor en el que se define el di&aacute;logo. 
	 * */
    public void changePadesPolicyDlg(final Container container) {

		// Cursor en espera
		container.setCursor(new Cursor(Cursor.WAIT_CURSOR));

		// Cursor por defecto
		container.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

		if (AOUIFactory.showConfirmDialog(container, this.padesPolicyDlg, SimpleAfirmaMessages.getString("PolicyDialog.0"), //$NON-NLS-1$
				JOptionPane.OK_CANCEL_OPTION, JOptionPane.DEFAULT_OPTION) == JOptionPane.OK_OPTION) {

			String policyId = null;
			
			try {
				if (this.padesPolicyDlg.getCurrentPolicy() != null) {
					policyId = this.padesPolicyDlg.getCurrentPolicy().getPolicyIdentifier();
				}
			} catch (IllegalArgumentException e) {
				LOGGER.warning("El identificador de la politica no puede ser nulo o vacio"); //$NON-NLS-1$
			}

			if ((policyId == null || "".equals(policyId)) && SimpleAfirmaMessages.getString("PreferencesPanel.26").equals(this.padesPolicyDlg.getSelectedPolicy())) { //$NON-NLS-1$ //$NON-NLS-2$
				AOUIFactory.showErrorMessage(null,
						SimpleAfirmaMessages.getString("PolicyDialog.1"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("PolicyDialog.2"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE);
				changePadesPolicyDlg(container);
				LOGGER.warning("El identificador de la politica no puede ser nulo o vacio"); //$NON-NLS-1$
			}

			else {
				
				try {
					checkPreferences();

					this.policyLabel.setText(this.padesPolicyDlg.getSelectedPolicy());

					final AdESPolicy padesPolicy = this.padesPolicyDlg.getCurrentPolicy();
					if (padesPolicy != null) {
						PreferencesManager.put(PREFERENCE_PADES_POLICY_IDENTIFIER, padesPolicy.getPolicyIdentifier());
						PreferencesManager.put(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH,
								padesPolicy.getPolicyIdentifierHash());
						PreferencesManager.put(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM,
								padesPolicy.getPolicyIdentifierHashAlgorithm());
						if (padesPolicy.getPolicyQualifier() != null) {
							PreferencesManager.put(PREFERENCE_PADES_POLICY_QUALIFIER,
									padesPolicy.getPolicyQualifier().toString());
						} else {
							PreferencesManager.remove(PREFERENCE_PADES_POLICY_QUALIFIER);
						}
					} else {
						PreferencesManager.remove(PREFERENCE_PADES_POLICY_IDENTIFIER);
						PreferencesManager.remove(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH);
						PreferencesManager.remove(PREFERENCE_PADES_POLICY_IDENTIFIER_HASH_ALGORITHM);
						PreferencesManager.remove(PREFERENCE_PADES_POLICY_QUALIFIER);
					}
					
					this.padesPolicyDlg.saveCurrentPolicy();
					
				} catch (AOException e) {
					
					AOUIFactory.showErrorMessage(
							null,
							"<html><p>" + SimpleAfirmaMessages.getString("PreferencesPanel.7") + ":<br>" + e.getLocalizedMessage() + "</p></html>", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
							SimpleAfirmaMessages.getString("SimpleAfirma.7"), //$NON-NLS-1$
							JOptionPane.ERROR_MESSAGE
						);
					changePadesPolicyDlg(container);
						
				}
				
				
			}
		}
    }


}

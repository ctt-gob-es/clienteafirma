package es.gob.afirma.standalone.ui;

import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_XML;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_GENERAL_OMIT_ASKONCLOSE;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_GENERAL_SIGNATURE_ALGORITHM;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ItemListener;
import java.awt.event.KeyListener;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.standalone.PreferencesManager;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

final class PreferencesPanelGeneral extends JPanel {

	private static final long serialVersionUID = 5442844766530064610L;

	private final JComboBox<String> signarureAlgorithms = new JComboBox<>();

	private static final String XADES = AOSignConstants.SIGN_FORMAT_XADES;
	private static final String CADES = AOSignConstants.SIGN_FORMAT_CADES;
	private static final String PADES = AOSignConstants.SIGN_FORMAT_PADES;
	private static final String OOXML = AOSignConstants.SIGN_FORMAT_OOXML;
	private static final String FACTURAE = AOSignConstants.SIGN_FORMAT_FACTURAE;
	private static final String ODF = AOSignConstants.SIGN_FORMAT_ODF;

	private final JComboBox<String> pdfFilesCombo = new JComboBox<>(new String[] { PADES, CADES, XADES });
	private final JComboBox<String> ooxmlFilesCombo = new JComboBox<>(new String[] { OOXML, CADES, XADES });
	private final JComboBox<String> facturaeFilesCombo = new JComboBox<>(new String[] { FACTURAE, XADES, CADES });
	private final JComboBox<String> xmlFilesCombo = new JComboBox<>(new String[] { XADES, CADES });
	private final JComboBox<String> binFilesCombo = new JComboBox<>(new String[] { CADES, XADES });
	private final JComboBox<String> odfFilesCombo = new JComboBox<>(new String[] { ODF, CADES, XADES });

	private final JCheckBox avoidAskForClose = new JCheckBox(
		SimpleAfirmaMessages.getString("PreferencesPanel.36"), //$NON-NLS-1$
		PreferencesManager.getBoolean(PREFERENCE_GENERAL_OMIT_ASKONCLOSE, false)
	);

	private final JCheckBox hideDniStartScreen = new JCheckBox(
		SimpleAfirmaMessages.getString("PreferencesPanel.81"), //$NON-NLS-1$
		PreferencesManager.getBoolean(PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN, false)
	);

	PreferencesPanelGeneral(final KeyListener keyListener, final ItemListener modificationListener) {
    	SwingUtilities.invokeLater(() -> createUI(keyListener, modificationListener));
	}

	void savePreferences() {
		PreferencesManager.put(PREFERENCE_GENERAL_SIGNATURE_ALGORITHM, this.signarureAlgorithms.getSelectedItem().toString());
		PreferencesManager.putBoolean(PREFERENCE_GENERAL_OMIT_ASKONCLOSE, this.avoidAskForClose.isSelected());
		PreferencesManager.putBoolean(PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN, this.hideDniStartScreen.isSelected());

		// Formatos por defecto
		PreferencesManager.put(PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN, this.binFilesCombo.getSelectedItem().toString());
		PreferencesManager.put(PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE, this.facturaeFilesCombo.getSelectedItem().toString());
		PreferencesManager.put(PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML, this.ooxmlFilesCombo.getSelectedItem().toString());
		PreferencesManager.put(PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF, this.pdfFilesCombo.getSelectedItem().toString());
		PreferencesManager.put(PREFERENCE_GENERAL_DEFAULT_FORMAT_XML, this.xmlFilesCombo.getSelectedItem().toString());
		PreferencesManager.put(PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF, this.odfFilesCombo.getSelectedItem().toString());
	}

	void createUI(final KeyListener keyListener, final ItemListener modificationListener) {

		setBorder(BorderFactory.createTitledBorder(SimpleAfirmaMessages.getString("PreferencesPanel.17"))); //$NON-NLS-1$
		setLayout(new GridBagLayout());

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;
		c.weightx = 1.0;
		c.gridx = 0;
		c.gridy = 0;

		final FlowLayout fLayout = new FlowLayout(FlowLayout.LEADING);
		final JPanel signatureAgorithmPanel = new JPanel(fLayout);
		signatureAgorithmPanel.setBorder(
			BorderFactory.createTitledBorder(
				BorderFactory.createEmptyBorder(), SimpleAfirmaMessages.getString("PreferencesPanel.18") //$NON-NLS-1$
			)
		);
		this.signarureAlgorithms.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.46") //$NON-NLS-1$
		);
		this.signarureAlgorithms.addItemListener(modificationListener);
		this.signarureAlgorithms.addKeyListener(keyListener);
		this.signarureAlgorithms.setModel(
			new DefaultComboBoxModel<>(
				new String[] {
					"SHA1withRSA", //$NON-NLS-1$
					"SHA512withRSA", //$NON-NLS-1$
					"SHA384withRSA", //$NON-NLS-1$
					"SHA256withRSA" //$NON-NLS-1$
				}
			)
		);
		this.signarureAlgorithms.setSelectedItem(
			PreferencesManager.get(PREFERENCE_GENERAL_SIGNATURE_ALGORITHM, "SHA1withRSA") //$NON-NLS-1$
		);
		signatureAgorithmPanel.add(this.signarureAlgorithms);

		add(signatureAgorithmPanel, c);

		final JPanel generalPreferencesPanel = new JPanel(new GridBagLayout());
		generalPreferencesPanel.setBorder(
			BorderFactory.createTitledBorder(
				BorderFactory.createEmptyBorder(), SimpleAfirmaMessages.getString("PreferencesPanel.37") //$NON-NLS-1$
			)
		);
		final GridBagConstraints generalConstraint = new GridBagConstraints();
		generalConstraint.fill = GridBagConstraints.HORIZONTAL;
		generalConstraint.weightx = 1.0;
		generalConstraint.gridy = 0;
		generalConstraint.insets = new Insets(5, 7, 5, 7);

		this.avoidAskForClose.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.44") //$NON-NLS-1$
		);
		this.avoidAskForClose.setMnemonic('N');
		this.avoidAskForClose.addItemListener(modificationListener);
		this.avoidAskForClose.addKeyListener(keyListener);
		generalPreferencesPanel.add(this.avoidAskForClose, generalConstraint);

		generalConstraint.gridy++;

		this.hideDniStartScreen.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.82") //$NON-NLS-1$
		);
		this.hideDniStartScreen.setMnemonic('D');
		this.hideDniStartScreen.addItemListener(modificationListener);
		this.hideDniStartScreen.addKeyListener(keyListener);
		//generalPreferencesPanel.add(this.hideDniStartScreen, generalConstraint);

		c.gridy++;

		add(generalPreferencesPanel, c);

		final JPanel signatureDefaultsFormats = createSignatureFormatPanel(modificationListener, keyListener);

		c.gridy++;

		add(signatureDefaultsFormats, c);

		c.weighty = 1.0;
		c.gridy++;
		add(new JPanel(), c);

	}

	/**
	 * Crea el panel con la configuraci&oacute;n de los formatos de firma a utilizar con cada tipo de fichero.
	 * @param modificationListener Listener para la detecci&oacute;n de cambio de configuraci&oacute;n.
	 * @param keyListener Listener para la deteccion del uso de teclas para el cierre de la pantalla.
	 * @return Panel con los componentes de configuraci&oacute;n.
	 */
	private JPanel createSignatureFormatPanel(final ItemListener modificationListener, final KeyListener keyListener) {

		final JPanel signatureDefaultsFormats = new JPanel(new GridBagLayout());
		signatureDefaultsFormats.setBorder(
			BorderFactory.createTitledBorder(
				BorderFactory.createEmptyBorder(),
				SimpleAfirmaMessages.getString("PreferencesPanel.39") //$NON-NLS-1$
			)
		);

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.gridx = 0;
		c.gridy = 0;
		c.insets = new Insets(5, 7, 0, 7);

		// PDF
		final JLabel pdfFilesLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.74")); //$NON-NLS-1$
		pdfFilesLabel.setLabelFor(this.pdfFilesCombo);
		this.pdfFilesCombo.setSelectedItem(
			PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF, PADES)
		);
		this.pdfFilesCombo.addItemListener(modificationListener);
		this.pdfFilesCombo.addKeyListener(keyListener);
		c.gridx = 0;
		c.weightx = 0;
		signatureDefaultsFormats.add(pdfFilesLabel, c);
		c.gridx = 1;
		c.weightx = 1.0;
		signatureDefaultsFormats.add(this.pdfFilesCombo, c);
		c.gridy++;

		// OOXML
		final JLabel ooxmlFilesLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.75")); //$NON-NLS-1$
		ooxmlFilesLabel.setLabelFor(this.ooxmlFilesCombo);
		this.ooxmlFilesCombo.setSelectedItem(
			PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML, OOXML)
		);
		this.ooxmlFilesCombo.addItemListener(modificationListener);
		this.ooxmlFilesCombo.addKeyListener(keyListener);
		c.gridx = 0;
		c.weightx = 0;
		signatureDefaultsFormats.add(ooxmlFilesLabel, c);
		c.gridx = 1;
		c.weightx = 1.0;
		signatureDefaultsFormats.add(this.ooxmlFilesCombo, c);
		c.gridy++;

		// FACTURAE
		final JLabel facturaeFilesLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.76")); //$NON-NLS-1$
		facturaeFilesLabel.setLabelFor(this.facturaeFilesCombo);
		this.facturaeFilesCombo.setSelectedItem(
			PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE, FACTURAE)
		);
		this.facturaeFilesCombo.addItemListener(modificationListener);
		this.facturaeFilesCombo.addKeyListener(keyListener);
		c.gridx = 0;
		c.weightx = 0;
		signatureDefaultsFormats.add(facturaeFilesLabel, c);
		c.gridx = 1;
		c.weightx = 1.0;
		signatureDefaultsFormats.add(this.facturaeFilesCombo, c);
		c.gridy++;

		// XML
		final JLabel xmlFilesLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.77")); //$NON-NLS-1$
		xmlFilesLabel.setLabelFor(this.xmlFilesCombo);
		this.xmlFilesCombo.setSelectedItem(
			PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_XML, XADES)
		);
		this.xmlFilesCombo.addItemListener(modificationListener);
		this.xmlFilesCombo.addKeyListener(keyListener);
		c.gridx = 0;
		c.weightx = 0;
		signatureDefaultsFormats.add(xmlFilesLabel, c);
		c.gridx = 1;
		c.weightx = 1.0;
		signatureDefaultsFormats.add(this.xmlFilesCombo, c);
		c.gridy++;

		// ODF
		final JLabel odfFilesLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.83")); //$NON-NLS-1$
		odfFilesLabel.setLabelFor(this.odfFilesCombo);
		this.odfFilesCombo.setSelectedItem(
			PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF, ODF)
		);
		this.odfFilesCombo.addItemListener(modificationListener);
		this.odfFilesCombo.addKeyListener(keyListener);
		c.gridx = 0;
		c.weightx = 0;
		signatureDefaultsFormats.add(odfFilesLabel, c);
		c.gridx = 1;
		c.weightx = 1.0;
		signatureDefaultsFormats.add(this.odfFilesCombo, c);
		c.gridy++;

		// BIN
		final JLabel binFilesLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.78")); //$NON-NLS-1$
		binFilesLabel.setLabelFor(this.binFilesCombo);
		this.binFilesCombo.setSelectedItem(
			PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN, CADES)
		);
		this.binFilesCombo.addItemListener(modificationListener);
		this.binFilesCombo.addKeyListener(keyListener);
		c.gridx = 0;
		c.weightx = 0;
		signatureDefaultsFormats.add(binFilesLabel, c);
		c.gridx = 1;
		signatureDefaultsFormats.add(this.binFilesCombo, c);
		c.gridy++;
		c.weightx = 1.0;

		return signatureDefaultsFormats;
	}
}

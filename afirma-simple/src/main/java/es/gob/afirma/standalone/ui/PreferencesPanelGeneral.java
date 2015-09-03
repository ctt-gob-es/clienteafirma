package es.gob.afirma.standalone.ui;

import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_OMIT_ASKONCLOSE;
import static es.gob.afirma.standalone.PreferencesManager.PREFERENCE_SIGNATURE_ALGORITHM;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
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

	private final JCheckBox avoidAskForClose = new JCheckBox(
		SimpleAfirmaMessages.getString("PreferencesPanel.36"), //$NON-NLS-1$
		PreferencesManager.getBoolean(PREFERENCE_OMIT_ASKONCLOSE, false)
	);

	PreferencesPanelGeneral(final KeyListener keyListener, final ItemListener modificationListener) {
    	SwingUtilities.invokeLater(
			new Runnable() {
				@Override
				public void run() {
					createUI(keyListener, modificationListener);
				}
			}
		);
	}

	void savePreferences() {
		PreferencesManager.put(PREFERENCE_SIGNATURE_ALGORITHM, this.signarureAlgorithms.getSelectedItem().toString());
		PreferencesManager.putBoolean(PREFERENCE_OMIT_ASKONCLOSE, this.avoidAskForClose.isSelected());
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
			PreferencesManager.get(PREFERENCE_SIGNATURE_ALGORITHM, "SHA1withRSA") //$NON-NLS-1$
		);
		signatureAgorithmPanel.add(this.signarureAlgorithms);

		add(signatureAgorithmPanel, c);

		final JPanel generalPreferencesPanel = new JPanel(fLayout);
		generalPreferencesPanel.setBorder(
			BorderFactory.createTitledBorder(
				BorderFactory.createEmptyBorder(), SimpleAfirmaMessages.getString("PreferencesPanel.37") //$NON-NLS-1$
			)
		);
		this.avoidAskForClose.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.44") //$NON-NLS-1$
		);
		this.avoidAskForClose.addItemListener(modificationListener);
		this.avoidAskForClose.addKeyListener(keyListener);
		generalPreferencesPanel.add(this.avoidAskForClose);

		c.gridy++;

		add(generalPreferencesPanel, c);

		final JPanel signatureDefaultsFormats = new JPanel(fLayout);
		signatureDefaultsFormats.setBorder(
			BorderFactory.createTitledBorder(
				BorderFactory.createEmptyBorder(),
				SimpleAfirmaMessages.getString("PreferencesPanel.39") //$NON-NLS-1$
			)
		);

		final FlowLayout layout = new FlowLayout();
		final JPanel formatCombosPanel = new JPanel(layout);

		// PDF
		final JLabel pdfFilesLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.74")); //$NON-NLS-1$
		final JComboBox<String> pdfFilesCombo = new JComboBox<>(new String[] { PADES, CADES, XADES });
		pdfFilesLabel.setLabelFor(pdfFilesCombo);
		formatCombosPanel.add(pdfFilesLabel);
		formatCombosPanel.add(pdfFilesCombo);

		// OOXML
		final JLabel ooxmlFilesLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.75")); //$NON-NLS-1$
		final JComboBox<String> ooxmlFilesCombo = new JComboBox<>(new String[] { OOXML, CADES, XADES });
		ooxmlFilesLabel.setLabelFor(ooxmlFilesCombo);
		formatCombosPanel.add(ooxmlFilesLabel);
		formatCombosPanel.add(ooxmlFilesCombo);

		// FACTURAE
		final JLabel facturaeFilesLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.76")); //$NON-NLS-1$
		final JComboBox<String> facturaeFilesCombo = new JComboBox<>(new String[] { FACTURAE, XADES, CADES });
		facturaeFilesLabel.setLabelFor(facturaeFilesCombo);
		formatCombosPanel.add(facturaeFilesLabel);
		formatCombosPanel.add(facturaeFilesCombo);

		// XML
		final JLabel xmlFilesLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.77")); //$NON-NLS-1$
		final JComboBox<String> xmlFilesCombo = new JComboBox<>(new String[] { XADES, CADES });
		xmlFilesLabel.setLabelFor(xmlFilesCombo);
		formatCombosPanel.add(xmlFilesLabel);
		formatCombosPanel.add(xmlFilesCombo);

		// BIN
		final JLabel binFilesLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.78")); //$NON-NLS-1$
		final JComboBox<String> binFilesCombo = new JComboBox<>(new String[] { CADES, XADES });
		binFilesLabel.setLabelFor(binFilesCombo);
		formatCombosPanel.add(binFilesLabel);
		formatCombosPanel.add(binFilesCombo);

		signatureDefaultsFormats.add(formatCombosPanel);

		c.gridy++;

		add(signatureDefaultsFormats, c);


		c.weighty = 1.0;
		c.gridy++;
		add(new JPanel(), c);

	}
}

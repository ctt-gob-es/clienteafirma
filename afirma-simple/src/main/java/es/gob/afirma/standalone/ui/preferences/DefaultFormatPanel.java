package es.gob.afirma.standalone.ui.preferences;

import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_XML;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;
import es.gob.afirma.standalone.configurator.common.PreferencesManager.PreferencesSource;

class DefaultFormatPanel extends JPanel {

	/** Serial Id. */
	private static final long serialVersionUID = 4101194604941066022L;

	private static final String XADES = AOSignConstants.SIGN_FORMAT_XADES;
	private static final String CADES = AOSignConstants.SIGN_FORMAT_CADES;
	private static final String PADES = AOSignConstants.SIGN_FORMAT_PADES;
	private static final String OOXML = AOSignConstants.SIGN_FORMAT_OOXML;
	private static final String FACTURAE = AOSignConstants.SIGN_FORMAT_FACTURAE;
	private static final String ODF = AOSignConstants.SIGN_FORMAT_ODF;

	private JComboBox<String> pdfFilesCombo;
	private JComboBox<String> ooxmlFilesCombo;
	private JComboBox<String> facturaeFilesCombo;
	private JComboBox<String> xmlFilesCombo;
	private JComboBox<String> binFilesCombo;
	private JComboBox<String> odfFilesCombo;

	private JButton resetButton;

	public DefaultFormatPanel() {

		super(new GridBagLayout());

		final JLabel descriptionLabel = new JLabel(SimpleAfirmaMessages.getString("DefaultFormatPanel.1")); //$NON-NLS-1$

		final JPanel formatsPanel = createFormatsPanel();

		final JPanel buttonsPanel = createButtonsPanel();

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 1.0;
		c.gridy = 0;
		add(descriptionLabel, c);
		c.insets = new Insets(11,  0,  0,  0);
		c.gridy++;
		add(formatsPanel, c);
		c.insets = new Insets(0,  0,  0,  0);
		c.gridy++;
		add(buttonsPanel, c);
	}

	/**
	 * Crea el panel con la configuracion de los formatos por defecto para cada
	 * tipo de archivo.
	 * @return Panel con el listado de configuraciones de formato.
	 */
	private JPanel createFormatsPanel() {

		this.pdfFilesCombo = new JComboBox<>(new String[] { PADES, CADES, XADES });
		this.ooxmlFilesCombo = new JComboBox<>(new String[] { OOXML, CADES, XADES });
		this.facturaeFilesCombo = new JComboBox<>(new String[] { FACTURAE, XADES, CADES });
		this.xmlFilesCombo = new JComboBox<>(new String[] { XADES, CADES });
		this.binFilesCombo = new JComboBox<>(new String[] { CADES, XADES });
		this.odfFilesCombo = new JComboBox<>(new String[] { ODF, CADES, XADES });

		final JPanel signatureDefaultsFormats = new JPanel(new GridBagLayout());

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.gridx = 0;
		c.gridy = 0;
		c.insets = new Insets(5, 7, 0, 7);

		// PDF
		final JLabel pdfFilesLabel = new JLabel(SimpleAfirmaMessages.getString("DefaultFormatPanel.5")); //$NON-NLS-1$
		pdfFilesLabel.setLabelFor(this.pdfFilesCombo);
		c.gridx = 0;
		c.weightx = 0;
		signatureDefaultsFormats.add(pdfFilesLabel, c);
		c.gridx = 1;
		c.weightx = 1.0;
		signatureDefaultsFormats.add(this.pdfFilesCombo, c);
		c.gridy++;

		// OOXML
		final JLabel ooxmlFilesLabel = new JLabel(SimpleAfirmaMessages.getString("DefaultFormatPanel.6")); //$NON-NLS-1$
		ooxmlFilesLabel.setLabelFor(this.ooxmlFilesCombo);

		c.gridx = 0;
		c.weightx = 0;
		signatureDefaultsFormats.add(ooxmlFilesLabel, c);
		c.gridx = 1;
		c.weightx = 1.0;
		signatureDefaultsFormats.add(this.ooxmlFilesCombo, c);
		c.gridy++;

		// FACTURAE
		final JLabel facturaeFilesLabel = new JLabel(SimpleAfirmaMessages.getString("DefaultFormatPanel.7")); //$NON-NLS-1$
		facturaeFilesLabel.setLabelFor(this.facturaeFilesCombo);

		c.gridx = 0;
		c.weightx = 0;
		signatureDefaultsFormats.add(facturaeFilesLabel, c);
		c.gridx = 1;
		c.weightx = 1.0;
		signatureDefaultsFormats.add(this.facturaeFilesCombo, c);
		c.gridy++;

		// XML
		final JLabel xmlFilesLabel = new JLabel(SimpleAfirmaMessages.getString("DefaultFormatPanel.8")); //$NON-NLS-1$
		xmlFilesLabel.setLabelFor(this.xmlFilesCombo);

		c.gridx = 0;
		c.weightx = 0;
		signatureDefaultsFormats.add(xmlFilesLabel, c);
		c.gridx = 1;
		c.weightx = 1.0;
		signatureDefaultsFormats.add(this.xmlFilesCombo, c);
		c.gridy++;

		// ODF
		final JLabel odfFilesLabel = new JLabel(SimpleAfirmaMessages.getString("DefaultFormatPanel.9")); //$NON-NLS-1$
		odfFilesLabel.setLabelFor(this.odfFilesCombo);

		c.gridx = 0;
		c.weightx = 0;
		signatureDefaultsFormats.add(odfFilesLabel, c);
		c.gridx = 1;
		c.weightx = 1.0;
		signatureDefaultsFormats.add(this.odfFilesCombo, c);
		c.gridy++;

		// BIN
		final JLabel binFilesLabel = new JLabel(SimpleAfirmaMessages.getString("DefaultFormatPanel.10")); //$NON-NLS-1$
		binFilesLabel.setLabelFor(this.binFilesCombo);

		c.gridx = 0;
		c.weightx = 0;
		signatureDefaultsFormats.add(binFilesLabel, c);
		c.gridx = 1;
		signatureDefaultsFormats.add(this.binFilesCombo, c);
		c.gridy++;
		c.weightx = 1.0;

		return signatureDefaultsFormats;
	}


	/**
	 * Crea el panel con los botones de acci&oacute;n internos del di&aacute;logo.
	 * @return Panel con los botones de acci&oacute;n.
	 */
	private JPanel createButtonsPanel() {

		final JPanel buttonsPanel = new JPanel(new FlowLayout(FlowLayout.TRAILING));

		this.resetButton = new JButton(SimpleAfirmaMessages.getString("DefaultFormatPanel.2")); //$NON-NLS-1$
		this.resetButton.setMnemonic('r');
		this.resetButton.getAccessibleContext().setAccessibleDescription(SimpleAfirmaMessages.getString("DefaultFormatPanel.3")); //$NON-NLS-1$
		this.resetButton.addActionListener(
				e -> resetPreferences()
			);

		buttonsPanel.add(this.resetButton);

		return buttonsPanel;
	}

	/**
	 * Establece que aparezcan bloqueados las opciones de este di&aacute;logo.
	 * @param blocked {@code true} para que aparezcan bloqueadas las opciones del
	 * di&aacute;logo, {@code false} en caso contrario.
	 */
	void setBlocked(final boolean blocked) {
		final boolean enabled = !blocked;
		this.pdfFilesCombo.setEnabled(enabled);
		this.ooxmlFilesCombo.setEnabled(enabled);
		this.facturaeFilesCombo.setEnabled(enabled);
		this.odfFilesCombo.setEnabled(enabled);
		this.xmlFilesCombo.setEnabled(enabled);
		this.binFilesCombo.setEnabled(enabled);
	}

	/**
	 * Carga los valores almacenados en las preferencias del sistema.
	 */
	void loadPreferences() {
		this.pdfFilesCombo.setSelectedItem(PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF));
		this.ooxmlFilesCombo.setSelectedItem(PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML));
		this.facturaeFilesCombo.setSelectedItem(PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE));
		this.odfFilesCombo.setSelectedItem(PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF));
		this.xmlFilesCombo.setSelectedItem(PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_XML));
		this.binFilesCombo.setSelectedItem(PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN));
	}

	/**
	 * Carga los valores por defecto de los campos del di&aacute;logo.
	 */
	void resetPreferences() {
		this.pdfFilesCombo.setSelectedItem(PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF, PreferencesSource.DEFAULT));
		this.ooxmlFilesCombo.setSelectedItem(PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML, PreferencesSource.DEFAULT));
		this.facturaeFilesCombo.setSelectedItem(PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE, PreferencesSource.DEFAULT));
		this.odfFilesCombo.setSelectedItem(PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF, PreferencesSource.DEFAULT));
		this.xmlFilesCombo.setSelectedItem(PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_XML, PreferencesSource.DEFAULT));
		this.binFilesCombo.setSelectedItem(PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN, PreferencesSource.DEFAULT));
	}

	/**
	 * Almacena los valores actuales de los campos del di&aacute;logo en las preferencias de la aplicaci&oacute;n.
	 */
	void savePreferences() {
		PreferencesManager.put(PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN, this.binFilesCombo.getSelectedItem().toString());
		PreferencesManager.put(PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE, this.facturaeFilesCombo.getSelectedItem().toString());
		PreferencesManager.put(PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML, this.ooxmlFilesCombo.getSelectedItem().toString());
		PreferencesManager.put(PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF, this.pdfFilesCombo.getSelectedItem().toString());
		PreferencesManager.put(PREFERENCE_GENERAL_DEFAULT_FORMAT_XML, this.xmlFilesCombo.getSelectedItem().toString());
		PreferencesManager.put(PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF, this.odfFilesCombo.getSelectedItem().toString());
	}
}

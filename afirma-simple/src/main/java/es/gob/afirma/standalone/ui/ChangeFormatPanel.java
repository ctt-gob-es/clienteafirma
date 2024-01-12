package es.gob.afirma.standalone.ui;

import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF;
import static es.gob.afirma.standalone.configurator.common.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_XML;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.standalone.SimpleAfirmaMessages;
import es.gob.afirma.standalone.configurator.common.PreferencesManager;

public class ChangeFormatPanel extends JPanel{

	private static final long serialVersionUID = 8338298450770919501L;
	
	private static final String XADES = AOSignConstants.SIGN_FORMAT_XADES;
	private static final String CADES = AOSignConstants.SIGN_FORMAT_CADES;
	private static final String PADES = AOSignConstants.SIGN_FORMAT_PADES;
	private static final String OOXML = AOSignConstants.SIGN_FORMAT_OOXML;
	private static final String FACTURAE = AOSignConstants.SIGN_FORMAT_FACTURAE;
	private static final String ODF = AOSignConstants.SIGN_FORMAT_ODF;
	
	private static String[] pdfFilesArray = new String[] { PADES, CADES, XADES };
	private static String[] ooxmlFilesArray = new String[] { OOXML, CADES, XADES };
	private static String[] facturaeFilesArray = new String[] { FACTURAE, XADES, CADES };
	private static String[] xmlFilesArray = new String[] { XADES, CADES };
	private static String[] binFilesArray = new String[] { CADES, XADES };
	private static String[] odfFilesArray = new String[] { ODF, CADES, XADES };
	
	private final JComboBox<String> usedCombo;

	private static String accessibleDescription = ""; //$NON-NLS-1$

	public ChangeFormatPanel(final SignOperationConfig config) {
		this.usedCombo = new JComboBox<String>();
		accessibleDescription = SimpleAfirmaMessages.getString("ChangeFormatDialog.0"); //$NON-NLS-1$
		createPanel(config);
	}

	private void createPanel(final SignOperationConfig config) {

		setLayout(new GridBagLayout());

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;

		c.anchor = GridBagConstraints.LINE_START;
		c.gridy = 0;
		c.gridx = 0;
		
		add(new JLabel(SimpleAfirmaMessages.getString("ChangeFormatDialog.1"))); //$NON-NLS-1$
		
		c.gridy++;
		
		String defaultFormat = null;
		
		String[] formatsArray = null;

		if (FileType.XML.equals(config.getFileType()) || FileType.SIGN_XADES.equals(config.getFileType())) {
			formatsArray = xmlFilesArray;
			defaultFormat = PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_XML);
		} else if (FileType.PDF.equals(config.getFileType())) {
			formatsArray = pdfFilesArray;
			defaultFormat = PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF);
		} else if (FileType.OOXML.equals(config.getFileType())) {
			formatsArray = ooxmlFilesArray;
			defaultFormat = PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML);
		} else if (FileType.FACTURAE.equals(config.getFileType())) {
			formatsArray = facturaeFilesArray;
			defaultFormat = PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE);
		} else if (FileType.BINARY.equals(config.getFileType()) || FileType.SIGN_CADES.equals(config.getFileType())) {
			formatsArray = binFilesArray;
			defaultFormat = PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN);
		} else if (FileType.ODF.equals(config.getFileType())) {
			formatsArray = odfFilesArray;
			defaultFormat = PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF);
		}
		
		createUsedCombo(defaultFormat, formatsArray);
		add(this.usedCombo, c);
	
	}
	
	private void createUsedCombo(final String preferenceDefaultFormat, final String[] formatsArray) {
		for (int i = 0 ; i < formatsArray.length ; i++) {
			String format = formatsArray[i];
			if (format.equals(preferenceDefaultFormat)) {
				format += " " + SimpleAfirmaMessages.getString("ChangeFormatDialog.2"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			this.usedCombo.addItem(format);
		}
	}

	public static String getAccessibleDescription() {
		return accessibleDescription;
	}
	
	public JComboBox<String> getUsedCombo() {
		return this.usedCombo;
	}

}

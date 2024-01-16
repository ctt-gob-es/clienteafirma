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
import es.gob.afirma.standalone.configurator.common.PreferencesManager.PreferencesSource;
import es.gob.afirma.standalone.ui.preferences.FormatItem;

/** Panel de selecci&oacute;n del formato con el que firmar */
public class ChangeFormatPanel extends JPanel{

	private static final long serialVersionUID = 8338298450770919501L;
	
	private FormatItem XADES;
	private FormatItem CADES;
	private FormatItem PADES;
	private FormatItem OOXML;
	private FormatItem FACTURAE;
	private FormatItem ODF;
	
	private FormatItem[] pdfFilesArray;
	private FormatItem[] ooxmlFilesArray;
	private FormatItem[] facturaeFilesArray;
	private FormatItem[] xmlFilesArray;
	private FormatItem[] binFilesArray;
	private FormatItem[] odfFilesArray;
	
	private final JComboBox<FormatItem> usedCombo;

	private static String accessibleDescription = ""; //$NON-NLS-1$

	public ChangeFormatPanel(final SignOperationConfig config) {
		this.usedCombo = new JComboBox<FormatItem>();
		initFormats();
		accessibleDescription = SimpleAfirmaMessages.getString("ChangeFormatDialog.0"); //$NON-NLS-1$
		createPanel(config);
	}

	/**
	 * Crea el panel con los formatos posibles para el archivo a firmar.
	 * @param config Configuraci&oacute;n de la firma.
	 */
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
		
		FormatItem[] formatsArray = null;

		if (FileType.XML.equals(config.getFileType()) || FileType.SIGN_XADES.equals(config.getFileType())) {
			formatsArray = this.xmlFilesArray;
			defaultFormat = PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_XML, PreferencesSource.DEFAULT);
		} else if (FileType.PDF.equals(config.getFileType())) {
			formatsArray = this.pdfFilesArray;
			defaultFormat = PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF, PreferencesSource.DEFAULT);
		} else if (FileType.OOXML.equals(config.getFileType())) {
			formatsArray = this.ooxmlFilesArray;
			defaultFormat = PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML, PreferencesSource.DEFAULT);
		} else if (FileType.FACTURAE.equals(config.getFileType())) {
			formatsArray = this.facturaeFilesArray;
			defaultFormat = PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE, PreferencesSource.DEFAULT);
		} else if (FileType.BINARY.equals(config.getFileType()) || FileType.SIGN_CADES.equals(config.getFileType())) {
			formatsArray = this.binFilesArray;
			defaultFormat = PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN, PreferencesSource.DEFAULT);
		} else if (FileType.ODF.equals(config.getFileType())) {
			formatsArray = this.odfFilesArray;
			defaultFormat = PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF, PreferencesSource.DEFAULT);
		}
		
		createUsedCombo(defaultFormat, formatsArray);
		for (int i = 0 ; i < this.usedCombo.getItemCount() ; i++) {
			final FormatItem formatItem = this.usedCombo.getItemAt(i);
			if (config.getSignatureFormatName().contains(formatItem.getName())) {
				this.usedCombo.setSelectedIndex(i);
			}
		}
		add(this.usedCombo, c);
	
	}
	
	/**
	 * Inicializa los formatos existentes.
	 */
	private void initFormats() {
		
		this.XADES = new FormatItem(AOSignConstants.SIGN_FORMAT_XADES);
		this.CADES = new FormatItem(AOSignConstants.SIGN_FORMAT_CADES);
		this.PADES = new FormatItem(AOSignConstants.SIGN_FORMAT_PADES);
		this.OOXML = new FormatItem(AOSignConstants.SIGN_FORMAT_OOXML);
		this.FACTURAE = new FormatItem(AOSignConstants.SIGN_FORMAT_FACTURAE);
		this.ODF = new FormatItem(AOSignConstants.SIGN_FORMAT_ODF);
		
		this.pdfFilesArray = new FormatItem[] { this.PADES, this.CADES, this.XADES };
		this.ooxmlFilesArray = new FormatItem[] { this.OOXML, this.CADES, this.XADES };
		this.facturaeFilesArray = new FormatItem[] { this.FACTURAE, this.XADES, this.CADES };
		this.xmlFilesArray = new FormatItem[] { this.XADES, this.CADES };
		this.binFilesArray = new FormatItem[] { this.CADES, this.XADES };
		this.odfFilesArray = new FormatItem[] { this.ODF, this.CADES, this.XADES };
	}
	
	/**
	 * Crea el combobox con los formatos.
	 * @param preferenceDefaultFormat Formato recomendado.
	 * @param formatsArray Lista de formatos a utilizar.
	 */
	private void createUsedCombo(final String preferenceDefaultFormat, final FormatItem[] formatsArray) {
		for (int i = 0 ; i < formatsArray.length ; i++) {
			final FormatItem format = formatsArray[i];
			if (preferenceDefaultFormat.equals(format.getName())) {
				format.setRecommended(true);
			}
			this.usedCombo.addItem(format);
		}
	}

	public static String getAccessibleDescription() {
		return accessibleDescription;
	}
	
	public JComboBox<FormatItem> getUsedCombo() {
		return this.usedCombo;
	}

}

package es.gob.afirma.standalone.ui.preferences;

import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_GENERAL_DEFAULT_FORMAT_XML;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_GENERAL_OMIT_ASKONCLOSE;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_GENERAL_SIGNATURE_ALGORITHM;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_GENERAL_UPDATECHECK;
import static es.gob.afirma.standalone.ui.preferences.PreferencesManager.PREFERENCE_GENERAL_USEANALYTICS;

import java.awt.Container;
import java.awt.Cursor;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyListener;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.prefs.BackingStoreException;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import es.gob.afirma.core.AOCancelledOperationException;
import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.core.ui.AOUIFactory;
import es.gob.afirma.standalone.AutoFirmaUtil;
import es.gob.afirma.standalone.SimpleAfirmaMessages;

final class PreferencesPanelGeneral extends JPanel {

	private static final long serialVersionUID = 5442844766530064610L;

	static final Logger LOGGER = Logger.getLogger("es.gob.afirma"); //$NON-NLS-1$

	private final PreferencesPanel preferencesPanel;
	PreferencesPanel getPrefPanel() {
		return this.preferencesPanel;
	}

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

	private final JCheckBox avoidAskForClose = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.36")); //$NON-NLS-1$

	private final JCheckBox hideDniStartScreen = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.81")); //$NON-NLS-1$

	private final JCheckBox checkForUpdates = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.87")); //$NON-NLS-1$

	private final JCheckBox sendAnalytics = new JCheckBox(SimpleAfirmaMessages.getString("PreferencesPanel.89")); //$NON-NLS-1$

	private final DisposableInterface disposableInterface;
	DisposableInterface getDisposableInterface() {
		return this.disposableInterface;
	}

	PreferencesPanelGeneral(final KeyListener keyListener,
			                final ItemListener modificationListener,
			                final DisposableInterface di,
			                final PreferencesPanel prefPanel) {
		this.disposableInterface = di;
		this.preferencesPanel = prefPanel;

		createUI(keyListener, modificationListener);
	}

	void savePreferences() {
		// Opciones varias
		PreferencesManager.put(PREFERENCE_GENERAL_SIGNATURE_ALGORITHM, this.signarureAlgorithms.getSelectedItem().toString());
		PreferencesManager.putBoolean(PREFERENCE_GENERAL_OMIT_ASKONCLOSE, this.avoidAskForClose.isSelected());
		PreferencesManager.putBoolean(PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN, this.hideDniStartScreen.isSelected());
		PreferencesManager.putBoolean(PREFERENCE_GENERAL_UPDATECHECK, this.checkForUpdates.isSelected());
		PreferencesManager.putBoolean(PREFERENCE_GENERAL_USEANALYTICS, this.sendAnalytics.isSelected());

		// Formatos por defecto
		PreferencesManager.put(PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN, this.binFilesCombo.getSelectedItem().toString());
		PreferencesManager.put(PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE, this.facturaeFilesCombo.getSelectedItem().toString());
		PreferencesManager.put(PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML, this.ooxmlFilesCombo.getSelectedItem().toString());
		PreferencesManager.put(PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF, this.pdfFilesCombo.getSelectedItem().toString());
		PreferencesManager.put(PREFERENCE_GENERAL_DEFAULT_FORMAT_XML, this.xmlFilesCombo.getSelectedItem().toString());
		PreferencesManager.put(PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF, this.odfFilesCombo.getSelectedItem().toString());
	}

	void loadPreferences() {
		this.signarureAlgorithms.setSelectedItem(
			PreferencesManager.get(PREFERENCE_GENERAL_SIGNATURE_ALGORITHM, "SHA256withRSA") //$NON-NLS-1$
		);
		this.avoidAskForClose.setSelected(PreferencesManager.getBoolean(PREFERENCE_GENERAL_OMIT_ASKONCLOSE, false));
		this.hideDniStartScreen.setSelected(PreferencesManager.getBoolean(PREFERENCE_GENERAL_HIDE_DNIE_START_SCREEN, false));
		this.checkForUpdates.setSelected(PreferencesManager.getBoolean(PREFERENCE_GENERAL_UPDATECHECK, true));
		this.sendAnalytics.setSelected(PreferencesManager.getBoolean(PREFERENCE_GENERAL_USEANALYTICS, true));

		// Formatos por defecto
		this.pdfFilesCombo.setSelectedItem(PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_PDF, PADES));
		this.ooxmlFilesCombo.setSelectedItem(PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_OOXML, OOXML));
		this.facturaeFilesCombo.setSelectedItem(PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_FACTURAE, FACTURAE));
		this.odfFilesCombo.setSelectedItem(PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_ODF, ODF));
		this.xmlFilesCombo.setSelectedItem(PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_XML, XADES));
		this.binFilesCombo.setSelectedItem(PreferencesManager.get(PREFERENCE_GENERAL_DEFAULT_FORMAT_BIN, CADES));
	}

	void createUI(final KeyListener keyListener,
				  final ItemListener modificationListener) {


		setLayout(new GridBagLayout());

		final GridBagConstraints gbc = new GridBagConstraints();
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.weightx = 1.0;
		gbc.gridx = 0;
		gbc.gridy = 0;

		final FlowLayout fLayout = new FlowLayout(FlowLayout.LEADING);

		final JPanel signConfigPanel = new JPanel(new GridBagLayout());
		signConfigPanel.setBorder(
			BorderFactory.createTitledBorder(
				BorderFactory.createTitledBorder(SimpleAfirmaMessages.getString("PreferencesPanel.108")) //$NON-NLS-1$
			)
		);

		final GridBagConstraints signConstraint = new GridBagConstraints();
		signConstraint.fill = GridBagConstraints.HORIZONTAL;
		signConstraint.weightx = 1.0;
		signConstraint.gridy = 0;
		signConstraint.insets = new Insets(0, 0, 0, 0);

		loadPreferences();
		final JButton importConfigFromFileButton = new JButton(
			SimpleAfirmaMessages.getString("PreferencesPanel.107") //$NON-NLS-1$
		);

		importConfigFromFileButton.setMnemonic('I');
		importConfigFromFileButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent ae) {
					if ((ae.getModifiers() & InputEvent.ALT_MASK) != 0) {
						final String url = (String) AOUIFactory.showInputDialog(
							getParent(),
							SimpleAfirmaMessages.getString("PreferencesPanel.109"), //$NON-NLS-1$
							SimpleAfirmaMessages.getString("PreferencesPanel.110"), //$NON-NLS-1$
							JOptionPane.QUESTION_MESSAGE,
							null,
							null,
							null
						);
						if (url == null || url.trim().isEmpty()) {
							return;
						}
						try {
							PreferencesPlistHandler.importPreferencesFromUrl(url);
						}
						catch(final Exception e) {
							LOGGER.log(
								Level.SEVERE,
								"Error importando la configuracion desde red (" + url + "): " + e, //$NON-NLS-1$ //$NON-NLS-2$
								e
							);
							AOUIFactory.showErrorMessage(
								getParent(),
								SimpleAfirmaMessages.getString("PreferencesPanel.116"), //$NON-NLS-1$
								SimpleAfirmaMessages.getString("PreferencesPanel.117"), //$NON-NLS-1$
								JOptionPane.ERROR_MESSAGE
							);
						}
					}
					else {
						final String configFilePath;
						try {
							configFilePath = AOUIFactory.getLoadFiles(
								SimpleAfirmaMessages.getString("PreferencesPanel.86"), //$NON-NLS-1$
								null,
								null,
								new String[] { "afconfig" }, //$NON-NLS-1$
								SimpleAfirmaMessages.getString("PreferencesPanel.111"), //$NON-NLS-1$
								false,
								false,
								AutoFirmaUtil.getDefaultDialogsIcon(),
								PreferencesPanelGeneral.this
							)[0].getAbsolutePath();
						}
						catch(final AOCancelledOperationException ex) {
							// Operacion cancelada por el usuario
							return;
						}
						PreferencesPlistHandler.importPreferences(configFilePath, getParent());
					}
					getDisposableInterface().disposeInterface();
				}
			}
		);
		importConfigFromFileButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.112") //$NON-NLS-1$
		);

		final JButton restoreConfigFromFileButton = new JButton(
			SimpleAfirmaMessages.getString("PreferencesPanel.135") //$NON-NLS-1$
		);

		restoreConfigFromFileButton.setMnemonic('R');
		restoreConfigFromFileButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent ae) {
					if (AOUIFactory.showConfirmDialog(
						getParent(),
						SimpleAfirmaMessages.getString("PreferencesPanel.140"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("PreferencesPanel.139"), //$NON-NLS-1$
						JOptionPane.YES_NO_OPTION,
			            JOptionPane.WARNING_MESSAGE
					) == JOptionPane.YES_OPTION) {
						try {
							PreferencesManager.clearAll();
							getPrefPanel().loadPreferences();
						} catch (final BackingStoreException e) {
							LOGGER.severe("Error eliminando las preferencias de la aplicacion: " + e); //$NON-NLS-1$
							AOUIFactory.showErrorMessage(
								getParent(),
								SimpleAfirmaMessages.getString("PreferencesPanel.141"), //$NON-NLS-1$
								SimpleAfirmaMessages.getString("PreferencesPanel.117"), //$NON-NLS-1$
								JOptionPane.ERROR_MESSAGE
							);
						}
					}
				}
			}
		);
		restoreConfigFromFileButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.136") //$NON-NLS-1$
		);

		final JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout(FlowLayout.LEFT));

		panel.add(importConfigFromFileButton);
		panel.add(restoreConfigFromFileButton);

		// TODO: Descomentar una vez se entregue
		signConfigPanel.add(panel, signConstraint);

		signConstraint.insets = new Insets(5, 7, 3, 7);
		signConstraint.anchor = GridBagConstraints.LINE_START;

		signConstraint.gridy++;

		this.avoidAskForClose.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.44") //$NON-NLS-1$
		);
		this.avoidAskForClose.setMnemonic('N');
		this.avoidAskForClose.addItemListener(modificationListener);
		this.avoidAskForClose.addKeyListener(keyListener);
		signConfigPanel.add(this.avoidAskForClose, signConstraint);

		signConstraint.gridy++;

		this.hideDniStartScreen.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.82") //$NON-NLS-1$
		);
		this.hideDniStartScreen.setMnemonic('D');
		this.hideDniStartScreen.addItemListener(modificationListener);
		this.hideDniStartScreen.addKeyListener(keyListener);
		signConfigPanel.add(this.hideDniStartScreen, signConstraint);

		signConstraint.gridy++;

		this.checkForUpdates.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.88") //$NON-NLS-1$
		);
		this.checkForUpdates.setMnemonic('B');
		this.checkForUpdates.addItemListener(modificationListener);
		this.checkForUpdates.addKeyListener(keyListener);
		signConfigPanel.add(this.checkForUpdates, signConstraint);

		signConstraint.gridy++;

		this.sendAnalytics.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.90") //$NON-NLS-1$
		);
		this.sendAnalytics.setMnemonic('t');
		this.sendAnalytics.addItemListener(modificationListener);
		this.sendAnalytics.addKeyListener(keyListener);
		signConfigPanel.add(this.sendAnalytics, signConstraint);

		add(signConfigPanel, gbc);

		final JPanel signGeneralPanel = new JPanel(new GridBagLayout());
		signGeneralPanel.setBorder(
			BorderFactory.createTitledBorder(
				BorderFactory.createTitledBorder(
					SimpleAfirmaMessages.getString("PreferencesPanel.17") //$NON-NLS-1$
				)
			)
		);

		final GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;
		c.weightx = 1.0;
		c.gridy = 0;
		c.insets = new Insets(0, 7, 0, 7);

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
		signatureAgorithmPanel.add(this.signarureAlgorithms);

		signGeneralPanel.add(signatureAgorithmPanel, c);

		final JPanel signatureDefaultsFormats = createSignatureFormatPanel(
			modificationListener,
			keyListener
		);

		final JPanel netConfigPanel = new JPanel(new FlowLayout(FlowLayout.LEADING));
		netConfigPanel.setBorder(
			BorderFactory.createTitledBorder(
				BorderFactory.createTitledBorder(SimpleAfirmaMessages.getString("PreferencesPanel.125")) //$NON-NLS-1$
			)
		);

		final JButton proxyConfigButton = new JButton(
			SimpleAfirmaMessages.getString("PreferencesPanel.126") //$NON-NLS-1$
		);

		proxyConfigButton.setMnemonic('P');
		proxyConfigButton.addActionListener(
			new ActionListener() {
				@Override
				public void actionPerformed(final ActionEvent ae) {
					changeProxyDlg(getParent());
				}
			}
		);
		proxyConfigButton.getAccessibleContext().setAccessibleDescription(
			SimpleAfirmaMessages.getString("PreferencesPanel.127") //$NON-NLS-1$
		);

		final JLabel proxyLabel = new JLabel(SimpleAfirmaMessages.getString("PreferencesPanel.128")); //$NON-NLS-1$
		proxyLabel.setLabelFor(proxyConfigButton);

		netConfigPanel.add(proxyLabel);
		netConfigPanel.add(proxyConfigButton);

		c.gridy++;
		signGeneralPanel.add(signatureDefaultsFormats, c);
		gbc.gridy++;
		add(signGeneralPanel, gbc);
		gbc.gridy++;
		add(netConfigPanel, gbc);
		gbc.weighty = 1.0;
		gbc.gridy++;
		add(new JPanel(), gbc);
	}

	/** Crea el panel con la configuraci&oacute;n de los formatos de firma a utilizar con cada tipo de fichero.
	 * @param modificationListener Listener para la detecci&oacute;n de cambio de configuraci&oacute;n.
	 * @param keyListener Listener para la deteccion del uso de teclas para el cierre de la pantalla.
	 * @return Panel con los componentes de configuraci&oacute;n. */
	private JPanel createSignatureFormatPanel(
			final ItemListener modificationListener,
			final KeyListener keyListener) {

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

	/** Di&aacute;logo para cambair la configuracion del proxy
	 * @param container Contenedor en el que se define el di&aacute;logo. */
    public static void changeProxyDlg(final Container container) {

    	// Cursor en espera
    	container.setCursor(new Cursor(Cursor.WAIT_CURSOR));

    	final ProxyPanel proxyDlg = new ProxyPanel();

    	// Cursor por defecto
    	container.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

    	if(AOUIFactory.showConfirmDialog(
				container,
				proxyDlg,
				SimpleAfirmaMessages.getString("ProxyDialog.0"), //$NON-NLS-1$
				JOptionPane.OK_CANCEL_OPTION,
				JOptionPane.DEFAULT_OPTION
		) == JOptionPane.OK_OPTION) {

			if (proxyDlg.isProxySelected()) {
				final String host = proxyDlg.getHost();
				final String port = proxyDlg.getPort();

				if(host == null || host == "") { //$NON-NLS-1$
					AOUIFactory.showErrorMessage(
						null,
						SimpleAfirmaMessages.getString("ProxyDialog.1"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("ProxyDialog.2"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE
					);
					changeProxyDlg(container);
					LOGGER.warning("El host no puede ser nulo o vacia"); //$NON-NLS-1$
				}
				else if(port == null || port == "") { //$NON-NLS-1$
					AOUIFactory.showErrorMessage(
						null,
						SimpleAfirmaMessages.getString("ProxyDialog.3"), //$NON-NLS-1$
						SimpleAfirmaMessages.getString("ProxyDialog.2"), //$NON-NLS-1$
						JOptionPane.ERROR_MESSAGE
					);
					changeProxyDlg(container);
					LOGGER.warning("El puerto no puede ser nulo, vacio o tener mas de 4 digitos"); //$NON-NLS-1$
				}
				else {
					PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_PROXY_HOST, host);
					PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_PROXY_PORT, port);
					PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_PROXY_USERNAME, proxyDlg.getUsername());
					PreferencesManager.put(PreferencesManager.PREFERENCE_GENERAL_PROXY_PASSWORD, proxyDlg.getPassword());
					AutoFirmaUtil.setProxySettings();
				}
			}
			LOGGER.warning("URL: "+  proxyDlg.getHost() + "\n Puerto: " + proxyDlg.getPort()); //$NON-NLS-1$ //$NON-NLS-2$
			PreferencesManager.putBoolean(
				PreferencesManager.PREFERENCE_GENERAL_PROXY_SELECTED,
				proxyDlg.isProxySelected()
			);
    	}
    }
}

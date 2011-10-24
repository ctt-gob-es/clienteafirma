package es.gob.afirma.ui.principal;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;


/**
 * Manejador de la configuraci&oacute;n principal de la interfaz.
 */
public class MainOptionsPane {

	/** Clave para la configuraci&oacute;n de vista. */
	public static final String MAIN_ADVANCED_VIEW = "main.advancedView";
	
	/** Clave para el algoritmo de firma por defecto. */
	public static final String MAIN_DEFAULT_ALGORITHM = "main.defaultAlgorithm";
	
	/** Clave para el algoritmo de firma por defecto. */
	public static final String MAIN_ALGORITHM_XML = "main.check.XMLAlgorithm";

	/** Clave para la activaci&oacute;n de la pol&iacute;tica de firma. */
	public static final String MAIN_POLICY_ESTABLISHED = "main.check.policy";
	
	/** Clave para la ruta de la pol&iacute;tica de firma. */
	public static final String MAIN_POLICY_URL = "main.policyUrl";
	
	/** Clave para el identificador de la pol&iacute;tica de firma. */
	public static final String MAIN_POLICY_OID = "main.policyOid";
	
	/** Panel sobre el que se montan los componentes. */
	final private JPanel panel;
	
	// Constantes de los algoritmos de huella digital
	final static List<String> algoritmoK = new ArrayList<String>(Arrays.asList("SHA-1","SHA-512","SHA-384","SHA-256"));
	final static List<String> algoritmoV = new ArrayList<String>(Arrays.asList("SHA1withRSA","SHA512withRSA","SHA384withRSA","SHA256withRSA"));

	/** Casilla de verificacion con el modo de vista a mostrar. */ 
	 private JCheckBox checkHabilitar;
	
	 /** Algoritmo a utilizar por defecto. */
	 private JComboBox comboAlgoritmo;
	 
	 /** Utilizar algoritmo por defecto para formatos internos XML */
	 private JCheckBox checkXML;

	 /** Casilla para activar el uso de politica de firma. */
	 private JCheckBox checkAddPolicy;
	 
	 /** URL de la pol&iacute;tica de firma. */
	 private JTextField textPolicyUrl;
	 
	 /** OID de la pol&iacute;tica de firma. */
	 private JTextField textPolicyOid;
	 
	/**
	 * Crea la vista y componentes de la pesta&ntilde;a principal de configuraci&oacute;n.
	 */
	public MainOptionsPane() {
		
    	panel = new JPanel(new GridBagLayout());
        
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weightx = 1.0;
        c.insets = new Insets(13, 13, 0, 13);
        c.gridy = 0;
        
    	// Panel general
    	JPanel generalPanel = new JPanel(new GridBagLayout());
    	generalPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.general"))); // NOI18N
    	Utils.setContrastColor(generalPanel);
    	Utils.setFontBold(generalPanel);

    	GridBagConstraints c2 = new GridBagConstraints();
        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(0, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = 0;
        
        JPanel panelCheckHabilitar = new JPanel(new GridLayout(1, 1));
        panelCheckHabilitar.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.general"));
        // Checkbox para habilitar las opciones de configuracion avanzada
        checkHabilitar = new JCheckBox();
        checkHabilitar.setText(Messages.getString("Opciones.general.habilitar")); // NOI18N
        checkHabilitar.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.general.habilitar")); // NOI18N
        checkHabilitar.setSelected(GeneralConfig.isAvanzados()); 
        checkHabilitar.setBounds(12, 20, 340, 23);
        checkHabilitar.setMnemonic(KeyEvent.VK_B); // Asignación de mnemónico al checkbox
        
        Utils.remarcar(checkHabilitar);

        Utils.setContrastColor(checkHabilitar);
        Utils.setFontBold(checkHabilitar);
        panelCheckHabilitar.add(checkHabilitar);
        generalPanel.add(panelCheckHabilitar, c2);
        
        panel.add(generalPanel, c);
        c.gridy = c.gridy + 1;
        
        // Panel criptografï¿½a
        JPanel criptografiaPanel = new JPanel(new GridBagLayout());
        criptografiaPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.criptografia"))); // NOI18N
        Utils.setContrastColor(criptografiaPanel);
        Utils.setFontBold(criptografiaPanel);

        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(4, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = 0;
        
        // Etiqueta algoritmo de huella digital
        JLabel etiquetaAlgoritmo = new JLabel(Messages.getString("Opciones.criptografia.algoritmo.parte"));
        Utils.setContrastColor(etiquetaAlgoritmo);
        Utils.setFontBold(etiquetaAlgoritmo);
        criptografiaPanel.add(etiquetaAlgoritmo, c2);
        
        c2.insets = new Insets(5, 13, 0, 13);
        c2.gridy = 1;
        c2.weighty = 0.1;
        c2.fill = GridBagConstraints.BOTH;
        
        // Combo con los algoritmos de huella digital
        comboAlgoritmo = new JComboBox();
        comboAlgoritmo.getAccessibleContext().setAccessibleName(etiquetaAlgoritmo.getText()+" ALT + R."); // NOI18N
        comboAlgoritmo.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.criptografia.algoritmo.parte")); // NOI18N
        //comboAlgoritmo.setModel(new DefaultComboBoxModel(Arrays.asList("SHA-1","SHA-512","SHA-384","SHA-256").toArray()));
        comboAlgoritmo.setModel(new DefaultComboBoxModel(algoritmoK.toArray()));

        Utils.remarcar(comboAlgoritmo);

        Utils.setContrastColor(comboAlgoritmo);
        Utils.setFontBold(comboAlgoritmo);
        criptografiaPanel.add(comboAlgoritmo, c2);
        
        //Relación entre etiqueta y combo
        etiquetaAlgoritmo.setLabelFor(comboAlgoritmo);
  		//Asignación de mnemónico
        etiquetaAlgoritmo.setDisplayedMnemonic(KeyEvent.VK_R);
        
        c2.insets = new Insets(5, 13, 0, 13);
        c2.gridy = 2;
        c2.weighty = 0.0;
        c2.fill = GridBagConstraints.HORIZONTAL;
        
        JPanel panelCheckXML = new JPanel(new GridLayout(1, 1));
        panelCheckXML.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.criptografia"));
        // Checkbox para utilizar XML
        checkXML = new JCheckBox();
        checkXML.setText(Messages.getString("Opciones.criptografia.utilizar")); // NOI18N
        checkXML.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.criptografia.utilizar")); // NOI18N
        checkXML.setMnemonic(KeyEvent.VK_U);  // Asignación de mnemónico al checkbox

        Utils.remarcar(checkXML);

        Utils.setContrastColor(checkXML);
        Utils.setFontBold(checkXML);
        panelCheckXML.add(checkXML);
        criptografiaPanel.add(panelCheckXML, c2);
        
        c.weighty = 0.3;
        c.fill = GridBagConstraints.BOTH;
        panel.add(criptografiaPanel, c);
        c.gridy = c.gridy + 1;
        c.weighty = 0.0;
        c.fill = GridBagConstraints.HORIZONTAL;
        
     // Panel Politica de firma
    	JPanel policyPanel = new JPanel(new GridBagLayout());
    	policyPanel.setBorder(BorderFactory.createTitledBorder("Pol\u00EDtica de firma")); // NOI18N
    	Utils.setContrastColor(policyPanel);
    	Utils.setFontBold(policyPanel);

        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(5, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = 0;
        
        JPanel panelCheckAddPolicy = new JPanel(new GridLayout(1, 1));
        panelCheckAddPolicy.getAccessibleContext().setAccessibleName("Pol\u00EDtica de firma");
        // Checkbox para habilitar las opciones de configuracion avanzada
        checkAddPolicy = new JCheckBox("Configurar pol\u00EDtica de firma");
        checkAddPolicy.getAccessibleContext().setAccessibleDescription("Habilitar para incorporar una po\u00EDtica a sus firmas.");
        checkAddPolicy.setMnemonic(KeyEvent.VK_O); // Asignación de mnemónico al checkbox
        
        Utils.remarcar(checkAddPolicy);

        Utils.setContrastColor(checkAddPolicy);
        Utils.setFontBold(checkAddPolicy);
        
        panelCheckAddPolicy.add(checkAddPolicy);
        policyPanel.add(panelCheckAddPolicy, c2);
        c2.gridy = c2.gridy + 1; 
        
        final JLabel policyOidLabel = new JLabel("Identificador de la pol\u00EDtica de firma (OID):");

        //Accesibilidad -- Lectores de pantalla
        policyOidLabel.setFocusable(true);
        policyOidLabel.getAccessibleContext().setAccessibleName(policyOidLabel.getText()+". Este cuadro de texto está deshabilitado por defecto.");
        
        Utils.remarcar(policyOidLabel);
        Utils.setContrastColor(policyOidLabel);
        Utils.setFontBold(policyOidLabel);
        policyPanel.add(policyOidLabel, c2);
        c2.gridy = c2.gridy + 1;
        
        textPolicyOid = new JTextField();
        textPolicyOid.setEnabled(false);
        
        Utils.remarcar(textPolicyOid);

        Utils.setContrastColor(textPolicyOid);
        Utils.setFontBold(textPolicyOid);
        
        //Relación entre etiqueta y campo de texto
        policyOidLabel.setLabelFor(textPolicyOid);
        
        policyPanel.add(textPolicyOid, c2);
        c2.gridy = c2.gridy + 1;
        
        final JLabel policyUrlLabel = new JLabel("Ruta de la pol\u00EDtica de firma (URL):");

        //Accesibilidad -- Lectores de pantalla
        policyUrlLabel.setFocusable(true);
        policyUrlLabel.getAccessibleContext().setAccessibleName(policyUrlLabel.getText()+". Este cuadro de texto está deshabilitado por defecto.");
        
        Utils.remarcar(policyUrlLabel);
        Utils.setContrastColor(policyUrlLabel);
        Utils.setFontBold(policyUrlLabel);
        policyPanel.add(policyUrlLabel, c2);
        c2.gridy = c2.gridy + 1;
        
        textPolicyUrl = new JTextField();
        textPolicyUrl.setEnabled(false);

        Utils.remarcar(textPolicyUrl);

        Utils.setContrastColor(textPolicyUrl);
        Utils.setFontBold(textPolicyUrl);
        
        //Relación entre etiqueta y campo de texto
        policyUrlLabel.setLabelFor(textPolicyUrl);
        
        policyPanel.add(textPolicyUrl, c2);
        
        panel.add(policyPanel, c);
                
        // Rellenamos el hueco libre con un panel vacio
        c.gridy = c.gridy + 1;
        c.weighty = 1.0;
        panel.add(new JPanel(), c);
        
        checkAddPolicy.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				boolean state = (e.getStateChange() == ItemEvent.SELECTED);
				textPolicyOid.setEnabled(state);
				textPolicyUrl.setEnabled(state);
				//Asignación de mnemónicos según el estado
				if (state) {
					policyOidLabel.setDisplayedMnemonic(KeyEvent.VK_I);
					policyOidLabel.getAccessibleContext().setAccessibleName(policyOidLabel.getText() + "ALT + I.");
					policyOidLabel.setFocusable(false);
					policyUrlLabel.setDisplayedMnemonic(KeyEvent.VK_T);
					policyUrlLabel.getAccessibleContext().setAccessibleName(policyUrlLabel.getText() + "ALT + T.");
					policyUrlLabel.setFocusable(false);
				
				} else {
					//Se eliminan los atajos porque los cuadros de texto están deshabilitados
					policyOidLabel.setDisplayedMnemonic(0);
					policyOidLabel.getAccessibleContext().setAccessibleName(policyOidLabel.getText()+". Este cuadro de texto está deshabilitado por defecto.");
					policyUrlLabel.setDisplayedMnemonic(0);
					policyUrlLabel.getAccessibleContext().setAccessibleName(policyUrlLabel.getText()+". Este cuadro de texto está deshabilitado por defecto.");
				}
			}
		});
        
		// Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(checkHabilitar, "opciones.general");
        HelpUtils.enableHelpKey(comboAlgoritmo, "opciones.algoritmo");
        HelpUtils.enableHelpKey(checkXML, "opciones.referenciasInternas");
    }
	
	public JPanel getConfigurationPanel() {
		return panel;
	}
	
	/**
	 * Introduce la configuraci&oacute;n establecida en el panel en un properties.
	 * @param config Configuraci&oacute;n para cargar en el panel.
	 */
	public void loadConfig(Properties config) {
		
		checkHabilitar.setSelected(Boolean.parseBoolean(config.getProperty(MainOptionsPane.MAIN_ADVANCED_VIEW, "false")));
		checkXML.setSelected(Boolean.parseBoolean(config.getProperty(MainOptionsPane.MAIN_ALGORITHM_XML, "false")));
		checkAddPolicy.setSelected(Boolean.parseBoolean(config.getProperty(MainOptionsPane.MAIN_POLICY_ESTABLISHED, "false")));
		textPolicyOid.setText(config.getProperty(MainOptionsPane.MAIN_POLICY_OID, ""));
		textPolicyUrl.setText(config.getProperty(MainOptionsPane.MAIN_POLICY_URL, ""));
		
		comboAlgoritmo.setSelectedIndex(0);
        for (int i = 0; i < algoritmoV.size(); i++){
            if (algoritmoV.get(i).equals(config.getProperty(MainOptionsPane.MAIN_DEFAULT_ALGORITHM))){
            	comboAlgoritmo.setSelectedIndex(i);
            	break;
            }
        }
	}
	
	/**
	 * Recupera el estado actual del panel.
	 * return Relaci&oacute;n con toda la configuraci&oacute;n del panel.
	 */
	public Properties getConfig() {
		Properties config = new Properties();
		config.setProperty(MainOptionsPane.MAIN_ADVANCED_VIEW, Boolean.toString(checkHabilitar.isSelected()));
    	config.setProperty(MainOptionsPane.MAIN_DEFAULT_ALGORITHM, algoritmoV.get(comboAlgoritmo.getSelectedIndex()));
    	config.setProperty(MainOptionsPane.MAIN_ALGORITHM_XML, Boolean.toString(checkXML.isSelected()));
    	config.setProperty(MainOptionsPane.MAIN_POLICY_ESTABLISHED, Boolean.toString(checkAddPolicy.isSelected()));
    	config.setProperty(MainOptionsPane.MAIN_POLICY_OID, textPolicyOid.getText());
    	config.setProperty(MainOptionsPane.MAIN_POLICY_URL, textPolicyUrl.getText());
    	
    	return config;
	}
	
	/**
	 * Recupera la configuraci&oacute;n de firma establecida en este panel.
	 * @return Propiedades para la configuraci&oacute;n de la firma.
	 */
	public Properties getSignatureConfig() {
		Properties config = new Properties();
		if (checkXML.isSelected()) {
			config.setProperty("referencesDigestMethod", comboAlgoritmo.getSelectedItem().toString());
		}
		if (checkAddPolicy.isSelected()) {
			config.setProperty("policyQualifier", textPolicyOid.getText());
			config.setProperty("policyIdentifier", textPolicyUrl.getText());
		}
		return config;
	}
}

package es.gob.afirma.ui.principal;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
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
	public static final String MAIN_ADVANCED_VIEW = "main.advancedView"; //$NON-NLS-1$
	
	/** Clave para el algoritmo de firma por defecto. */
	public static final String MAIN_DEFAULT_ALGORITHM = "main.defaultAlgorithm"; //$NON-NLS-1$
	
	/** Clave para el algoritmo de firma por defecto. */
	public static final String MAIN_ALGORITHM_XML = "main.check.XMLAlgorithm"; //$NON-NLS-1$

	/** Clave para la activaci&oacute;n de la pol&iacute;tica de firma. */
	public static final String MAIN_POLICY_ESTABLISHED = "main.check.policy"; //$NON-NLS-1$
	
	/** Clave para la ruta de la pol&iacute;tica de firma. */
	public static final String MAIN_POLICY_URL = "main.policyUrl"; //$NON-NLS-1$
	
	/** Clave para el identificador de la pol&iacute;tica de firma. */
	public static final String MAIN_POLICY_OID = "main.policyOid"; //$NON-NLS-1$
	
	/** Algoritmo por defecto para su uso por defecto en las firmas. */
	public static final String DEFAULT_DEFAULT_ALGORITHM = "SHA1withRSA";
	
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
	 JTextField textPolicyUrl;
	 
	 /** OID de la pol&iacute;tica de firma. */
	 JTextField textPolicyOid;
	 
	/**
	 * Crea la vista y componentes de la pesta&ntilde;a principal de configuraci&oacute;n.
	 */
	public MainOptionsPane() {
		
    	this.panel = new JPanel(new GridBagLayout());
        
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weightx = 1.0;
        c.insets = new Insets(13, 13, 0, 13);
        c.gridy = 0;
        
    	// Panel general
    	JPanel generalPanel = new JPanel(new GridBagLayout());
    	generalPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.general"))); // NOI18N //$NON-NLS-1$
    	Utils.setContrastColor(generalPanel);
    	Utils.setFontBold(generalPanel);

    	GridBagConstraints c2 = new GridBagConstraints();
        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(0, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = 0;
        
        JPanel panelCheckHabilitar = new JPanel(new GridLayout(1, 1));
        panelCheckHabilitar.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.general")); //$NON-NLS-1$
        // Checkbox para habilitar las opciones de configuracion avanzada
        this.checkHabilitar = new JCheckBox();
        this.checkHabilitar.setText(Messages.getString("Opciones.general.habilitar")); // NOI18N //$NON-NLS-1$
        this.checkHabilitar.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.general.habilitar")); // NOI18N //$NON-NLS-1$
        this.checkHabilitar.setSelected(GeneralConfig.isAvanzados()); 
        this.checkHabilitar.setBounds(12, 20, 340, 23);
        this.checkHabilitar.setMnemonic(KeyEvent.VK_B); // Asignacion de mnemonico al checkbox
        
        Utils.remarcar(this.checkHabilitar);

        Utils.setContrastColor(this.checkHabilitar);
        Utils.setFontBold(this.checkHabilitar);
        panelCheckHabilitar.add(this.checkHabilitar);
        generalPanel.add(panelCheckHabilitar, c2);
        
        this.panel.add(generalPanel, c);
        c.gridy = c.gridy + 1;
        
        // Panel criptografia
        JPanel criptografiaPanel = new JPanel(new GridBagLayout());
        criptografiaPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.criptografia"))); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(criptografiaPanel);
        Utils.setFontBold(criptografiaPanel);

        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(4, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = 0;
        
        // Etiqueta algoritmo de huella digital
        JLabel etiquetaAlgoritmo = new JLabel(Messages.getString("Opciones.criptografia.algoritmo.parte")); //$NON-NLS-1$
        Utils.setContrastColor(etiquetaAlgoritmo);
        Utils.setFontBold(etiquetaAlgoritmo);
        criptografiaPanel.add(etiquetaAlgoritmo, c2);
        
        c2.insets = new Insets(5, 13, 0, 13);
        c2.gridy = 1;
        c2.weighty = 0.1;
        c2.fill = GridBagConstraints.BOTH;
        
        // Combo con los algoritmos de huella digital
        this.comboAlgoritmo = new JComboBox();
        this.comboAlgoritmo.getAccessibleContext().setAccessibleName(etiquetaAlgoritmo.getText()+" ALT + R."); // NOI18N
        this.comboAlgoritmo.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.criptografia.algoritmo.parte")); // NOI18N //$NON-NLS-1$
        //comboAlgoritmo.setModel(new DefaultComboBoxModel(Arrays.asList("SHA-1","SHA-512","SHA-384","SHA-256").toArray()));
        this.comboAlgoritmo.setModel(new DefaultComboBoxModel(algoritmoK.toArray()));

        Utils.remarcar(this.comboAlgoritmo);

        Utils.setContrastColor(this.comboAlgoritmo);
        Utils.setFontBold(this.comboAlgoritmo);
        criptografiaPanel.add(this.comboAlgoritmo, c2);
        
        //Relacion entre etiqueta y combo
        etiquetaAlgoritmo.setLabelFor(this.comboAlgoritmo);
  		//Asignacion de mnemonico
        etiquetaAlgoritmo.setDisplayedMnemonic(KeyEvent.VK_R);
        
        c2.insets = new Insets(5, 13, 0, 13);
        c2.gridy = 2;
        c2.weighty = 0.0;
        c2.fill = GridBagConstraints.HORIZONTAL;
        
        JPanel panelCheckXML = new JPanel(new GridLayout(1, 1));
        panelCheckXML.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.criptografia")); //$NON-NLS-1$
        // Checkbox para utilizar XML
        this.checkXML = new JCheckBox();
        this.checkXML.setText(Messages.getString("Opciones.criptografia.utilizar")); // NOI18N //$NON-NLS-1$
        this.checkXML.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.criptografia.utilizar")); // NOI18N //$NON-NLS-1$
        this.checkXML.setMnemonic(KeyEvent.VK_U);  // Asignacion de mnemonico al checkbox

        Utils.remarcar(this.checkXML);

        Utils.setContrastColor(this.checkXML);
        Utils.setFontBold(this.checkXML);
        panelCheckXML.add(this.checkXML);
        criptografiaPanel.add(panelCheckXML, c2);
        
        c.weighty = 0.3;
        c.fill = GridBagConstraints.BOTH;
        this.panel.add(criptografiaPanel, c);
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
        this.checkAddPolicy = new JCheckBox("Configurar pol\u00EDtica de firma");
        this.checkAddPolicy.getAccessibleContext().setAccessibleDescription("Habilitar para incorporar una po\u00EDtica a sus firmas.");
        this.checkAddPolicy.setMnemonic(KeyEvent.VK_F); // Asignacion de mnemonico al checkbox
        
        Utils.remarcar(this.checkAddPolicy);

        Utils.setContrastColor(this.checkAddPolicy);
        Utils.setFontBold(this.checkAddPolicy);
        
        panelCheckAddPolicy.add(this.checkAddPolicy);
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
        
        this.textPolicyOid = new JTextField();
        this.textPolicyOid.setEnabled(false);
        
        Utils.remarcar(this.textPolicyOid);

        Utils.setContrastColor(this.textPolicyOid);
        Utils.setFontBold(this.textPolicyOid);
        
        //Relacion entre etiqueta y campo de texto
        policyOidLabel.setLabelFor(this.textPolicyOid);
        
        policyPanel.add(this.textPolicyOid, c2);
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
        
        this.textPolicyUrl = new JTextField();
        this.textPolicyUrl.setEnabled(false);

        Utils.remarcar(this.textPolicyUrl);

        Utils.setContrastColor(this.textPolicyUrl);
        Utils.setFontBold(this.textPolicyUrl);
        
        //Relacion entre etiqueta y campo de texto
        policyUrlLabel.setLabelFor(this.textPolicyUrl);
        
        policyPanel.add(this.textPolicyUrl, c2);
        
        this.panel.add(policyPanel, c);
                
        c.gridy = c.gridy + 1;
        
        //Botones
        JPanel buttonPanel = new JPanel( new FlowLayout(FlowLayout.RIGHT, 1, 1));
        
        //Definicion de botones
        final JButton valores = new JButton();
        
        JPanel panelValores = new JPanel(new GridLayout(1, 1));
        //Boton Valores por defecto
        valores.setText(Messages.getString("Opciones.accesibilidad.valores")); //$NON-NLS-1$
        valores.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				valoresActionPerformed();				
			}
		});
        valores.setMnemonic(KeyEvent.VK_O);
        Utils.remarcar(valores);
        Utils.setContrastColor(valores);
        Utils.setFontBold(valores);
        panelValores.add(valores);
        buttonPanel.add(panelValores);        

        this.panel.add(buttonPanel, c);
        // Rellenamos el hueco libre con un panel vacio
        c.gridy = c.gridy + 1;
        c.weighty = 1.0;
        this.panel.add(new JPanel(), c);
        
        this.checkAddPolicy.addItemListener(new ItemListener() {
			@Override
            public void itemStateChanged(ItemEvent e) {
				boolean state = (e.getStateChange() == ItemEvent.SELECTED);
				MainOptionsPane.this.textPolicyOid.setEnabled(state);
				MainOptionsPane.this.textPolicyUrl.setEnabled(state);
				//Asignacion de mnemonicos segun el estado
				if (state) {
					policyOidLabel.setDisplayedMnemonic(KeyEvent.VK_I);
					policyOidLabel.getAccessibleContext().setAccessibleName(policyOidLabel.getText() + "ALT + I.");
					policyOidLabel.setFocusable(false);
					policyUrlLabel.setDisplayedMnemonic(KeyEvent.VK_T);
					policyUrlLabel.getAccessibleContext().setAccessibleName(policyUrlLabel.getText() + "ALT + T.");
					policyUrlLabel.setFocusable(false);
				
				} 
				else {
					//Se eliminan los atajos porque los cuadros de texto están deshabilitados
					policyOidLabel.setDisplayedMnemonic(0);
					policyOidLabel.getAccessibleContext().setAccessibleName(policyOidLabel.getText()+". Este cuadro de texto esta deshabilitado por defecto.");
					policyUrlLabel.setDisplayedMnemonic(0);
					policyUrlLabel.getAccessibleContext().setAccessibleName(policyUrlLabel.getText()+". Este cuadro de texto esta deshabilitado por defecto.");
				}
			}
		});
        
		// Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(this.checkHabilitar, "opciones.general"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.comboAlgoritmo, "opciones.algoritmo"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.checkXML, "opciones.referenciasInternas"); //$NON-NLS-1$
    }
	
	JPanel getConfigurationPanel() {
		return this.panel;
	}
	
	/**
	 * Configura los componentes del panel con las propiedades introducidas.
	 * @param config Configuraci&oacute;n para cargar en el panel.
	 */
	public void loadConfig(Properties config) {

		this.checkHabilitar.setSelected(Boolean.parseBoolean(config.getProperty(MainOptionsPane.MAIN_ADVANCED_VIEW, "false")));
		this.checkXML.setSelected(Boolean.parseBoolean(config.getProperty(MainOptionsPane.MAIN_ALGORITHM_XML, "false")));
		this.checkAddPolicy.setSelected(Boolean.parseBoolean(config.getProperty(MainOptionsPane.MAIN_POLICY_ESTABLISHED, "false")));
		this.textPolicyOid.setText(config.getProperty(MainOptionsPane.MAIN_POLICY_OID, ""));
		this.textPolicyUrl.setText(config.getProperty(MainOptionsPane.MAIN_POLICY_URL, ""));
		
		this.comboAlgoritmo.setSelectedIndex(0);
        for (int i = 0; i < algoritmoV.size(); i++){
            if (algoritmoV.get(i).equals(config.getProperty(MainOptionsPane.MAIN_DEFAULT_ALGORITHM))){
            	this.comboAlgoritmo.setSelectedIndex(i);
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
		config.setProperty(MainOptionsPane.MAIN_ADVANCED_VIEW, Boolean.toString(this.checkHabilitar.isSelected()));
    	config.setProperty(MainOptionsPane.MAIN_DEFAULT_ALGORITHM, algoritmoV.get(this.comboAlgoritmo.getSelectedIndex()));
    	config.setProperty(MainOptionsPane.MAIN_ALGORITHM_XML, Boolean.toString(this.checkXML.isSelected()));
    	config.setProperty(MainOptionsPane.MAIN_POLICY_ESTABLISHED, Boolean.toString(this.checkAddPolicy.isSelected()));
    	config.setProperty(MainOptionsPane.MAIN_POLICY_OID, this.textPolicyOid.getText());
    	config.setProperty(MainOptionsPane.MAIN_POLICY_URL, this.textPolicyUrl.getText());
    	
    	return config;
	}
	
	/**
	 * Recupera la configuraci&oacute;n de firma establecida en este panel.
	 * @return Propiedades para la configuraci&oacute;n de la firma.
	 */
	public Properties getSignatureConfig() {
		Properties config = new Properties();
		if (this.checkXML.isSelected()) {
			config.setProperty("referencesDigestMethod", this.comboAlgoritmo.getSelectedItem().toString());
		}
		if (this.checkAddPolicy.isSelected()) {
			config.setProperty("policyQualifier", this.textPolicyOid.getText());
			config.setProperty("policyIdentifier", this.textPolicyUrl.getText());
		}
		return config;
	}
	
	/**
	 * Aplica los valores por defecto.
	 */
	void valoresActionPerformed(){
		Opciones.setUpdate(true);
		restore(this.panel);
	}
	
	/**
	 * Aplica el estado por defecto de los componentes de la ventana 
	 */
	private void restore(JPanel panel1){
		for (int i=0; i<panel1.getComponentCount();i++){
			if (panel1.getComponent(i) instanceof JTextField){
				((JTextField)panel1.getComponent(i)).setText("");
			} 
			else if(panel1.getComponent(i) instanceof JCheckBox){
				((JCheckBox)panel1.getComponent(i)).setSelected(false);				
			} 
			else if (panel1.getComponent(i) instanceof JComboBox){
				((JComboBox)panel1.getComponent(i)).setSelectedIndex(0);
				
			}
			else if (panel1.getComponent(i) instanceof JPanel){
				JPanel interiorPanel = (JPanel)panel1.getComponent(i);
				restore(interiorPanel);
			}
		}
	}
}

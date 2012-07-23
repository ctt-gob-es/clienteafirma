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

/** Manejador de la configuraci&oacute;n principal de la interfaz. */
public class MainOptionsPane {

    // Constantes de los algoritmos de huella digital
	static final List<String> ALGORITHM_K = new ArrayList<String>(Arrays.asList("SHA-1", "SHA-512", "SHA-384", "SHA-256")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

	static final List<String> ALGORITHM_V = new ArrayList<String>(Arrays.asList("SHA1withRSA", "SHA512withRSA", "SHA384withRSA", "SHA256withRSA")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    /** Algoritmo por defecto para su uso por defecto en las firmas. */
    public static final String DEFAULT_DEFAULT_ALGORITHM = "SHA1withRSA"; //$NON-NLS-1$

    /** Clave para la configuraci&oacute;n de vista. */
    public static final String MAIN_ADVANCED_VIEW = "main.advancedView"; //$NON-NLS-1$

    /** Clave para el algoritmo de firma por defecto. */
    public static final String MAIN_ALGORITHM_XML = "main.check.XMLAlgorithm"; //$NON-NLS-1$

    /** Clave para el algoritmo de firma por defecto. */
    public static final String MAIN_DEFAULT_ALGORITHM = "main.defaultAlgorithm"; //$NON-NLS-1$

    /** Clave para la activaci&oacute;n de la pol&iacute;tica de firma. */
    public static final String MAIN_POLICY_ESTABLISHED = "main.check.policy"; //$NON-NLS-1$

    /** Clave para el identificador de la pol&iacute;tica de firma. */
    public static final String MAIN_POLICY_IDENTIFIER = "main.policyOid"; //$NON-NLS-1$

    /** Clave para la ruta de la pol&iacute;tica de firma. */
    public static final String MAIN_POLICY_QUALIFIER = "main.policyUrl"; //$NON-NLS-1$

    /** Clave para la huella digital de la pol&iacute;tica de firma. */
    public static final String MAIN_POLICY_HASH = "main.policyHash"; //$NON-NLS-1$

    /** Algoritmo de huella digital utilizado por defecto para la pol&iacute;tica. */
    public static final String DEFAULT_POLICY_HASH_ALGORITHM = "http://www.w3.org/2000/09/xmldsig#sha1"; //$NON-NLS-1$

    /** Casilla para activar el uso de politica de firma. */
    private final JCheckBox checkAddPolicy;

    /** Casilla de verificacion con el modo de vista a mostrar. */
    private final JCheckBox checkHabilitar;

    /** Utilizar algoritmo por defecto para formatos internos XML */
    private final JCheckBox checkXML;

    /** Algoritmo a utilizar por defecto. */
    private final JComboBox comboAlgoritmo;

    /** Panel sobre el que se montan los componentes. */
    private final JPanel panel;

    /** Etiqueta del identificador de la pol&iacute;tica (puede ser una URN, OID,...). */
    private final JLabel policyIdentifierLabel;

    /** Identificador de la pol&iacute;tica (puede ser una URN, OID,...). */
    private final JTextField textPolicyIdentifier;

    /** Etiqueta del cualificador de la pol&iacute;tica. */
    private final JLabel policyQualifierLabel;

    /** Cualificador de la pol&iacute;tica de firma. */
    private final JTextField textPolicyQualifier;

    /** Etiqueta de la huella digital SHA 1 de la pol&iacute;tica. */
    private final JLabel policyHashLabel;

    /** Huella digital SHA1 de la pol&iacute;tica de firma. */
    private final JTextField textPolicyHash;

    /** Crea la vista y componentes de la pesta&ntilde;a principal de configuraci&oacute;n. */
    public MainOptionsPane() {

        this.panel = new JPanel(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weightx = 1.0;
        c.insets = new Insets(13, 13, 0, 13);
        c.gridy = 0;

        // Panel general
        final JPanel generalPanel = new JPanel(new GridBagLayout());
        generalPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.general"))); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(generalPanel);
        Utils.setFontBold(generalPanel);

        final GridBagConstraints c2 = new GridBagConstraints();
        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(0, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = 0;

        final JPanel panelCheckHabilitar = new JPanel(new GridLayout(1, 1));
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
        final JPanel criptografiaPanel = new JPanel(new GridBagLayout());
        criptografiaPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.criptografia"))); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(criptografiaPanel);
        Utils.setFontBold(criptografiaPanel);

        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(4, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = 0;

        // Etiqueta algoritmo de huella digital
        final JLabel etiquetaAlgoritmo = new JLabel(Messages.getString("Opciones.criptografia.algoritmo.parte")); //$NON-NLS-1$
        Utils.setContrastColor(etiquetaAlgoritmo);
        Utils.setFontBold(etiquetaAlgoritmo);
        criptografiaPanel.add(etiquetaAlgoritmo, c2);

        c2.insets = new Insets(5, 13, 0, 13);
        c2.gridy = 1;
        c2.fill = GridBagConstraints.BOTH;

        // Combo con los algoritmos de huella digital
        this.comboAlgoritmo = new JComboBox();
        this.comboAlgoritmo.getAccessibleContext().setAccessibleName(etiquetaAlgoritmo.getText() + " ALT + R."); // NOI18N
        this.comboAlgoritmo.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.criptografia.algoritmo.parte")); // NOI18N //$NON-NLS-1$
        // comboAlgoritmo.setModel(new DefaultComboBoxModel(Arrays.asList("SHA-1","SHA-512","SHA-384","SHA-256").toArray()));
        this.comboAlgoritmo.setModel(new DefaultComboBoxModel(ALGORITHM_K.toArray()));

        Utils.remarcar(this.comboAlgoritmo);

        Utils.setContrastColor(this.comboAlgoritmo);
        Utils.setFontBold(this.comboAlgoritmo);
        criptografiaPanel.add(this.comboAlgoritmo, c2);

        // Relacion entre etiqueta y combo
        etiquetaAlgoritmo.setLabelFor(this.comboAlgoritmo);
        // Asignacion de mnemonico
        etiquetaAlgoritmo.setDisplayedMnemonic(KeyEvent.VK_R);

        c2.insets = new Insets(5, 13, 0, 13);
        c2.gridy = 2;
        c2.weighty = 0.0;
        c2.fill = GridBagConstraints.HORIZONTAL;

        final JPanel panelCheckXML = new JPanel(new GridLayout(1, 1));
        panelCheckXML.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.criptografia")); //$NON-NLS-1$
        // Checkbox para utilizar XML
        this.checkXML = new JCheckBox();
        this.checkXML.setText(Messages.getString("Opciones.criptografia.utilizar")); // NOI18N //$NON-NLS-1$
        this.checkXML.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.criptografia.utilizar")); // NOI18N //$NON-NLS-1$
        this.checkXML.setMnemonic(KeyEvent.VK_U); // Asignacion de mnemonico al checkbox

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
        final JPanel policyPanel = new JPanel(new GridBagLayout());
        policyPanel.setBorder(BorderFactory.createTitledBorder("Pol\u00EDtica de firma")); // NOI18N
        Utils.setContrastColor(policyPanel);
        Utils.setFontBold(policyPanel);

        c2.fill = GridBagConstraints.HORIZONTAL;
        c2.insets = new Insets(5, 13, 0, 13);
        c2.weightx = 1.0;
        c2.gridy = 0;

        final JPanel panelCheckAddPolicy = new JPanel(new GridLayout(1, 1));
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

        this.policyIdentifierLabel = new JLabel("Identificador de la pol\u00EDtica de firma:");
        this.policyIdentifierLabel.setEnabled(false);

        // Accesibilidad -- Lectores de pantalla
        this.policyIdentifierLabel.setFocusable(true);
        this.policyIdentifierLabel.getAccessibleContext().setAccessibleName(this.policyIdentifierLabel.getText() + ". Este cuadro de texto esta deshabilitado por defecto.");

        Utils.remarcar(this.policyIdentifierLabel);
        Utils.setContrastColor(this.policyIdentifierLabel);
        Utils.setFontBold(this.policyIdentifierLabel);
        policyPanel.add(this.policyIdentifierLabel, c2);
        c2.gridy = c2.gridy + 1;

        this.textPolicyIdentifier = new JTextField();
        this.textPolicyIdentifier.setEnabled(false);

        Utils.remarcar(this.textPolicyIdentifier);

        Utils.setContrastColor(this.textPolicyIdentifier);
        Utils.setFontBold(this.textPolicyIdentifier);

        // Relacion entre etiqueta y campo de texto
        this.policyIdentifierLabel.setLabelFor(this.textPolicyIdentifier);

        policyPanel.add(this.textPolicyIdentifier, c2);
        c2.gridy = c2.gridy + 1;

        this.policyQualifierLabel = new JLabel("Cualificador de la pol\u00EDtica de firma (URL):");
        this.policyQualifierLabel.setEnabled(false);

        // Accesibilidad -- Lectores de pantalla
        this.policyQualifierLabel.setFocusable(true);
        this.policyQualifierLabel.getAccessibleContext().setAccessibleName(this.policyQualifierLabel.getText() + ". Este cuadro de texto esta deshabilitado por defecto.");

        Utils.remarcar(this.policyQualifierLabel);
        Utils.setContrastColor(this.policyQualifierLabel);
        Utils.setFontBold(this.policyQualifierLabel);
        policyPanel.add(this.policyQualifierLabel, c2);
        c2.gridy = c2.gridy + 1;

        this.textPolicyQualifier = new JTextField();
        this.textPolicyQualifier.setEnabled(false);

        Utils.remarcar(this.textPolicyQualifier);

        Utils.setContrastColor(this.textPolicyQualifier);
        Utils.setFontBold(this.textPolicyQualifier);

        // Relacion entre etiqueta y campo de texto
        this.policyQualifierLabel.setLabelFor(this.textPolicyQualifier);

        policyPanel.add(this.textPolicyQualifier, c2);
        c2.gridy = c2.gridy + 1;

        this.policyHashLabel = new JLabel("Huella digital SHA1:");
        this.policyHashLabel.setEnabled(false);

        // Accesibilidad -- Lectores de pantalla
        this.policyHashLabel.setFocusable(true);
        this.policyHashLabel.getAccessibleContext().setAccessibleName(this.policyHashLabel.getText() + ". Este cuadro de texto esta deshabilitado por defecto.");

        Utils.remarcar(this.policyHashLabel);
        Utils.setContrastColor(this.policyHashLabel);
        Utils.setFontBold(this.policyHashLabel);
        policyPanel.add(this.policyHashLabel, c2);
        c2.gridy = c2.gridy + 1;

        this.textPolicyHash = new JTextField();
        this.textPolicyHash.setEnabled(false);

        Utils.remarcar(this.textPolicyHash);

        Utils.setContrastColor(this.textPolicyHash);
        Utils.setFontBold(this.textPolicyHash);

        // Relacion entre etiqueta y campo de texto
        this.policyHashLabel.setLabelFor(this.textPolicyHash);

        policyPanel.add(this.textPolicyHash, c2);

        this.panel.add(policyPanel, c);

        c.gridy = c.gridy + 1;

        // Botones
        final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 1, 1));

        // Definicion de botones
        final JButton valores = new JButton();

        final JPanel panelValores = new JPanel(new GridLayout(1, 1));
        // Boton Valores por defecto
        valores.setText(Messages.getString("Opciones.accesibilidad.valores")); //$NON-NLS-1$
        valores.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(final ActionEvent e) {
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
            public void itemStateChanged(final ItemEvent e) {
                final boolean state = (e.getStateChange() == ItemEvent.SELECTED);
                MainOptionsPane.this.policyIdentifierLabel.setEnabled(state);
                MainOptionsPane.this.policyQualifierLabel.setEnabled(state);
                MainOptionsPane.this.policyHashLabel.setEnabled(state);
                MainOptionsPane.this.textPolicyIdentifier.setEnabled(state);
                MainOptionsPane.this.textPolicyQualifier.setEnabled(state);
                MainOptionsPane.this.textPolicyHash.setEnabled(state);
                // Asignacion de mnemonicos segun el estado
                if (state) {
                    MainOptionsPane.this.policyIdentifierLabel.setDisplayedMnemonic(KeyEvent.VK_I);
                    MainOptionsPane.this.policyIdentifierLabel.getAccessibleContext().setAccessibleName(MainOptionsPane.this.policyIdentifierLabel.getText() + "ALT + I.");
                    MainOptionsPane.this.policyIdentifierLabel.setFocusable(false);
                    MainOptionsPane.this.policyQualifierLabel.setDisplayedMnemonic(KeyEvent.VK_T);
                    MainOptionsPane.this.policyQualifierLabel.getAccessibleContext().setAccessibleName(MainOptionsPane.this.policyQualifierLabel.getText() + "ALT + T.");
                    MainOptionsPane.this.policyQualifierLabel.setFocusable(false);
                    MainOptionsPane.this.policyHashLabel.setDisplayedMnemonic(KeyEvent.VK_D);
                    MainOptionsPane.this.policyHashLabel.getAccessibleContext().setAccessibleName(MainOptionsPane.this.policyHashLabel.getText() + "ALT + D.");
                    MainOptionsPane.this.policyHashLabel.setFocusable(false);

                }
                else {
                    // Se eliminan los atajos porque los cuadros de texto estan deshabilitados
                    MainOptionsPane.this.policyIdentifierLabel.setDisplayedMnemonic(0);
                    MainOptionsPane.this.policyIdentifierLabel.getAccessibleContext()
                    .setAccessibleName(MainOptionsPane.this.policyIdentifierLabel.getText() + ". Este cuadro de texto esta deshabilitado por defecto.");
                    MainOptionsPane.this.policyIdentifierLabel.setFocusable(true);
                    MainOptionsPane.this.policyQualifierLabel.setDisplayedMnemonic(0);
                    MainOptionsPane.this.policyQualifierLabel.getAccessibleContext()
                    .setAccessibleName(MainOptionsPane.this.policyQualifierLabel.getText() + ". Este cuadro de texto esta deshabilitado por defecto.");
                    MainOptionsPane.this.policyQualifierLabel.setFocusable(true);
                    MainOptionsPane.this.policyHashLabel.setDisplayedMnemonic(0);
                    MainOptionsPane.this.policyHashLabel.getAccessibleContext()
                    .setAccessibleName(MainOptionsPane.this.policyHashLabel.getText() + ". Este cuadro de texto esta deshabilitado por defecto.");
                    MainOptionsPane.this.policyHashLabel.setFocusable(true);
                }
            }
        });

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(this.checkHabilitar, "opciones.general"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.comboAlgoritmo, "opciones.algoritmo"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.checkXML, "opciones.referenciasInternas"); //$NON-NLS-1$
    }

    /** Recupera el estado actual del panel.
     * return Relaci&oacute;n con toda la configuraci&oacute;n del panel. */
    Properties getConfig() {
        final Properties config = new Properties();
        config.setProperty(MainOptionsPane.MAIN_ADVANCED_VIEW, Boolean.toString(this.checkHabilitar.isSelected()));
        config.setProperty(MainOptionsPane.MAIN_DEFAULT_ALGORITHM, ALGORITHM_V.get(this.comboAlgoritmo.getSelectedIndex()));
        config.setProperty(MainOptionsPane.MAIN_ALGORITHM_XML, Boolean.toString(this.checkXML.isSelected()));
        config.setProperty(MainOptionsPane.MAIN_POLICY_ESTABLISHED, Boolean.toString(this.checkAddPolicy.isSelected()));
        config.setProperty(MainOptionsPane.MAIN_POLICY_IDENTIFIER, this.textPolicyIdentifier.getText());
        config.setProperty(MainOptionsPane.MAIN_POLICY_QUALIFIER, this.textPolicyQualifier.getText());
        config.setProperty(MainOptionsPane.MAIN_POLICY_HASH, this.textPolicyHash.getText());

        return config;
    }

    JPanel getConfigurationPanel() {
        return this.panel;
    }

    /** Recupera la configuraci&oacute;n de firma establecida en este panel.
     * @return Propiedades para la configuraci&oacute;n de la firma. */
    public Properties getSignatureConfig() {
        final Properties config = new Properties();
        if (this.checkXML.isSelected()) {
            config.setProperty("referencesDigestMethod", this.comboAlgoritmo.getSelectedItem().toString()); //$NON-NLS-1$
        }
        if (this.checkAddPolicy.isSelected()) {
            config.setProperty("policyIdentifier", this.textPolicyIdentifier.getText()); //$NON-NLS-1$
            config.setProperty("policyQualifier", this.textPolicyQualifier.getText()); //$NON-NLS-1$
            config.setProperty("policyIdentifierHash", this.textPolicyHash.getText()); //$NON-NLS-1$
            config.setProperty("policyIdentifierHashAlgorithm", DEFAULT_POLICY_HASH_ALGORITHM); //$NON-NLS-1$
        }
        return config;
    }

    /** Configura los componentes del panel con las propiedades introducidas.
     * @param config Configuraci&oacute;n para cargar en el panel. */
    public void loadConfig(final Properties config) {

        this.checkHabilitar.setSelected(Boolean.parseBoolean(config.getProperty(MainOptionsPane.MAIN_ADVANCED_VIEW, "false"))); //$NON-NLS-1$
        this.checkXML.setSelected(Boolean.parseBoolean(config.getProperty(MainOptionsPane.MAIN_ALGORITHM_XML, "false"))); //$NON-NLS-1$
        this.checkAddPolicy.setSelected(Boolean.parseBoolean(config.getProperty(MainOptionsPane.MAIN_POLICY_ESTABLISHED, "false"))); //$NON-NLS-1$
        this.textPolicyIdentifier.setText(config.getProperty(MainOptionsPane.MAIN_POLICY_IDENTIFIER, "")); //$NON-NLS-1$
        this.textPolicyQualifier.setText(config.getProperty(MainOptionsPane.MAIN_POLICY_QUALIFIER, "")); //$NON-NLS-1$
        this.textPolicyHash.setText(config.getProperty(MainOptionsPane.MAIN_POLICY_HASH, "")); //$NON-NLS-1$

        this.comboAlgoritmo.setSelectedIndex(0);
        for (int i = 0; i < ALGORITHM_V.size(); i++) {
            if (ALGORITHM_V.get(i).equals(config.getProperty(MainOptionsPane.MAIN_DEFAULT_ALGORITHM))) {
                this.comboAlgoritmo.setSelectedIndex(i);
                break;
            }
        }
    }

    /** Aplica el estado por defecto de los componentes de la ventana */
    private void restore(final JPanel panel1) {
        for (int i = 0; i < panel1.getComponentCount(); i++) {
            if (panel1.getComponent(i) instanceof JTextField) {
                ((JTextField) panel1.getComponent(i)).setText(""); //$NON-NLS-1$
            }
            else if (panel1.getComponent(i) instanceof JCheckBox) {
                ((JCheckBox) panel1.getComponent(i)).setSelected(false);
            }
            else if (panel1.getComponent(i) instanceof JComboBox) {
                ((JComboBox) panel1.getComponent(i)).setSelectedIndex(0);

            }
            else if (panel1.getComponent(i) instanceof JPanel) {
                final JPanel interiorPanel = (JPanel) panel1.getComponent(i);
                restore(interiorPanel);
            }
        }
    }

    /** Aplica los valores por defecto. */
    void valoresActionPerformed() {
        Opciones.setUpdate(true);
        restore(this.panel);
    }
}

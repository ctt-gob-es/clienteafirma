/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */
package es.gob.afirma.ui.principal;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Properties;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.WindowConstants;

import es.gob.afirma.misc.Platform;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.Messages;

/**
 * Clase que muestra el panel de opciones.
 */
public class Opciones extends JDialog {

	private static final long serialVersionUID = 1L;

    /** Pantalla principal de la aplicaci&oacute;n. */
    private PrincipalGUI mainGui;
    
    /** Panel con la configuraci&oacute;n general del aplicativo. */
    private MainOptionsPane mainOptions;
    
    /** Panel con la configurac&oacute;n de las firmas PDF del aplicativo. */
    private ContextOptionsPane contextOptions;
    
    public Opciones(PrincipalGUI mainGUI) {
    	this.mainGui = mainGUI;
        initComponents();
    }
  
    /**
     * Inicializacion de componentes
     */
    private void initComponents() {
    	// Configuracion de la ventana
    	setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setTitle(Messages.getString("Opciones.opciones")); // NOI18N
        setResizable(false);
        getContentPane().setLayout(new GridBagLayout());
        
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.weighty = 1.0;
//        c.insets = new Insets(13, 13, 0, 13);
        c.gridy = 0;
        
        // Dimensiones de la ventana en Windows y Linux
        Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
        setBounds((screenSize.width - 426) / 2, (screenSize.height - 456) / 2, 426, 456);
        
        if (Platform.getOS().equals(Platform.OS.MACOSX))
        	setBounds((screenSize.width - 426) / 2, (screenSize.height - 485) / 2, 426, 485);
        
        // Panel superior con las opciones de configuracion
        JTabbedPane mainPanel = new JTabbedPane();
        
        mainOptions =  new MainOptionsPane();
        mainPanel.addTab(Messages.getString("Opciones.general"),
        		null,
        		mainOptions.getConfigurationPanel(),
        		Messages.getString("Opciones.general"));

        mainOptions.loadConfig(GeneralConfig.getConfig());
        
        contextOptions =  new ContextOptionsPane();
        mainPanel.addTab("Contexto de firma",
        		null,
        		contextOptions.getConfigurationPanel(),
        		"Pesta\u00F1a para la configuraci\u00F3n del contexto de firma");
        
        contextOptions.loadConfig(GeneralConfig.getConfig());
        
        getContentPane().add(mainPanel, c);
        
        c.weighty = 0.0;
        c.gridy = 1;
        getContentPane().add(createButtonsPanel(), c);
    }

    private Component createButtonsPanel() {
    	// Panel inferior
        JPanel bottomPanel = new JPanel(new GridBagLayout());
        
		GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.ipadx = 15;
		cons.gridx = 0;
		cons.insets = new Insets(11, 0, 13, 0);
		
		// Etiqueta para rellenar a la izquierda
		JLabel label = new JLabel();
		bottomPanel.add(label, cons);
        
		// Boton aceptar
        JButton aceptar = new JButton();
        aceptar.setText(Messages.getString("PrincipalGUI.aceptar")); // NOI18N
        aceptar.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
            	
            	Properties config = new Properties();
            	config.putAll(mainOptions.getConfig());
            	config.putAll(contextOptions.getConfig());
            	
            	Properties signatureConfig = new Properties();
            	signatureConfig.putAll(mainOptions.getSignatureConfig());
            	signatureConfig.putAll(contextOptions.getSignatureConfig());
            	
                aceptarActionPerformed(config, signatureConfig);
            }
        });
        aceptar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.aceptar")); // NOI18N
        
        // Boton cancelar
        JButton	cancelar = new JButton();
        cancelar.setText(Messages.getString("PrincipalGUI.cancelar")); // NOI18N
        cancelar.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                cancelarActionPerformed();
            }
        });
        cancelar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.cancelar")); // NOI18N
        
        
        // Panel en donde se insertan los botones aceptar y cancelar
        JPanel buttonPanel = new JPanel();
		buttonPanel.add(aceptar, BorderLayout.CENTER);
		buttonPanel.add(cancelar, BorderLayout.CENTER);
		
        cons.ipadx = 0;
		cons.weightx = 1.0;
		cons.gridx = 1;
		
		bottomPanel.add(buttonPanel, cons);
        
        // Boton ayuda
        JLabel botonAyuda = HelpUtils.fechButton("opciones.configuracion");
		
        cons.ipadx = 15;
		cons.weightx = 0.0;
		cons.gridx = 2;
        
        bottomPanel.add(botonAyuda, cons);
        
        return bottomPanel;
    }
    
//    private Component createMainOptionsPanel() {
//    	
//        GridBagConstraints c = new GridBagConstraints();
//        c.fill = GridBagConstraints.HORIZONTAL;
//        c.weightx = 1.0;
//        c.insets = new Insets(13, 13, 0, 13);
//        c.gridy = 0;
//    	
//        JPanel mainPanel = new JPanel(new GridBagLayout());
//        
//    	// Panel general
//    	JPanel generalPanel = new JPanel(new GridBagLayout());
//    	generalPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.general"))); // NOI18N
//
//    	GridBagConstraints c2 = new GridBagConstraints();
//        c2.fill = GridBagConstraints.HORIZONTAL;
//        c2.insets = new Insets(0, 13, 0, 13);
//        c2.weightx = 1.0;
//        c2.gridy = 0;
//        
//        // Checkbox para habilitar las opciones de configuracion avanzada
//        checkHabilitar = new JCheckBox();
//        checkHabilitar.setText(Messages.getString("Opciones.general.habilitar")); // NOI18N
//        checkHabilitar.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.general.habilitar")); // NOI18N
//        checkHabilitar.setSelected(GeneralConfig.isAvanzados()); 
//        checkHabilitar.setBounds(12, 20, 340, 23);
//        generalPanel.add(checkHabilitar, c2);
//        
//        mainPanel.add(generalPanel, c);
//
//        c.gridy = 1;
//        
//        // Panel criptografía
//        JPanel criptografiaPanel = new JPanel(new GridBagLayout());
//        criptografiaPanel.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.criptografia"))); // NOI18N
//
//        c2.fill = GridBagConstraints.HORIZONTAL;
//        c2.insets = new Insets(4, 13, 0, 13);
//        c2.weightx = 1.0;
//        c2.gridy = 0;
//        
//        // Etiqueta algoritmo de huella digital
//        JLabel etiquetaAlgoritmo = new JLabel(Messages.getString("Opciones.criptografia.algoritmo.parte"));
//        criptografiaPanel.add(etiquetaAlgoritmo, c2);
//        
//        c2.insets = new Insets(5, 13, 0, 13);
//        c2.gridy = 1;
//        
//        // Combo con los algoritmos de huella digital
//        comboAlgoritmo = new JComboBox();
//        comboAlgoritmo.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.criptografia.algoritmo.parte")); // NOI18N
//        comboAlgoritmo.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.criptografia.algoritmo.parte")); // NOI18N
//        comboAlgoritmo.setModel(new DefaultComboBoxModel(Arrays.asList("SHA-1","SHA-512","SHA-384","SHA-256").toArray()));
//        for (int i = 0; i < algoritmoV.size(); i++){
//            if (algoritmoV.get(i).equals(GeneralConfig.getSignConfig().getSignAlgorithm())){
//            	comboAlgoritmo.setSelectedIndex(i);
//            	break;
//            }
//        }
//        criptografiaPanel.add(comboAlgoritmo, c2);
//        
//        c2.insets = new Insets(5, 13, 0, 13);
//        c2.gridy = 2;
//        
//        // Checkbox para utilizar XML
//        checkXML = new JCheckBox();
//        checkXML.setSelected(GeneralConfig.getSignConfig().isUseAlgorithmInternally());
//        checkXML.setText(Messages.getString("Opciones.criptografia.utilizar")); // NOI18N
//        checkXML.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.criptografia.utilizar")); // NOI18N
//        criptografiaPanel.add(checkXML, c2);
//        
//        mainPanel.add(criptografiaPanel, c);
//        
//        c.gridy = 2;
//        
//        // Panel firmas de documentos PDF
//        JPanel panelFirmaPDF = new JPanel(new GridBagLayout());
//        panelFirmaPDF.setBorder(BorderFactory.createTitledBorder(Messages.getString("Opciones.firmas"))); // NOI18N
//        
//        c2.fill = GridBagConstraints.HORIZONTAL;
//        c2.insets = new Insets(4, 13, 0, 13);
//        c2.weightx = 1.0;
//        c2.gridy = 0;
//        
//        // Etiqueta motivo / razon de la firma
//        JLabel etiquetaMotivo = new JLabel();
//        etiquetaMotivo.setText(Messages.getString("Opciones.firmas.motivo")); // NOI18N
//        panelFirmaPDF.add(etiquetaMotivo, c2);
//        
//        c2.insets = new Insets(5, 13, 0, 13);
//        c2.gridy = 1;
//        
//        // Caja de texto para el motivo de la firma
//        campoMotivo = new JTextField();
//        campoMotivo.setText(GeneralConfig.getSignConfig().getSignReason());
//        campoMotivo.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.firmas.motivo")); // NOI18N
//        campoMotivo.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.firmas.motivo")); // NOI18N
//        panelFirmaPDF.add(campoMotivo, c2);
//        
//        c2.insets = new Insets(13, 13, 0, 13);
//        c2.gridy = 2;
//        
//        // Etiqueta lugar donde se realiza la firma
//        JLabel etiquetaLugar = new JLabel();
//        etiquetaLugar.setText(Messages.getString("Opciones.firmas.lugar")); // NOI18N
//        panelFirmaPDF.add(etiquetaLugar, c2);
//        
//        c2.insets = new Insets(5, 13, 0, 13);
//        c2.gridy = 3;
//        
//        // Caja de texto para el lugar donde se realiza la firma
//        campoLugar = new JTextField();
//        campoLugar.setText(GeneralConfig.getSignConfig().getSignatureProductionPlace());
//        campoLugar.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.firmas.lugar")); // NOI18N
//        campoLugar.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.firmas.lugar")); // NOI18N
//        panelFirmaPDF.add(campoLugar, c2);
//        
//        c2.insets = new Insets(13, 13, 0, 13);
//        c2.gridy = 4;
//        
//        // Etiqueta de los datos de contacto
//        JLabel etiquetaDatos = new JLabel();
//        etiquetaDatos.setText(Messages.getString("Opciones.firmas.datos")); // NOI18N
//        panelFirmaPDF.add(etiquetaDatos, c2);
//
//        c2.insets = new Insets(5, 13, 5, 13);
//        c2.gridy = 5;
//        
//        // Caja de texto para los datos de contacto
//        campoDatos = new JTextField();
//        campoDatos.setText(GeneralConfig.getSignConfig().getSignContact());
//        campoDatos.getAccessibleContext().setAccessibleName(Messages.getString("Opciones.firmas.datos")); // NOI18N
//        campoDatos.getAccessibleContext().setAccessibleDescription(Messages.getString("Opciones.firmas.datos")); // NOI18N
//        panelFirmaPDF.add(campoDatos, c2);
//
//        mainPanel.add(panelFirmaPDF, c);
//        
//		// Accesos rapidos al menu de ayuda
//        HelpUtils.enableHelpKey(checkHabilitar, "opciones.general");
//        HelpUtils.enableHelpKey(comboAlgoritmo, "opciones.algoritmo");
//        HelpUtils.enableHelpKey(checkXML, "opciones.referenciasInternas");
//        HelpUtils.enableHelpKey(campoMotivo, "opciones.pdf.motivo");
//        HelpUtils.enableHelpKey(campoLugar, "opciones.pdf.lugar");
//        HelpUtils.enableHelpKey(campoDatos, "opciones.pdf.datos");
//    	
//    	return mainPanel;
//    }
    
	/**
	 * Cierra la ventana y aplica todas las opciones seleccionadas
	 * @param config Configuraci&oacute;n actualmente establecida en la ventana de opciones.
	 */
    private void aceptarActionPerformed(Properties config, Properties signatureConfig) {

    	// Si se ha cambiado de vista (simple <-> avanzada) actualizamos la ventana principal
    	Boolean needUpdateGUI = (GeneralConfig.isAvanzados() != Boolean.parseBoolean(config.getProperty(MainOptionsPane.MAIN_ADVANCED_VIEW)));

    	// Guardamos el estado actual de la configuracion de la herramienta
    	GeneralConfig.loadConfig(config);
    	
    	// Guardamos la configuracion de firma establecida
    	GeneralConfig.loadSignatureConfig(signatureConfig);
    	
    	// Si se ha cambiado la vista de simple a avanzada o viceversa reconstruimos la interfaz
    	if (needUpdateGUI && mainGui != null) {
    		mainGui.crearPaneles();
    	}
    	
    	// Cerramos la pantalla
    	dispose();
    }

    /**
     * Cierra la ventana
     */
    private void cancelarActionPerformed() {
    	dispose();
    }
}
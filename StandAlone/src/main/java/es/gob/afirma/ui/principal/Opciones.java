/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.principal;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Panel;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.Properties;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.WindowConstants;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.utils.Constants;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.JAccessibilityDialog;
import es.gob.afirma.ui.utils.Messages;

/**
 * Clase que muestra el panel de opciones.
 */
public class Opciones extends JAccessibilityDialog {

	private static final long serialVersionUID = 1L;

    /** Pantalla principal de la aplicaci&oacute;n. */
    private PrincipalGUI mainGui;
    
    /** Panel con la configuraci&oacute;n general del aplicativo. */
    private MainOptionsPane mainOptions;
    
    /** Panel con la configurac&oacute;n de las firmas PDF del aplicativo. */
    private ContextOptionsPane contextOptions;
    
    /** Panel con la configurac&oacute;n de las firmas PDF del aplicativo. */
    private AccessibilityOptionsPane accessibilityOptions;
    
    public Opciones(PrincipalGUI mainGUI) {
    	this.mainGui = mainGUI;
        initComponents();
    }
  
    @Override
	public int getMinimumRelation(){
		return 9;
	}
	
    /**
	 * Posición X inicial de la ventana dependiendo de la resolución de pantalla.
	 * @return int Posición X
	 */
    public int getInitialX() {
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize(); //329
		return (screenSize.width - 426) / 2 ;
	}
    
    /**
	 * Posición Y inicial de la ventana dependiendo del sistema operativo y de la
	 * resolución de pantalla.
	 * @return int Posición Y
	 */
	public int getInitialY() {
        Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
        if (Platform.getOS().equals(Platform.OS.MACOSX)){
        	return (screenSize.height - 485) / 2;
        } else {
        	return (screenSize.height - 456) / 2;
        }
	}
	
    /**
     * Inicializacion de componentes
     */
    private void initComponents() {
    	// Dimensiones de la ventana en Windows y Linux
    	if (GeneralConfig.isMaximized()){
    		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
			this.setBounds(0,0,(int)screenSize.getWidth(), (int)screenSize.getHeight()-35);
    	} else {
    		setBounds(this.getInitialX(), this.getInitialY(), Constants.OPTION_INITIAL_WIDTH, Constants.OPTION_INITIAL_HEIGHT);
    	}
    	// Configuracion de la ventana
    	setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setTitle(Messages.getString("Opciones.opciones")); // NOI18N
        setResizable(true);
        getContentPane().setLayout(new GridBagLayout());
        setMinimumSize(new Dimension(getSize().width, getSize().height));
        
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.weightx = 1.0;
        c.weighty = 1.0;
//        c.insets = new Insets(13, 13, 0, 13);
        c.gridy = 0;
                
        // Panel superior con las opciones de configuracion
        JTabbedPane mainPanel = new JTabbedPane();
        
        mainOptions =  new MainOptionsPane();
        mainPanel.addTab(Messages.getString("Opciones.general"),
        		null,
        		mainOptions.getConfigurationPanel(),
        		Messages.getString("Opciones.general"));

        mainOptions.loadConfig(GeneralConfig.getConfig());
        
        contextOptions =  new ContextOptionsPane();
        
        mainPanel.addTab(Messages.getString("Opciones.contextoFirma"),
        		null,
        		contextOptions.getConfigurationPanel(),
        		Messages.getString("Opciones.contexto"));
        
        contextOptions.loadConfig(GeneralConfig.getConfig());
        
        //Opciones de accesibilidad
        accessibilityOptions =  new AccessibilityOptionsPane();
        
        mainPanel.addTab(Messages.getString("Opciones.accesibilidad"),
        		null,
        		accessibilityOptions.getConfigurationPanel(),
        		Messages.getString("Opciones.accesibilidadTip"));
        
        accessibilityOptions.loadConfig(GeneralConfig.getConfig());
        
        // Definicion de mnemï¿½nicos.
        int tabNum = 0;
        mainPanel.setMnemonicAt(tabNum, KeyEvent.VK_G); //atajo para la primera pestaï¿½a
        mainPanel.setMnemonicAt(tabNum+1, KeyEvent.VK_X); //atajo para la segunda pestaï¿½a

        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;
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
        
		//Boton maximizar ventana
		JButton maximizar = new JButton();
		maximizar.setText(Messages.getString("Wizard.maximizar"));
	    maximizar.setName("maximizar");
	    maximizar.setMnemonic(KeyEvent.VK_M);
	    maximizar.addActionListener(new ActionListener() {
	    	public void actionPerformed(ActionEvent e) {
	    		maximizarActionPerformed();
			}
		});
		
	    //Espacio entre botones
		Panel panelVacio = new Panel();
		panelVacio.setPreferredSize(new Dimension(50, 10));
	    
		// Boton aceptar
        JButton aceptar = new JButton();
        aceptar.setText(Messages.getString("PrincipalGUI.aceptar")); // NOI18N
        aceptar.setMnemonic(KeyEvent.VK_A); //Se asigna un atajo al botï¿½n aceptar
        aceptar.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
            	
            	Properties config = new Properties();
            	config.putAll(mainOptions.getConfig());
            	config.putAll(contextOptions.getConfig());
            	config.putAll(accessibilityOptions.getConfig());
            	
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
        cancelar.setMnemonic(KeyEvent.VK_C); //Se asigna un atajo al botï¿½n cancelar
        cancelar.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                cancelarActionPerformed();
            }
        });
        cancelar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.cancelar")); // NOI18N
        
        
        // Panel en donde se insertan los botones maximizar, aceptar y cancelar
        JPanel buttonPanel = new JPanel();
        buttonPanel.add(maximizar, BorderLayout.CENTER);
        buttonPanel.add(panelVacio, BorderLayout.CENTER);
		buttonPanel.add(aceptar, BorderLayout.CENTER);
		buttonPanel.add(cancelar, BorderLayout.CENTER);
		
        cons.ipadx = 0;
		cons.weightx = 1.0;
		cons.gridx = 1;
		
		bottomPanel.add(buttonPanel, cons);
        
        // Boton ayuda
		JButton botonAyuda = HelpUtils.helpButton("opciones.configuracion");
		
        cons.ipadx = 15;
		cons.weightx = 0.02;
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
//        // Panel criptografï¿½a
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

    	// Si se ha cambiado de vista (simple <-> avanzada) o se ha indicado que se desean todas las ventanas maximizadas o se ha indicado que se desean los cursores de texto grandes, actualizamos la ventana principal
    	Boolean needUpdateGUI = ((GeneralConfig.isAvanzados() != Boolean.parseBoolean(config.getProperty(MainOptionsPane.MAIN_ADVANCED_VIEW)))|| (GeneralConfig.isMaximized() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_WINDOWS_SIZE))) || (GeneralConfig.isBigCaret() != Boolean.parseBoolean(config.getProperty(AccessibilityOptionsPane.MAIN_CURSOR_SIZE))));
    	    	
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
    
    /**
	 * Cambia el tamaÃ±o de la ventana al tamaÃ±o mÃ¡ximo de pantalla menos el tamaÃ±o de la barra de tareas de windows
	 */
	public void maximizarActionPerformed(){
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		JAccessibilityDialog j = getJAccessibilityDialog(this);
		j.setBounds(0,0,(int)screenSize.getWidth(), (int)screenSize.getHeight()-35);
		
	}
}
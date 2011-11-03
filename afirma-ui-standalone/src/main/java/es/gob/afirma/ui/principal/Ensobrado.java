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
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.text.Caret;

import es.gob.afirma.ui.listeners.ElementDescriptionFocusListener;
import es.gob.afirma.ui.listeners.ElementDescriptionMouseListener;
import es.gob.afirma.ui.utils.ConfigureCaret;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.RequestFocusListener;
import es.gob.afirma.ui.utils.SelectionDialog;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardsobres.AsistenteEnsobrar;
import es.gob.afirma.ui.wizardsobresremitentes.AsistenteRemitentes;


public class Ensobrado extends JPanel {
	
	private static final long serialVersionUID = 1L;

    public Ensobrado() {
        initComponents();
    }

    // Checkbox con texto "Anadir nuevos remitentes al sobre
    private JCheckBox checkAnadir = new JCheckBox();
    // Combo de tipos de sobre digital
    private JComboBox comboTipos = new JComboBox();
    
    /**
     * Inicializacion de los componentes
     */
    private void initComponents() {
    	setLayout(new GridBagLayout());
		
		GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
		c.insets = new Insets(13, 13, 0, 13);
		c.weightx = 1.0;
		c.gridwidth = 2;
		c.gridx = 0;

        // Etiqueta fichero a ensobrar digitalmente
        JLabel etiquetaFichero = new JLabel();
        etiquetaFichero.setText(Messages.getString("Ensobrado.buscar")); // NOI18N
        Utils.setContrastColor(etiquetaFichero);
        Utils.setFontBold(etiquetaFichero);
		add(etiquetaFichero, c);
		
		c.insets = new Insets(0, 13, 0, 0);
		c.gridwidth = 1;
		c.gridy	= 1;

        // Campo donde se guarda el nombre del archivo a ensobrar
        final JTextField campoFichero = new JTextField();
        campoFichero.setToolTipText(Messages.getString("Ensobrado.buscar.caja.descripcion")); // NOI18N
        campoFichero.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Ensobrado.buscar.caja.descripcion")));
		campoFichero.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Ensobrado.buscar.caja.descripcion")));
		campoFichero.getAccessibleContext().setAccessibleName(etiquetaFichero.getText()+" ALT + O."); // NOI18N
        campoFichero.getAccessibleContext().setAccessibleDescription(Messages.getString("Ensobrado.buscar.caja.descripcion")); // NOI18N
        campoFichero.addAncestorListener(new RequestFocusListener(false));
        if (GeneralConfig.isBigCaret()) {
			Caret caret = new ConfigureCaret();
			campoFichero.setCaret(caret);
		}
        Utils.remarcar(campoFichero);
        Utils.setFontBold(campoFichero);
		add(campoFichero, c);
		
		//Relación entre etiqueta y campo de texto
		etiquetaFichero.setLabelFor(campoFichero);
		//Asignación de mnemónico
		etiquetaFichero.setDisplayedMnemonic(KeyEvent.VK_O);
		
		c.insets = new Insets(0, 10, 0, 13);
		c.weightx = 0.0;
		c.gridx = 1;
        
		JPanel panelExaminar = new JPanel(new GridLayout(1, 1));
        // Boton examinar
        JButton  examinar = new JButton();   
        examinar.setMnemonic(KeyEvent.VK_E);
        examinar.setText(Messages.getString("PrincipalGUI.Examinar")); // NOI18N
        examinar.setToolTipText(Messages.getString("PrincipalGUI.Examinar.description")); // NOI18N
        examinar.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("PrincipalGUI.Examinar.description.status")));
        examinar.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("PrincipalGUI.Examinar.description.status")));
        examinar.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                examinarActionPerformed(campoFichero);
            }
        });
        examinar.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.Examinar") + " " + Messages.getString("PrincipalGUI.Examinar.description.status"));
		examinar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.Examinar.description")); // NOI18N
        Utils.remarcar(examinar);
        Utils.setContrastColor(examinar);
        Utils.setFontBold(examinar);
        
        panelExaminar.add(examinar);
		add(panelExaminar, c);
		
		c.insets = new Insets(5, 13, 0, 13);
		c.weightx = 1.0;
		c.gridwidth = 2;
		c.gridx = 0;
		c.gridy	= 2;
        
        // Checkbox con texto "Anadir nuevos remitentes al sobre
		checkAnadir.setVisible(false); // Ocultada hasta comprobar la funcionalidad
        checkAnadir.setMnemonic(KeyEvent.VK_G);
        checkAnadir.setText(Messages.getString("Ensobrado.check")); // NOI18N
        checkAnadir.setToolTipText(Messages.getString("Ensobrado.check.description")); // NOI18N
        checkAnadir.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Ensobrado.check.description.status")));
        checkAnadir.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Ensobrado.check.description.status")));
        checkAnadir.getAccessibleContext().setAccessibleName(Messages.getString("Ensobrado.check") + " " + Messages.getString("Ensobrado.check.description.status")); // NOI18N
        checkAnadir.getAccessibleContext().setAccessibleDescription(Messages.getString("Ensobrado.check.description")); // NOI18N
        checkAnadir.addChangeListener(new ChangeListener() {	
			public void stateChanged(ChangeEvent arg0) {
				if (checkAnadir.isSelected())
					comboTipos.setEnabled(false);
				else
					comboTipos.setEnabled(true);
			}
		});
        Utils.remarcar(checkAnadir);
        Utils.setContrastColor(checkAnadir);
        Utils.setFontBold(checkAnadir);
        add(checkAnadir, c);

        //Espacio en blanco
        JPanel emptyPanel01 = new JPanel();
        emptyPanel01.setPreferredSize(new Dimension(1, 1));
        c.weightx = 1.0;
        c.weighty = 0.2;
        c.gridwidth = 3;
        c.gridx = 0;
        c.gridy = 3;
        c.insets = new Insets(0, 0, 0, 0);
        add(emptyPanel01, c);
        
        c.insets = new Insets(13, 13, 0, 13);
		c.gridy	= 4;
		c.weighty = 0.0;
        
        // Etiqueta almacen o repositorio
        JLabel etiquetaOpciones = new JLabel();
        etiquetaOpciones.setText(Messages.getString("Ensobrado.opciones.combo")); // NOI18N
        Utils.setContrastColor(etiquetaOpciones);
        Utils.setFontBold(etiquetaOpciones);
        add(etiquetaOpciones, c);

		c.insets = new Insets(0, 13, 0, 13);
		c.gridy = 5;
		c.weighty = 0.1;
		c.fill = GridBagConstraints.BOTH;
        
        // Combo con el almacen o repositorio de certificados
        comboTipos.setToolTipText(Messages.getString("Ensobrado.opciones.combo")); // NOI18N
        comboTipos.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Ensobrado.opciones.combo.status")));
        comboTipos.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Ensobrado.opciones.combo.status")));
        comboTipos.getAccessibleContext().setAccessibleName(etiquetaOpciones.getText() + " " + Messages.getString("Ensobrado.opciones.combo.status") +" ALT + T."); // NOI18N
        comboTipos.getAccessibleContext().setAccessibleDescription(Messages.getString("Ensobrado.opciones.combo.description")); // NOI18N
        cargarComboTipos();
        Utils.remarcar(comboTipos);
        Utils.setContrastColor(comboTipos);
        Utils.setFontBold(comboTipos);
        add(comboTipos, c);
        
      //Relación entre etiqueta y combo
        etiquetaOpciones.setLabelFor(comboTipos);
		//Asignación de mnemónico
        etiquetaOpciones.setDisplayedMnemonic(KeyEvent.VK_T);
        
        c.weighty = 1.0;
		c.gridy = 6;
		c.gridheight = 4;
		c.fill = GridBagConstraints.HORIZONTAL;
        
		// Panel vacio para alinear el boton de aceptar en la parte de abajo de la pantalla
		JPanel emptyPanel = new JPanel();
		add(emptyPanel, c);
		
		// Panel con los botones
		JPanel panelBotones = new JPanel(new GridBagLayout());
		
		GridBagConstraints cons = new GridBagConstraints();
		cons.anchor = GridBagConstraints.FIRST_LINE_START; //control de la orientación de componentes al redimensionar
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.ipadx = 15;
		cons.gridx = 0;
		
		// Etiqueta para rellenar a la izquierda
		JLabel label = new JLabel();
		panelBotones.add(label, cons);
        		
		JPanel panelGenerar = new JPanel(new GridLayout(1, 1));
        // Boton generar
        JButton generar = new JButton();
        generar.setMnemonic(KeyEvent.VK_G);
        generar.setText(Messages.getString("Ensobrado.btnGenerar")); // NOI18N
        generar.setToolTipText(Messages.getString("Ensobrado.btnGenerar.description")); // NOI18N
        generar.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Ensobrado.btnGenerar.description.status")));
        generar.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Ensobrado.btnGenerar.description.status")));
        generar.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
            	generarActionPerformed(campoFichero);
            	PrincipalGUI.setNuevoEstado(Messages.getString("Ensobrado.btnGenerar.generado"));
            }
        });
        generar.getAccessibleContext().setAccessibleName(generar.getText() + " " + Messages.getString("Ensobrado.btnGenerar.description.status")); // NOI18N
        generar.getAccessibleContext().setAccessibleDescription(Messages.getString("Ensobrado.btnGenerar.description")); // NOI18N
        Utils.remarcar(generar);
        Utils.setContrastColor(generar);
        Utils.setFontBold(generar);
        
        panelGenerar.add(generar);
		JPanel buttonPanel = new JPanel();
		buttonPanel.add(panelGenerar, BorderLayout.CENTER);
		
		cons.ipadx = 0;
		cons.gridx = 1;
		cons.weightx = 1.0;
        
		panelBotones.add(buttonPanel, cons);

		JPanel panelAyuda = new JPanel();
        // Boton ayuda
		JButton botonAyuda = HelpUtils.helpButton("ensobrado");
		botonAyuda.setName("helpButton");
        
        cons.ipadx = 15;
		cons.weightx = 0.0;
		cons.gridx = 2;
		
		panelAyuda.add(botonAyuda);
		panelBotones.add(panelAyuda, cons);

		c.gridwidth	= 2;
        c.insets = new Insets(13,13,13,13);
        c.weightx = 1.0;
        c.weighty = 0.0;
        c.gridy = 10;
		
		add(panelBotones, c);
        
        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(campoFichero, "ensobrado.fichero");
        HelpUtils.enableHelpKey(examinar, "ensobrado.fichero");
        HelpUtils.enableHelpKey(checkAnadir,"ensobrado.anadirremitentes");
        HelpUtils.enableHelpKey(comboTipos,"ensobrado.firmarsobre");
    }

	/**
     * Carga el combo de opciones con las diferentes opciones
     */
    private void cargarComboTipos() {
    	List<String> opciones = new ArrayList<String>();
    	opciones.add(Messages.getString("Ensobrado.combo.autenticado"));
    	opciones.add(Messages.getString("Ensobrado.combo.firmado"));
    	opciones.add(Messages.getString("Ensobrado.combo.simple"));
    	
    	comboTipos.setModel(new DefaultComboBoxModel(opciones.toArray()));
	}

	/**
	 * Pulsar boton examinar: Muestra una ventana para seleccinar un archivo.
	 * Modifica el valor de la caja con el nombre del archivo seleccionado
	 * @param campoFichero	Campo en el que se escribe el nombre del fichero seleccionado
	 */
    private void examinarActionPerformed(JTextField campoFichero) {  
    	File selectedFile = SelectionDialog.showFileOpenDialog(this, Messages.getString("Seleccione.fichero.ensobrar"));
    	if (selectedFile != null) {
    		campoFichero.setText(selectedFile.getAbsolutePath());
    	}
    }

    /**
     * Ensobra el archivo seleccionado con las opciones indicadas
     * @param campoFichero	Campo con el nombre del fichero a ensobrar
     */
    private void generarActionPerformed (JTextField campoFichero) {
    	if(campoFichero.getText() == null || campoFichero.getText().equals("")) {
    		CustomDialog.showMessageDialog(this, true, Messages.getString("Ensobrado.msg.error.fichero"), Messages.getString("Ensobrado.msg.titulo"), JOptionPane.WARNING_MESSAGE);
    		campoFichero.requestFocusInWindow(); //Foco al campo que contiene el path al fichero
    	}
        else {
        	if (checkAnadir.isSelected()) 
        		// Se muestra el asistente de anadir nuevos remitentes
        		new AsistenteRemitentes(campoFichero.getText());
        	else 
	        	 // Se muestra el asistente
	            new AsistenteEnsobrar(campoFichero.getText(), comboTipos.getSelectedIndex());                       
        }
    }
}

        	
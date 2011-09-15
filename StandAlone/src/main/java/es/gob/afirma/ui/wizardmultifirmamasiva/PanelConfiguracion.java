/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardmultifirmamasiva;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Panel;
import java.awt.event.KeyEvent;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextPane;

import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.CabeceraAsistente;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;
import es.gob.afirma.ui.wizardUtils.PanelesTexto;

class PanelConfiguracion extends JAccessibilityDialogWizard {
	
	private static final long serialVersionUID = 1L;
	
	@Override
	public int getMinimumRelation(){
		return 9;
	}
	
	/**
	 * Muestra/oculta el panel de los radio buttons y su explicacion
	 * @param visible
	 */
	void setMostrar(Boolean visible) {
		panelTextoRealizar.setVisible(visible);
		panelRadios.setVisible(visible);
	}
	
	/**
	 * Guarda todas las ventanas del asistente para poder controlar la botonera
	 * @param ventanas	Listado con todas las paginas del asistente
	 */
	void setVentanas(List<JDialogWizard> ventanas) {
		Botonera botonera = new Botonera(ventanas, 2);
		getContentPane().add(botonera, BorderLayout.PAGE_END);
	}
	
    PanelConfiguracion() {
        initComponents();      
    }

    // Panel con el texto "Para realizar el proceso..."
    private JTextPane panelTextoRealizar = PanelesTexto.generarPanelTexto("Wizard.multifirma.ventana2.explicacion2", false);
    // Checkbox con texto "Respetar el formato..."
	private JCheckBox checkRespectar = new JCheckBox();
	// Panel que engloba los radiobuttons
	private JPanel panelRadios = new JPanel();
    // Radio buton "Contrafirmar unicamente"
	private JRadioButton radioUltimos = new JRadioButton();
    
    
    /**
     * Inicializacion de componentes
     */
    private void initComponents() {
    	// Titulo de la ventana
    	setTitulo(Messages.getString("Wizard.multifirma.titulo.ventana"));
    	
    	// Panel con la cabecera
        CabeceraAsistente panelSuperior = new CabeceraAsistente("Wizard.multifirma.pagina2.titulo", "Wizard.multifirma.pagina2.titulo.explicacion", null, true);
        getContentPane().add(panelSuperior, BorderLayout.NORTH);
		
        // Panel central
    	JPanel panelCentral = new JPanel();
    	panelCentral.setMinimumSize(new Dimension(603, 289));
    	panelCentral.setLayout(new GridBagLayout());
        
    	// Configuramos el layout
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(20, 20, 0, 20);
		c.weightx = 1.0;
		c.gridy = 0;
    	
    	// Panel con el texto "Respetar el formato..."
		panelCentral.add(PanelesTexto.generarPanelTexto(
				"Wizard.multifirma.ventana2.explicacion1", false), c);
		
		c.gridy = 1;
		
    	// Checkbox con texto "Respetar el formato..."
    	checkRespectar.setText(Messages.getString("Wizard.multifirma.ventana2.check.respetar"));
        checkRespectar.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.multifirma.ventana2.check.respetar")); 
        checkRespectar.getAccessibleContext().setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana2.check.respetar.description"));
        checkRespectar.setMnemonic(KeyEvent.VK_R); //Se asigna un atajo al checkbox
        if (GeneralConfig.isRemarked()){
        	Utils.remarcar(checkRespectar);
        }
        panelCentral.add(checkRespectar, c);
        
        c.gridy = 2;
        
    	// Panel con el texto "Para realizar el proceso..."
    	panelCentral.add(panelTextoRealizar, c);
    	
    	c.insets = new Insets(20, 20, 20, 20);
		c.gridy = 3;
    	
    	// Panel que engloba los radiobuttons
    	panelRadios.setBorder(BorderFactory.createTitledBorder(Messages.getString("Wizard.multifirma.ventana2.panel"))); 
    	panelRadios.setLayout(new GridBagLayout());
    	
    	GridBagConstraints cons = new GridBagConstraints();
    	cons.fill = GridBagConstraints.HORIZONTAL;
    	cons.weightx = 1.0;
    	
    	// Radio button "Contrafirmar a todos"
    	JRadioButton radioTodos = new JRadioButton();
    	radioTodos.setText(Messages.getString("Wizard.multifirma.ventana2.panel.radio1")); 
        radioTodos.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.multifirma.ventana2.panel.radio1")); 
        radioTodos.getAccessibleContext().setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana2.panel.radio1.description"));
        radioTodos.setMnemonic(KeyEvent.VK_O); //Se asigna un atajo al botón de radio
        radioTodos.setSelected(true);
        if (GeneralConfig.isRemarked()){
        	Utils.remarcar(radioTodos);
        }
        panelRadios.add(radioTodos, cons);
        
        cons.gridy = 1;
        
        // Radio buton "Contrafirmar unicamente"
        radioUltimos.setText(Messages.getString("Wizard.multifirma.ventana2.panel.radio2")); 
        radioUltimos.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.multifirma.ventana2.panel.radio2")); 
        radioUltimos.getAccessibleContext().setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana2.panel.radio2.description"));
        radioUltimos.setMnemonic(KeyEvent.VK_N); //Se asigna un atajo al botón de radio
        if (GeneralConfig.isRemarked()){
        	Utils.remarcar(radioUltimos);
        }
        panelRadios.add(radioUltimos, cons);
        
        // Grupo de radiobuttons
        ButtonGroup grupoRadios = new ButtonGroup();
        grupoRadios.add(radioTodos);
        grupoRadios.add(radioUltimos);
        
        panelCentral.add(panelRadios, c);
        
		c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(20, 20, 0, 20);
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.gridx = 0;
		c.gridy = 4;
		
		// Panel introducido para poder mantener la linea superior correcta
		Panel panelVacio = new Panel();
		panelCentral.add(panelVacio, c);
        
        getContentPane().add(panelCentral, BorderLayout.CENTER);
        
        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(checkRespectar,"multifirma.masiva.wizard.contrafirma.respetar");
        HelpUtils.enableHelpKey(radioTodos,"multifirma.masiva.wizard.contrafirma.opciones");
        HelpUtils.enableHelpKey(radioUltimos,"multifirma.masiva.wizard.contrafirma.opciones");    
    }
    
	/**
	 * Botonera con funciones para la pagina panel de multifirma - cofirma
	 */
	private class Botonera extends BotoneraInferior {

		private static final long serialVersionUID = 1L;

		public Botonera(List<JDialogWizard> ventanas, Integer posicion) {
			super(ventanas, posicion);
		}

		@Override
		protected void siguienteActionPerformed(JButton anterior,
				JButton siguiente, JButton finalizar) {

			// Cargamos el tipo de contrafirma
			((PanelMultifirmaMasiva) getVentanas().get(4)).setTipoContrafirma(radioUltimos.isSelected());
			
			// Cargamos el valor del check respetar
			((PanelMultifirmaMasiva) getVentanas().get(4)).setRespetar(checkRespectar.isSelected());
			
			super.siguienteActionPerformed(anterior, siguiente, finalizar);
		}
	}
}

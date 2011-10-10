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
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.InfoLabel;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.CabeceraAsistente;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;

class PanelFormatos extends JAccessibilityDialogWizard implements ItemListener {

	private static final long serialVersionUID = 1L;

	static Logger logger = Logger.getLogger(PanelFormatos.class.getName());
	
	// Listado con las constantes de los formatos del combo
	private List<String> formatosV = new ArrayList<String>(Arrays.asList(
			AOSignConstants.SIGN_FORMAT_XADES_DETACHED,
			AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING,
			AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED,
			AOSignConstants.SIGN_FORMAT_CADES,
			AOSignConstants.SIGN_FORMAT_PDF,
			AOSignConstants.SIGN_FORMAT_ODF,
			AOSignConstants.SIGN_FORMAT_OOXML));
	
	@Override
	public int getMinimumRelation(){
		return 8;
	}
	
	/**
	 * Guarda todas las ventanas del asistente para poder controlar la botonera
	 * @param ventanas	Listado con todas las paginas del asistente
	 */
	void setVentanas(List<JDialogWizard> ventanas) {
		Botonera botonera = new Botonera(ventanas, 1);
		getContentPane().add(botonera, BorderLayout.PAGE_END);
	}
	
	PanelFormatos() {
		initComponents();
	}

	//Etiqueta para los formatos
	private JLabel etiquetaFormato = new JLabel();
	// Combo con los formatos
	private JComboBox comboFormatos = new JComboBox();
	// Radio button firma
	private JRadioButton radioFirma = new JRadioButton();
	// Radio button cofirma
	private JRadioButton radioCofirma = new JRadioButton();
	// Radio button contrafirma
	private JRadioButton radioContrafirma = new JRadioButton();
	// Checkbox con el modo del formato
	private JCheckBox checkFormato = new JCheckBox();
	
	/**
	 * Inicializacion de componentes
	 */
	private void initComponents() {
		// Titulo de la ventana
    	setTitulo(Messages.getString("Wizard.multifirma.titulo.ventana"));
		
		// Panel con la cabecera
        CabeceraAsistente panelSuperior = new CabeceraAsistente("Wizard.firma.pagina1.titulo", "Wizard.firma.pafina1.titulo.explicacion", null, true);
        Utils.setContrastColor(panelSuperior);
        Utils.setFontBold(panelSuperior);
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
    	
		//Etiqueta "La operacion seleccionada..."
        InfoLabel operationLabel = new InfoLabel(Messages.getString("Wizard.multifirma.ventana1.explicacion"), false);
        panelCentral.add(operationLabel, c);
        
    	// Etiqueta "La operacion seleccionada..."
		/*panelCentral.add(PanelesTexto.generarPanelTexto(
				"Wizard.multifirma.ventana1.explicacion", false), c);*/

        c.insets = new Insets(20, 20, 0, 20);
		c.weightx = 1.0;
		c.gridy = 1;
		
        // Panel que engloba los radio buttons
        JPanel panelOperaciones = new JPanel();
        panelOperaciones.setBorder(BorderFactory.createTitledBorder(Messages.getString("Wizard.multifirma.ventana1.panel.operacion")));
        Utils.setContrastColor(panelOperaciones);
        Utils.setFontBold(panelOperaciones);
		panelOperaciones.setLayout(new GridBagLayout());
		
		GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.insets = new Insets(0, 0, 0, 0);
		cons.weightx = 1.0;
		
		JPanel panelRadioFirma = new JPanel(new GridLayout(1, 1));
	    panelRadioFirma.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.multifirma.ventana1.panel.operacion"));
		// Radio button firma
		radioFirma.setText(Messages.getString("Wizard.multifirma.ventana1.radio.firma"));
		radioFirma.getAccessibleContext().setAccessibleName(radioFirma.getText() + " " + Messages.getString("Wizard.multifirma.ventana1.radio.firma.description"));
		radioFirma.getAccessibleContext().setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana1.radio.firma.description"));
		radioFirma.addItemListener(this);
		radioFirma.setName("radioFirma");
		radioFirma.setSelected(true);
		radioFirma.setMnemonic(KeyEvent.VK_F); //Se asigna un atajo al botón de radio
		Utils.remarcar(radioFirma);
        Utils.setContrastColor(radioFirma);
		Utils.setFontBold(radioFirma);
		panelRadioFirma.add(radioFirma);
		panelOperaciones.add(panelRadioFirma, cons);
		
		cons.insets = new Insets(0, 0, 0, 0);
		cons.weightx = 1.0;
		cons.gridy = 1;
		
		JPanel panelRadioCofirma = new JPanel(new GridLayout(1, 1));
		// Radio button cofirma
		radioCofirma.setText(Messages.getString("Wizard.multifirma.ventana1.radio.cofirma"));
		radioCofirma.getAccessibleContext().setAccessibleName(radioCofirma.getText() + " " + Messages.getString("Wizard.multifirma.ventana1.radio.cofirma.description"));
		radioCofirma.getAccessibleContext().setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana1.radio.cofirma.description"));
		radioCofirma.addItemListener(this);
		radioCofirma.setName("radioCofirma");
		radioCofirma.setMnemonic(KeyEvent.VK_O); //Se asigna un atajo al botón de radio
		Utils.remarcar(radioCofirma);
        Utils.setContrastColor(radioCofirma);
		Utils.setFontBold(radioCofirma);
		panelRadioCofirma.add(radioCofirma);
		panelOperaciones.add(panelRadioCofirma, cons);
		
		cons.insets = new Insets(0, 0, 0, 0);
		cons.weightx = 1.0;
		cons.gridy = 2;
		
		JPanel panelRadioContrafirma = new JPanel(new GridLayout(1, 1));
		// Radio button contrafirma
		radioContrafirma.setText(Messages.getString("Wizard.multifirma.ventana1.radio.contrafirma"));
		radioContrafirma.getAccessibleContext().setAccessibleName(radioContrafirma.getText() + " " + Messages.getString("Wizard.multifirma.ventana1.radio.contrafirma.description"));
		radioContrafirma.getAccessibleContext().setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana1.radio.contrafirma.description"));
		radioContrafirma.addItemListener(this);
		radioContrafirma.setName("radioContrafirma");
		radioContrafirma.setMnemonic(KeyEvent.VK_N); //Se asigna un atajo al botón de radio
		Utils.remarcar(radioContrafirma);
        Utils.setContrastColor(radioContrafirma);
		Utils.setFontBold(radioContrafirma);
		panelRadioContrafirma.add(radioContrafirma);
		panelOperaciones.add(panelRadioContrafirma, cons);
		
		// Agrupamos los radiobutton para que solo se pueda marcar uno
		ButtonGroup radioGrupo = new ButtonGroup();
		radioGrupo.add(radioFirma);
		radioGrupo.add(radioCofirma);
		radioGrupo.add(radioContrafirma);
		
		panelCentral.add(panelOperaciones, c);
		
        c.insets = new Insets(20, 20, 0, 20);
		c.weightx = 1.0;
		c.gridy = 2;
		
		// Etiqueta con el texto formato
		etiquetaFormato.setText(Messages.getString("Firma.formato"));
		Utils.setContrastColor(etiquetaFormato);
		Utils.setFontBold(etiquetaFormato);
		panelCentral.add(etiquetaFormato, c);
		
		c.insets = new Insets(0, 20, 0, 20);
		c.weightx = 1.0;
		c.gridy = 3;
		c.weighty = 0.1;
		c.fill = GridBagConstraints.BOTH;
		
		// Combo con los formatos
		comboFormatos.setToolTipText(Messages.getString("Firma.formato.description"));
		comboFormatos.getAccessibleContext().setAccessibleName(etiquetaFormato.getText() + " " + comboFormatos.getToolTipText() + "ALT + R.");
		comboFormatos.getAccessibleContext().setAccessibleDescription(comboFormatos.getToolTipText());
		comboFormatos.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				checkFormatoModo();
			}
		});
		cargarCombo();
		Utils.remarcar(comboFormatos);
        Utils.setContrastColor(comboFormatos);
		Utils.setFontBold(comboFormatos);
		panelCentral.add(comboFormatos, c);
		
		//Relación entre etiqueta y combo
		etiquetaFormato.setLabelFor(comboFormatos);
		//Asignación de mnemónico
		etiquetaFormato.setDisplayedMnemonic(KeyEvent.VK_R);
		
		c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(10, 20, 15, 20);
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.gridy = 4;
		
		JPanel panelCheckFormato = new JPanel(new GridLayout(1, 1));
		// Checkbox con el modo de CADES
		checkFormato.setSelected(true);
		checkFormato.setText(Messages.getString("Firma.modo.formato")); // NOI18N
		checkFormato.setToolTipText(Messages.getString("Firma.modo.formato.description")); // NOI18N
		checkFormato.getAccessibleContext().setAccessibleName(checkFormato.getText() + " " + checkFormato.getToolTipText()); // NOI18N
		checkFormato.getAccessibleContext().setAccessibleDescription(checkFormato.getToolTipText()); // NOI18N
		checkFormato.setMnemonic(0); //Se quita el atajo al deshabilitar el checkbox
		checkFormato.setEnabled(false);
		Utils.remarcar(checkFormato);
        Utils.setContrastColor(checkFormato);
		Utils.setFontBold(checkFormato);
		panelCheckFormato.add(checkFormato);
		panelCentral.add(panelCheckFormato, c);
		
		getContentPane().add(panelCentral, BorderLayout.CENTER);
		
		// Accesos rapidos al menu de ayuda
		HelpUtils.enableHelpKey(radioFirma, "multifirma.masiva.wizard.operacion");
		HelpUtils.enableHelpKey(radioContrafirma, "multifirma.masiva.wizard.operacion");
		HelpUtils.enableHelpKey(radioCofirma, "multifirma.masiva.wizard.operacion");
		HelpUtils.enableHelpKey(comboFormatos, "multifirma.masiva.wizard.formato");		
		HelpUtils.enableHelpKey(checkFormato, "multifirma.masiva.wizard.formato.modo");	
	}

	/**
	 * Activa y desactiva el checkbox del modo del formato dependiendo de la opcion
	 * elegida
	 */
	private void checkFormatoModo() {
		if (formatosV.get(comboFormatos.getSelectedIndex()).equals(AOSignConstants.SIGN_FORMAT_CADES)) {
			if (radioFirma.isSelected())
				checkFormato.setEnabled(true); //Se habilita el checkbox
				checkFormato.setMnemonic(KeyEvent.VK_I); //Se asigna un atajo al checkbox
		}
		else {
			checkFormato.setEnabled(false);
			checkFormato.setMnemonic(0); //Se quita el atajo al deshabilitar el checkbox
		}	
	}

	/**
	 * Carga el combo con los diferentes formatos
	 */
	private void cargarCombo() {
		comboFormatos.setModel(new DefaultComboBoxModel(new String[] {
				"Firma est\u00E1ndar (XAdES Detached)",
				"XAdES Enveloping",
				"XAdES Enveloped",
				"CAdES",
				"PAdES",
				"ODF (Open Document Format)",
				"OOXML (Office Open XML)"
		}));
	}
	
	/**
	 * Activa y desactiva el combo en funcion de la operacion
	 */
	public void itemStateChanged(ItemEvent e) {
		JRadioButton radio = (JRadioButton) e.getSource();
		// El listado de seleccion de formato solo se mantiene activo para la firma y la cofirma
		if (radio.getName().equals("radioFirma")) {
			comboFormatos.setEnabled(true);
			etiquetaFormato.setDisplayedMnemonic(KeyEvent.VK_R); //Se asigna un atajo a la etiqueta
			if (comboFormatos.getItemCount() != 0 && formatosV.get(comboFormatos.getSelectedIndex()).equals(AOSignConstants.SIGN_FORMAT_CADES)) {
				checkFormato.setSelected(true);
				checkFormato.setMnemonic(KeyEvent.VK_I); //Se asigna un atajo al checkbox
				checkFormato.setEnabled(true);
			}
		}
		else if (radio.getName().equals("radioCofirma")) {
			comboFormatos.setEnabled(true);
			etiquetaFormato.setDisplayedMnemonic(KeyEvent.VK_R); //Se asigna un atajo a la etiqueta
			checkFormato.setSelected(true);
			checkFormato.setMnemonic(0); //Se quita el atajo al deshabilitar el checkbox
			checkFormato.setEnabled(false);
		}
		else if (radio.getName().equals("radioContrafirma")) {
			comboFormatos.setEnabled(false);
			etiquetaFormato.setDisplayedMnemonic(0); //Se quita el atajo al deshabilitar el combo
			checkFormato.setSelected(true);
			checkFormato.setMnemonic(0); //Se quita el atajo al deshabilitar el checkbox
			checkFormato.setEnabled(false);
		}
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
			
			// Carga el formato de firmado
			String formato = formatosV.get(comboFormatos.getSelectedIndex());
			((PanelMultifirmaMasiva) getVentanas().get(4)).setAlgoritmo(formato);
			
			if (radioFirma.isSelected()) {
				// Carga Firma
				((PanelMultifirmaMasiva) getVentanas().get(4)).setTipo(0);
				
				// Modo de la firma (solo si es CADES)
				((PanelMultifirmaMasiva) getVentanas().get(4)).setModoFormato(checkFormato.isSelected());
				
				// Registramos en la pagina 3 que hemos saltado la 2
				((PanelEntrada) getVentanas().get(3)).setSalto(true);
				
				Integer indice = 3;
				
				//mantenemos el tamaÃ±o y posiciÃ³n de la ventana acutual en la ventana siguiente
				getVentanas().get(indice).setBounds(getVentanas().get(1).getX(), getVentanas().get(1).getY(), getVentanas().get(1).getWidth(), getVentanas().get(1).getHeight());
				
				// Nos saltamos la pagina 2
				getVentanas().get(indice).setVisibleAndHide(true, getVentanas().get(1));
			} else if (radioCofirma.isSelected()) {
				// Carga Cofirma
				((PanelMultifirmaMasiva) getVentanas().get(4)).setTipo(1);
				
				((PanelConfiguracion) getVentanas().get(2)).setMostrar(false);
				((PanelEntrada) getVentanas().get(3)).setSalto(false);
				super.siguienteActionPerformed(anterior, siguiente, finalizar);
			} else {
				// Carga Contrafirma
				((PanelMultifirmaMasiva) getVentanas().get(4)).setTipo(2);
				
				((PanelConfiguracion) getVentanas().get(2)).setMostrar(true);
				((PanelEntrada) getVentanas().get(3)).setSalto(false);
				super.siguienteActionPerformed(anterior, siguiente, finalizar);
			}
		}
	}
}

/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */
package es.gob.afirma.ui.wizardmultifirmamasiva;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
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

import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.CabeceraAsistente;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;
import es.gob.afirma.ui.wizardUtils.PanelesTexto;

class PanelFormatos extends JDialogWizard implements ItemListener {

	private static final long serialVersionUID = 1L;

	static Logger logger = Logger.getLogger(PanelFormatos.class.getName());
	
	// Listado con las constantes de los formatos del combo
	private List<String> formatosV = new ArrayList<String>(Arrays.asList(
			AOConstants.SIGN_FORMAT_XADES_DETACHED,
			AOConstants.SIGN_FORMAT_XADES_ENVELOPING,
			AOConstants.SIGN_FORMAT_XADES_ENVELOPED,
			AOConstants.SIGN_FORMAT_CADES,
			AOConstants.SIGN_FORMAT_PDF,
			AOConstants.SIGN_FORMAT_ODF,
			AOConstants.SIGN_FORMAT_OOXML));
	
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
    	
    	// Etiqueta "La operacion seleccionada..."
		panelCentral.add(PanelesTexto.generarPanelTexto(
				"Wizard.multifirma.ventana1.explicacion", false), c);

        c.insets = new Insets(20, 20, 0, 20);
		c.weightx = 1.0;
		c.gridy = 1;
		
        // Panel que engloba los radio buttons
        JPanel panelOperaciones = new JPanel();
        panelOperaciones.setBorder(BorderFactory.createTitledBorder(Messages.getString("Wizard.multifirma.ventana1.panel.operacion")));
		panelOperaciones.setLayout(new GridBagLayout());
		
		GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.insets = new Insets(0, 0, 0, 0);
		cons.weightx = 1.0;
		
		// Radio button firma
		radioFirma.setText(Messages.getString("Wizard.multifirma.ventana1.radio.firma"));
		radioFirma.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.multifirma.ventana1.radio.firma"));
		radioFirma.getAccessibleContext().setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana1.radio.firma.description"));
		radioFirma.addItemListener(this);
		radioFirma.setName("radioFirma");
		radioFirma.setSelected(true);
		panelOperaciones.add(radioFirma, cons);
		
		cons.insets = new Insets(0, 0, 0, 0);
		cons.weightx = 1.0;
		cons.gridy = 1;
		
		// Radio button cofirma
		radioCofirma.setText(Messages.getString("Wizard.multifirma.ventana1.radio.cofirma"));
		radioCofirma.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.multifirma.ventana1.radio.cofirma"));
		radioCofirma.getAccessibleContext().setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana1.radio.cofirma.description"));
		radioCofirma.addItemListener(this);
		radioCofirma.setName("radioCofirma");
		panelOperaciones.add(radioCofirma, cons);
		
		cons.insets = new Insets(0, 0, 0, 0);
		cons.weightx = 1.0;
		cons.gridy = 2;
		
		// Radio button contrafirma
		radioContrafirma.setText(Messages.getString("Wizard.multifirma.ventana1.radio.contrafirma"));
		radioContrafirma.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.multifirma.ventana1.radio.contrafirma"));
		radioContrafirma.getAccessibleContext().setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana1.radio.contrafirma.description"));
		radioContrafirma.addItemListener(this);
		radioContrafirma.setName("radioContrafirma");
		panelOperaciones.add(radioContrafirma, cons);
		
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
		JLabel etiquetaFormato = new JLabel(Messages.getString("Firma.formato"));
		panelCentral.add(etiquetaFormato, c);
		
		c.insets = new Insets(0, 20, 0, 20);
		c.weightx = 1.0;
		c.gridy = 3;
		
		// Combo con los formatos
		comboFormatos.setToolTipText(Messages.getString("Firma.formato.description"));
		comboFormatos.getAccessibleContext().setAccessibleName(Messages.getString("Firma.formato"));
		comboFormatos.getAccessibleContext().setAccessibleDescription(Messages.getString("Firma.formato.description"));
		comboFormatos.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				checkFormatoModo();
			}
		});
		cargarCombo();
		panelCentral.add(comboFormatos, c);
		
		c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(10, 20, 15, 20);
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.gridy = 4;
		
		// Checkbox con el modo de CADES
		checkFormato.setSelected(true);
		checkFormato.setText(Messages.getString("Firma.modo.formato")); // NOI18N
		checkFormato.setToolTipText(Messages.getString("Firma.modo.formato.description")); // NOI18N
		checkFormato.getAccessibleContext().setAccessibleName(Messages.getString("Firma.modo.formato")); // NOI18N
		checkFormato.getAccessibleContext().setAccessibleDescription(Messages.getString("Firma.modo.formato.description")); // NOI18N
		checkFormato.setEnabled(false);
		panelCentral.add(checkFormato, c);
		
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
		if (formatosV.get(comboFormatos.getSelectedIndex()).equals(AOConstants.SIGN_FORMAT_CADES)) {
			if (radioFirma.isSelected())
				checkFormato.setEnabled(true);
		}
		else {
			checkFormato.setEnabled(false);
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
			if (comboFormatos.getItemCount() != 0 && formatosV.get(comboFormatos.getSelectedIndex()).equals(AOConstants.SIGN_FORMAT_CADES)) {
				checkFormato.setSelected(true);
				checkFormato.setEnabled(true);
			}
		}
		else if (radio.getName().equals("radioCofirma")) {
			comboFormatos.setEnabled(true);
			checkFormato.setSelected(true);
			checkFormato.setEnabled(false);
		}
		else if (radio.getName().equals("radioContrafirma")) {
			comboFormatos.setEnabled(false);
			checkFormato.setSelected(true);
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
				
				// Nos saltamos la pagina 2
				Integer indice = 3;
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

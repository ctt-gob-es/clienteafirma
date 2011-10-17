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
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import es.gob.afirma.keystores.common.KeyStoreConfiguration;
import es.gob.afirma.ui.listeners.ElementDescriptionFocusListener;
import es.gob.afirma.ui.listeners.ElementDescriptionMouseListener;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.KeyStoreLoader;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.RequestFocusListener;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardmultifirmamasiva.AsistenteMultifirmaMasiva;

/**
 * Clase para realizar una multifirma masiva
 */
public class MultifirmaMasiva extends JPanel {

	private static final long serialVersionUID = 1L;

	public MultifirmaMasiva() {
		initComponents();
	}

	/**
	 * Inicializacion de los componentes
	 */
	private void initComponents() {
		setLayout(new GridBagLayout());
		
		GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
		c.insets = new Insets(13, 13, 0, 13);
		c.weightx = 1.0;
		c.gridx = 0;

		// Etiqueta almacen / repositorio de certificados
		JLabel etiquetaAlmacen = new JLabel();
		etiquetaAlmacen.setText(Messages.getString("Firma.almacen.certificados")); // NOI18N
		Utils.setContrastColor(etiquetaAlmacen);
		Utils.setFontBold(etiquetaAlmacen);
		add(etiquetaAlmacen, c);

		c.insets = new Insets(0, 13, 0, 13);
		c.gridy = 1;
		c.weighty = 0.1;
		c.fill = GridBagConstraints.BOTH;

		// Combo con los almacenes / repositorios disponibles
		final JComboBox comboAlmacen = new JComboBox();
		comboAlmacen.setToolTipText(Messages.getString("Firma.almacen.certificados.description")); // NOI18N
		comboAlmacen.getAccessibleContext().setAccessibleName(etiquetaAlmacen.getText()+" "+Messages.getString("Firma.almacen.certificados.description") + " ALT + A."); // NOI18N
		comboAlmacen.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Firma.almacen.certificados.description.status")));
		comboAlmacen.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Firma.almacen.certificados.description.status")));
		comboAlmacen.addAncestorListener(new RequestFocusListener(false));
		cargarComboAlmacen(comboAlmacen);
		Utils.remarcar(comboAlmacen);
		Utils.setContrastColor(comboAlmacen);
		Utils.setFontBold(comboAlmacen);
		add(comboAlmacen, c);
		
		//Relación entre etiqueta y combo
		etiquetaAlmacen.setLabelFor(comboAlmacen);
		//Asignación de mnemónico
		etiquetaAlmacen.setDisplayedMnemonic(KeyEvent.VK_A);
		
		c.insets = new Insets(13, 13, 0, 13);
		c.gridy = 2;
		c.weighty = 0.0;
		c.fill = GridBagConstraints.HORIZONTAL;
		// Panel que engloba los tipos de multifirma
		JPanel panelTipos = new JPanel(new GridLayout());
		panelTipos.setBorder(BorderFactory.createTitledBorder(Messages.getString("PrincipalGUI.multifirma.panel.opciones"))); // NOI18N
		Utils.setContrastColor(panelTipos);
		Utils.setFontBold(panelTipos);
		
		JPanel panelAlerta = new JPanel(new GridLayout(1,1));
		// Checkbox alerta sonora
		final JCheckBox alerta = new JCheckBox();
		alerta.setSelected(true);
		alerta.setText(Messages.getString("PrincipalGUI.multifirma.panel.opciones.timbre")); // NOI18N
		alerta.setToolTipText(Messages.getString("Ensobrado.check.firmar.description")); // NOI18N
		alerta.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("PrincipalGUI.multifirma.panel.opciones.timbre.description.status")));
		alerta.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("PrincipalGUI.multifirma.panel.opciones.timbre.description.status")));
		alerta.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.multifirma.panel.opciones.timbre") +" "+Messages.getString("PrincipalGUI.multifirma.panel.opciones.timbre.description")); // NOI18N
		alerta.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.multifirma.panel.opciones.timbre.description")); // NOI18N
		alerta.setMnemonic(KeyEvent.VK_L); //Se asigna un atajo
		Utils.remarcar(alerta);
		Utils.setContrastColor(alerta);
		Utils.setFontBold(alerta);
		panelAlerta.add(alerta);
		panelTipos.add(panelAlerta);
		
		add(panelTipos, c);

		c.weighty = 1.0;
		c.gridy = 3;
		
		// Panel vacio para alinear el boton de aceptar en la parte de abajo de la pantalla
		JPanel emptyPanel = new JPanel();
		add(emptyPanel, c);
		
		// Panel con los botones
		JPanel panelBotones = new JPanel(new GridBagLayout());
		
		GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.ipadx = 15;
		cons.gridx = 0;
		
		// Etiqueta para rellenar a la izquierda
		JLabel label = new JLabel();
		panelBotones.add(label, cons);
		
		JPanel panelFirmar = new JPanel(new GridLayout(1, 1));
		// Boton firmar
		JButton firmar = new JButton();
		firmar.setMnemonic(KeyEvent.VK_R);
		firmar.setText(Messages.getString("PrincipalGUI.firmar")); // NOI18N
		firmar.setToolTipText(Messages.getString("PrincipalGUI.firmar.description")); // NOI18N
		firmar.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.firmar") + " " + Messages.getString("PrincipalGUI.firmar.description.status"));
		firmar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.firmar.description")); // NOI18N
		firmar.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("PrincipalGUI.firmar.description.status")));
		firmar.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("PrincipalGUI.firmar.description.status")));
		firmar.addActionListener(new ActionListener() {
			@Override
            public void actionPerformed(ActionEvent evt) {
				firmarActionPerformed(comboAlmacen, alerta);
			}
		});

		Utils.remarcar(firmar);
		Utils.setContrastColor(firmar);
		Utils.setFontBold(firmar);
		
		cons.ipadx = 0;
		cons.weightx = 1.0;
		cons.gridx = 1;
		
		panelFirmar.add(firmar);		
		JPanel buttonPanel = new JPanel();
		buttonPanel.add(panelFirmar, BorderLayout.CENTER);
		panelBotones.add(buttonPanel, cons);

		cons.ipadx = 15;
		cons.weightx = 0.0;
		cons.gridx = 2;
				
		JPanel panelAyuda = new JPanel(new GridLayout(1, 1));
		// Boton ayuda
		JButton botonAyuda = HelpUtils.helpButton("firma.masiva");
		
		panelAyuda.add(botonAyuda);
		panelBotones.add(panelAyuda, cons);

		c.gridwidth	= 2;
        c.insets = new Insets(13,13,13,13);
        c.weighty = 0.0;
        c.gridy = 4;
		
		add(panelBotones, c);
		
		// Accesos rapidos al menu de ayuda
		HelpUtils.enableHelpKey(comboAlmacen,"multifirma.masiva.almacen");
		HelpUtils.enableHelpKey(alerta,"multifirma.masiva.alerta");
	}

	/**
	 * Firma masivamente haciendo uso del almacen / repositorio
	 * @param comboAlmacen 	Combo con los almacenes / repositorios de certificados
	 * @param alerta		Checkbox para emitir un pitido al finalizar la operacion
	 */
	void firmarActionPerformed(JComboBox comboAlmacen, JCheckBox alerta) {
		KeyStoreConfiguration kssc = (KeyStoreConfiguration)comboAlmacen.getSelectedItem();
		
		// Se muestra el asistente
		new AsistenteMultifirmaMasiva(kssc, alerta.isSelected());
	}

	/**
	 * Carga el combo almac&eacute;n con los almacenes y repositorios disponibles
	 * @param comboAlmacen	Combo con los almacenes y repositorios
	 */
	private void cargarComboAlmacen(JComboBox comboAlmacen) {
		comboAlmacen.setModel(new DefaultComboBoxModel(KeyStoreLoader.getKeyStoresToSign()));
	}
}

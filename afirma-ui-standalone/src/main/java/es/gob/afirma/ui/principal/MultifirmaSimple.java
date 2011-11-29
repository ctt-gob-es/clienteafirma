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

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import es.gob.afirma.keystores.common.KeyStoreConfiguration;
import es.gob.afirma.ui.listeners.ElementDescriptionFocusListener;
import es.gob.afirma.ui.listeners.ElementDescriptionMouseListener;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.KeyStoreLoader;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.RequestFocusListener;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardmultifirmacofirma.AsistenteCofirma;
import es.gob.afirma.ui.wizardmultifirmacontrafirma.AsistenteContrafirmas;

public class MultifirmaSimple extends JPanel {

	private static final long serialVersionUID = 1L;

	public MultifirmaSimple() {
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
		//comboAlmacen.getAccessibleContext().setAccessibleName(etiquetaAlmacen.getText()+" "+Messages.getString("Firma.almacen.certificados.description")+" ALT + A."); // NOI18N
		comboAlmacen.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Firma.almacen.certificados.description.status")));
		comboAlmacen.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Firma.almacen.certificados.description.status")));
		comboAlmacen.addAncestorListener(new RequestFocusListener(false));
		Utils.remarcar(comboAlmacen);
		Utils.setContrastColor(comboAlmacen);
		Utils.setFontBold(comboAlmacen);
		cargarComboAlmacen(comboAlmacen);
		add(comboAlmacen, c);
		
		//Espacio en blanco
        JPanel emptyPanel01 = new JPanel();
        emptyPanel01.setPreferredSize(new Dimension(1, 1));
        c.weightx = 1.0;
        c.weighty = 0.2;
        c.gridwidth = 3;
        c.gridx = 0;
        c.gridy = 2;
        c.insets = new Insets(0, 0, 0, 0);
        add(emptyPanel01, c);
		
		//Relación entre etiqueta y combo
		etiquetaAlmacen.setLabelFor(comboAlmacen);
		//Asignación de mnemónico
		etiquetaAlmacen.setDisplayedMnemonic(KeyEvent.VK_A);
		
		c.insets = new Insets(13, 13, 0, 13);
		c.gridy = 3;
		c.fill = GridBagConstraints.HORIZONTAL;
		c.weighty = 0.0;
		
		// Panel que engloba los tipos de multifirma
		JPanel panelTipos = new JPanel(new GridLayout(0, 1));
		panelTipos.setBorder(BorderFactory.createTitledBorder(Messages.getString("MultifirmaSimple.opciones"))); // NOI18N
		Utils.setContrastColor(panelTipos);
		Utils.setFontBold(panelTipos);

		JPanel panelCofirma = new JPanel(new GridLayout(1, 1));
        panelCofirma.getAccessibleContext().setAccessibleName(Messages.getString("MultifirmaSimple.opciones"));
		// Radiobutton cofirma
		final JRadioButton cofirma = new JRadioButton();
		cofirma.setSelected(true);
		cofirma.setText(Messages.getString(
				"Multifirma.opcion.cofirma." + (GeneralConfig.isAvanzados() ? "av" : "sp")   //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		));
		cofirma.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Multifirma.opcion.cofirma.description.status")));
		cofirma.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Multifirma.opcion.cofirma.description.status")));
		//cofirma.getAccessibleContext().setAccessibleName(cofirma.getText() +" "+ Messages.getString("Multifirma.opcion.cofirma.description.status")); // NOI18N
		cofirma.getAccessibleContext().setAccessibleDescription(Messages.getString("Multifirma.opcion.cofirma.description")); // NOI18N
		cofirma.setToolTipText(Messages.getString("Multifirma.opcion.cofirma.description.status"));
		//Se comprueba si el modo es el avanzado o no
		if (GeneralConfig.isAvanzados()){
			cofirma.setMnemonic(KeyEvent.VK_O); //Se asigna el atajo para el modo avanzado
		} else {
			cofirma.setMnemonic(KeyEvent.VK_G); //Se asigna el atajo para el modo simple
		}
		Utils.remarcar(cofirma);
		Utils.setContrastColor(cofirma);
		Utils.setFontBold(cofirma);
		panelCofirma.add(cofirma);
		panelTipos.add(panelCofirma);

		JPanel panelContrafirma = new JPanel(new GridLayout(1, 1));        
		// Radiobutton contrafirma
		JRadioButton contrafirma = new JRadioButton();
		contrafirma.setText(Messages.getString(
				"Multifirma.opcion.contrafirma." + (GeneralConfig.isAvanzados() ? "av" : "sp")   //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		));
		contrafirma.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Multifirma.opcion.contrafirma.description.status")));
		contrafirma.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Multifirma.opcion.contrafirma.description.status")));
		//contrafirma.getAccessibleContext().setAccessibleName(contrafirma.getText()+" "+ Messages.getString("Multifirma.opcion.contrafirma.description.status")); // NOI18N
		contrafirma.getAccessibleContext().setAccessibleDescription(Messages.getString("Multifirma.opcion.contrafirma.description")); // NOI18N
		contrafirma.setToolTipText(Messages.getString("Multifirma.opcion.contrafirma.description.status"));
		//Se comprueba si el modo es el avanzado o no
		if (GeneralConfig.isAvanzados()){
			contrafirma.setMnemonic(KeyEvent.VK_T); //Se asigna el atajo para el modo avanzado
		} else {
			contrafirma.setMnemonic(KeyEvent.VK_E); //Se asigna el atajo para el modo simple
		}
		Utils.remarcar(contrafirma);
		Utils.setContrastColor(contrafirma);
		Utils.setFontBold(contrafirma);

		panelContrafirma.add(contrafirma);
		panelTipos.add(panelContrafirma);
		
		add(panelTipos, c);

		// Grupo con los radio button para poder seleccionar solo uno
		ButtonGroup grupoButtons = new ButtonGroup();
		grupoButtons.add(cofirma);
		grupoButtons.add(contrafirma);
		
		c.weighty = 1.0;
        c.gridheight = 4;
		c.gridy = 4;
		
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
		
		JPanel panelFirmar = new JPanel(new GridLayout(1, 1));
		// Boton firmar
		JButton firmar = new JButton();
		firmar.setMnemonic(KeyEvent.VK_R);
		firmar.setText(Messages.getString("PrincipalGUI.firmar")); // NOI18N
		firmar.setToolTipText(Messages.getString("PrincipalGUI.firmar.description")); // NOI18N
		//firmar.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.firmar") + " " + Messages.getString("PrincipalGUI.firmar.description.status"));
		firmar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.firmar.description")); // NOI18N
		firmar.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("PrincipalGUI.firmar.description.status")));
		firmar.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("PrincipalGUI.firmar.description.status")));
		firmar.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				firmarActionPerformed(comboAlmacen, cofirma);
			}
		});
		Utils.remarcar(firmar);
		Utils.setContrastColor(firmar);
		Utils.setFontBold(firmar);
		
		cons.ipadx = 0;
		cons.gridx = 1;
		cons.weightx = 1.0;
		
		panelFirmar.add(firmar);
		JPanel buttonPanel = new JPanel();
		buttonPanel.add(panelFirmar, BorderLayout.CENTER);
		panelBotones.add(buttonPanel, cons);

		cons.ipadx = 15;
		cons.weightx = 0.0;
		cons.gridx = 2;
		
		JPanel panelAyuda = new JPanel();
		// Boton ayuda
		JButton botonAyuda = HelpUtils.helpButton("multifirma");
		botonAyuda.setName("helpButton");
		
		panelAyuda.add(botonAyuda);
		panelBotones.add(panelAyuda, cons);

		c.fill = GridBagConstraints.HORIZONTAL;
		c.insets = new Insets(13,13,13,13);
        c.weightx = 1.0;
        c.weighty = 0.0;
        c.gridwidth	= 2;
        c.gridy = 8;
		
		add(panelBotones, c);
		
		// Accesos rapidos al menu de ayuda
		HelpUtils.enableHelpKey(comboAlmacen, "multifirma.almacen");
		HelpUtils.enableHelpKey(cofirma, "multifirma.tipo");
		HelpUtils.enableHelpKey(contrafirma, "multifirma.tipo");
		HelpUtils.enableHelpKey(firmar, "multifirma");
	}

	/**
	 * Se realiza la multifirma
	 * @param comboAlmacen	Combo con el almacen / repositorio de certificados
	 * @param cofirma		Radiobutton cofirma. No es necesario pasar el de contrafirma ya que solo existen dos
	 */
	private void firmarActionPerformed(JComboBox comboAlmacen, JRadioButton cofirma) {
		KeyStoreConfiguration kssc = (KeyStoreConfiguration) comboAlmacen.getSelectedItem();
		// Se muestar el asistente
		if (cofirma.isSelected())
			new AsistenteCofirma(kssc);
		else {
			new AsistenteContrafirmas(kssc);
		}
	}

	/**
	 * Carga el combo almacen con los almacenes y repositorios disponibles
	 * @param comboAlmacen	Combo con los almacenes y repositorios
	 */
	private void cargarComboAlmacen(JComboBox comboAlmacen) {
		comboAlmacen.setModel(new DefaultComboBoxModel(KeyStoreLoader.getKeyStoresToSign()));
	}
}

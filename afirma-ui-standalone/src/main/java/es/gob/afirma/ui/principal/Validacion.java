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
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

import es.gob.afirma.ui.listeners.ElementDescriptionFocusListener;
import es.gob.afirma.ui.listeners.ElementDescriptionMouseListener;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;

/**
 *
 * Clase que muestra el panel de validacion VALIDe
 */
public class Validacion extends JPanel {

	private static final long serialVersionUID = 1L;

	public Validacion() {
		initComponents();
	}

	/**
	 * Inicializacion de componentes
	 */
	private void initComponents() {
		// Eliminamos el layout
		setLayout(new GridBagLayout());
		
		GridBagConstraints c = new GridBagConstraints();

		c.weightx = 1.0;
		c.insets = new Insets(8, 13, 0, 13);
		c.gridy = 0;

		// Etiqueta con el icono de VALIDE
		JLabel etiquetaValide = new JLabel();
		etiquetaValide.setIcon(new ImageIcon(getClass().getResource("/resources/images/logo_VALIDe.png"))); // NOI18N
//		etiquetaValide.setBounds(171, 10, 140, 50);
		add(etiquetaValide, c);

		c.insets = new Insets(8, 13, 0, 13);
		c.gridy = 1;
		
		// Etiqueta Uno: Puede realizar las validaciones
		JLabel etiqueta = new JLabel();
		etiqueta.setText(Messages.getString("Validacion.texto.parte1")); // NOI18N
//		etiqueta.setBounds(91, 60, 402, 20);
		etiqueta.setFocusable(true);
		if (GeneralConfig.isRemarked()){
			Utils.remarcar(etiqueta);
		}
		Utils.setContrastColor(etiqueta);
		Utils.setFontBold(etiqueta);
		add(etiqueta, c);

		c.insets = new Insets(5, 13, 0, 13);
		c.gridy = 2;
		
		// Etiqueta Dos: mediante la aplicacion web
		etiqueta = new JLabel();
		etiqueta.setText(Messages.getString("Validacion.texto.parte2")); // NOI18N
//		etiqueta.setBounds(71, 80, 422, 20);
		etiqueta.setFocusable(true);
		if (GeneralConfig.isRemarked()){
			Utils.remarcar(etiqueta);
		}
		Utils.setContrastColor(etiqueta);
		Utils.setFontBold(etiqueta);
		add(etiqueta, c);

		c.gridy = 3;
		
		// Etiqueta Tres: enlace inicioAPP
		etiqueta = new JLabel();
		etiqueta.setForeground(new Color(55, 55, 254));
		etiqueta.setText(Messages.getString("Validacion.texto.parte4")); // NOI18N
//		etiqueta.setBounds(111, 100, 312, 20);
		etiqueta.setFocusable(true);
		if (GeneralConfig.isRemarked()){
			Utils.remarcar(etiqueta);
		}
		Utils.setContrastColor(etiqueta);
		Utils.setFontBold(etiqueta);
		add(etiqueta, c);

		c.gridy = 4;
		
		// Etiqueta Cuatro: Pulse el boton inferior
		etiqueta = new JLabel();
		etiqueta.setText(Messages.getString("Validacion.texto.parte5")); // NOI18N
		etiqueta.setFocusable(true);
		if (GeneralConfig.isRemarked()){
			Utils.remarcar(etiqueta);
		}
		Utils.setContrastColor(etiqueta);
		Utils.setFontBold(etiqueta);
		add(etiqueta, c);

		c.gridy = 5;
		
		// Etiqueta Cinco: En su navegador web
		etiqueta = new JLabel();
		etiqueta.setText(Messages.getString("Validacion.texto.parte6")); // NOI18N
		etiqueta.setFocusable(true);
		if (GeneralConfig.isRemarked()){
			Utils.remarcar(etiqueta);
		}
		Utils.setContrastColor(etiqueta);
		Utils.setFontBold(etiqueta);
		add(etiqueta, c);

		c.insets = new Insets(0, 13, 0, 13);
		c.weighty = 1.0;
		c.gridy = 6;
		
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
		
		// Boton valide
		JButton valide = new JButton();
		valide.setMnemonic(KeyEvent.VK_L);
		valide.setText(Messages.getString("Validacion.btnValide")); // NOI18N
		valide.setToolTipText(Messages.getString("Validacion.btnValide.description")); // NOI18N
		valide.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Validacion.btnValide.description.status")));
		valide.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Validacion.btnValide.description.status")));
		valide.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				valideActionPerformed();
			}
		});
		valide.getAccessibleContext().setAccessibleName(Messages.getString("Validacion.btnValide")); // NOI18N
		valide.getAccessibleContext().setAccessibleDescription(Messages.getString("Validacion.btnValide.description")); // NOI18N
		if (GeneralConfig.isRemarked()){
			Utils.remarcar(valide);
		}
		Utils.setContrastColor(valide);
		Utils.setFontBold(valide);
		cons.ipadx = 0;
		cons.gridx = 1;
		cons.weightx = 1.0;
		
		JPanel buttonPanel = new JPanel();
		buttonPanel.add(valide, BorderLayout.CENTER);
		panelBotones.add(buttonPanel, cons);

		cons.ipadx = 15;
		cons.weightx = 0.0;
		cons.gridx = 2;
		
		
		// Boton de ayuda
		JButton botonAyuda = HelpUtils.helpButton("validacion");
		panelBotones.add(botonAyuda, cons);
		
		c.fill = GridBagConstraints.HORIZONTAL;
		c.gridwidth	= 2;
        c.insets = new Insets(13,13,13,13);
        c.weighty = 0.0;
        c.weightx = 1.0;
        c.gridy = 7;
		
		add(panelBotones, c);
	}

	/**
	 * Accede a la pagina de VALIDE
	 */
	private void valideActionPerformed() {
		// Obtenemos la url de valide.
		String url = Messages.getString("Validacion.texto.parte4");

		String os = System.getProperty("os.name").toLowerCase();
		Runtime rt = Runtime.getRuntime();
		try {
			if (os.indexOf( "win" ) >= 0) {
				String[] cmd = new String[4];
				cmd[0] = "cmd.exe";
				cmd[1] = "/C";
				cmd[2] = "start";
				cmd[3] = url;
				rt.exec(cmd);
			} else if (os.indexOf( "mac" ) >= 0) 
				rt.exec( "open " + url);
			else {
				//prioritized 'guess' of users' preference
				List<String> browsers = new ArrayList<String>(Arrays.asList("epiphany", "firefox", "mozilla", "konqueror",
						"netscape","opera","links","lynx"));

				StringBuffer cmd = new StringBuffer();
				for (String browser : browsers)
					cmd.append( (browsers.get(0).equals(browser) ? "" : " || " ) + browser +" \"" + url + "\" ");

				rt.exec(new String[] { "sh", "-c", cmd.toString() });
			}
		} catch (IOException e) {
			e.printStackTrace();
			PrincipalGUI.setNuevoEstado(Messages.getString("Validacion.error.valide"));
		}
	}
}

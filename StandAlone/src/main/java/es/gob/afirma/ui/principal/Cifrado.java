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
import java.awt.Insets;
import java.awt.Panel;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import es.gob.afirma.misc.AOConstants;
import es.gob.afirma.ui.listeners.ElementDescriptionFocusListener;
import es.gob.afirma.ui.listeners.ElementDescriptionMouseListener;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.SelectionDialog;
import es.gob.afirma.ui.wizardcifradoclave.AsistenteCifradoClave;
import es.gob.afirma.ui.wizardcifradocontrasenia.AsistenteCifradoContrasenia;

/**
 * Clase Cifrado que contiene el interfaz de cifrado.
 */
public class Cifrado extends JPanel {
	
	private static final long serialVersionUID = 1L;

	// Constantes de los mecanismos de cifrado
	private List<String> mecanismos = new ArrayList<String>(Arrays.asList(
		AOConstants.KEY_MODE_PASSWORD, 
		AOConstants.KEY_MODE_USERINPUT
	));

	// Algoritmos para mecanismo contrasena de cifrado
	private List<String> algoritmoLc = new ArrayList<String>(Arrays.asList(
			Messages.getString("Cifrado.origenLc.0"),
			Messages.getString("Cifrado.origenLc.1"),
			Messages.getString("Cifrado.origenLc.2")
	));

	// Constantes algoritmos / Mecanismo Contrasena de cifrado
	private String[] algoritmoVc = new String[]{
		AOConstants.AOCipherAlgorithm.PBEWITHSHA1ANDDESEDE.getName(),
		AOConstants.AOCipherAlgorithm.PBEWITHSHA1ANDRC2_40.getName(),
		AOConstants.AOCipherAlgorithm.PBEWITHMD5ANDDES.getName()
	};

	// Constantes algoritmos / Mecanismo Clave de cifrado
	private String[] algoritmoVr = new String[]{
		AOConstants.AOCipherAlgorithm.AES.getName(),
		AOConstants.AOCipherAlgorithm.ARCFOUR.getName(),
		AOConstants.AOCipherAlgorithm.BLOWFISH.getName(),
		AOConstants.AOCipherAlgorithm.DES.getName(),
		AOConstants.AOCipherAlgorithm.TRIPLEDES.getName(),
		AOConstants.AOCipherAlgorithm.RC2.getName()
	};

	public Cifrado() {
		initComponents();
	}

	/**
	 * Iniciamos componentes
	 */
	private void initComponents() {
		setLayout(new GridBagLayout());
		
		GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
		c.insets = new Insets(13, 13, 0, 13);
		c.weightx = 1.0;
		c.gridwidth = 2;
		c.gridx = 0;
		
		// Etiqueta abrir fichero
		JLabel etiquetaFichero = new JLabel();
		etiquetaFichero.setText(Messages.getString("Cifrado.buscar")); // NOI18N
		etiquetaFichero.getAccessibleContext().setAccessibleDescription(Messages.getString("Cifrado.buscar.description")); // NOI18N
		add(etiquetaFichero, c);
		
		c.insets = new Insets(0, 13, 0, 0);
		c.gridwidth = 1;
		c.gridy	= 1;
		
		// Caja con el nombre del archivo seleccionado
		final JTextField campoFichero = new JTextField();
		campoFichero.setToolTipText(Messages.getString("Cifrado.buscar.caja.description")); // NOI18N
		campoFichero.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Cifrado.buscar.caja.description.status")));
		campoFichero.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Cifrado.buscar.caja.description.status")));
		campoFichero.getAccessibleContext().setAccessibleName(Messages.getString("Cifrado.buscar.caja")); // NOI18N
		campoFichero.getAccessibleContext().setAccessibleDescription(Messages.getString("Cifrado.buscar.caja.description")); // NOI18N
		add(campoFichero, c);
		
		c.insets = new Insets(0, 10, 0, 13);
		c.weightx = 0.0;
		c.gridx = 1;
		
		// Boton seleccionar
		JButton examinar = new JButton();
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
		examinar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.Examinar.description")); // NOI18N
		add(examinar, c);
		
		c.insets = new Insets(13, 13, 0, 13);
		c.weightx = 1.0;
		c.gridwidth = 2;
		c.gridx = 0;
		c.gridy	= 2;
		
		// Etiqueta mecanismo cifrado
		JLabel etiquetaMecanismo = new JLabel();
		etiquetaMecanismo.setText(Messages.getString("Cifrado.origen.clave")); // NOI18N
		add(etiquetaMecanismo, c);

		c.insets = new Insets(0, 13, 0, 13);
		c.gridy = 3;
		
		// Combo mecanismos de cifrado
		final JComboBox comboMecanismo = new JComboBox();
		final JComboBox comboAlgoritmo = new JComboBox();
		comboMecanismo.setToolTipText(Messages.getString("Cifrado.origen.clave.combo.description")); // NOI18N
		comboMecanismo.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Cifrado.origen.clave.combo.description.status")));
		comboMecanismo.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Cifrado.origen.clave.combo.description.status")));
		comboMecanismo.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent evt) {
				comboMecanismoItemStateChanged(comboMecanismo, comboAlgoritmo);
			}
		});
		comboMecanismo.getAccessibleContext().setAccessibleName(Messages.getString("Cifrado.origen.clave")); // NOI18N
		comboMecanismo.getAccessibleContext().setAccessibleDescription(Messages.getString("Cifrado.origen.clave")); // NOI18N
		comboMecanismo.setModel(new DefaultComboBoxModel(new String[]{Messages.getString("Cifrado.origenL.0"),Messages.getString("Cifrado.origenL.1")}));
		add(comboMecanismo, c);
		
		c.insets = new Insets(13, 13, 0, 13);
		c.weightx = 1.0;
		c.gridy = 4;
		
		// Etiqueta algoritmos de cifrado
		JLabel etiquetaAlgoritmo = new JLabel();
		etiquetaAlgoritmo.setText(Messages.getString("Cifrado.formato")); // NOI18N
		etiquetaAlgoritmo.getAccessibleContext().setAccessibleDescription(Messages.getString("Cifrado.formato.description")); // NOI18N
		add(etiquetaAlgoritmo, c);
		
		c.insets = new Insets(0, 13, 0, 13);
		c.gridy = 5;
		
		// Combo algoritmos de cifrado
		comboAlgoritmo.setToolTipText(Messages.getString("Cifrado.formato.combo.description")); // NOI18N
		comboAlgoritmo.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Cifrado.formato.combo.description.status")));
		comboAlgoritmo.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Cifrado.formato.combo.description.status")));
		comboAlgoritmo.getAccessibleContext().setAccessibleName(Messages.getString("Cifrado.formato.combo")); // NOI18N
		comboAlgoritmo.getAccessibleContext().setAccessibleDescription(Messages.getString("Cifrado.formato.combo.description")); // NOI18N
		comboAlgoritmo.setModel(new DefaultComboBoxModel(algoritmoLc.toArray()));
		add(comboAlgoritmo, c);
		
		// En la vista simple, no se permitira configurar el origen de la clave ni el algoritmo de cifrado
		if(!GeneralConfig.isAvanzados()) {
			comboMecanismo.setEnabled(false);
			comboAlgoritmo.setEnabled(false);
		}
		
		c.weighty = 1.0;
		c.gridy = 6;
		
		// Panel vacio para alinear el boton de aceptar en la parte de abajo de la pantalla
		JPanel emptyPanel = new JPanel();
		add(emptyPanel, c);
		
		// Panel con los botones
		Panel panelBotones = new Panel(new GridBagLayout());
		
		GridBagConstraints cons = new GridBagConstraints();
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.ipadx = 15;
		cons.gridx = 0;
		
		// Etiqueta para rellenar a la izquierda
		JLabel label = new JLabel();
		panelBotones.add(label, cons);
		
		// Boton cifrar
		JButton cifrar = new JButton();
		cifrar.setMnemonic(KeyEvent.VK_I);
		cifrar.setText(Messages.getString("Cifrado.btncifrar")); // NOI18N
		cifrar.setToolTipText(Messages.getString("Cifrado.btncifrar.description")); // NOI18N
		cifrar.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.bar, Messages.getString("Cifrado.btncifrar.description.status")));
		cifrar.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.bar, Messages.getString("Cifrado.btncifrar.description.status")));
		cifrar.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				cifrarActionPerformed(comboMecanismo, comboAlgoritmo, campoFichero);
				PrincipalGUI.setNuevoEstado(Messages.getString("Cifrado.statusbar.cipher.action"));
			}
		});
		cifrar.getAccessibleContext().setAccessibleDescription(Messages.getString("Cifrado.btncifrar.description")); // NOI18N
		
		JPanel buttonPanel = new JPanel();
		buttonPanel.add(cifrar, BorderLayout.CENTER);
		
		cons.ipadx = 0;
		cons.gridx = 1;
		cons.weightx = 1.0;
		
		panelBotones.add(buttonPanel, cons);

		// Boton ayuda
		JLabel botonAyuda = HelpUtils.fechButton("cifrado");
		
		cons.ipadx = 15;
		cons.weightx = 0.0;
		cons.gridx = 2;
		
		panelBotones.add(botonAyuda, cons);

		c.gridwidth	= 2;
        c.insets = new Insets(13,13,13,13);
        c.weightx = 1.0;
        c.weighty = 0.0;
        c.gridy = 7;
		
		add(panelBotones, c);
		
		// Accesos rapidos al menu de ayuda
		HelpUtils.enableHelpKey(campoFichero, "cifrado.fichero");
		HelpUtils.enableHelpKey(examinar, "cifrado.fichero");
		HelpUtils.enableHelpKey(comboMecanismo, "cifrado.mecanismo");
		HelpUtils.enableHelpKey(comboAlgoritmo, "cifrado.algoritmo");
	}
	
	/**
	 * Pulsar boton cifrar: Cifra el archivo seleccionado con la configuracion seleccionada
	 * @param comboMecanismo 	Combo con el mecanismo de cifrado
	 * @param comboAlgoritmo	Combo con el algoritmo de cifrado
	 * @param campoFichero 		Campo con el nombre del fichero a cifrar
	 */
	private void cifrarActionPerformed(JComboBox comboMecanismo, JComboBox comboAlgoritmo, 
			JTextField campoFichero) {
		String algoritmo;
		String mecanismo = mecanismos.get(comboMecanismo.getSelectedIndex());
		if (mecanismo.equals(AOConstants.KEY_MODE_PASSWORD))
			algoritmo = algoritmoVc[comboAlgoritmo.getSelectedIndex()];
		else
			algoritmo = algoritmoVr[comboAlgoritmo.getSelectedIndex()];
		
		// Sacamos la ruta del archivo
		if (campoFichero.getText() == null || campoFichero.getText().equals("")) 
			JOptionPane.showMessageDialog(this, Messages.getString("Cifrado.msg.error.fichero"), Messages.getString("Cifrado.msg.titulo"), JOptionPane.WARNING_MESSAGE);
		else {
			// Se selecciona el primer elemento del combo
			if (mecanismo.equals(AOConstants.KEY_MODE_PASSWORD)) 
				// Se muestra el asistente de cifrado con contrasena
				new AsistenteCifradoContrasenia(algoritmo,campoFichero.getText());
			else 
				// Se muestra el asistente de cifrado con clave
				new AsistenteCifradoClave(algoritmo,campoFichero.getText());
		}
	}

	/**
	 * Pulsar boton examinar: Muestra una ventana para seleccinar un archivo.
	 * Modifica el valor de la caja con el nombre del archivo seleccionado
	 * @param campoFichero	Campo en el que se escribe el nombre del fichero seleccionado
	 */
	private void examinarActionPerformed(JTextField campoFichero) {
		File selectedFile = new SelectionDialog().showFileOpenDialog(this, Messages.getString("Cifrado.browse.data.file"));
		if (selectedFile != null) {
			campoFichero.setText(selectedFile.getAbsolutePath());
		}
	}

	/**
	 * Cambio de seleccion en el combo de los mecanismos
	 * @param comboMecanismo	Combo que contiene el listado de mecanismos de cifrado
	 * @param comboAlgoritmo	Combo que contiene el listado de algoritmos
	 */
	private void comboMecanismoItemStateChanged(JComboBox comboMecanismo, JComboBox comboAlgoritmo) {
		String mecanismo = mecanismos.get(comboMecanismo.getSelectedIndex());
		if (mecanismo.equals(AOConstants.KEY_MODE_PASSWORD))
			comboAlgoritmo.setModel(new DefaultComboBoxModel(algoritmoLc.toArray()));
		else {
			String[] algoritmoLr = new String[]{
					"Advanced Encryption Standard (AES)",
					"Alleged RC4","Blowfish",
					"Data Encryption Standard (DES)",
					"Triple DES (3DES)",
					"RC2"
			};
			comboAlgoritmo.setModel(new DefaultComboBoxModel(algoritmoLr));
		}
	}
}

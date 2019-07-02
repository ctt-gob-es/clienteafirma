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
import javax.swing.SwingUtilities;
import javax.swing.text.Caret;

import es.gob.afirma.ciphers.AOCipherConstants;
import es.gob.afirma.core.ciphers.CipherConstants;
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
import es.gob.afirma.ui.wizardcifradoclave.AsistenteCifradoClave;
import es.gob.afirma.ui.wizardcifradocontrasenia.AsistenteCifradoContrasenia;

/** Clase Cifrado que contiene el interfaz de cifrado. */
final class Cifrado extends JPanel {

    private static final long serialVersionUID = 1L;

    // Algoritmos para mecanismo contrasena de cifrado
    private final List<String> algoritmoLc = new ArrayList<>(
		Arrays.asList(
			Messages.getString("Cifrado.origenLc.0"), //$NON-NLS-1$
            Messages.getString("Cifrado.origenLc.1"), //$NON-NLS-1$
            Messages.getString("Cifrado.origenLc.2") //$NON-NLS-1$
		)
	);

    // Constantes algoritmos / Mecanismo Contrasena de cifrado
    private final String[] algoritmoVc = new String[] {
                                                       CipherConstants.AOCipherAlgorithm.PBEWITHSHA1ANDDESEDE.getName(),
                                                       CipherConstants.AOCipherAlgorithm.PBEWITHSHA1ANDRC2_40.getName(),
                                                       CipherConstants.AOCipherAlgorithm.PBEWITHMD5ANDDES.getName()
    };

    // Constantes algoritmos / Mecanismo Clave de cifrado
    private final String[] algoritmoVr = new String[] {
           CipherConstants.AOCipherAlgorithm.AES.getName(),
           CipherConstants.AOCipherAlgorithm.BLOWFISH.getName(),
           CipherConstants.AOCipherAlgorithm.DES.getName(),
           CipherConstants.AOCipherAlgorithm.TRIPLEDES.getName(),
    };

    // Constantes de los mecanismos de cifrado
    private final List<String> mecanismos = new ArrayList<>(
		Arrays.asList(
			AOCipherConstants.KEY_MODE_STRING,
			AOCipherConstants.KEY_MODE_USERINPUT
		)
	);

    public Cifrado() {
        initComponents();
    }

    /** Pulsar boton cifrar: Cifra el archivo seleccionado con la configuracion seleccionada
     * @param comboMecanismo Combo con el mecanismo de cifrado
     * @param comboAlgoritmo Combo con el algoritmo de cifrado
     * @param campoFichero Campo con el nombre del fichero a cifrar */
    void cifrarActionPerformed(final JComboBox<String> comboMecanismo, final JComboBox<String> comboAlgoritmo, final JTextField campoFichero) {
        String algoritmo;
        final String mecanismo = this.mecanismos.get(comboMecanismo.getSelectedIndex());
        if (mecanismo.equals(AOCipherConstants.KEY_MODE_STRING)) {
            algoritmo = this.algoritmoVc[comboAlgoritmo.getSelectedIndex()];
        }
        else {
            algoritmo = this.algoritmoVr[comboAlgoritmo.getSelectedIndex()];
        }

        // Sacamos la ruta del archivo
        if (campoFichero.getText() == null || campoFichero.getText().equals("")) { //$NON-NLS-1$
            CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
                                           true,
                                           Messages.getString("Cifrado.msg.error.fichero"), Messages.getString("Cifrado.msg.titulo"), JOptionPane.WARNING_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            campoFichero.requestFocusInWindow(); // Foco al campo que contiene el path al fichero
        }
        else {
            // Se selecciona el primer elemento del combo
            if (mecanismo.equals(AOCipherConstants.KEY_MODE_STRING)) {
                // Se muestra el asistente de cifrado con contrasena
                new AsistenteCifradoContrasenia(algoritmo, campoFichero.getText());
            }
            else {
                // Se muestra el asistente de cifrado con clave
                new AsistenteCifradoClave(algoritmo, campoFichero.getText());
            }
        }
    }

    /** Cambio de seleccion en el combo de los mecanismos
     * @param comboMecanismo Combo que contiene el listado de mecanismos de cifrado
     * @param comboAlgoritmo Combo que contiene el listado de algoritmos */
    void comboMecanismoItemStateChanged(final JComboBox<String> comboMecanismo, final JComboBox<String> comboAlgoritmo) {
        final String mecanismo = this.mecanismos.get(comboMecanismo.getSelectedIndex());
        if (mecanismo.equals(AOCipherConstants.KEY_MODE_STRING)) {
            comboAlgoritmo.setModel(new DefaultComboBoxModel<>(this.algoritmoLc.toArray(new String[0])));
        }
        else {
            final String[] algoritmoLr = new String[] {
               "Advanced Encryption Standard (AES)", //$NON-NLS-1$
               "Blowfish", //$NON-NLS-1$
               "Data Encryption Standard (DES)", //$NON-NLS-1$
               "Triple DES (3DES)", //$NON-NLS-1$
            };
            comboAlgoritmo.setModel(new DefaultComboBoxModel<>(algoritmoLr));
        }
    }

    /** Pulsar boton examinar: Muestra una ventana para seleccinar un archivo.
     * Modifica el valor de la caja con el nombre del archivo seleccionado
     * @param campoFichero Campo en el que se escribe el nombre del fichero seleccionado */
    void examinarActionPerformed(final JTextField campoFichero) {
        final File selectedFile = SelectionDialog.showFileOpenDialog(this, Messages.getString("Cifrado.browse.data.file"), Main.getPreferences().get("dialog.load.dir", null)); //$NON-NLS-1$ //$NON-NLS-2$
        if (selectedFile != null) {
            campoFichero.setText(selectedFile.getAbsolutePath());
        }
    }

    /** Iniciamos componentes */
    private void initComponents() {
        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(13, 13, 0, 13);
        c.weightx = 1.0;
        c.gridwidth = 2;
        c.gridx = 0;

        // Etiqueta abrir fichero
        final JLabel etiquetaFichero = new JLabel();
        etiquetaFichero.setText(Messages.getString("Cifrado.buscar")); // NOI18N //$NON-NLS-1$
        etiquetaFichero.getAccessibleContext().setAccessibleDescription(Messages.getString("Cifrado.buscar.description")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaFichero);
        Utils.setFontBold(etiquetaFichero);
        add(etiquetaFichero, c);

        c.insets = new Insets(0, 13, 0, 0);
        c.gridwidth = 1;
        c.gridy = 1;

        // Caja con el nombre del archivo seleccionado
        final JTextField campoFichero = new JTextField();
        campoFichero.setToolTipText(Messages.getString("Cifrado.buscar.caja.description")); // NOI18N //$NON-NLS-1$
        campoFichero.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                          Messages.getString("Cifrado.buscar.caja.description.status"))); //$NON-NLS-1$
        campoFichero.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                          Messages.getString("Cifrado.buscar.caja.description.status"))); //$NON-NLS-1$
        campoFichero.getAccessibleContext().setAccessibleName(etiquetaFichero.getText() + " ALT + T."); // NOI18N //$NON-NLS-1$
        campoFichero.getAccessibleContext().setAccessibleDescription(Messages.getString("Cifrado.buscar.caja.description")); // NOI18N //$NON-NLS-1$
        if (GeneralConfig.isBigCaret()) {
            final Caret caret = new ConfigureCaret();
            campoFichero.setCaret(caret);
        }
        Utils.remarcar(campoFichero);
        Utils.setFontBold(campoFichero);
        campoFichero.addAncestorListener(new RequestFocusListener(false));
        add(campoFichero, c);

        // Relacion entre etiqueta y campo de texto
        etiquetaFichero.setLabelFor(campoFichero);
        // Asignacion de mnemonico
        etiquetaFichero.setDisplayedMnemonic(KeyEvent.VK_T);

        c.insets = new Insets(0, 10, 0, 13);
        c.weightx = 0.0;
        c.gridx = 1;

        final JPanel panelExaminar = new JPanel(new GridLayout(1, 1));
        // Boton seleccionar
        final JButton examinar = new JButton();
        examinar.setMnemonic(KeyEvent.VK_E);
        examinar.setText(Messages.getString("PrincipalGUI.Examinar")); // NOI18N //$NON-NLS-1$
        examinar.setToolTipText(Messages.getString("PrincipalGUI.Examinar.description")); // NOI18N //$NON-NLS-1$

        examinar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.Examinar.description")); // NOI18N //$NON-NLS-1$
        examinar.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                      Messages.getString("PrincipalGUI.Examinar.description.status"))); //$NON-NLS-1$
        examinar.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                      Messages.getString("PrincipalGUI.Examinar.description.status"))); //$NON-NLS-1$
        examinar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                examinarActionPerformed(campoFichero);
            }
        });

        Utils.remarcar(examinar);
        Utils.setContrastColor(examinar);
        Utils.setFontBold(examinar);
        panelExaminar.add(examinar);
        add(panelExaminar, c);

        // Espacio en blanco
        final JPanel emptyPanel01 = new JPanel();
        emptyPanel01.setPreferredSize(new Dimension(1, 1));
        c.weightx = 1.0;
        c.weighty = 0.2;
        c.gridwidth = 3;
        c.gridx = 0;
        c.gridy = 2;
        c.insets = new Insets(0, 0, 0, 0);
        add(emptyPanel01, c);

        c.insets = new Insets(13, 13, 0, 13);
        c.weighty = 0.0;
        c.weightx = 1.0;
        c.gridwidth = 2;
        c.gridx = 0;
        c.gridy = 3;

        // Etiqueta mecanismo cifrado
        final JLabel etiquetaMecanismo = new JLabel();
        etiquetaMecanismo.setText(Messages.getString("Cifrado.origen.clave")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaMecanismo);
        Utils.setFontBold(etiquetaMecanismo);
        add(etiquetaMecanismo, c);

        c.insets = new Insets(0, 13, 0, 13);
        c.gridy = 4;
        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;

        // Combo mecanismos de cifrado
        final JComboBox<String> comboMecanismo = new JComboBox<>();
        final JComboBox<String> comboAlgoritmo = new JComboBox<>();
        comboMecanismo.setToolTipText(Messages.getString("Cifrado.origen.clave.combo.description")); // NOI18N //$NON-NLS-1$
        comboMecanismo.addMouseListener(
    		new ElementDescriptionMouseListener(
				PrincipalGUI.getBar(),
                Messages.getString("Cifrado.origen.clave.combo.description.status") //$NON-NLS-1$
			)
		);
        comboMecanismo.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                            Messages.getString("Cifrado.origen.clave.combo.description.status"))); //$NON-NLS-1$
        comboMecanismo.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(final ItemEvent evt) {
                comboMecanismoItemStateChanged(comboMecanismo, comboAlgoritmo);
            }
        });

        comboMecanismo.getAccessibleContext().setAccessibleDescription(Messages.getString("Cifrado.origen.clave.combo.description")); // NOI18N //$NON-NLS-1$
        comboMecanismo.setModel(
    		new DefaultComboBoxModel<>(
        		new String[] {
    				Messages.getString("Cifrado.origenL.0"), //$NON-NLS-1$
    				Messages.getString("Cifrado.origenL.1") //$NON-NLS-1$
        		}
    		)
		);
        Utils.remarcar(comboMecanismo);
        Utils.setContrastColor(comboMecanismo);
        Utils.setFontBold(comboMecanismo);
        add(comboMecanismo, c);

        // En la vista simple, no se permitira configurar el origen de la clave
        if (!GeneralConfig.isAvanzados()) {
            comboMecanismo.setEnabled(false); // Se deshabilita la opcion
            // Opciones para el lector de pantalla
            etiquetaMecanismo.setFocusable(true);
            Utils.remarcar(etiquetaMecanismo);
            etiquetaMecanismo.getAccessibleContext()
            .setAccessibleName(etiquetaMecanismo.getText() + " " + Messages.getString("Cifrado.origen.clave.combo.defaultOpcion")  //$NON-NLS-1$//$NON-NLS-2$
                               + Messages.getString("Cifrado.origenL.0") + " " + Messages.getString("Cifrado.origen.clave.combo.disabled")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
        else {
            // Para la vista avanzada se asigna mnemonico puesto que esta opcion estara habilitada
            // Relacion entre etiqueta y combo
            etiquetaMecanismo.setLabelFor(comboMecanismo);
            // Asignacion de mnemonico
            etiquetaMecanismo.setDisplayedMnemonic(KeyEvent.VK_A);
        }

        // Espacio en blanco
        final JPanel emptyPanel02 = new JPanel();
        emptyPanel02.setPreferredSize(new Dimension(1, 1));
        c.weightx = 1.0;
        c.weighty = 0.2;
        c.gridwidth = 3;
        c.gridx = 0;
        c.gridy = 5;
        c.insets = new Insets(0, 0, 0, 0);
        add(emptyPanel02, c);

        c.insets = new Insets(13, 13, 0, 13);
        c.weightx = 1.0;
        c.gridy = 6;
        c.weighty = 0.0;
        c.fill = GridBagConstraints.HORIZONTAL;

        // Etiqueta algoritmos de cifrado
        final JLabel etiquetaAlgoritmo = new JLabel();
        etiquetaAlgoritmo.setText(Messages.getString("Cifrado.formato")); // NOI18N //$NON-NLS-1$
        etiquetaAlgoritmo.getAccessibleContext().setAccessibleDescription(Messages.getString("Cifrado.formato.description")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaAlgoritmo);
        Utils.setFontBold(etiquetaAlgoritmo);
        add(etiquetaAlgoritmo, c);

        c.insets = new Insets(0, 13, 0, 13);
        c.gridy = 7;
        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;

        // Combo algoritmos de cifrado
        comboAlgoritmo.setToolTipText(Messages.getString("Cifrado.formato.combo.description")); // NOI18N //$NON-NLS-1$
        comboAlgoritmo.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                            Messages.getString("Cifrado.formato.combo.description.status"))); //$NON-NLS-1$
        comboAlgoritmo.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                            Messages.getString("Cifrado.formato.combo.description.status"))); //$NON-NLS-1$

        comboAlgoritmo.getAccessibleContext().setAccessibleDescription(Messages.getString("Cifrado.formato.combo.description")); // NOI18N //$NON-NLS-1$
        comboAlgoritmo.setModel(new DefaultComboBoxModel<>(this.algoritmoLc.toArray(new String[0])));
        Utils.remarcar(comboAlgoritmo);
        Utils.setContrastColor(comboAlgoritmo);
        Utils.setFontBold(comboAlgoritmo);
        add(comboAlgoritmo, c);

        // En la vista simple, no se permitira configurar el algoritmo de cifrado
        if (!GeneralConfig.isAvanzados()) {
            comboAlgoritmo.setEnabled(false); // Se deshabilita la opcion
            // Opciones para el lector de pantalla
            etiquetaAlgoritmo.setFocusable(true);
            Utils.remarcar(etiquetaAlgoritmo);
            etiquetaAlgoritmo.getAccessibleContext()
            .setAccessibleName(etiquetaAlgoritmo.getText() + " " + Messages.getString("Cifrado.origen.clave.combo.defaultOpcion")  //$NON-NLS-1$//$NON-NLS-2$
                               + Messages.getString("Cifrado.origenLc.0") + " " + Messages.getString("Cifrado.origen.clave.combo.disabled")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
        else {
            // Para la vista avanzada se asigna mnemonico puesto que esta opcion estara habilitada
            // Relacion entre etiqueta y combo
            etiquetaAlgoritmo.setLabelFor(comboAlgoritmo);
            // Asignacion de mnemonico
            etiquetaAlgoritmo.setDisplayedMnemonic(KeyEvent.VK_G);
        }

        c.weighty = 1.0;
        c.gridy = 8;
        c.gridheight = 4;
        c.fill = GridBagConstraints.HORIZONTAL;

        // Panel vacio para alinear el boton de aceptar en la parte de abajo de la pantalla
        final JPanel emptyPanel = new JPanel();
        add(emptyPanel, c);

        // Panel con los botones
        final JPanel panelBotones = new JPanel(new GridBagLayout());

        final GridBagConstraints cons = new GridBagConstraints();
        cons.anchor = GridBagConstraints.FIRST_LINE_START; // control de la orientacion de componentes al redimensionar
        cons.fill = GridBagConstraints.HORIZONTAL;
        cons.ipadx = 15;
        cons.gridx = 0;

        // Etiqueta para rellenar a la izquierda
        final JLabel label = new JLabel();
        panelBotones.add(label, cons);

        final JPanel panelCifrar = new JPanel(new GridLayout(1, 1));
        // Boton cifrar
        final JButton cifrar = new JButton();
        cifrar.setMnemonic(KeyEvent.VK_R);
        cifrar.setText(Messages.getString("Cifrado.btncifrar")); // NOI18N //$NON-NLS-1$
        cifrar.setToolTipText(Messages.getString("Cifrado.btncifrar.description")); // NOI18N //$NON-NLS-1$
        cifrar.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(), Messages.getString("Cifrado.btncifrar.description.status"))); //$NON-NLS-1$
        cifrar.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(), Messages.getString("Cifrado.btncifrar.description.status"))); //$NON-NLS-1$
        cifrar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                cifrarActionPerformed(comboMecanismo, comboAlgoritmo, campoFichero);
                PrincipalGUI.setNuevoEstado(Messages.getString("Cifrado.statusbar.cipher.action")); //$NON-NLS-1$
            }
        });

        cifrar.getAccessibleContext().setAccessibleDescription(Messages.getString("Cifrado.btncifrar.description")); // NOI18N //$NON-NLS-1$
        Utils.remarcar(cifrar);
        Utils.setContrastColor(cifrar);
        Utils.setFontBold(cifrar);
        panelCifrar.add(cifrar);
        final JPanel buttonPanel = new JPanel();
        buttonPanel.add(panelCifrar, BorderLayout.CENTER);

        cons.ipadx = 0;
        cons.gridx = 1;
        cons.weightx = 1.0;

        panelBotones.add(buttonPanel, cons);

        final JPanel panelAyuda = new JPanel();
        // Boton ayuda
        final JButton botonAyuda = HelpUtils.helpButton("cifrado"); //$NON-NLS-1$
        botonAyuda.setName("helpButton"); //$NON-NLS-1$

        cons.ipadx = 15;
        cons.weightx = 0.0;
        cons.gridx = 2;
        panelAyuda.add(botonAyuda);
        panelBotones.add(panelAyuda, cons);

        c.gridwidth = 2;
        c.insets = new Insets(13, 13, 13, 13);
        c.weightx = 1.0;
        c.weighty = 0.0;
        c.gridy = 12;

        add(panelBotones, c);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(campoFichero, "cifrado.fichero"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(examinar, "cifrado.fichero"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(comboMecanismo, "cifrado.mecanismo"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(comboAlgoritmo, "cifrado.algoritmo"); //$NON-NLS-1$
    }
}

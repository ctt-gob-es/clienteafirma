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
import es.gob.afirma.ui.wizarddescifradoclave.AsistenteDescifradoClave;
import es.gob.afirma.ui.wizarddescifradocontrasenia.AsistenteDescifradoContrasenia;

/** Clase Descifrado que se encarga de descifrar un fichero cifrado. */
final class Descifrado extends JPanel {

    // Algoritmos para mecanismo contrasena de cifrado
    private static final List<String> ALGORITMO_LC = new ArrayList<>(
		Arrays.asList(
			"Contrase\u00F1a con SHA1 y 3DES",
            "Contrase\u00F1a con SHA1 y RC2",
			"Contrase\u00F1a con MD5 y DES"
        )
    );

    // Algoritmos para mecanismo clave de cifrado
    private static final List<String> ALGORITMO_LR = new ArrayList<>(
		Arrays.asList(
			"Advanced Encryption Standard (AES)",
            "Blowfish",
            "Data Encryption Standard (DES)",
            "Triple DES (3DES)"
        )
    );

    private static final List<String> ALGORITMO_VC = new ArrayList<>(
		Arrays.asList(
			"PBEWithSHA1AndDESede", //$NON-NLS-1$
			"PBEWithSHA1AndRC2_40", //$NON-NLS-1$
			"PBEWithMD5AndDES" //$NON-NLS-1$
		)
	);
    private static final List<String> ALGORITMO_VR = new ArrayList<>(Arrays.asList("AES", "ARCFOUR", "Blowfish", "DES", "DESede", "RC2")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$

    // Origen de la clave
    private static final List<String> MECANISMOS = new ArrayList<>(Arrays.asList("PASSWORD", "USERINPUT")); //$NON-NLS-1$//$NON-NLS-2$
    private static final long serialVersionUID = 1L;

    public Descifrado() {
        initComponents();
    }

    /** Cambio de seleccion en el combo de los MECANISMOS
     * @param comboMecanismo Combo que contiene el listado de MECANISMOS de cifrado
     * @param comboAlgoritmo Combo que contiene el listado de algoritmos */
    static void comboMecanismoItemStateChanged(final JComboBox<String> comboMecanismo, final JComboBox<String> comboAlgoritmo) {
        final String mecanismo = MECANISMOS.get(comboMecanismo.getSelectedIndex());
        if (mecanismo.equals("PASSWORD")) { //$NON-NLS-1$
            comboAlgoritmo.setModel(new DefaultComboBoxModel<>(ALGORITMO_LC.toArray(new String[0])));
        }
        else {
            comboAlgoritmo.setModel(new DefaultComboBoxModel<>(ALGORITMO_LR.toArray(new String[0])));
        }
    }

    /** Pulsar boton descifrar: Descifra el archivo seleccionado con la configuracion seleccionada
     * @param comboMecanismo Combo con el mecanismo de cifrado
     * @param comboAlgoritmo Combo con el algoritmo de cifrado
     * @param campoFichero Campo con el nombre del fichero a cifrar */
    void descifrarActionPerformed(final JComboBox<String> comboMecanismo, final JComboBox<String> comboAlgoritmo, final JTextField campoFichero) {
        final String algoritmo;
        final String mecanismo = MECANISMOS.get(comboMecanismo.getSelectedIndex());
        if (mecanismo.equals("PASSWORD")) { //$NON-NLS-1$
            algoritmo = ALGORITMO_VC.get(comboAlgoritmo.getSelectedIndex());
        }
        else {
            algoritmo = ALGORITMO_VR.get(comboAlgoritmo.getSelectedIndex());
        }

        // Sacamos la ruta del archivo
        if (campoFichero.getText() == null || campoFichero.getText().isEmpty()) {
            CustomDialog.showMessageDialog(
        		SwingUtilities.getRoot(this),
                true,
                Messages.getString("Descifrado.msg.error.fichero"), //$NON-NLS-1$
                Messages.getString("Descifrado.msg.titulo"), //$NON-NLS-1$
                JOptionPane.WARNING_MESSAGE
            );
            campoFichero.requestFocusInWindow(); // Foco al campo que contiene el path al fichero
        }
        else
            // Se selecciona el primer elemento del combo
            if (mecanismo.equals("PASSWORD")) { //$NON-NLS-1$
                // Se muestra el asistente de descifrado con contrasena
                new AsistenteDescifradoContrasenia(algoritmo, campoFichero.getText());
            }
            else {
                // Se muestra el asistente de descifrado con clave
                new AsistenteDescifradoClave(algoritmo, campoFichero.getText());
            }
    }

    /** Boton examinar pulsado: Se muestra una ventana para seleccionar un fichero.
     * El nombre del fichero seleccionado se guardara en el campo.
     * @param campoFichero Campo donde se guarda el nombre del fichero seleccionado */
    void examinarActionPerformed(final JTextField campoFichero) {
        final File selectedFile = SelectionDialog.showFileOpenDialog(this, Messages.getString("Seleccione.fichero.descifrar"), Main.getPreferences().get("dialog.load.dir", null)); //$NON-NLS-1$ //$NON-NLS-2$
        if (selectedFile != null) {
            campoFichero.setText(selectedFile.getAbsolutePath());
        }
    }

    /** Inicializacion de componentes */
    private void initComponents() {
        // Eliminamos el layout
        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(13, 13, 0, 13);
        c.weightx = 1.0;
        c.gridwidth = 2;
        c.gridx = 0;

        // Etiqueta fichero a descrifrar
        final JLabel etiquetaFichero = new JLabel();
        etiquetaFichero.setText(Messages.getString("Descifrado.buscar")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaFichero);
        Utils.setFontBold(etiquetaFichero);
        add(etiquetaFichero, c);

        c.insets = new Insets(0, 13, 0, 0);
        c.gridwidth = 1;
        c.gridy = 1;

        // Caja con el nombre del archivo seleccionado
        final JTextField campoFichero = new JTextField();
        campoFichero.setToolTipText(Messages.getString("Descifrado.buscar.caja.description")); // NOI18N //$NON-NLS-1$
        campoFichero.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(), Messages.getString("Descifrado.buscar.caja.description"))); //$NON-NLS-1$
        campoFichero.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(), Messages.getString("Descifrado.buscar.caja.description"))); //$NON-NLS-1$
        campoFichero.getAccessibleContext().setAccessibleName(etiquetaFichero.getText() + " ALT + E."); // NOI18N //$NON-NLS-1$
        campoFichero.getAccessibleContext().setAccessibleDescription(Messages.getString("Cifrado.buscar.caja.description")); // NOI18N //$NON-NLS-1$
        campoFichero.addAncestorListener(new RequestFocusListener(false));

        if (GeneralConfig.isBigCaret()) {
            final Caret caret = new ConfigureCaret();
            campoFichero.setCaret(caret);
        }
        Utils.remarcar(campoFichero);
        Utils.setContrastColor(campoFichero);
        Utils.setFontBold(campoFichero);
        add(campoFichero, c);

        // Relacion entre etiqueta y campo de texto
        etiquetaFichero.setLabelFor(campoFichero);
        // Asignacion de mnemonico
        etiquetaFichero.setDisplayedMnemonic(KeyEvent.VK_E);

        c.insets = new Insets(0, 10, 0, 13);
        c.weightx = 0.0;
        c.gridx = 1;

        final JPanel panelExaminar = new JPanel(new GridLayout(1, 1));
        // Boton examinar
        final JButton examinar = new JButton();
        examinar.setMnemonic(KeyEvent.VK_X);
        examinar.setText(Messages.getString("PrincipalGUI.Examinar")); // NOI18N //$NON-NLS-1$
        examinar.setToolTipText(Messages.getString("PrincipalGUI.Examinar.description")); // NOI18N //$NON-NLS-1$
        examinar.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(), Messages.getString("PrincipalGUI.Examinar.description"))); //$NON-NLS-1$
        examinar.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(), Messages.getString("PrincipalGUI.Examinar.description"))); //$NON-NLS-1$
        examinar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                examinarActionPerformed(campoFichero);
            }
        });
        // examinar.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.Examinar") + " " +
        // Messages.getString("PrincipalGUI.Examinar.description"));
        examinar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.Examinar.description")); // NOI18N //$NON-NLS-1$

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

        // Etiqueta MECANISMOS de descifrado
        final JLabel etiquetaMecanismo = new JLabel();
        etiquetaMecanismo.setText(Messages.getString("Descifrado.origen.clave")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaMecanismo);
        Utils.setFontBold(etiquetaMecanismo);
        add(etiquetaMecanismo, c);

        c.insets = new Insets(0, 13, 0, 13);
        c.gridy = 4;
        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;

        // Combo MECANISMOS de cifrado
        final JComboBox<String> comboMecanismo = new JComboBox<>();
        final JComboBox<String> comboAlgoritmo = new JComboBox<>();
        comboMecanismo.setToolTipText(Messages.getString("Descifrado.origen.clave.combo.description")); // NOI18N //$NON-NLS-1$

        // comboMecanismo.getAccessibleContext().setAccessibleName(Messages.getString("Cifrado.origen.clave") +" "+
        // Messages.getString("Descifrado.origen.clave.combo.description")+" ALT + A."); // NOI18N
        comboMecanismo.getAccessibleContext().setAccessibleDescription(Messages.getString("Cifrado.origen.clave.combo.description")); // NOI18N //$NON-NLS-1$
        comboMecanismo.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                            Messages.getString("Descifrado.origen.clave.combo.description"))); //$NON-NLS-1$
        comboMecanismo.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                            Messages.getString("Descifrado.origen.clave.combo.description"))); //$NON-NLS-1$
        comboMecanismo.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(final ItemEvent evt) {
                comboMecanismoItemStateChanged(comboMecanismo, comboAlgoritmo);
            }
        });
        comboMecanismo.setModel(
    		new DefaultComboBoxModel<>(
				new String[] {
					Messages.getString("Descifrado.origenL.0"), //$NON-NLS-1$
					Messages.getString("Descifrado.origenL.1") //$NON-NLS-1$
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

        // Etiqueta algoritmos de descifrado
        final JLabel etiquetaAlgoritmo = new JLabel();
        etiquetaAlgoritmo.setText(Messages.getString("Descifrado.formato")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaAlgoritmo);
        Utils.setFontBold(etiquetaAlgoritmo);
        add(etiquetaAlgoritmo, c);

        c.insets = new Insets(0, 13, 0, 13);
        c.gridy = 7;
        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;

        // Combo con los algoritmos de descifrado
        comboAlgoritmo.setModel(new DefaultComboBoxModel<>(ALGORITMO_LC.toArray(new String[0])));
        comboAlgoritmo.setToolTipText(Messages.getString("Descifrado.formato.combo.description")); // NOI18N //$NON-NLS-1$
        comboAlgoritmo.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                            Messages.getString("Descifrado.formato.combo.description"))); //$NON-NLS-1$
        comboAlgoritmo.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                            Messages.getString("Descifrado.formato.combo.description"))); //$NON-NLS-1$
        comboAlgoritmo.getAccessibleContext().setAccessibleDescription(Messages.getString("Descifrado.formato.combo.description")); // NOI18N //$NON-NLS-1$

        Utils.remarcar(comboAlgoritmo);
        Utils.setContrastColor(comboAlgoritmo);
        Utils.setFontBold(comboAlgoritmo);
        add(comboAlgoritmo, c);

        // En la vista simple, no se permitira configurar el algoritmo de descifrado
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

        final JPanel panelDescifrar = new JPanel(new GridLayout(1, 1));
        // Boton descifrar
        final JButton descifrar = new JButton();
        descifrar.setMnemonic(KeyEvent.VK_R);
        descifrar.setText(Messages.getString("Descifrado.btndescifrar")); // NOI18N //$NON-NLS-1$
        descifrar.setToolTipText(Messages.getString("Descifrado.btndescifrar.description")); // NOI18N //$NON-NLS-1$
        descifrar.setMaximumSize(null);
        descifrar.setMinimumSize(null);
        descifrar.setPreferredSize(null);
        descifrar.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(), Messages.getString("Desensobrado.btnDescifrar.description"))); //$NON-NLS-1$
        descifrar.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(), Messages.getString("Desensobrado.btnDescifrar.description"))); //$NON-NLS-1$
        descifrar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                descifrarActionPerformed(comboMecanismo, comboAlgoritmo, campoFichero);
            }
        });
        // descifrar.getAccessibleContext().setAccessibleName(descifrar.getText()+ " " + Messages.getString("Desensobrado.btnDescifrar.description"));
        // // NOI18N
        descifrar.getAccessibleContext().setAccessibleDescription(Messages.getString("Desensobrado.btnDescifrar.description")); // NOI18N //$NON-NLS-1$
        Utils.remarcar(descifrar);
        Utils.setContrastColor(descifrar);
        Utils.setFontBold(descifrar);

        panelDescifrar.add(descifrar);
        final JPanel buttonPanel = new JPanel();
        buttonPanel.add(panelDescifrar, BorderLayout.CENTER);

        cons.ipadx = 0;
        cons.gridx = 1;
        cons.weightx = 1.0;

        panelBotones.add(buttonPanel, cons);

        final JPanel panelAyuda = new JPanel();
        // Boton ayuda
        final JButton botonAyuda = HelpUtils.helpButton("descifrado"); //$NON-NLS-1$
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
        HelpUtils.enableHelpKey(campoFichero, "descifrado.fichero"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(examinar, "descifrado.fichero"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(comboMecanismo, "descifrado.mecanismo"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(comboAlgoritmo, "descifrado.algoritmo"); //$NON-NLS-1$
    }
}

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
import javax.swing.SwingUtilities;
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

final class Ensobrado extends JPanel {

    private static final long serialVersionUID = 1L;

    // Checkbox con texto "Anadir nuevos remitentes al sobre
    final JCheckBox checkAnadir = new JCheckBox();

    // Combo de tipos de sobre digital
    final JComboBox<String> comboTipos = new JComboBox<>();
    Ensobrado() {
        initComponents();
    }

    /** Carga el combo de opciones con las diferentes opciones */
    private void cargarComboTipos() {
        final List<String> opciones = new ArrayList<>();
        opciones.add(Messages.getString("Ensobrado.combo.autenticado")); //$NON-NLS-1$
        opciones.add(Messages.getString("Ensobrado.combo.firmado")); //$NON-NLS-1$
        opciones.add(Messages.getString("Ensobrado.combo.simple")); //$NON-NLS-1$

        this.comboTipos.setModel(new DefaultComboBoxModel<>(opciones.toArray(new String[0])));
    }

    /** Pulsar boton examinar: Muestra una ventana para seleccinar un archivo.
     * Modifica el valor de la caja con el nombre del archivo seleccionado
     * @param campoFichero Campo en el que se escribe el nombre del fichero seleccionado */
    void examinarActionPerformed(final JTextField campoFichero) {
        final File selectedFile = SelectionDialog.showFileOpenDialog(this, Messages.getString("Seleccione.fichero.ensobrar"), Main.getPreferences().get("dialog.load.dir.wrap", null)); //$NON-NLS-1$ //$NON-NLS-2$
        if (selectedFile != null) {
            campoFichero.setText(selectedFile.getAbsolutePath());
            Main.getPreferences().put("dialog.load.dir.wrap", selectedFile.getAbsolutePath()); //$NON-NLS-1$
        }
    }

    /** Ensobra el archivo seleccionado con las opciones indicadas
     * @param campoFichero Campo con el nombre del fichero a ensobrar */
    void generarActionPerformed(final JTextField campoFichero) {
        if (campoFichero.getText() == null || campoFichero.getText().equals("")) { //$NON-NLS-1$
            CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
                                           true,
                                           Messages.getString("Ensobrado.msg.error.fichero"), Messages.getString("Ensobrado.msg.titulo"), JOptionPane.WARNING_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            campoFichero.requestFocusInWindow(); // Foco al campo que contiene el path al fichero
        }
        else {
            // Mensaje que indica que se va a realizar el proceso de firma y que puede llevar un tiempo
            CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
                                           true,
                                           Messages.getString("Firma.msg.info"), Messages.getString("PrincipalGUI.TabConstraints.tabTitleEnsobrado"), JOptionPane.INFORMATION_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$

            if (this.checkAnadir.isSelected()) {
                // Se muestra el asistente de anadir nuevos remitentes
                new AsistenteRemitentes(campoFichero.getText());
            }
            else {
                // Se muestra el asistente
            	Main.getPreferences().put("envelop.combo.contenttype", this.comboTipos.getSelectedItem().toString()); //$NON-NLS-1$
                new AsistenteEnsobrar(campoFichero.getText(), this.comboTipos.getSelectedIndex());
            }
        }
    }

    /** Inicializacion de los componentes */
    private void initComponents() {
        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(13, 13, 0, 13);
        c.weightx = 1.0;
        c.gridwidth = 2;
        c.gridx = 0;

        // Etiqueta fichero a ensobrar digitalmente
        final JLabel etiquetaFichero = new JLabel();
        etiquetaFichero.setText(Messages.getString("Ensobrado.buscar")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaFichero);
        Utils.setFontBold(etiquetaFichero);
        add(etiquetaFichero, c);

        c.insets = new Insets(0, 13, 0, 0);
        c.gridwidth = 1;
        c.gridy = 1;

        // Campo donde se guarda el nombre del archivo a ensobrar
        final JTextField campoFichero = new JTextField();
        campoFichero.setToolTipText(Messages.getString("Ensobrado.buscar.caja.descripcion")); // NOI18N //$NON-NLS-1$
        campoFichero.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(), Messages.getString("Ensobrado.buscar.caja.descripcion"))); //$NON-NLS-1$
        campoFichero.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(), Messages.getString("Ensobrado.buscar.caja.descripcion"))); //$NON-NLS-1$
        campoFichero.getAccessibleContext().setAccessibleName(etiquetaFichero.getText() + " ALT + O."); // NOI18N //$NON-NLS-1$
        campoFichero.getAccessibleContext().setAccessibleDescription(Messages.getString("Ensobrado.buscar.caja.descripcion")); // NOI18N //$NON-NLS-1$
        campoFichero.addAncestorListener(new RequestFocusListener(false));
        if (GeneralConfig.isBigCaret()) {
            final Caret caret = new ConfigureCaret();
            campoFichero.setCaret(caret);
        }
        Utils.remarcar(campoFichero);
        Utils.setFontBold(campoFichero);
        add(campoFichero, c);

        // Relacion entre etiqueta y campo de texto
        etiquetaFichero.setLabelFor(campoFichero);
        // Asignacion de mnemonico
        etiquetaFichero.setDisplayedMnemonic(KeyEvent.VK_O);

        c.insets = new Insets(0, 10, 0, 13);
        c.weightx = 0.0;
        c.gridx = 1;

        final JPanel panelExaminar = new JPanel(new GridLayout(1, 1));
        // Boton examinar
        final JButton examinar = new JButton();
        examinar.setMnemonic(KeyEvent.VK_E);
        examinar.setText(Messages.getString("PrincipalGUI.Examinar")); // NOI18N //$NON-NLS-1$
        examinar.setToolTipText(Messages.getString("PrincipalGUI.Examinar.description")); // NOI18N //$NON-NLS-1$
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

        examinar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.Examinar.description")); // NOI18N //$NON-NLS-1$
        Utils.remarcar(examinar);
        Utils.setContrastColor(examinar);
        Utils.setFontBold(examinar);

        panelExaminar.add(examinar);
        add(panelExaminar, c);

        c.insets = new Insets(5, 13, 0, 13);
        c.weightx = 1.0;
        c.gridwidth = 2;
        c.gridx = 0;
        c.gridy = 2;

        // Checkbox con texto "Anadir nuevos remitentes al sobre
        this.checkAnadir.setVisible(false); // Ocultada hasta comprobar la funcionalidad
        this.checkAnadir.setMnemonic(KeyEvent.VK_G);
        this.checkAnadir.setText(Messages.getString("Ensobrado.check")); // NOI18N //$NON-NLS-1$
        this.checkAnadir.setToolTipText(Messages.getString("Ensobrado.check.description")); // NOI18N //$NON-NLS-1$
        this.checkAnadir.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                              Messages.getString("Ensobrado.check.description.status"))); //$NON-NLS-1$
        this.checkAnadir.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                              Messages.getString("Ensobrado.check.description.status"))); //$NON-NLS-1$

        this.checkAnadir.getAccessibleContext().setAccessibleDescription(Messages.getString("Ensobrado.check.description")); // NOI18N //$NON-NLS-1$
        this.checkAnadir.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(final ChangeEvent arg0) {
                if (Ensobrado.this.checkAnadir.isSelected()) {
                    Ensobrado.this.comboTipos.setEnabled(false);
                }
                else {
                    Ensobrado.this.comboTipos.setEnabled(true);
                }
            }
        });
        Utils.remarcar(this.checkAnadir);
        Utils.setContrastColor(this.checkAnadir);
        Utils.setFontBold(this.checkAnadir);
        add(this.checkAnadir, c);

        // Espacio en blanco
        final JPanel emptyPanel01 = new JPanel();
        emptyPanel01.setPreferredSize(new Dimension(1, 1));
        c.weightx = 1.0;
        c.weighty = 0.2;
        c.gridwidth = 3;
        c.gridx = 0;
        c.gridy = 3;
        c.insets = new Insets(0, 0, 0, 0);
        add(emptyPanel01, c);

        c.insets = new Insets(13, 13, 0, 13);
        c.gridy = 4;
        c.weighty = 0.0;

        // Etiqueta almacen o repositorio
        final JLabel etiquetaOpciones = new JLabel();
        etiquetaOpciones.setText(Messages.getString("Ensobrado.opciones.combo")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaOpciones);
        Utils.setFontBold(etiquetaOpciones);
        add(etiquetaOpciones, c);

        c.insets = new Insets(0, 13, 0, 13);
        c.gridy = 5;
        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;

        // Combo con el almacen o repositorio de certificados
        this.comboTipos.setToolTipText(Messages.getString("Ensobrado.opciones.combo")); // NOI18N //$NON-NLS-1$
        this.comboTipos.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(), Messages.getString("Ensobrado.opciones.combo.status"))); //$NON-NLS-1$
        this.comboTipos.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(), Messages.getString("Ensobrado.opciones.combo.status"))); //$NON-NLS-1$

        this.comboTipos.getAccessibleContext().setAccessibleDescription(Messages.getString("Ensobrado.opciones.combo.description")); // NOI18N //$NON-NLS-1$
        cargarComboTipos();
        if (Main.getPreferences().get("envelop.combo.contenttype", null) != null) { //$NON-NLS-1$
        	this.comboTipos.setSelectedItem(Main.getPreferences().get("envelop.combo.contenttype", null)); //$NON-NLS-1$
        }

        Utils.remarcar(this.comboTipos);
        Utils.setContrastColor(this.comboTipos);
        Utils.setFontBold(this.comboTipos);
        add(this.comboTipos, c);

        // Relacion entre etiqueta y combo
        etiquetaOpciones.setLabelFor(this.comboTipos);
        // Asignacion de mnemonico
        etiquetaOpciones.setDisplayedMnemonic(KeyEvent.VK_T);

        c.weighty = 1.0;
        c.gridy = 6;
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

        final JPanel panelGenerar = new JPanel(new GridLayout(1, 1));
        // Boton generar
        final JButton generar = new JButton();
        generar.setMnemonic(KeyEvent.VK_G);
        generar.setText(Messages.getString("Ensobrado.btnGenerar")); // NOI18N //$NON-NLS-1$
        generar.setToolTipText(Messages.getString("Ensobrado.btnGenerar.description")); // NOI18N //$NON-NLS-1$
        generar.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(), Messages.getString("Ensobrado.btnGenerar.description.status"))); //$NON-NLS-1$
        generar.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(), Messages.getString("Ensobrado.btnGenerar.description.status"))); //$NON-NLS-1$
        generar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                generarActionPerformed(campoFichero);
                PrincipalGUI.setNuevoEstado(Messages.getString("Ensobrado.btnGenerar.generado")); //$NON-NLS-1$
            }
        });

        generar.getAccessibleContext().setAccessibleDescription(Messages.getString("Ensobrado.btnGenerar.description")); // NOI18N //$NON-NLS-1$
        Utils.remarcar(generar);
        Utils.setContrastColor(generar);
        Utils.setFontBold(generar);

        panelGenerar.add(generar);
        final JPanel buttonPanel = new JPanel();
        buttonPanel.add(panelGenerar, BorderLayout.CENTER);

        cons.ipadx = 0;
        cons.gridx = 1;
        cons.weightx = 1.0;

        panelBotones.add(buttonPanel, cons);

        final JPanel panelAyuda = new JPanel();
        // Boton ayuda
        final JButton botonAyuda = HelpUtils.helpButton("ensobrado"); //$NON-NLS-1$
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
        c.gridy = 10;

        add(panelBotones, c);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(campoFichero, "ensobrado.fichero"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(examinar, "ensobrado.fichero"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.checkAnadir, "ensobrado.anadirremitentes"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.comboTipos, "ensobrado.firmarsobre"); //$NON-NLS-1$
    }
}

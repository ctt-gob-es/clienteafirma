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
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.ui.listeners.ElementDescriptionFocusListener;
import es.gob.afirma.ui.listeners.ElementDescriptionMouseListener;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.KeyStoreLoader;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.RequestFocusListener;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardmultifirmamasiva.AsistenteMultifirmaMasiva;

/** Clase para realizar una multifirma masiva */
final class MultifirmaMasiva extends JPanel {

    private static final long serialVersionUID = 1L;

    private JCheckBox alerta;

    /** @return the alerta */
    protected JCheckBox getAlerta() {
        return this.alerta;
    }

    public MultifirmaMasiva() {
        initComponents();
    }

    /** Carga el combo almac&eacute;n con los almacenes y repositorios disponibles
     * @param comboAlmacen Combo con los almacenes y repositorios */
    private static void cargarComboAlmacen(final JComboBox<KeyStoreConfiguration> comboAlmacen) {
        comboAlmacen.setModel(new DefaultComboBoxModel<>(KeyStoreLoader.getKeyStoresToSign()));
    }

    /** Firma masivamente haciendo uso del almacen / repositorio
     * @param comboAlmacen Combo con los almacenes / repositorios de certificados
     * @param alerta1 Checkbox para emitir un pitido al finalizar la operacion */
    @SuppressWarnings("unused")
    void firmarActionPerformed(final JComboBox<KeyStoreConfiguration> comboAlmacen, final JCheckBox alerta1) {

        // Mensaje que indica que se va a realizar el proceso de firma y que puede llevar un tiempo
        CustomDialog.showMessageDialog(
    		SwingUtilities.getRoot(this),
           true,
           Messages.getString("Firma.msg.info"), //$NON-NLS-1$
           Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirmaMasiva"), //$NON-NLS-1$
           JOptionPane.INFORMATION_MESSAGE
       );

        final KeyStoreConfiguration kssc = (KeyStoreConfiguration) comboAlmacen.getSelectedItem();

        // Se muestra el asistente
        new AsistenteMultifirmaMasiva(kssc, alerta1.isSelected());
    }

    /** Inicializacion de los componentes */
    private void initComponents() {
        setLayout(new GridBagLayout());

        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(13, 13, 0, 13);
        c.weightx = 1.0;
        c.gridx = 0;

        // Etiqueta almacen / repositorio de certificados
        final JLabel etiquetaAlmacen = new JLabel();
        etiquetaAlmacen.setText(Messages.getString("Firma.almacen.certificados")); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(etiquetaAlmacen);
        Utils.setFontBold(etiquetaAlmacen);
        add(etiquetaAlmacen, c);

        c.insets = new Insets(0, 13, 0, 13);
        c.gridy = 1;
        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;

        // Combo con los almacenes / repositorios disponibles
        final JComboBox<KeyStoreConfiguration> comboAlmacen = new JComboBox<>();
        comboAlmacen.setToolTipText(Messages.getString("Firma.almacen.certificados.description")); // NOI18N //$NON-NLS-1$
        // comboAlmacen.getAccessibleContext().setAccessibleName(etiquetaAlmacen.getText()+" "+Messages.getString("Firma.almacen.certificados.description")
        // + " ALT + A."); // NOI18N
        comboAlmacen.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                          Messages.getString("Firma.almacen.certificados.description.status"))); //$NON-NLS-1$
        comboAlmacen.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                          Messages.getString("Firma.almacen.certificados.description.status"))); //$NON-NLS-1$
        comboAlmacen.addAncestorListener(new RequestFocusListener(false));
        cargarComboAlmacen(comboAlmacen);
        Utils.remarcar(comboAlmacen);
        Utils.setContrastColor(comboAlmacen);
        Utils.setFontBold(comboAlmacen);
        add(comboAlmacen, c);

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

        // Relacion entre etiqueta y combo
        etiquetaAlmacen.setLabelFor(comboAlmacen);
        // Asignacion de mnemonico
        etiquetaAlmacen.setDisplayedMnemonic(KeyEvent.VK_A);

        c.insets = new Insets(13, 13, 0, 13);
        c.gridy = 3;
        c.weighty = 0.0;
        c.fill = GridBagConstraints.HORIZONTAL;
        // Panel que engloba los tipos de multifirma
        final JPanel panelTipos = new JPanel(new GridLayout());
        panelTipos.setBorder(BorderFactory.createTitledBorder(Messages.getString("PrincipalGUI.multifirma.panel.opciones"))); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(panelTipos);
        Utils.setFontBold(panelTipos);

        final JPanel panelAlerta = new JPanel(new GridLayout(1, 1));
        panelAlerta.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.multifirma.panel.opciones")); //$NON-NLS-1$
        // Checkbox alerta sonora
        this.alerta = new JCheckBox();
        this.alerta.setSelected(true);
        this.alerta.setText(Messages.getString("PrincipalGUI.multifirma.panel.opciones.timbre")); // NOI18N //$NON-NLS-1$
        this.alerta.setToolTipText(Messages.getString("Ensobrado.check.firmar.description")); // NOI18N //$NON-NLS-1$
        this.alerta.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                         Messages.getString("PrincipalGUI.multifirma.panel.opciones.timbre.description.status"))); //$NON-NLS-1$
        this.alerta.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                         Messages.getString("PrincipalGUI.multifirma.panel.opciones.timbre.description.status"))); //$NON-NLS-1$
        // alerta.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.multifirma.panel.opciones.timbre")
        // +" "+Messages.getString("PrincipalGUI.multifirma.panel.opciones.timbre.description")); // NOI18N
        this.alerta.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.multifirma.panel.opciones.timbre.description")); // NOI18N //$NON-NLS-1$
        this.alerta.setMnemonic(KeyEvent.VK_L); // Se asigna un atajo
        Utils.remarcar(this.alerta);
        Utils.setContrastColor(this.alerta);
        Utils.setFontBold(this.alerta);
        panelAlerta.add(this.alerta);
        panelTipos.add(panelAlerta);

        add(panelTipos, c);

        c.weighty = 1.0;
        c.gridheight = 4;
        c.gridy = 4;

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

        final JPanel panelFirmar = new JPanel(new GridLayout(1, 1));
        // Boton firmar
        final JButton firmar = new JButton();
        firmar.setMnemonic(KeyEvent.VK_R);
        firmar.setText(Messages.getString("PrincipalGUI.firmar")); // NOI18N //$NON-NLS-1$
        firmar.setToolTipText(Messages.getString("PrincipalGUI.firmar.description")); // NOI18N //$NON-NLS-1$
        // firmar.getAccessibleContext().setAccessibleName(Messages.getString("PrincipalGUI.firmar") + " " +
        // Messages.getString("PrincipalGUI.firmar.description.status"));
        firmar.getAccessibleContext().setAccessibleDescription(Messages.getString("PrincipalGUI.firmar.description")); // NOI18N //$NON-NLS-1$
        firmar.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                    Messages.getString("PrincipalGUI.firmar.description.status"))); //$NON-NLS-1$
        firmar.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                    Messages.getString("PrincipalGUI.firmar.description.status"))); //$NON-NLS-1$
        firmar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                firmarActionPerformed(comboAlmacen, getAlerta());
            }
        });

        Utils.remarcar(firmar);
        Utils.setContrastColor(firmar);
        Utils.setFontBold(firmar);

        cons.ipadx = 0;
        cons.weightx = 1.0;
        cons.gridx = 1;

        panelFirmar.add(firmar);
        final JPanel buttonPanel = new JPanel();
        buttonPanel.add(panelFirmar, BorderLayout.CENTER);
        panelBotones.add(buttonPanel, cons);

        cons.ipadx = 15;
        cons.weightx = 0.0;
        cons.gridx = 2;

        final JPanel panelAyuda = new JPanel();
        // Boton ayuda
        final JButton botonAyuda = HelpUtils.helpButton("firma.masiva"); //$NON-NLS-1$
        botonAyuda.setName("helpButton"); //$NON-NLS-1$

        panelAyuda.add(botonAyuda);
        panelBotones.add(panelAyuda, cons);

        c.gridwidth = 2;
        c.insets = new Insets(13, 13, 13, 13);
        c.weighty = 0.0;
        c.gridy = 8;

        add(panelBotones, c);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(comboAlmacen, "multifirma.masiva.almacen"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.alerta, "multifirma.masiva.alerta"); //$NON-NLS-1$
    }
}
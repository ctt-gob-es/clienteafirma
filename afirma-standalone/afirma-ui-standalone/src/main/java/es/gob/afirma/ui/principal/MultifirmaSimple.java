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
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.SwingUtilities;

import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.ui.listeners.ElementDescriptionFocusListener;
import es.gob.afirma.ui.listeners.ElementDescriptionMouseListener;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.KeyStoreLoader;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.RequestFocusListener;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardmultifirmacofirma.AsistenteCofirma;
import es.gob.afirma.ui.wizardmultifirmacontrafirma.AsistenteContrafirmas;

final class MultifirmaSimple extends JPanel {

    private static final long serialVersionUID = 1L;

    MultifirmaSimple() {
        initComponents();
    }

    /** Carga el combo almacen con los almacenes y repositorios disponibles
     * @param comboAlmacen Combo con los almacenes y repositorios */
    private static void cargarComboAlmacen(final JComboBox<KeyStoreConfiguration> comboAlmacen) {
        comboAlmacen.setModel(new DefaultComboBoxModel<>(KeyStoreLoader.getKeyStoresToSign()));
    }

    /** Se realiza la multifirma
     * @param comboAlmacen Combo con el almacen / repositorio de certificados
     * @param cofirma Radiobutton cofirma. No es necesario pasar el de contrafirma ya que solo existen dos */
    void firmarActionPerformed(final JComboBox<KeyStoreConfiguration> comboAlmacen, final JRadioButton cofirma) {
        // Mensaje que indica que se va a realizar el proceso de firma y que puede llevar un tiempo
        CustomDialog.showMessageDialog(SwingUtilities.getRoot(this),
                                       true,
                                       Messages.getString("Firma.msg.info"), Messages.getString("PrincipalGUI.TabConstraints.tabTitleMultifirma"), JOptionPane.INFORMATION_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$

        final KeyStoreConfiguration kssc = (KeyStoreConfiguration) comboAlmacen.getSelectedItem();
        // Se muestar el asistente
        if (cofirma.isSelected()) {
            new AsistenteCofirma(kssc);
        }
        else {
            new AsistenteContrafirmas(kssc);
        }
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
        // comboAlmacen.getAccessibleContext().setAccessibleName(etiquetaAlmacen.getText()+" "+Messages.getString("Firma.almacen.certificados.description")+" ALT + A.");
        // // NOI18N
        comboAlmacen.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                          Messages.getString("Firma.almacen.certificados.description.status"))); //$NON-NLS-1$
        comboAlmacen.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                          Messages.getString("Firma.almacen.certificados.description.status"))); //$NON-NLS-1$
        comboAlmacen.addAncestorListener(new RequestFocusListener(false));
        Utils.remarcar(comboAlmacen);
        Utils.setContrastColor(comboAlmacen);
        Utils.setFontBold(comboAlmacen);
        cargarComboAlmacen(comboAlmacen);
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
        c.fill = GridBagConstraints.HORIZONTAL;
        c.weighty = 0.0;

        // Panel que engloba los tipos de multifirma
        final JPanel panelTipos = new JPanel(new GridLayout(0, 1));
        panelTipos.setBorder(BorderFactory.createTitledBorder(Messages.getString("MultifirmaSimple.opciones"))); // NOI18N //$NON-NLS-1$
        Utils.setContrastColor(panelTipos);
        Utils.setFontBold(panelTipos);

        final JPanel panelCofirma = new JPanel(new GridLayout(1, 1));
        panelCofirma.getAccessibleContext().setAccessibleName(Messages.getString("MultifirmaSimple.opciones")); //$NON-NLS-1$
        // Radiobutton cofirma
        final JRadioButton cofirma = new JRadioButton();
        cofirma.setSelected(true);
        cofirma.setText(Messages.getString("Multifirma.opcion.cofirma." + (GeneralConfig.isAvanzados() ? "av" : "sp") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        ));
        cofirma.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                     Messages.getString("Multifirma.opcion.cofirma.description.status"))); //$NON-NLS-1$
        cofirma.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                     Messages.getString("Multifirma.opcion.cofirma.description.status"))); //$NON-NLS-1$
        // cofirma.getAccessibleContext().setAccessibleName(cofirma.getText() +" "+
        // Messages.getString("Multifirma.opcion.cofirma.description.status")); // NOI18N
        cofirma.getAccessibleContext().setAccessibleDescription(Messages.getString("Multifirma.opcion.cofirma.description")); // NOI18N //$NON-NLS-1$
        cofirma.setToolTipText(Messages.getString("Multifirma.opcion.cofirma.description.status")); //$NON-NLS-1$
        // Se comprueba si el modo es el avanzado o no
        if (GeneralConfig.isAvanzados()) {
            cofirma.setMnemonic(KeyEvent.VK_O); // Se asigna el atajo para el modo avanzado
        }
        else {
            cofirma.setMnemonic(KeyEvent.VK_G); // Se asigna el atajo para el modo simple
        }
        Utils.remarcar(cofirma);
        Utils.setContrastColor(cofirma);
        Utils.setFontBold(cofirma);
        panelCofirma.add(cofirma);
        panelTipos.add(panelCofirma);

        final JPanel panelContrafirma = new JPanel(new GridLayout(1, 1));
        // Radiobutton contrafirma
        final JRadioButton contrafirma = new JRadioButton();
        contrafirma.setText(Messages.getString("Multifirma.opcion.contrafirma." + (GeneralConfig.isAvanzados() ? "av" : "sp") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        ));
        contrafirma.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(),
                                                                         Messages.getString("Multifirma.opcion.contrafirma.description.status"))); //$NON-NLS-1$
        contrafirma.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(),
                                                                         Messages.getString("Multifirma.opcion.contrafirma.description.status"))); //$NON-NLS-1$
        // contrafirma.getAccessibleContext().setAccessibleName(contrafirma.getText()+" "+
        // Messages.getString("Multifirma.opcion.contrafirma.description.status")); // NOI18N
        contrafirma.getAccessibleContext().setAccessibleDescription(Messages.getString("Multifirma.opcion.contrafirma.description")); // NOI18N //$NON-NLS-1$
        contrafirma.setToolTipText(Messages.getString("Multifirma.opcion.contrafirma.description.status")); //$NON-NLS-1$
        // Se comprueba si el modo es el avanzado o no
        if (GeneralConfig.isAvanzados()) {
            contrafirma.setMnemonic(KeyEvent.VK_T); // Se asigna el atajo para el modo avanzado
        }
        else {
            contrafirma.setMnemonic(KeyEvent.VK_E); // Se asigna el atajo para el modo simple
        }
        Utils.remarcar(contrafirma);
        Utils.setContrastColor(contrafirma);
        Utils.setFontBold(contrafirma);

        panelContrafirma.add(contrafirma);
        panelTipos.add(panelContrafirma);

        add(panelTipos, c);

        // Grupo con los radio button para poder seleccionar solo uno
        final ButtonGroup grupoButtons = new ButtonGroup();
        grupoButtons.add(cofirma);
        grupoButtons.add(contrafirma);

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
        firmar.addMouseListener(new ElementDescriptionMouseListener(PrincipalGUI.getBar(), Messages.getString("PrincipalGUI.firmar.description.status"))); //$NON-NLS-1$
        firmar.addFocusListener(new ElementDescriptionFocusListener(PrincipalGUI.getBar(), Messages.getString("PrincipalGUI.firmar.description.status"))); //$NON-NLS-1$
        firmar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
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
        final JPanel buttonPanel = new JPanel();
        buttonPanel.add(panelFirmar, BorderLayout.CENTER);
        panelBotones.add(buttonPanel, cons);

        cons.ipadx = 15;
        cons.weightx = 0.0;
        cons.gridx = 2;

        final JPanel panelAyuda = new JPanel();
        // Boton ayuda
        final JButton botonAyuda = HelpUtils.helpButton("multifirma"); //$NON-NLS-1$
        botonAyuda.setName("helpButton"); //$NON-NLS-1$

        panelAyuda.add(botonAyuda);
        panelBotones.add(panelAyuda, cons);

        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(13, 13, 13, 13);
        c.weightx = 1.0;
        c.weighty = 0.0;
        c.gridwidth = 2;
        c.gridy = 8;

        add(panelBotones, c);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(comboAlmacen, "multifirma.almacen"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(cofirma, "multifirma.tipo"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(contrafirma, "multifirma.tipo"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(firmar, "multifirma"); //$NON-NLS-1$
    }
}

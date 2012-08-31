/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardmultifirmamasiva;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Panel;
import java.awt.event.KeyEvent;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.InfoLabel;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardutils.BotoneraInferior;
import es.gob.afirma.ui.wizardutils.CabeceraAsistente;
import es.gob.afirma.ui.wizardutils.JDialogWizard;

/** Panel de configuracion para el wizard de multifirma masiva.
 * @author inteco */
final class PanelConfiguracion extends JAccessibilityDialogWizard {

    /** Botonera con funciones para la pagina panel de multifirma - cofirma */
    private class Botonera extends BotoneraInferior {

        /** UID. */
        private static final long serialVersionUID = 1L;

        /** Constructor.
         * @param ventanas Lista de ventanas que componen el wizard.
         * @param posicion posicion de la ventana donde se inserta esta botonera. */
        Botonera(final List<JDialogWizard> ventanas, final int posicion) {
            super(ventanas, posicion);
        }

        /** Accion para el boton siguiente. */
        @Override
        protected void siguienteActionPerformed(final JButton anterior, final JButton siguiente, final JButton finalizar) {

            // Cargamos el tipo de contrafirma
            ((PanelMultifirmaMasiva) getVentanas().get(4)).setTipoContrafirma(PanelConfiguracion.this.getRadioUltimos().isSelected());

            // Cargamos el valor del check respetar
            ((PanelMultifirmaMasiva) getVentanas().get(4)).setRespetar(PanelConfiguracion.this.getCheckRespectar().isSelected());

            super.siguienteActionPerformed(anterior, siguiente, finalizar);
        }
    }

    /** UID. */
    private static final long serialVersionUID = 1L;

    /** Checkbox con texto "Respetar el formato...". */
    private final JCheckBox checkRespectar = new JCheckBox();
    JCheckBox getCheckRespectar() {
    	return this.checkRespectar;
    }

    /** Etiqueta con el texto "Para realizar el proceso...". */
    private final InfoLabel labelTextoRealizar = new InfoLabel(Messages.getString("Wizard.multifirma.ventana2.explicacion2"), false); //$NON-NLS-1$

    /** Panel que engloba los radiobuttons. */
    private final JPanel panelRadios = new JPanel();

    /** Radio buton "Contrafirmar unicamente". */
    private final JRadioButton radioUltimos = new JRadioButton();
    JRadioButton getRadioUltimos() {
    	return this.radioUltimos;
    }

    /** Constructor. */
    PanelConfiguracion() {
        initComponents();
    }

    /** Relaci&oacute;n m&iacute;nima para el redimensionado de componentes. */
    @Override
    public int getMinimumRelation() {
        return 9;
    }

    /** Inicializaci&oacute;n de componentes */
    private void initComponents() {
        // Titulo de la ventana
        setTitulo(Messages.getString("Wizard.multifirma.titulo.ventana")); //$NON-NLS-1$

        // Panel con la cabecera
        final CabeceraAsistente panelSuperior =
            new CabeceraAsistente("Wizard.multifirma.pagina2.titulo", "Wizard.multifirma.pagina2.titulo.explicacion1", "Wizard.multifirma.pagina2.titulo.explicacion2", null); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        Utils.setContrastColor(panelSuperior);
        Utils.setFontBold(panelSuperior);
        getContentPane().add(panelSuperior, BorderLayout.NORTH);

        // Panel central
        final JPanel panelCentral = new JPanel();
        panelCentral.setMinimumSize(new Dimension(603, 289));
        panelCentral.setLayout(new GridBagLayout());

        // Configuramos el layout
        final GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(0, 0, 0, 0);
        c.weightx = 1.0;
        c.weighty = 1.0;
        c.gridx = 0;
        c.gridy = 0;

        // Etiqueta con el texto "Respetar el formato..."
        final InfoLabel respectLabel = new InfoLabel(Messages.getString("Wizard.multifirma.ventana2.explicacion1"), false); //$NON-NLS-1$

        // Panel para agregar la etiqueta y asi desvincularla del check posterior.
        final JPanel panel = new JPanel(new GridBagLayout());
        panel.add(respectLabel, c);

        c.insets = new Insets(10, 20, 0, 20);
        panelCentral.add(panel, c); // Se anade el panel con la etiqueta

        c.gridy = 1;

        // Checkbox con texto "Respetar el formato..."
        this.checkRespectar.setText(Messages.getString("Wizard.multifirma.ventana2.check.respetar")); //$NON-NLS-1$
        this.checkRespectar.getAccessibleContext().setAccessibleName(this.checkRespectar.getText() + " " //$NON-NLS-1$
                                                                     + Messages.getString("Wizard.multifirma.ventana2.check.respetar.description")); //$NON-NLS-1$
        this.checkRespectar.getAccessibleContext()
        .setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana2.check.respetar.description")); //$NON-NLS-1$
        this.checkRespectar.setMnemonic(KeyEvent.VK_T); // Se asigna un atajo al checkbox

        Utils.remarcar(this.checkRespectar);
        Utils.setContrastColor(this.checkRespectar);
        Utils.setFontBold(this.checkRespectar);
        panelCentral.add(this.checkRespectar, c);

        c.gridy = 2;

        // Etiqueta con el texto "Para realizar el proceso..."
        panelCentral.add(this.labelTextoRealizar, c);

        c.insets = new Insets(15, 20, 10, 20);
        c.gridy = 3;

        // Panel que engloba los radiobuttons
        this.panelRadios.setBorder(BorderFactory.createTitledBorder(Messages.getString("Wizard.multifirma.ventana2.panel"))); //$NON-NLS-1$
        Utils.setContrastColor(this.panelRadios);
        Utils.setFontBold(this.panelRadios);
        this.panelRadios.setLayout(new GridBagLayout());

        final GridBagConstraints cons = new GridBagConstraints();
        cons.fill = GridBagConstraints.HORIZONTAL;
        cons.weightx = 1.0;

        // Radio button "Contrafirmar a todos"
        final JRadioButton radioTodos = new JRadioButton();
        radioTodos.setText(Messages.getString("Wizard.multifirma.ventana2.panel.radio1")); //$NON-NLS-1$
        radioTodos.getAccessibleContext().setAccessibleName(radioTodos.getText() + " " //$NON-NLS-1$
                                                            + Messages.getString("Wizard.multifirma.ventana2.panel.radio1.description")); //$NON-NLS-1$
        radioTodos.getAccessibleContext().setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana2.panel.radio1.description")); //$NON-NLS-1$
        radioTodos.setMnemonic(KeyEvent.VK_O); // Se asigna un atajo al boton de radio
        radioTodos.setSelected(true);
        Utils.remarcar(radioTodos);
        Utils.setContrastColor(radioTodos);
        Utils.setFontBold(radioTodos);
        this.panelRadios.add(radioTodos, cons);

        cons.gridy = 1;

        // Radio buton "Contrafirmar unicamente"
        this.radioUltimos.setText(Messages.getString("Wizard.multifirma.ventana2.panel.radio2")); //$NON-NLS-1$
        this.radioUltimos.getAccessibleContext()
        .setAccessibleName(this.radioUltimos.getText() + " " + Messages.getString("Wizard.multifirma.ventana2.panel.radio2.description"));  //$NON-NLS-1$//$NON-NLS-2$
        this.radioUltimos.getAccessibleContext().setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana2.panel.radio2.description")); //$NON-NLS-1$
        this.radioUltimos.setMnemonic(KeyEvent.VK_N); // Se asigna un atajo al boton de radio
        Utils.remarcar(this.radioUltimos);
        Utils.setContrastColor(this.radioUltimos);
        Utils.setFontBold(this.radioUltimos);
        this.panelRadios.add(this.radioUltimos, cons);

        // Grupo de radiobuttons
        final ButtonGroup grupoRadios = new ButtonGroup();
        grupoRadios.add(radioTodos);
        grupoRadios.add(this.radioUltimos);

        panelCentral.add(this.panelRadios, c);

        c.fill = GridBagConstraints.BOTH;
        c.insets = new Insets(10, 20, 0, 20);
        c.weightx = 1.0;
        c.weighty = 1.0;
        c.gridx = 0;
        c.gridy = 4;

        // Panel introducido para poder mantener la linea superior correcta
        final Panel panelVacio = new Panel();
        panelCentral.add(panelVacio, c);

        getContentPane().add(panelCentral, BorderLayout.CENTER);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(this.checkRespectar, "multifirma.masiva.wizard.contrafirma.respetar"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(radioTodos, "multifirma.masiva.wizard.contrafirma.opciones"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.radioUltimos, "multifirma.masiva.wizard.contrafirma.opciones"); //$NON-NLS-1$
    }

    /** Muestra/oculta el panel de los radio buttons y su explicacion
     * @param visible */
    void setMostrar(final boolean visible) {
        this.labelTextoRealizar.setVisible(visible);
        this.panelRadios.setVisible(visible);
    }

    /** Guarda todas las ventanas del asistente para poder controlar la botonera
     * @param ventanas Listado con todas las paginas del asistente */
    void setVentanas(final List<JDialogWizard> ventanas) {
        this.setBotonera(new Botonera(ventanas, 2));
        getContentPane().add(getBotonera(), BorderLayout.PAGE_END);
    }
}

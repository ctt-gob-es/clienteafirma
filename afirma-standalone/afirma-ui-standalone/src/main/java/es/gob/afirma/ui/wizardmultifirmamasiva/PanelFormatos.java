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
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import es.gob.afirma.core.signers.AOSignConstants;
import es.gob.afirma.ui.utils.HelpUtils;
import es.gob.afirma.ui.utils.InfoLabel;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardutils.BotoneraInferior;
import es.gob.afirma.ui.wizardutils.CabeceraAsistente;
import es.gob.afirma.ui.wizardutils.JDialogWizard;

/** Panel de formatos para el wizard de multifirma masiva.
 * @author inteco */
final class PanelFormatos extends JAccessibilityDialogWizard implements ItemListener {
    /** Botonera con funciones para la pagina panel de multifirma - cofirma */
    private class Botonera extends BotoneraInferior {
        /** UID. */
        private static final long serialVersionUID = 1L;

        /** Constructor.
         * @param ventanas Lista de ventanas que componen el wizard.
         * @param posicion posicion de la ventana donde se inserta esta botonera. */
        public Botonera(final List<JDialogWizard> ventanas, final int posicion) {
            super(ventanas, posicion);
        }

        /** Accion para el bot&oacute;n siguiente. */
        @Override
        protected void siguienteActionPerformed(final JButton anterior, final JButton siguiente, final JButton finalizar) {

            // Carga el formato de firmado
            final String formato = PanelFormatos.getFormats().get(PanelFormatos.this.getComboFormatos().getSelectedIndex());
            ((PanelMultifirmaMasiva) getVentanas().get(4)).setAlgoritmo(formato);

            if (PanelFormatos.this.getRadioFirma().isSelected()) {
                // Carga Firma
                ((PanelMultifirmaMasiva) getVentanas().get(4)).setTipo(0);

                // Modo de la firma (solo si es CADES)
                ((PanelMultifirmaMasiva) getVentanas().get(4)).setModoFormato(PanelFormatos.this.getCheckFormato().isSelected());

                // Registramos en la pagina 3 que hemos saltado la 2
                ((PanelEntrada) getVentanas().get(3)).setSalto(true);

                final int indice = 3;

                // mantenemos el tamano y posicion de la ventana acutual en la ventana siguiente
                getVentanas().get(indice).setBounds(getVentanas().get(1).getX(),
                                                    getVentanas().get(1).getY(),
                                                    getVentanas().get(1).getWidth(),
                                                    getVentanas().get(1).getHeight());

                // Se asigna un boton por defecto al wizard
                if (getVentanas().get(indice) instanceof JAccessibilityDialogWizard) {
                    // Se obtiene el dialogo
                    final JAccessibilityDialogWizard wizard = (JAccessibilityDialogWizard) getVentanas().get(indice);
                    // Se comprueba si estamos en la ultima ventana del wizard
                    if (indice < getVentanas().size() - 1) {
                        wizard.getRootPane().setDefaultButton(wizard.getBotonera().getSiguiente());
                    }
                    else {
                        wizard.getRootPane().setDefaultButton(wizard.getBotonera().getFinalizar());
                    }
                }

                // Nos saltamos la pagina 2
                getVentanas().get(indice).setVisibleAndHide(true, getVentanas().get(1));
            }
            else if (PanelFormatos.this.getRadioCofirma().isSelected()) {
                // Carga Cofirma
                ((PanelMultifirmaMasiva) getVentanas().get(4)).setTipo(1);

                ((PanelConfiguracion) getVentanas().get(2)).setMostrar(false);
                ((PanelEntrada) getVentanas().get(3)).setSalto(false);
                super.siguienteActionPerformed(anterior, siguiente, finalizar);
            }
            else {
                // Carga Contrafirma
                ((PanelMultifirmaMasiva) getVentanas().get(4)).setTipo(2);

                ((PanelConfiguracion) getVentanas().get(2)).setMostrar(true);
                ((PanelEntrada) getVentanas().get(3)).setSalto(false);
                super.siguienteActionPerformed(anterior, siguiente, finalizar);
            }
        }
    }

    /** UID. */
    private static final long serialVersionUID = 1L;

    // Checkbox con el modo del formato
    private final JCheckBox checkFormato = new JCheckBox();
    JCheckBox getCheckFormato() {
    	return this.checkFormato;
    }

    // Combo con los formatos
    private final JComboBox<String> comboFormatos = new JComboBox<>();
    JComboBox<String> getComboFormatos() {
    	return this.comboFormatos;
    }


    // Etiqueta para los formatos
    private final JLabel etiquetaFormato = new JLabel();

    /** Listado con las constantes de los formatos del combo. */
    private static final List<String> FORMATS = new ArrayList<>(Arrays.asList(
		AOSignConstants.SIGN_FORMAT_XADES_DETACHED,
        AOSignConstants.SIGN_FORMAT_XADES_ENVELOPING,
        AOSignConstants.SIGN_FORMAT_XADES_ENVELOPED,
        AOSignConstants.SIGN_FORMAT_CADES,
        AOSignConstants.SIGN_FORMAT_PDF,
        AOSignConstants.SIGN_FORMAT_ODF,
        AOSignConstants.SIGN_FORMAT_OOXML,
        AOSignConstants.SIGN_FORMAT_FACTURAE
    ));
    static List<String> getFormats() {
    	return FORMATS;
    }

    // Radio button cofirma
    private final JRadioButton radioCofirma = new JRadioButton();
    JRadioButton getRadioCofirma() {
    	return this.radioCofirma;
    }

    // Radio button contrafirma
    private final JRadioButton radioContrafirma = new JRadioButton();

    // Radio button firma
    private final JRadioButton radioFirma = new JRadioButton();
    JRadioButton getRadioFirma() {
    	return this.radioFirma;
    }


    /** Constructor. */
    PanelFormatos() {
        initComponents();
    }

    /** Carga el combo con los diferentes formatos */
    private void cargarCombo() {
        this.comboFormatos.setModel(new DefaultComboBoxModel<>(new String[] {
               "Firma est\u00E1ndar (XAdES Detached)",
               "XAdES Enveloping",
               "XAdES Enveloped",
               "CAdES",
               "PAdES",
               "ODF (Open Document Format)",
               "OOXML (Office Open XML)",
               "Factura Electr\u00F3nica"
        }));
    }

    /** Activa y desactiva el checkbox del modo del formato dependiendo de la opci&oacute;n
     * elegida */
    void checkFormatoModo() {
        if (FORMATS.get(this.comboFormatos.getSelectedIndex()).equals(AOSignConstants.SIGN_FORMAT_CADES)) {
            if (this.radioFirma.isSelected()) {
                this.checkFormato.setEnabled(true); // Se habilita el checkbox
            }
            this.checkFormato.setMnemonic(KeyEvent.VK_I); // Se asigna un atajo al checkbox
        }
        else {
            this.checkFormato.setEnabled(false);
            this.checkFormato.setMnemonic(0); // Se quita el atajo al deshabilitar el checkbox
        }
        // En factura no se admiten multifirmas
        if (FORMATS.get(this.comboFormatos.getSelectedIndex()).equals(AOSignConstants.SIGN_FORMAT_FACTURAE)) {
        	this.radioFirma.setSelected(true);
        	this.radioCofirma.setEnabled(false);
        	this.radioContrafirma.setEnabled(false);
        }
        else {
        	this.radioCofirma.setEnabled(true);
        	this.radioContrafirma.setEnabled(true);
        }
    }

    /** Relaci&oacute;n m&iacute;nima para el redimensionado de componentes. */
    @Override
    public int getMinimumRelation() {
        return 8;
    }

    /** Inicializaci&oacute;n de componentes */
    private void initComponents() {
        // Titulo de la ventana
        setTitulo(Messages.getString("Wizard.multifirma.titulo.ventana")); //$NON-NLS-1$

        // Panel con la cabecera
        final CabeceraAsistente panelSuperior = new CabeceraAsistente("Wizard.firma.pagina1.titulo", "Wizard.firma.pafina1.titulo.explicacion", null); //$NON-NLS-1$ //$NON-NLS-2$
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
        c.insets = new Insets(20, 20, 0, 20);
        c.weightx = 1.0;

        // Etiqueta "La operacion seleccionada..."
        final InfoLabel operationLabel = new InfoLabel(Messages.getString("Wizard.multifirma.ventana1.explicacion"), false); //$NON-NLS-1$
        panelCentral.add(operationLabel, c);

        // Etiqueta "La operacion seleccionada..."
        /*panelCentral.add(PanelesTexto.generarPanelTexto(
        		"Wizard.multifirma.ventana1.explicacion", false), c);*/

        c.insets = new Insets(20, 20, 0, 20);
        c.weightx = 1.0;
        c.gridy = 1;

        // Panel que engloba los radio buttons
        final JPanel panelOperaciones = new JPanel();
        panelOperaciones.setBorder(BorderFactory.createTitledBorder(Messages.getString("Wizard.multifirma.ventana1.panel.operacion"))); //$NON-NLS-1$
        Utils.setContrastColor(panelOperaciones);
        Utils.setFontBold(panelOperaciones);
        panelOperaciones.setLayout(new GridBagLayout());

        final GridBagConstraints cons = new GridBagConstraints();
        cons.fill = GridBagConstraints.HORIZONTAL;
        cons.insets = new Insets(0, 0, 0, 0);
        cons.weightx = 1.0;

        final JPanel panelRadioFirma = new JPanel(new GridLayout(1, 1));
        panelRadioFirma.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.multifirma.ventana1.panel.operacion")); //$NON-NLS-1$
        // Radio button firma
        this.radioFirma.setText(Messages.getString("Wizard.multifirma.ventana1.radio.firma")); //$NON-NLS-1$
        this.radioFirma.getAccessibleContext()
        .setAccessibleName(this.radioFirma.getText() + " " + Messages.getString("Wizard.multifirma.ventana1.radio.firma.description"));  //$NON-NLS-1$//$NON-NLS-2$
        this.radioFirma.getAccessibleContext().setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana1.radio.firma.description")); //$NON-NLS-1$
        this.radioFirma.addItemListener(this);
        this.radioFirma.setName("radioFirma"); //$NON-NLS-1$
        this.radioFirma.setSelected(true);
        this.radioFirma.setMnemonic(KeyEvent.VK_F); // Se asigna un atajo al boton de radio
        Utils.remarcar(this.radioFirma);
        Utils.setContrastColor(this.radioFirma);
        Utils.setFontBold(this.radioFirma);
        panelRadioFirma.add(this.radioFirma);
        panelOperaciones.add(panelRadioFirma, cons);

        cons.insets = new Insets(0, 0, 0, 0);
        cons.weightx = 1.0;
        cons.gridy = 1;

        final JPanel panelRadioCofirma = new JPanel(new GridLayout(1, 1));
        // Radio button cofirma
        this.radioCofirma.setText(Messages.getString("Wizard.multifirma.ventana1.radio.cofirma")); //$NON-NLS-1$
        this.radioCofirma.getAccessibleContext()
        .setAccessibleName(this.radioCofirma.getText() + " " + Messages.getString("Wizard.multifirma.ventana1.radio.cofirma.description"));  //$NON-NLS-1$//$NON-NLS-2$
        this.radioCofirma.getAccessibleContext().setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana1.radio.cofirma.description")); //$NON-NLS-1$
        this.radioCofirma.addItemListener(this);
        this.radioCofirma.setName("radioCofirma"); //$NON-NLS-1$
        this.radioCofirma.setMnemonic(KeyEvent.VK_O); // Se asigna un atajo al boton de radio
        Utils.remarcar(this.radioCofirma);
        Utils.setContrastColor(this.radioCofirma);
        Utils.setFontBold(this.radioCofirma);
        panelRadioCofirma.add(this.radioCofirma);
        panelOperaciones.add(panelRadioCofirma, cons);

        cons.insets = new Insets(0, 0, 0, 0);
        cons.weightx = 1.0;
        cons.gridy = 2;

        final JPanel panelRadioContrafirma = new JPanel(new GridLayout(1, 1));
        // Radio button contrafirma
        this.radioContrafirma.setText(Messages.getString("Wizard.multifirma.ventana1.radio.contrafirma")); //$NON-NLS-1$
        this.radioContrafirma.getAccessibleContext()
        .setAccessibleName(this.radioContrafirma.getText() + " " + Messages.getString("Wizard.multifirma.ventana1.radio.contrafirma.description"));  //$NON-NLS-1$//$NON-NLS-2$
        this.radioContrafirma.getAccessibleContext()
        .setAccessibleDescription(Messages.getString("Wizard.multifirma.ventana1.radio.contrafirma.description")); //$NON-NLS-1$
        this.radioContrafirma.addItemListener(this);
        this.radioContrafirma.setName("radioContrafirma"); //$NON-NLS-1$
        this.radioContrafirma.setMnemonic(KeyEvent.VK_N); // Se asigna un atajo al boton de radio
        Utils.remarcar(this.radioContrafirma);
        Utils.setContrastColor(this.radioContrafirma);
        Utils.setFontBold(this.radioContrafirma);
        panelRadioContrafirma.add(this.radioContrafirma);
        panelOperaciones.add(panelRadioContrafirma, cons);

        // Agrupamos los radiobutton para que solo se pueda marcar uno
        final ButtonGroup radioGrupo = new ButtonGroup();
        radioGrupo.add(this.radioFirma);
        radioGrupo.add(this.radioCofirma);
        radioGrupo.add(this.radioContrafirma);

        panelCentral.add(panelOperaciones, c);

        c.insets = new Insets(10, 20, 0, 20);
        c.weightx = 1.0;
        c.gridy = 2;

        // Etiqueta con el texto formato
        this.etiquetaFormato.setText(Messages.getString("Firma.formato")); //$NON-NLS-1$
        Utils.setContrastColor(this.etiquetaFormato);
        Utils.setFontBold(this.etiquetaFormato);
        panelCentral.add(this.etiquetaFormato, c);

        c.insets = new Insets(0, 20, 0, 20);
        c.weightx = 1.0;
        c.gridy = 3;
        c.weighty = 0.1;
        c.fill = GridBagConstraints.BOTH;

        // Combo con los formatos
        this.comboFormatos.setToolTipText(Messages.getString("Firma.formato.description")); //$NON-NLS-1$
        this.comboFormatos.getAccessibleContext().setAccessibleName(this.etiquetaFormato.getText() + " " //$NON-NLS-1$
                                                                    + this.comboFormatos.getToolTipText()
                                                                    + "ALT + R."); //$NON-NLS-1$
        this.comboFormatos.getAccessibleContext().setAccessibleDescription(this.comboFormatos.getToolTipText());
        this.comboFormatos.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                checkFormatoModo();
            }
        });
        cargarCombo();
        Utils.remarcar(this.comboFormatos);
        Utils.setContrastColor(this.comboFormatos);
        Utils.setFontBold(this.comboFormatos);
        panelCentral.add(this.comboFormatos, c);

        // Relacion entre etiqueta y combo
        this.etiquetaFormato.setLabelFor(this.comboFormatos);
        // Asignacion de mnemonico
        this.etiquetaFormato.setDisplayedMnemonic(KeyEvent.VK_T);

        c.fill = GridBagConstraints.BOTH;
        c.insets = new Insets(10, 20, 15, 20);
        c.weightx = 1.0;
        c.weighty = 1.0;
        c.gridy = 4;

        final JPanel panelCheckFormato = new JPanel(new GridLayout(1, 1));
        // Checkbox con el modo de CADES
        this.checkFormato.setSelected(true);
        this.checkFormato.setText(Messages.getString("Firma.modo.formato")); // NOI18N //$NON-NLS-1$
        this.checkFormato.setToolTipText(Messages.getString("Firma.modo.formato.description")); // NOI18N //$NON-NLS-1$
        this.checkFormato.getAccessibleContext().setAccessibleName(this.checkFormato.getText() + " " + this.checkFormato.getToolTipText()); // NOI18N //$NON-NLS-1$
        this.checkFormato.getAccessibleContext().setAccessibleDescription(this.checkFormato.getToolTipText()); // NOI18N
        this.checkFormato.setMnemonic(0); // Se quita el atajo al deshabilitar el checkbox
        this.checkFormato.setEnabled(false);
        Utils.remarcar(this.checkFormato);
        Utils.setContrastColor(this.checkFormato);
        Utils.setFontBold(this.checkFormato);
        panelCheckFormato.add(this.checkFormato);
        panelCentral.add(panelCheckFormato, c);

        getContentPane().add(panelCentral, BorderLayout.CENTER);

        // Accesos rapidos al menu de ayuda
        HelpUtils.enableHelpKey(this.radioFirma, "multifirma.masiva.wizard.operacion"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.radioContrafirma, "multifirma.masiva.wizard.operacion"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.radioCofirma, "multifirma.masiva.wizard.operacion"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.comboFormatos, "multifirma.masiva.wizard.formato"); //$NON-NLS-1$
        HelpUtils.enableHelpKey(this.checkFormato, "multifirma.masiva.wizard.formato.modo"); //$NON-NLS-1$
    }

    /** Activa y desactiva el combo en funcion de la operacion */
    @Override
    public void itemStateChanged(final ItemEvent e) {
        final JRadioButton radio = (JRadioButton) e.getSource();
        // El listado de seleccion de formato solo se mantiene activo para la firma y la cofirma
        if (radio.getName().equals("radioFirma")) { //$NON-NLS-1$
            this.comboFormatos.setEnabled(true);
            this.etiquetaFormato.setDisplayedMnemonic(KeyEvent.VK_R); // Se asigna un atajo a la etiqueta
            if (this.comboFormatos.getItemCount() != 0 && PanelFormatos.FORMATS.get(this.comboFormatos.getSelectedIndex())
                    .equals(AOSignConstants.SIGN_FORMAT_CADES)) {
                this.checkFormato.setSelected(true);
                this.checkFormato.setMnemonic(KeyEvent.VK_I); // Se asigna un atajo al checkbox
                this.checkFormato.setEnabled(true);
            }
        }
        else if (radio.getName().equals("radioCofirma")) { //$NON-NLS-1$
            this.comboFormatos.setEnabled(true);
            this.etiquetaFormato.setDisplayedMnemonic(KeyEvent.VK_R); // Se asigna un atajo a la etiqueta
            this.checkFormato.setSelected(true);
            this.checkFormato.setMnemonic(0); // Se quita el atajo al deshabilitar el checkbox
            this.checkFormato.setEnabled(false);
        }
        else if (radio.getName().equals("radioContrafirma")) { //$NON-NLS-1$
            this.comboFormatos.setEnabled(false);
            this.etiquetaFormato.setDisplayedMnemonic(0); // Se quita el atajo al deshabilitar el combo
            this.checkFormato.setSelected(true);
            this.checkFormato.setMnemonic(0); // Se quita el atajo al deshabilitar el checkbox
            this.checkFormato.setEnabled(false);
        }
    }

    /** Guarda todas las ventanas del asistente para poder controlar la botonera
     * @param ventanas Listado con todas las paginas del asistente */
    void setVentanas(final List<JDialogWizard> ventanas) {
        this.setBotonera(new Botonera(ventanas, 1));
        getContentPane().add(getBotonera(), BorderLayout.PAGE_END);
    }
}

/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardutils;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;

import es.gob.afirma.ui.principal.PrincipalGUI;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;

/** Clase para generar la parte inferior de la ventana con la botonera */
public class BotoneraInferior extends JPanel {

    private static final long serialVersionUID = 1L;
    private Dimension dimensiones = new Dimension(603, 47);
    private JButton finalizar = null;
    private int posicion;

    // private JButton restaurar = null;
    // private JButton maximizar = null;
    private JButton siguiente = null;
    private List<JDialogWizard> ventanas;

    /** Genera una botonera con unas dimensiones dadas
     * @param dimensiones Dimensiones de la botonera */
    public BotoneraInferior(final Dimension dimensiones) {
        this.dimensiones = dimensiones;
        initParamenters();
    }

    /** Genera una botonera con la configuracion predefinida
     * @param ventanas Listado que contiene todas las ventanas en orden de aparicion
     * @param posicion Numero de la pagina */
    public BotoneraInferior(final List<JDialogWizard> ventanas, final int posicion) {
        this.ventanas = ventanas;
        this.posicion = posicion;
        initParamenters();
    }

    /** Muestra el dialogo anterior
     * @param finalizar1 Boton finalizar
     * @param siguiente1 Boton siguiente
     * @param anterior Boton anterior */
    protected void anteriorActionPerformed(final JButton anterior, final JButton siguiente1, final JButton finalizar1) {
        // Nos movemos al indice anterior
        final int indice = this.posicion - 1;

        // Mantenemos el tamano y posicion de la ventana actual en la ventana anterior
        this.ventanas.get(this.posicion - 1).setBounds(this.ventanas.get(this.posicion).getX(),
                                                       this.ventanas.get(this.posicion).getY(),
                                                       this.ventanas.get(this.posicion).getWidth(),
                                                       this.ventanas.get(this.posicion).getHeight());

        // Se asigna un boton por defecto al wizard
        if (this.ventanas.get(indice) instanceof JAccessibilityDialogWizard) {
            this.ventanas.get(indice)
            .getRootPane()
            .setDefaultButton(((JAccessibilityDialogWizard) this.ventanas.get(indice)).getBotonera().getSiguiente());
        }

        this.ventanas.get(indice).setVisibleAndHide(true, this.ventanas.get(this.posicion));

    }

    /** Devuelve el boton de finalizar.
     * @return boton de finalizar. */
    public JButton getFinalizar() {
        return this.finalizar;
    }

    /** Devuelve el boton de siguiente.
     * @return boton de siguiente. */
    public JButton getSiguiente() {
        return this.siguiente;
    }

    public List<JDialogWizard> getVentanas() {
        return this.ventanas;
    }

    /** Inicializacion de parametros */
    private void initParamenters() {
        // Configuracion del panel
        setBorder(BorderFactory.createEtchedBorder());
        setPreferredSize(this.dimensiones);
        setLayout(new FlowLayout(FlowLayout.CENTER, 1, 1));

        // Definicion de botones
        // maximizar = new JButton();
        final JButton anterior = new JButton();
        this.siguiente = new JButton();
        final JButton cancelar = new JButton();
        this.finalizar = new JButton();

        // JPanel panelMaximizar = new JPanel(new GridLayout(1, 1));
        // //Boton maximizar
        // maximizar.setText(Messages.getString("Wizard.maximizar"));
        // maximizar.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.maximizar") + ". " +
        // Messages.getString("Wizard.maximizar.description"));
        // maximizar.setName("maximizar");
        // maximizar.setMnemonic(KeyEvent.VK_M);
        // maximizar.addActionListener(new ActionListener() {
        // public void actionPerformed(ActionEvent e) {
        // maximizarActionPerformed();
        // }
        // });
        // Utils.remarcar(maximizar);
        // Utils.setContrastColor(maximizar);
        // Utils.setFontBold(maximizar);
        //
        // panelMaximizar.add(maximizar);
        // //add(panelMaximizar);
        //
        // JPanel panelRestaurar = new JPanel(new GridLayout(1, 1));
        // // Boton restaurar
        // restaurar = new JButton();
        // restaurar.setText(Messages.getString("Wizard.restaurar"));
        // restaurar.setName("restaurar");
        // restaurar.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.restaurar") + ". " +
        // Messages.getString("Wizard.restaurar.description"));
        // restaurar.setMnemonic(KeyEvent.VK_R);
        // restaurar.addActionListener(new ActionListener() {
        // public void actionPerformed(ActionEvent e) {
        // restaurarActionPerformed();
        // }
        // });
        // Utils.remarcar(restaurar);
        // Utils.setContrastColor(restaurar);
        // Utils.setFontBold(restaurar);
        //
        // panelRestaurar.add(restaurar);
        // //add(panelRestaurar);
        //
        // //Control de habilitado de los botones de maximizar y restaurar segun la configuracion de
        // //accesibilidad
        // if (GeneralConfig.isMaximized()){
        // maximize.setEnabled(false);
        // } else {
        // restaurar.setEnabled(false);
        // }

        // Espacio entre botones
        JPanel panelVacio = new JPanel();
        panelVacio.setPreferredSize(new Dimension(60, 10));
        add(panelVacio);

        final JPanel panelAnterior = new JPanel(new GridLayout(1, 1));
        // Boton anterior
        final int paginas = this.ventanas.size() - 1;
        if (this.posicion == 0 || paginas == this.posicion) {
            anterior.setEnabled(false);
        }
        else {
            anterior.setMnemonic(KeyEvent.VK_A); // Mnemonico para el boton de anterior
            anterior.setEnabled(true);
        }
        anterior.setText(Messages.getString("Wizard.anterior")); // NOI18N //$NON-NLS-1$

        anterior.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                anteriorActionPerformed(anterior, BotoneraInferior.this.siguiente, BotoneraInferior.this.finalizar);
            }
        });
        Utils.remarcar(anterior);
        Utils.setContrastColor(anterior);
        Utils.setFontBold(anterior);

        panelAnterior.add(anterior);
        add(panelAnterior);

        final JPanel panelSiguiente = new JPanel(new GridLayout(1, 1));
        // Boton siguiente
        if (this.ventanas.size() == 1 || paginas == this.posicion) {
            this.siguiente.setVisible(false);
        }
        else {
            this.siguiente.setMnemonic(KeyEvent.VK_S); // Mnemonico para el boton de siguiente
            this.siguiente.setVisible(true);
        }
        this.siguiente.setText(Messages.getString("Wizard.siguiente")); // NOI18N //$NON-NLS-1$

        this.siguiente.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                siguienteActionPerformed(anterior, BotoneraInferior.this.siguiente, BotoneraInferior.this.finalizar);
            }
        });
        Utils.remarcar(this.siguiente);
        Utils.setContrastColor(this.siguiente);
        Utils.setFontBold(this.siguiente);

        panelSiguiente.add(this.siguiente);
        add(panelSiguiente);

        // Espacio entre botones
        panelVacio = new JPanel();
        panelVacio.setSize(new Dimension(20, 10));
        add(panelVacio);

        final JPanel panelCancelar = new JPanel(new GridLayout(1, 1));
        // Boton cancelar
        if (paginas == this.posicion) {
            cancelar.setVisible(false);
        }
        else {
            cancelar.setMnemonic(KeyEvent.VK_C); // Mnemonico para el boton de cancelar
            cancelar.setVisible(true);
        }
        cancelar.setText(Messages.getString("Wizard.cancelar")); // NOI18N //$NON-NLS-1$
        cancelar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                saveSizePosition();
                for (final JDialogWizard ventana : BotoneraInferior.this.getVentanas()) {
                    ventana.dispose();
                }
            }
        });
        Utils.remarcar(cancelar);
        Utils.setContrastColor(cancelar);
        Utils.setFontBold(cancelar);

        panelCancelar.add(cancelar);
        add(panelCancelar);

        final JPanel panelFinalizar = new JPanel(new GridLayout(1, 1));
        // Boton finalizar
        if (this.ventanas.size() == 1 || paginas == this.posicion) {
            this.finalizar.setMnemonic(KeyEvent.VK_F); // Mnemonico para el boton de finalizar
            this.finalizar.setVisible(true);
        }
        else {
            this.finalizar.setVisible(false);
        }

        this.finalizar.setText(Messages.getString("Wizard.finalizar")); // NOI18N //$NON-NLS-1$

        this.finalizar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(final ActionEvent evt) {
                saveSizePosition();
                for (final JDialogWizard ventana : BotoneraInferior.this.getVentanas()) {
                    ventana.dispose();
                }
            }
        });
        Utils.remarcar(this.finalizar);
        Utils.setContrastColor(this.finalizar);
        Utils.setFontBold(this.finalizar);

        panelFinalizar.add(this.finalizar);
        add(panelFinalizar);
    }

    /** Guarda el tamano y posicion de la ventana antes de cerrarse */
    public void saveSizePosition() {
        // Guardamos la posicion y tamano actual de la ventana solo en caso de no estar maximizada por configuracion
        if (!GeneralConfig.isMaximized()) {
            final JAccessibilityDialogWizard j = JAccessibilityDialogWizard.getJAccessibilityDialogWizard(this);
            PrincipalGUI.setWizardActualPositionX(j.getX());
            PrincipalGUI.setWizardActualPositionY(j.getY());
            PrincipalGUI.setWizardActualWidth(j.getWidth());
            PrincipalGUI.setWizardActualHeight(j.getHeight());
        }
    }

    /** Muestra el dialogo siguiente
     * @param finalizar1 Boton finalizar
     * @param siguiente1 Boton siguiente
     * @param anterior Boton anterior */
    protected void siguienteActionPerformed(final JButton anterior, final JButton siguiente1, final JButton finalizar1) {
        final int indice = this.posicion + 1;

        // Mantenemos el tamano y posicion de la ventana acutual en la ventana siguiente
        this.ventanas.get(this.posicion + 1).setBounds(this.ventanas.get(this.posicion).getX(),
                                                       this.ventanas.get(this.posicion).getY(),
                                                       this.ventanas.get(this.posicion).getWidth(),
                                                       this.ventanas.get(this.posicion).getHeight());

        // Se asigna un boton por defecto al wizard
        if (this.ventanas.get(indice) instanceof JAccessibilityDialogWizard) {
            // Se obtiene el dialogo
            final JAccessibilityDialogWizard wizard = (JAccessibilityDialogWizard) this.ventanas.get(indice);
            // Se comprueba si estamos en la ultima ventana del wizard
            if (indice < this.ventanas.size() - 1) {
                wizard.getRootPane().setDefaultButton(wizard.getBotonera().getSiguiente());
            }
            else {
                wizard.getRootPane().setDefaultButton(wizard.getBotonera().getFinalizar());
            }
        }
        this.ventanas.get(indice).setVisibleAndHide(true, this.ventanas.get(this.posicion));

    }

}

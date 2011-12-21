/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardUtils;

import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;

import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.WindowConstants;

/**
 * Dialogo configurado con los parametros del asistente
 */
public class JDialogWizard extends JDialog implements ComponentListener {

	/**
	 * UID.
	 */
    private static final long serialVersionUID = 1L;
    /**
     * Ventana anterior.
     */
    private JDialogWizard ventanaAnterior;

    /**
     * Método que asigna el titulo.
     * @param titulo titulo de la ventana.
     */
    public void setTitulo(String titulo) {
        setTitle(titulo);
    }
    /**
     * Constructor.
     */
    public JDialogWizard () {
        // Configuracion de la ventana
        setDefaultLookAndFeelDecorated(false);
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setModal(true);
        addComponentListener(this);

        // Icono de @firma
        setIconImage(new ImageIcon(getClass().getResource("/resources/images/afirma_ico.png")).getImage());
    }

    /**
     * Muestra la ventana modal y oculta la anterior
     * @param mostrar	Indica si muestra o esconde la ventana
     * @param ventana	Ventana que se debe ocultar al mostrar la ventana
     */
    public void setVisibleAndHide(boolean mostrar, JDialogWizard ventana) {
        this.ventanaAnterior = ventana;
        super.setVisible(mostrar);
    }

    /**
     * Evento que se lanza cuando se oculta la ventana modal.
     */
    @Override
    public void componentHidden(ComponentEvent e) { /* No es necesario implementarlo */ }

    /**
     * Evento que se lanza cuando se mueve la ventana modal.
     */
    @Override
    public void componentMoved(ComponentEvent e) { /* No es necesario implementarlo */ }

    /**
     * Evento que se lanza cuando se redimensiona la ventana modal.
     */
    @Override
    public void componentResized(ComponentEvent e) { /* No es necesario implementarlo */ }

    /**
     * Cuando se muestra la ventana modal ocultamos la pagina anterior
     */
    @Override
    public void componentShown(ComponentEvent e) {
        if (this.ventanaAnterior != null)
            this.ventanaAnterior.dispose();
    }
}

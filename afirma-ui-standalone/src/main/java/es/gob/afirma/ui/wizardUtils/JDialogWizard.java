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

import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;

import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.WindowConstants;

import es.gob.afirma.core.misc.Platform;

/**
 * Dialogo configurado con los parametros del asistente
 */
public class JDialogWizard extends JDialog implements ComponentListener {

    private static final long serialVersionUID = 1L;

    private JDialogWizard ventanaAnterior;

    public void setTitulo(String titulo) {
        setTitle(titulo);
    }

    public JDialogWizard () {
        // Configuracion de la ventana
        setDefaultLookAndFeelDecorated(false);
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setModal(true);
        addComponentListener(this);

        // Icono de @firma
        setIconImage(new ImageIcon(getClass().getResource("/resources/images/afirma_ico.png")).getImage());

        // Dimensiones de la ventana
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        if (Platform.getOS().equals(Platform.OS.LINUX))
            setBounds((screenSize.width - 700) / 2, (screenSize.height - 440) / 2, 700, 440);
        else
            setBounds((screenSize.width - 630) / 2, (screenSize.height - 440) / 2, 630, 440);
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

    @Override
    public void componentHidden(ComponentEvent e) { /* No es necesario implementarlo */ }

    @Override
    public void componentMoved(ComponentEvent e) { /* No es necesario implementarlo */ }

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

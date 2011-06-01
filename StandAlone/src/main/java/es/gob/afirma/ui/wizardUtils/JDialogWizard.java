/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */
package es.gob.afirma.ui.wizardUtils;

import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;

import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.WindowConstants;

import es.gob.afirma.misc.Platform;

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
		setIconImage(new ImageIcon(getClass().getResource("/images/afirma_ico.png")).getImage());
        
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

	public void componentHidden(ComponentEvent e) {
		
	}

	public void componentMoved(ComponentEvent e) {
		
	}

	public void componentResized(ComponentEvent e) {
		
	}

	/**
	 * Cuando se muestra la ventana modal ocultamos la pagina anterior
	 */
	public void componentShown(ComponentEvent e) {
		if (ventanaAnterior != null)
			ventanaAnterior.dispose();
	}
}

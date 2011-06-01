/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */
package es.gob.afirma.ui.wizardsobres;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JPanel;

import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.ImagenLateral;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;
import es.gob.afirma.ui.wizardUtils.PanelesTexto;

/**
 *
 * Panel explicativo de finalización
 */
public class PanelFinalizar extends JDialogWizard {

	private static final long serialVersionUID = 1L;

	/**
	 * Guarda todas las ventanas del asistente para poder controlar la botonera
	 * @param ventanas	Listado con todas las paginas del asistente
	 */
	public void setVentanas(List<JDialogWizard> ventanas) {
		BotoneraInferior botonera = new BotoneraInferior(ventanas, 3);
		getContentPane().add(botonera, BorderLayout.PAGE_END);
	}
	
    public PanelFinalizar() {
        initComponents();
    }

    /**
     * Inicializacion de componentes
     */
    private void initComponents() {
    	// Titulo de la ventana
    	setTitulo(Messages.getString("Wizard.sobres.titulo"));
    	
    	// Panel con la imagen lateral
        ImagenLateral panelIzdo = new ImagenLateral();
        getContentPane().add(panelIzdo, BorderLayout.WEST);
    	
    	// Panel central
        JPanel panelCentral = new JPanel();
        panelCentral.setBackground(Color.WHITE);
        panelCentral.setLayout(new GridBagLayout());
    	
        // Configuramos el layout
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.insets = new Insets(20, 20, 0, 20);
		c.weightx = 1.0;
		c.gridx = 0;
        
        // Etiqueta felicidades
    	JLabel etiqueta = new JLabel(Messages.getString("Wizard.sobres.final1")); // NOI18N
    	panelCentral.add(etiqueta, c);
    	
    	// Panel con el texto "El sobre digital..."
    	panelCentral.add(PanelesTexto.generarPanelTexto(
    			"Wizard.sobres.final2", true), c);
    	
    	// Imagen de DNIe
    	JLabel etiquetaDNIe = new JLabel();
    	etiquetaDNIe.setIcon(new javax.swing.ImageIcon(getClass().getResource("/images/dnie.png")));
        panelCentral.add(etiquetaDNIe, c);
        
        c.weighty = 1.0;
        
        // Panel con el texto "Recuerde que si ha..."
        panelCentral.add(PanelesTexto.generarPanelTexto(
        		"Wizard.sobres.final3", true), c);

    	getContentPane().add(panelCentral, BorderLayout.CENTER);
    }
}

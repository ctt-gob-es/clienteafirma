/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardmultifirmacofirma;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.List;

import javax.swing.JPanel;

import es.gob.afirma.ui.utils.InfoLabel;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;
import es.gob.afirma.ui.wizardUtils.BotoneraInferior;
import es.gob.afirma.ui.wizardUtils.ImagenLateral;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;

/**
 * Panel explicativo de presentacion
 */
public class PanelPresentacion extends JAccessibilityDialogWizard {

	private static final long serialVersionUID = 1L;

    public PanelPresentacion() {
        initComponents();
    }
    
    @Override
	public int getMinimumRelation(){
		return 9;
	}
	
    /**
     * Guarda todas las ventanas del asistente para poder controlar la botonera
     * @param ventanas	Listado con todas las paginas del asistente
     */
    public void setVentanas(List<JDialogWizard> ventanas) {
    	BotoneraInferior botonera = new BotoneraInferior(ventanas, 0);
    	getContentPane().add(botonera, BorderLayout.PAGE_END);
    }
    
    /**
     * Inicializacion de componentes
     */
    private void initComponents() {
    	// Titulo de la ventana
    	setTitulo(Messages.getString("Wizard.multifirma.simple.cofirma.titulo"));
    	
    	// Panel con la imagen lateral
        ImagenLateral panelIzdo = new ImagenLateral();
        getContentPane().add(panelIzdo, BorderLayout.WEST);
        
        // Panel con el contenido
        JPanel panelCentral = new JPanel();
        panelCentral.setBackground(Color.WHITE);
        Utils.setContrastColor(panelCentral);
        panelCentral.setLayout(new GridBagLayout());

        // Configuramos el layout
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(20, 20, 20, 20);
		c.weightx = 1.0;
		c.weighty = 1.0;
		c.gridx = 0;
		c.gridy = 0;
		c.anchor = GridBagConstraints.NORTHWEST;
		
		//Etiqueta con el texto "Bienvenido al asistente..."
        String textLabel = Messages.getString("Wizard.multifirma.simple.cofirma.presentacion.texto1") +
        		"<br>"+"<br>"+Messages.getString("Wizard.multifirma.simple.cofirma.presentacion.texto2")+
        		"<br>"+"<br>"+Messages.getString("Wizard.multifirma.simple.cofirma.presentacion.texto3")+
        		"<br>"+"<br>"+Messages.getString("Wizard.multifirma.simple.cofirma.presentacion.texto4");
        InfoLabel presentationLabel = new InfoLabel(textLabel, false);
        
        panelCentral.add(presentationLabel, c);
        
        getContentPane().add(panelCentral, BorderLayout.CENTER);
    }
}

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

import java.util.ArrayList;
import java.util.List;

import es.gob.afirma.ui.wizardUtils.JDialogWizard;


/**
 *
 * Clase principal que contiene la operatividad de movimiento dentro del wizard
 * y que contiene los paneles de explicacion y contenido para crear un sobre digital.
 */
public class AsistenteEnsobrar {

	/**
	 * Tipo de ensobrado
	 */
	private Integer tipo = 0;
	
	/**
	 * Ruta del fichero a ensobrar
	 */
	private String rutaFichero;
	
    public AsistenteEnsobrar(String rutafichero, Integer tipo) {
        this.rutaFichero = rutafichero;
        this.tipo = tipo;
        initComponents();
    }

    /**
     * Inicializacion de componentes
     */
    private void initComponents() {   	
    	// Generamos la lista para el control de la botonera
    	List<JDialogWizard> ventanas = new ArrayList<JDialogWizard>();
    	
    	// Obtenemos todas las paginas
    	// Pagina 1: Panel presentacion
    	PanelPresentacion panelPresentacion = new PanelPresentacion();
    	ventanas.add(panelPresentacion);
    	
    	// Pagina 2: Panel seleccion destinatarios
    	PanelDestinatarios panelDestinatarios = new PanelDestinatarios();
    	ventanas.add(panelDestinatarios);
    	
    	// Pagina 3: Panel seleccion remitentes y ensobrado
    	PanelRemitentes panelRemitentes = new PanelRemitentes(rutaFichero, tipo);
    	ventanas.add(panelRemitentes);
    	
    	// Pagina 4: 
    	PanelFinalizar panelFinalizar = new PanelFinalizar();
    	ventanas.add(panelFinalizar);
    	
    	// Cargamos el listado de ventanas en todas las paginas con controles
    	// para inicializar sus botoneras
        panelPresentacion.setVentanas(ventanas);
        panelDestinatarios.setVentanas(ventanas);
        panelRemitentes.setVentanas(ventanas);
        panelFinalizar.setVentanas(ventanas);
        
        // Mostramos la primera ventana
        panelPresentacion.setVisible(true);
    }
}
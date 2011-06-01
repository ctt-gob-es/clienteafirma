/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */
package es.gob.afirma.ui.wizardcifradoclave;

import java.util.ArrayList;
import java.util.List;

import es.gob.afirma.ui.wizardUtils.JDialogWizard;

/**
 * Clase principal del asistente de cifrado de clave. 
 */
public class AsistenteCifradoClave {

	private static final long serialVersionUID = 1L;

	/**
	 * Algoritmo de cifrado seleccionado
	 */
	private String algoritmo;
	
	/**
	 * Ruta donde se encuentra el archivo a cifrar
	 */
	private String rutaFichero;

    public AsistenteCifradoClave(String algoritmo, String rutaFichero) {
        this.algoritmo = algoritmo;
        this.rutaFichero = rutaFichero;
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
    	
    	// Pagina 2: Panel contrasenias
    	PanelClaveCifrado panelClaveCifrado = new PanelClaveCifrado(algoritmo, rutaFichero);
    	ventanas.add(panelClaveCifrado);
    	
    	// Pagina 3: Dialogo finalizar
    	PanelFinalizar panelFinalizar = new PanelFinalizar();
    	ventanas.add(panelFinalizar);
        
        // Cargamos el listado de ventanas en todas las paginas con controles
    	// para inicializar sus botoneras
        panelPresentacion.setVentanas(ventanas);
        panelClaveCifrado.setVentanas(ventanas);
        panelFinalizar.setVentanas(ventanas);
        
        // Mostramos la primera ventana
        panelPresentacion.setVisible(true);
    }
}

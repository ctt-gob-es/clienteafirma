/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */
package es.gob.afirma.ui.wizardmultifirmamasiva;

import java.util.ArrayList;
import java.util.List;

import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;

/**
 *
 * Clase que realiza la multifirma masiva.
 */
public class AsistenteMultifirmaMasiva {
	
    /**
	 * Configuracion del KeyStore
	 */
    private KeyStoreConfiguration kssc;
    
    /**
	 * Indica si debe emitir un beep al finalizar
	 */
    private Boolean beep;

    public AsistenteMultifirmaMasiva(KeyStoreConfiguration kssc, Boolean beep) {
        this.kssc = kssc;
        this.beep = beep;
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
    	
    	// Pagina 2: Panel seleccion de algoritmos
    	PanelFormatos panelAlgoritmos = new PanelFormatos();
    	ventanas.add(panelAlgoritmos);
    	
    	// Pagina 3: Panel configuracion
    	PanelConfiguracion panelConfiguracion = new PanelConfiguracion();
    	ventanas.add(panelConfiguracion);
    	
    	// Pagina 4: 
    	PanelEntrada contenidoPantalla3 = new PanelEntrada();
    	ventanas.add(contenidoPantalla3);
    	
    	// Pagina 5: Panel multifirma masiva
    	PanelMultifirmaMasiva panelMultifirma = new PanelMultifirmaMasiva(kssc, beep);
    	ventanas.add(panelMultifirma);
    	
    	// Pagina 6: Dialogo finalizar
    	PanelFinalizar panelFinalizar = new PanelFinalizar();
    	ventanas.add(panelFinalizar);
        
        // Cargamos el listado de ventanas en todas las paginas con controles
    	// para inicializar sus botoneras
        panelPresentacion.setVentanas(ventanas);
        panelAlgoritmos.setVentanas(ventanas);
        panelConfiguracion.setVentanas(ventanas);
        contenidoPantalla3.setVentanas(ventanas);
        panelMultifirma.setVentanas(ventanas);
        panelFinalizar.setVentanas(ventanas);
        
        // Mostramos la primera ventana
        panelPresentacion.setVisible(true);
    }
}

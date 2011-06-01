/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de España (opcional: correo de contacto)
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3  según las
 * condiciones que figuran en el fichero 'licence' que se acompaña.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */
package es.gob.afirma.ui.wizardmultifirmacontrafirma;

import java.util.ArrayList;
import java.util.List;

import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.ui.wizardUtils.JDialogWizard;

/**
 *
 * Clase que realiza contrafirma
 */
public class AsistenteContrafirmas {
	
	/**
	 * Configuracion del KeyStore
	 */
    private KeyStoreConfiguration kssc;

    public AsistenteContrafirmas(KeyStoreConfiguration kss ) {
        this.kssc = kss;
        initComponents();
    }
	
	/**
	 * Inicializar componentes
	 */
	private void initComponents() {
		// Generamos la lista para el control de la botonera
    	List<JDialogWizard> ventanas = new ArrayList<JDialogWizard>();
    	
    	// Obtenemos todas las paginas
    	// Pagina 1: Panel presentacion
    	PanelPresentacion panelPresentacion = new PanelPresentacion();
    	ventanas.add(panelPresentacion);
    	
    	// Pagina 2: Panel entrada de un archivo
    	PanelEntrada panelEntrada = new PanelEntrada(kssc);
    	ventanas.add(panelEntrada);
    	
    	// Pagina 3: Panel multifirma de un archivo
    	PanelMultifirma panelMultifirma = new PanelMultifirma(kssc);
    	ventanas.add(panelMultifirma);
    	
    	// Pagina 4: Dialogo finalizar
    	PanelFinalizar panelFinalizar = new PanelFinalizar();
    	ventanas.add(panelFinalizar);
        
        // Cargamos el listado de ventanas en todas las paginas con controles
    	// para inicializar sus botoneras
        panelPresentacion.setVentanas(ventanas);
        panelEntrada.setVentanas(ventanas);
        panelMultifirma.setVentanas(ventanas);
        panelFinalizar.setVentanas(ventanas);
        
        // Mostramos la primera ventana
        panelPresentacion.setVisible(true);
	}
}

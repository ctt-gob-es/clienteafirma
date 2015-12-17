/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardmultifirmacontrafirma;

import java.util.ArrayList;
import java.util.List;

import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.ui.wizardutils.JDialogWizard;

/**
 *
 * Clase que realiza contrafirma
 */
public final class AsistenteContrafirmas {

	/**
	 * Configuracion del KeyStore
	 */
    private final KeyStoreConfiguration kssc;

    /** Construye un nuevo asistente de contrafirma.
     * @param kss Configuraci&oacute;n del almac&eacute;n de claves */
    public AsistenteContrafirmas(final KeyStoreConfiguration kss ) {
        this.kssc = kss;
        initComponents();
    }

	/**
	 * Inicializar componentes
	 */
	private void initComponents() {
		// Generamos la lista para el control de la botonera
    	final List<JDialogWizard> ventanas = new ArrayList<>();

    	// Obtenemos todas las paginas
    	// Pagina 1: Panel presentacion
    	final PanelPresentacion panelPresentacion = new PanelPresentacion();
    	ventanas.add(panelPresentacion);

    	// Pagina 2: Panel entrada de un archivo
    	final PanelEntrada panelEntrada = new PanelEntrada(this.kssc);
    	ventanas.add(panelEntrada);

    	// Pagina 3: Panel multifirma de un archivo
    	final PanelMultifirma panelMultifirma = new PanelMultifirma(this.kssc);
    	ventanas.add(panelMultifirma);

    	// Pagina 4: Dialogo finalizar
    	final PanelFinalizar panelFinalizar = new PanelFinalizar();
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

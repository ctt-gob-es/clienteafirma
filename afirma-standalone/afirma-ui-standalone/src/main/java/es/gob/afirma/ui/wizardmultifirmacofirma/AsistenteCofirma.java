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

import java.util.ArrayList;
import java.util.List;

import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.ui.wizardutils.JDialogWizard;

/**
 * Clase que realiza la cofirma.
 */
public final class AsistenteCofirma {

	/**
	 * Configuracion del KeyStore
	 */
    private final KeyStoreConfiguration kssc;

    /** Construye un nuevo asistente de cofirma.
     * @param kss Configuraci&oacute;n del almac&eacute;n de claves */
    public AsistenteCofirma(final KeyStoreConfiguration kss ) {
        this.kssc = kss;
        initComponents();
    }

    /**
     * Inicializacion de componentes
     */
    private void initComponents() {
    	// Generamos la lista para el control de la botonera
    	final List<JDialogWizard> ventanas = new ArrayList<>();

    	// Obtenemos todas las paginas
    	// Pagina 1: Panel presentacion
    	final PanelPresentacion panelPresentacion = new PanelPresentacion();
    	ventanas.add(panelPresentacion);

    	// Pagina 2: Panel descifrar por contrasena
    	final PanelCofirma panelContrasenia = new PanelCofirma(this.kssc);
    	ventanas.add(panelContrasenia);

    	// Pagina 3: Dialogo finalizar
    	final PanelFinalizar panelFinalizar = new PanelFinalizar();
    	ventanas.add(panelFinalizar);

        // Cargamos el listado de ventanas en todas las paginas con controles
    	// para inicializar sus botoneras
        panelPresentacion.setVentanas(ventanas);
        panelContrasenia.setVentanas(ventanas);
        panelFinalizar.setVentanas(ventanas);

        // Mostramos la primera ventana
        panelPresentacion.setVisible(true);
    }
}

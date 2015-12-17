/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizarddescifradocontrasenia;

import java.util.ArrayList;
import java.util.List;

import es.gob.afirma.ui.wizardutils.JDialogWizard;



/**
 *
 * Clase principal del wizard de descifrado de contrasena. Contiene a los paneles de
 * explicacion y contenido.
 */
public final class AsistenteDescifradoContrasenia {

    private final String algoritmo;

    private final String rutaFichero;

    /** Construye un nuevo asistente de cifrado por contrase&ntilde;a.
     * @param algoritmo ALgoritmo de cifrado
     * @param rutaFichero Fichero a cifrar */
    public AsistenteDescifradoContrasenia(final String algoritmo, final String rutaFichero) {
        this.algoritmo = algoritmo;
        this.rutaFichero  =rutaFichero;
        initComponents();
    }

    /**
     * Inicializamos componentes
     */
    private void initComponents() {
    	// Generamos la lista para el control de la botonera
    	final List<JDialogWizard> ventanas = new ArrayList<>();

    	// Obtenemos todas las paginas
    	// Pagina 1: Panel presentacion
    	final PanelPresentacion panelPresentacion = new PanelPresentacion();
    	ventanas.add(panelPresentacion);

    	// Pagina 2: Panel descifrar por contrasena
    	final PanelContrasenia panelContrasenia = new PanelContrasenia(this.algoritmo, this.rutaFichero);
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

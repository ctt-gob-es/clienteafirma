/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardcifradoclave;

import java.util.ArrayList;
import java.util.List;

import es.gob.afirma.ui.wizardutils.JDialogWizard;

/** Clase principal del asistente de cifrado de clave. */
public final class AsistenteCifradoClave {

    /** Algoritmo de cifrado seleccionado */
    private final String algoritmo;

    /** Ruta donde se encuentra el archivo a cifrar */
    private final String rutaFichero;

    /** Construye un nuevo asistente de cifrado por clave.
     * @param algoritmo Algoritmo de cifrado
     * @param rutaFichero Fichero a cifrar */
    public AsistenteCifradoClave(final String algoritmo, final String rutaFichero) {
        this.algoritmo = algoritmo;
        this.rutaFichero = rutaFichero;
        initComponents();
    }

    /** Inicializacion de componentes */
    private void initComponents() {
        // Generamos la lista para el control de la botonera
        final List<JDialogWizard> ventanas = new ArrayList<>();

        // Obtenemos todas las paginas
        // Pagina 1: Panel presentacion
        final PanelPresentacion panelPresentacion = new PanelPresentacion();
        ventanas.add(panelPresentacion);

        // Pagina 2: Panel contrasenias
        final PanelClaveCifrado panelClaveCifrado = new PanelClaveCifrado(this.algoritmo, this.rutaFichero);
        ventanas.add(panelClaveCifrado);

        // Pagina 3: Dialogo finalizar
        final PanelFinalizar panelFinalizar = new PanelFinalizar();
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

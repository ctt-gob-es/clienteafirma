/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardsobresremitentes;

import java.util.ArrayList;
import java.util.List;

import es.gob.afirma.ui.wizardutils.JDialogWizard;

/** Clase principal que contiene la operatividad de movimiento dentro del wizard
 * y que contiene los paneles de explicacion y contenido para anadir nuevos remitentes
 * a un sobre digital. */
public final class AsistenteRemitentes {

    /** Ruta del fichero a ensobrar */
    private final String rutaFichero;

    /** Construye un nuevo asistente para el ensobrado digital de datos.
     * @param rutafichero Fichero a ensobrar */
    public AsistenteRemitentes(final String rutafichero) {
        this.rutaFichero = rutafichero;
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

        // Pagina 2: Panel anadir nuevos remitentes
        final PanelRemitentes panelDestinatarios = new PanelRemitentes(this.rutaFichero);
        ventanas.add(panelDestinatarios);

        // Pagina 3: Panel finalizar
        final PanelFinalizar panelFinalizar = new PanelFinalizar();
        ventanas.add(panelFinalizar);

        // Cargamos el listado de ventanas en todas las paginas con controles
        // para inicializar sus botoneras
        panelPresentacion.setVentanas(ventanas);
        panelDestinatarios.setVentanas(ventanas);
        panelFinalizar.setVentanas(ventanas);

        // Mostramos la primera ventana
        panelPresentacion.setVisible(true);
    }
}

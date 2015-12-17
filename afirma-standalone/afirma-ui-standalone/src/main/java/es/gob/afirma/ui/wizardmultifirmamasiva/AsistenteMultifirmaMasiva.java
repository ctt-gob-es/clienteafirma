/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardmultifirmamasiva;

import java.util.ArrayList;
import java.util.List;

import es.gob.afirma.keystores.KeyStoreConfiguration;
import es.gob.afirma.ui.wizardutils.JDialogWizard;

/** Clase que realiza la multifirma masiva. */
public final class AsistenteMultifirmaMasiva {

    /** Indica si debe emitir un beep al finalizar */
    private final boolean beep;

    /** Configuracion del KeyStore */
    private final KeyStoreConfiguration kssc;

    /** Construye un nuevo asistente de multifirmas masivas.
     * @param kssc Configuraci&oacute;n del almac&eacute;n de claves
     * @param beep <code>true</code> habilita la reproducci&oacute;n de sonidos de advertencia,
     *             <code>false</code> evita el uso de estos */
    public AsistenteMultifirmaMasiva(final KeyStoreConfiguration kssc, final boolean beep) {
        this.kssc = kssc;
        this.beep = beep;
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

        // Pagina 2: Panel seleccion de algoritmos
        final PanelFormatos panelAlgoritmos = new PanelFormatos();
        ventanas.add(panelAlgoritmos);

        // Pagina 3: Panel configuracion
        final PanelConfiguracion panelConfiguracion = new PanelConfiguracion();
        ventanas.add(panelConfiguracion);

        // Pagina 4:
        final PanelEntrada contenidoPantalla3 = new PanelEntrada();
        ventanas.add(contenidoPantalla3);

        // Pagina 5: Panel multifirma masiva
        final PanelMultifirmaMasiva panelMultifirma = new PanelMultifirmaMasiva(this.kssc, this.beep);
        ventanas.add(panelMultifirma);

        // Pagina 6: Dialogo finalizar
        final PanelFinalizar panelFinalizar = new PanelFinalizar();
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

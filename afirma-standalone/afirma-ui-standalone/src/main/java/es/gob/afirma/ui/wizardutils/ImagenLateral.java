/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardutils;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;

/** Clase para generar la imagen lateral de los asistente */
public class ImagenLateral extends JPanel {

    private static final long serialVersionUID = 1L;
    private Dimension dimensiones = new Dimension(145, 388);
    private String rutaImagen = "/resources/images/nubes.png"; //$NON-NLS-1$

    /** Construye la imagen lateral del asistente. */
    public ImagenLateral() {
        initComponents();
    }

    /** Inicializa la imagen a mostrar en el panel
     * @param rutaImagen Ruta del panel */
    public ImagenLateral(final String rutaImagen) {
        this.rutaImagen = rutaImagen;
        initComponents();
    }

    /** Inicializa la imagen a mostrar en el panel
     * @param rutaImagen Ruta del panel
     * @param dimensiones Dimensiones del panel */
    public ImagenLateral(final String rutaImagen, final Dimension dimensiones) {
        this.rutaImagen = rutaImagen;
        this.dimensiones = dimensiones;
        initComponents();
    }

    /** Inicializamos los componentes */
    private void initComponents() {
        // Configuracion panel
        setLayout(new BorderLayout());
        setPreferredSize(this.dimensiones);
        setBackground(Color.WHITE); // El fondo se pone a blanco

        final JLabel etiqueta = new JLabel();
        etiqueta.setIcon(new ImageIcon(getClass().getResource(this.rutaImagen))); // NOI18N

        add(etiqueta, BorderLayout.CENTER);
    }
}

/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.utils;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JLabel;

import es.gob.afirma.ui.principal.Main;

/** @author A171648
 *         Componente para generar una barra de estado */
public class JStatusBar extends JComponent {

    private static final long serialVersionUID = 1L;

    /** Genera una barra de estado */
    public JStatusBar() {
        setPreferredSize(new Dimension(10, 20));
        setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));
        setBorder(BorderFactory.createCompoundBorder(BorderFactory.createLoweredBevelBorder(),
                                                     BorderFactory.createEmptyBorder(0, 0, 0, 0)));
    }

    /** Devuelve el valor de la barra de estado
     * @return Valor de la barra de estado */
    public String getStatus() {
        final JLabel lb = (JLabel) getComponent(0);
        if (lb.getText() != null) {
            return lb.getText();
        }
        return null;
    }

    /** Define el tamano de la etiqueta
     * @param width Ancho de la etiqueta
     * @param height Alto de la etiqueta */
    public void setLabelSize(final int width, final int height) {
        final JLabel etiqueta = (JLabel) getComponent(0);
        etiqueta.setPreferredSize(new Dimension(width, height));
    }

    /** Define la lontigud de la etiqueta
     * @param width Ancho de la etiqueta */
    public void setLabelWidth(final int width) {
        final JLabel etiqueta = new JLabel();
        etiqueta.setPreferredSize(new Dimension(width, getPreferredSize().height));
        add(etiqueta);
    }

    /** Establece el margen izquierdo del texto
     * @param leftMargin Margen izquierdo */
    public void setLeftMargin(final int leftMargin) {
        setBorder(BorderFactory.createCompoundBorder(BorderFactory.createLoweredBevelBorder(), BorderFactory.createEmptyBorder(0, leftMargin, 0, 0)));
    }

    /** Modifica el texto de la barra de estado
     * @param status Nuevo texto para la barra de estado */
    public void setStatus(final String status) {
        final JLabel lb = (JLabel) getComponent(0);
        // Control de texto en negrita para la barra de estado
        if (GeneralConfig.isFontBold()) {
            // Se pone el texto en negrita
            lb.setFont(new Font(lb.getFont().getName(), Font.BOLD, lb.getFont().getSize()));
        }
        else {
            // Se pone el texto en estilo normal
            lb.setFont(new Font(lb.getFont().getName(), Font.PLAIN, lb.getFont().getSize()));
        }
        if (Main.isOSHighContrast()) {
            lb.setForeground(Color.WHITE);
        }
        lb.setText(status);
    }
}

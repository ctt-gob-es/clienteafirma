/*
 * Este fichero forma parte del Cliente @firma. 
 * El Cliente @firma es un applet de libre distribución cuyo código fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Gobierno de España
 * Este fichero se distribuye bajo las licencias EUPL versión 1.1  y GPL versión 3, o superiores, según las
 * condiciones que figuran en el fichero 'LICENSE.txt' que se acompaña.  Si se   distribuyera este 
 * fichero individualmente, deben incluirse aquí las condiciones expresadas allí.
 */


package es.gob.afirma.cliente.interfaz;

import java.awt.BorderLayout;
import java.awt.GridLayout;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JTextField;


class ProgressStatus extends JComponent
{
    private JProgressBar barra = null;

    private JTextField text = null;

    private String message = "";

    private int maxValue = 0;

    static final long serialVersionUID = 1;

    public ProgressStatus(String info, String message)
    {
        this.message = message;

        setLayout(new BorderLayout());

        this.text = new JTextField();
        add(text, BorderLayout.NORTH);

        this.barra = new JProgressBar();

        JPanel panelInferior = new JPanel();
        panelInferior.setLayout(new GridLayout(0, 1));
        panelInferior.add(this.barra);
        panelInferior.add(new JLabel(info));

        add(panelInferior, BorderLayout.SOUTH);
    }

    public void setMaxValue(int value)
    {
        this.maxValue = value;

        this.barra.setMaximum(value);
    }

    public void updateValue(int value)
    {
        double percentage = 0.0;

        this.barra.setValue(value);

        if (this.maxValue > 0)
            percentage = ((double) value / this.maxValue) * 100;
        else
            percentage = 0.0;

        // Formateamos el valor obtenido
        String valorPorcentaje = new Double(percentage).toString();
        int dotIndex = valorPorcentaje.indexOf(".");
        if (valorPorcentaje.length() - dotIndex > 2)
            valorPorcentaje = valorPorcentaje.substring(0, dotIndex + 2);

        this.text.setText(this.message + " " + valorPorcentaje + "%");
    }
}

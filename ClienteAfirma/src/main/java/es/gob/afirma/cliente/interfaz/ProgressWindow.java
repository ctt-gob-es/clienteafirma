/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un aplicativo de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010,2011 Gobierno de Espana
 * Este fichero se distribuye bajo  bajo licencia GPL version 2  segun las
 * condiciones que figuran en el fichero 'licence' que se acompana. Si se distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */

package es.gob.afirma.cliente.interfaz;

import java.awt.BorderLayout;
import java.awt.HeadlessException;

import javax.swing.JFrame;
import javax.swing.WindowConstants;

/** Di&aacute;logo de progreso. */
public final class ProgressWindow extends JFrame {

    private static final long serialVersionUID = 1L;

    private final ProgressStatus status;

    private final long max;

    private int progreso = 0;

    /** Crea un di&aacute;logo de progreso
     * @param windowTitle
     *        T&iacute;tulo de la ventana
     * @param statusTitle
     *        T&iacute;tulo de la l&iacute;nea de estado
     * @param max
     *        Valor m&aacute;ximo de la barra de progreso
     * @throws HeadlessException
     *         Si no hay interfaz gr&aacute;fico */
    public ProgressWindow(final String windowTitle, final String statusTitle, final long max) throws HeadlessException {
        super(windowTitle);

        this.max = max;
        this.status = new ProgressStatus(statusTitle, "   ");

        status.setMaxValue(Integer.MAX_VALUE);

        getContentPane().add(status, BorderLayout.CENTER);
        setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        setSize(400, 150);
        setLocationRelativeTo(null);
        setVisible(true);
    }

    /** Incrementa la barra de progreso.
     * @param amount
     *        Cantidad de incremento de la barra de progreso */
    public void inc(final int amount) {
        progreso += amount;

        final double ratio = (double) progreso / max;
        final int newVal = (int) (ratio * Integer.MAX_VALUE);

        status.updateValue(newVal);

        status.paint(status.getGraphics());
    }
}

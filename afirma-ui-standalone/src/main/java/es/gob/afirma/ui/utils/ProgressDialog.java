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

import java.awt.Container;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.WindowConstants;

/**
 * Di&aacute;logo con barra de progreso.
 */
public class ProgressDialog {

    /** Componente padre del di&aacute;logo. */ 
    private JDialog parent = null;
    
    /** Basrra de progreso. */
    private JProgressBar progressBar = null;
    
    /** Texto que se muestra sobre la barra de progreso. */
    private JLabel ltexto = null;
    
    /** Di&aacute;logo con la barra de progreso. */
    private JDialog dialog = null;
    
    /** valor actual de la barra de progreso. */
    private int currentValue = 0;
    
    /**
     * Crea un di&aacute;logo pero no lo muestra.
     * @param parent Componente padre sobre el que se mostrara el di&aacute;logo.
     * @param maxValue Valor m&aacute;ximo de la barra de progres (maxValue > 0).
     * @param title T&iacute;tulo del di&aacute;logo.
     */
    public ProgressDialog(JDialog parent, int maxValue, String title) {
        
        if(maxValue <= 0)
            throw new IllegalArgumentException("El valor maximo de la barra de progreso no puede ser nulo"); //$NON-NLS-1$
         
        // Barra de progreso
        this.parent = parent; 
        this.progressBar = new JProgressBar(0, maxValue);
        this.ltexto = new JLabel(""); //$NON-NLS-1$
    }

    /** Muestra el di&aacute;logo con al barra de progreso. */
    public void show() {
        Container panel = new Container();
        this.progressBar.setValue(0);
        this.progressBar.setStringPainted(true);
        panel.setLayout(null);
        panel.add(this.ltexto);
        this.ltexto.setBounds(20, 15, 250, 20);
        panel.add(this.progressBar);
        this.progressBar.setBounds(20, 35, 260, 19);

        this.dialog = new JDialog(this.parent, Messages.getString("Wizard.multifirma.progress.titulo"), false); //$NON-NLS-1$
        this.dialog.setBounds(this.parent.getX()+(this.parent.getWidth()-300)/2, this.parent.getY()+(this.parent.getHeight()-100)/2, 300, 100);
        this.dialog.setResizable(false);
        this.dialog.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        this.dialog.setVisible(true);
    }
    
    /** Cierra el di&aacute;logo. */
    public void close() {
        if (this.dialog != null)
            this.dialog.setVisible(false);
    }
    
    /**
     * Aumenta en una unidad el valor de la barra de progreso e indica estar procesando el elemento indicado.
     * @param elementName Nombre del elemento que se esta procesando.
     */
    public synchronized void processElement(String elementName) {
        if(elementName == null) 
            this.ltexto.setText(Messages.getString("Procesando")); //$NON-NLS-1$
        else 
            this.ltexto.setText(Messages.getString("Procesando.elemento", elementName)); //$NON-NLS-1$
        
        this.progressBar.setValue(++this.currentValue);
    }
}

/*
 * Este fichero forma parte del Cliente @firma.
 * El Cliente @firma es un applet de libre distribucion cuyo codigo fuente puede ser consultado
 * y descargado desde www.ctt.map.es.
 * Copyright 2009,2010 Ministerio de la Presidencia, Gobierno de Espana
 * Este fichero se distribuye bajo licencia GPL version 3 segun las
 * condiciones que figuran en el fichero 'licence' que se acompana.  Si se   distribuyera este
 * fichero individualmente, deben incluirse aqui las condiciones expresadas alli.
 */
package es.gob.afirma.ui.wizardUtils;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextPane;

import es.gob.afirma.ui.utils.Messages;

/**
 * Clase para generar la parte superior de los asistentes
 */
public class CabeceraAsistente extends JPanel {

	private static final long serialVersionUID = 1L;
	
	private String MessagesTitulo;
	private String MessagesDescripcion;
	private Dimension dimensiones = new Dimension(607, 110);
	private Boolean bloqueTexto = false;

	/**
	 * Genera una cabecera para un asistente
	 * @param MessagesTitulo		Texto para obtener del ResourceMessages el titulo del asistente
	 * @param MessagesDescripcion	Texto para obtener del ResourceMessages la descripcion del asistente
	 */
	public CabeceraAsistente(String MessagesTitulo, String MessagesDescripcion) {
		this.MessagesTitulo = MessagesTitulo;
		this.MessagesDescripcion = MessagesDescripcion;
		
        initComponents();
    }
	
	/**
	 * Genera una cabecera para un asistente
	 * @param MessagesTitulo		Texto para obtener del ResourceMessages el titulo del asistente
	 * @param MessagesDescripcion	Texto para obtener del ResourceMessages la descripcion del asistente
	 * @param dimensiones		Dimensiones de la cabecera
	 */
	public CabeceraAsistente(String MessagesTitulo, String MessagesDescripcion, Dimension dimensiones) {
		this.MessagesTitulo = MessagesTitulo;
		this.MessagesDescripcion = MessagesDescripcion;
		this.dimensiones = dimensiones;
		
        initComponents();
    }
	
	/**
	 * Genera una cabecera para un asistente
	 * @param MessagesTitulo		Texto para obtener del ResourceMessages el titulo del asistente
	 * @param MessagesDescripcion	Texto para obtener del ResourceMessages la descripcion del asistente
	 * @param dimensiones		Dimensiones de la cabecera. Puede tomar el valor null y en tal caso se
	 * 							asignaran las dimensiones predeterminadas
	 * @param bloqueTexto		True: La descripcion tiene mas de una linea
	 * 							False: La descripcion tiene solo una linea
	 */
	public CabeceraAsistente(String MessagesTitulo, String MessagesDescripcion, Dimension dimensiones, 
			Boolean bloqueTexto) {
		this.MessagesTitulo = MessagesTitulo;
		this.MessagesDescripcion = MessagesDescripcion;
		if (dimensiones != null)
			this.dimensiones = dimensiones;
		
		this.bloqueTexto = bloqueTexto;
		
        initComponents();
    }

    /**
     * Inicializa componentes
     */
    private void initComponents() {
    	// Configuracion de la ventana
    	setBackground(Color.WHITE);
    	setPreferredSize(this.dimensiones);
        setBorder(BorderFactory.createEtchedBorder());
        setLayout(new GridBagLayout());

        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(1, 10, 0, 10);
		c.weightx = 1.0;
		c.gridx = 0;
		c.weighty = 1.0;
        
    	// Etiqueta con el titulo de la ventana
    	JLabel etiquetaTitulo = new JLabel();
    	etiquetaTitulo.setFont(new Font(getFont().getFamily(), 1, getFont().getSize()));
    	etiquetaTitulo.setText(Messages.getString(this.MessagesTitulo)); // NOI18N
    	add(etiquetaTitulo, c);
    	
    	c.insets = new Insets(0, 15, 0, 10);
    	c.weighty = 1.0;
		
    	if (this.bloqueTexto == false) {
	    	// Etiqueta con la descripcion de la ventana
	    	JLabel etiquetaDescripcion = new JLabel();
	    	etiquetaDescripcion.setText(Messages.getString(this.MessagesDescripcion));
	        add(etiquetaDescripcion, c);
    	}
    	else {
    		JTextPane textoPanel = PanelesTexto.generarPanelTexto(this.MessagesDescripcion, true);
	        add(textoPanel, c);
    	}
    }
}

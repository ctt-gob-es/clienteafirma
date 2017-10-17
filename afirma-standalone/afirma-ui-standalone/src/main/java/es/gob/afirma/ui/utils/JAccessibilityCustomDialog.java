/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.ui.utils;

import java.awt.Image;
import java.awt.Rectangle;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JFrame;


/**
 * Componente que define un dialogo de alerta.
 * @author inteco
 *
 */
abstract class JAccessibilityCustomDialog extends JDialog {

	/** Ruta del JAR en donde se almacenan los iconos de la aplicaci&oacute;n. */
    private static final String ICON_DIR_PATH = "/resources/images/"; //$NON-NLS-1$

	private static final String AFIRMA_ICON_FILE = "afirma_ico.png"; //$NON-NLS-1$

	private final Image afirmaIcon = new ImageIcon(JAccessibilityCustomDialog.this.getClass().getResource(ICON_DIR_PATH + AFIRMA_ICON_FILE)).getImage();

	/**
	 * uid.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Posicion X actual.
	 */
	private static int actualPositionX = -1;

	/**
	 * Posicion Y actual.
	 */
	private static int actualPositionY = -1;

	/**
	 * Ancho actual.
	 */
	private static int actualWidth = -1;

	/**
	 * Alto actual.
	 */
	private static int actualHeight = -1;

	/**
	 * Indica si el dialogo requiere un tamano grande por defecto.
	 */
	private boolean bigSizeDefault = false;

	/**
	 * Constructor con parametros.
	 */
	JAccessibilityCustomDialog(final JDialog dialog, final boolean modal){
		super(dialog, modal);
		// Icono de @firma
        setIconImage(this.afirmaIcon);
		final ResizingAdaptor adaptador = new ResizingAdaptor(null,null,null,null,null,null,this,null);
		this.addComponentListener(adaptador);
		this.addComponentListener(new ComponentAdapter() {
			/**
			 * Evento que se lanza cuando el componente se redimensiona.
			 */
		    @Override
			public void componentResized(final ComponentEvent e)
		    {
		    	resized(e);
		    }
		    /**
			 * Evento que se lanza cuando el componente se mueve.
			 */
		    @Override
			public void componentMoved(final ComponentEvent e)
		    {
		    	resized(e);
		    }
		});
	}

	/**
	 * Constructor con parametros.
	 */
	JAccessibilityCustomDialog(final JFrame frame, final boolean modal){
		super(frame, modal);
		// Icono de @firma
        setIconImage(this.afirmaIcon);
		final ResizingAdaptor adaptador = new ResizingAdaptor(null,null,null,null,null,null,this,null);
		this.addComponentListener(adaptador);
		this.addComponentListener(new ComponentAdapter() {
			/**
			 * Evento que se lanza cuando el componente se redimensiona.
			 */
		    @Override
			public void componentResized(final ComponentEvent e)
		    {
		    	resized(e);
		    }
		    /**
			 * Evento que se lanza cuando el componente se mueve.
			 */
		    @Override
			public void componentMoved(final ComponentEvent e)
		    {
		    	resized(e);
		    }
		});
	}

	/**
	 * Constructor.
	 */
	JAccessibilityCustomDialog(){
		super();
		// Icono de @firma
        setIconImage(this.afirmaIcon);
		final ResizingAdaptor adaptador = new ResizingAdaptor(null,null,null,null,null,null,this,null);
		this.addComponentListener(adaptador);
		this.addComponentListener(new ComponentAdapter() {
			/**
			 * Evento que se lanza cuando el componente se redimensiona.
			 */
		    @Override
			public void componentResized(final ComponentEvent e)
		    {
		    	resized(e);
		    }
		    /**
			 * Evento que se lanza cuando el componente se mueve.
			 */
		    @Override
			public void componentMoved(final ComponentEvent e)
		    {
		    	resized(e);
		    }
		});


	}

	/**
	 * Relacion minima que se aplica para la redimension de los componentes.
	 * Cuanto menor es este numero menor es la redimension aplicada.
	 * @return int Relacion minima
	 */
	abstract int getMinimumRelation();


	/**
	 * Evento de redimensionado. Comprueba el tamanio de la ventana para habilitar o deshabilitar el boton
	 *  de Maximizar ventana. Tambien almacena el tamano y posicion de la ventana para su restauracion.
	 */
	void resized(final ComponentEvent e) {
		//Variable que controlara sin las dimensiones van a exceder el limite
		boolean limitControl = false;

		//Se obtienen las dimensiones de maximizado
		final int maxWidth = Constants.CUSTOMDIALOG_MAX_WIDTH;
		final int maxHeight = Constants.CUSTOMDIALOG_MAX_HEIGHT;

	    //Se comprueba las bounds del dialogo actual
	    if (e.getSource() instanceof CustomDialog) {
	    	final CustomDialog customDialog = (CustomDialog) e.getSource();
	    	final Rectangle rect = customDialog.getBounds();

	    	//Se comprueba que no sobrepasen el limite
	    	if (rect.width > maxWidth) {
	    		rect.width = maxWidth;
	    		limitControl = true;
	    	}
	    	if (rect.height > maxHeight) {
	    		rect.height = maxHeight;
	    		limitControl = true;
	    	}
	    	//Si sobrepasaban el limite, se hace un resize a las dimensiones limite indicadas
	    	if (limitControl) {
	    		//this.setBounds(rect.x, rect.y, rect.width, rect.height);
	    		this.setSize(rect.width, rect.height);
	    	}
	    }
	}


    /**
     * Getter para la variable ActualPositionX.
     * @return ActualPositionX
     */
	static int getActualPositionX() {
		return actualPositionX;
	}
	/**
     * Setter para la variable ActualPositionX.
     */
	static void setActualPositionX(final int actualPositionX) {
		JAccessibilityCustomDialog.actualPositionX = actualPositionX;
	}
	/**
     * Getter para la variable ActualPositionY.
     * @return ActualPositionY
     */
	static int getActualPositionY() {
		return actualPositionY;
	}
	/**
     * Setter para la variable ActualPositionY.
     */
	static void setActualPositionY(final int actualPositionY) {
		JAccessibilityCustomDialog.actualPositionY = actualPositionY;
	}
	/**
     * Getter para la variable ActualWidth.
     * @return ActualWidth
     */
	static int getActualWidth() {
		return actualWidth;
	}
	/**
     * Setter para la variable ActualWidth.
     */
	static void setActualWidth(final int actualWidth) {
		JAccessibilityCustomDialog.actualWidth = actualWidth;
	}
	/**
     * Getter para la variable ActualHeight.
     * @return ActualHeight
     */
	static int getActualHeight() {
		return actualHeight;
	}
	/**
     * Setter para la variable ActualHeight.
     */
	static void setActualHeight(final int actualHeight) {
		JAccessibilityCustomDialog.actualHeight = actualHeight;
	}

	/**
	 * Indica si el dialogo debe tener un tamano grande por defecto.
	 * @return boolean
	 */
	boolean isBigSizeDefault() {
		return this.bigSizeDefault;
	}

	/**
	 * Asigna la variable que indica si el dialogo debe tener un tamano grande por defecto.
	 * @param bigSizeDefault
	 */
	void setBigSizeDefault(final boolean bigSizeDefault) {
		this.bigSizeDefault = bigSizeDefault;
	}

}

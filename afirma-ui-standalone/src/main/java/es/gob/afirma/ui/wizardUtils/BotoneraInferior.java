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

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GraphicsEnvironment;
import java.awt.GridLayout;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.principal.PrincipalGUI;
import es.gob.afirma.ui.utils.Constants;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;

/**
 * Clase para generar la parte inferior de la ventana con la botonera
 */
public class BotoneraInferior extends JPanel {

	private static final long serialVersionUID = 1L;
	private Dimension dimensiones = new Dimension(603, 47);
	private List<JDialogWizard> ventanas;
	private int posicion;
	
	private JButton restaurar = null; 
	private JButton maximizar = null; 
	
	public List<JDialogWizard> getVentanas() {
		return this.ventanas;
	}
	
	/**
	 * Genera una botonera con la configuracion predefinida
	 * @param ventanas	Listado que contiene todas las ventanas en orden de aparicion
	 * @param posicion	Numero de la pagina
	 */
	public BotoneraInferior(List<JDialogWizard> ventanas, Integer posicion) {
		this.ventanas = ventanas;
		this.posicion = posicion;
		initParamenters();
	}
	
	/**
	 * Genera una botonera con unas dimensiones dadas
	 * @param dimensiones	Dimensiones de la botonera
	 */
	public BotoneraInferior(Dimension dimensiones) {
		this.dimensiones = dimensiones;
		initParamenters();
	}

	/**
	 * Inicializacion de parametros
	 */
	private void initParamenters() {
		// Configuracion del panel
    	setBorder(BorderFactory.createEtchedBorder());
    	setPreferredSize(this.dimensiones);
        setLayout(new FlowLayout(FlowLayout.CENTER, 1, 1));

        // Definicion de botones
        maximizar = new JButton();
        final JButton anterior = new JButton();
        final JButton siguiente = new JButton();
        final JButton cancelar = new JButton();
        final JButton finalizar = new JButton();
        
        JPanel panelMaximizar = new JPanel(new GridLayout(1, 1));
        //Boton maximizar
        maximizar.setText(Messages.getString("Wizard.maximizar"));
        maximizar.setName("maximizar");
        maximizar.setMnemonic(KeyEvent.VK_M);
        maximizar.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				maximizarActionPerformed();
			}
		});
        Utils.remarcar(maximizar);
        Utils.setContrastColor(maximizar);
        Utils.setFontBold(maximizar);
        
        panelMaximizar.add(maximizar);
        add(panelMaximizar);
        
        JPanel panelRestaurar = new JPanel(new GridLayout(1, 1));
	    // Boton restaurar
	    restaurar = new JButton();
	    restaurar.setText(Messages.getString("Wizard.restaurar"));
	    restaurar.setName("restaurar");
	    restaurar.setMnemonic(KeyEvent.VK_R);
	    restaurar.addActionListener(new ActionListener() {
	    	public void actionPerformed(ActionEvent e) {
	    		restaurarActionPerformed();
			}
		});
	    Utils.remarcar(restaurar);
        Utils.setContrastColor(restaurar);
	    Utils.setFontBold(restaurar);
	    
	    panelRestaurar.add(restaurar);
	    add(panelRestaurar);
	    
	    //Control de habilitado de los botones de maximizar y restaurar según la configuración de
	    //accesibilidad
	    if (GeneralConfig.isMaximized()){
        	maximizar.setEnabled(false);
        } else {
        	restaurar.setEnabled(false);
        }
        
        //Espacio entre botones
		JPanel panelVacio = new JPanel();
		panelVacio.setPreferredSize(new Dimension(60, 10));
		add(panelVacio);		
		
		JPanel panelAnterior = new JPanel(new GridLayout(1, 1));
    	// Boton anterior
		 int paginas = this.ventanas.size() - 1;
	     if (this.posicion == 0 || paginas == this.posicion)
        	anterior.setEnabled(false);
        else {
        	 anterior.setMnemonic(KeyEvent.VK_A); //Mnem�nico para el bot�n de anterior
        	anterior.setEnabled(true);
        }
        anterior.setText(Messages.getString("Wizard.anterior")); // NOI18N

        anterior.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                anteriorActionPerformed(anterior, siguiente, finalizar);
            }
        });
        Utils.remarcar(anterior);
        Utils.setContrastColor(anterior);
        Utils.setFontBold(anterior);
       
        panelAnterior.add(anterior);
        add(panelAnterior);
        
        JPanel panelSiguiente = new JPanel(new GridLayout(1, 1));
        // Boton siguiente
        if (this.ventanas.size() == 1 || paginas == this.posicion)
        	siguiente.setVisible(false);
        else {
        	siguiente.setMnemonic(KeyEvent.VK_S); //Mnem�nico para el bot�n de siguiente
        	siguiente.setVisible(true);
        }
        siguiente.setText(Messages.getString("Wizard.siguiente")); // NOI18N

        siguiente.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                siguienteActionPerformed(anterior, siguiente, finalizar);
            }
        });
        Utils.remarcar(siguiente);
        Utils.setContrastColor(siguiente);
        Utils.setFontBold(siguiente);
        
        panelSiguiente.add(siguiente);
        add(panelSiguiente);

        // Espacio entre botones
		panelVacio = new JPanel();
		panelVacio.setSize(new Dimension(20, 10));
		add(panelVacio);
        
		JPanel panelCancelar = new JPanel(new GridLayout(1, 1));
        // Boton cancelar
		if (paginas == this.posicion)
			cancelar.setVisible(false);
        else {
        	cancelar.setMnemonic(KeyEvent.VK_C); //Mnem�nico para el bot�n de cancelar
        	cancelar.setVisible(true);
        }
        cancelar.setText(Messages.getString("Wizard.cancelar")); // NOI18N
        cancelar.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
            	saveSizePosition();
            	for (JDialogWizard ventana : BotoneraInferior.this.ventanas)
            		ventana.dispose();
            }
        });
        Utils.remarcar(cancelar);
        Utils.setContrastColor(cancelar);
        Utils.setFontBold(cancelar);
        
        panelCancelar.add(cancelar);
        add(panelCancelar);

        JPanel panelFinalizar = new JPanel(new GridLayout(1, 1));
        // Boton finalizar
        if (this.ventanas.size() == 1 || paginas == this.posicion) {
            finalizar.setMnemonic(KeyEvent.VK_F); //Mnemonico para el boton de finalizar
            finalizar.setVisible(true);
        } else 
        	finalizar.setVisible(false);

        finalizar.setText(Messages.getString("Wizard.finalizar")); // NOI18N

        finalizar.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
            	saveSizePosition();
            	for (JDialogWizard ventana : BotoneraInferior.this.ventanas)
            		ventana.dispose();
            }
        });
        Utils.remarcar(finalizar);
        Utils.setContrastColor(finalizar);
        Utils.setFontBold(finalizar);
        
        panelFinalizar.add(finalizar);
        add(panelFinalizar);
	}

	/**
	 * Muestra el dialogo siguiente
	 * @param finalizar Boton finalizar
	 * @param siguiente Boton siguiente
	 * @param anterior 	Boton anterior
	 */
	protected void siguienteActionPerformed(JButton anterior, JButton siguiente, JButton finalizar) {
		int indice = this.posicion + 1;
		
		// Mantenemos el tamano y posicion de la ventana acutual en la ventana siguiente
		this.ventanas.get(this.posicion+1).setBounds(this.ventanas.get(this.posicion).getX(), this.ventanas.get(this.posicion).getY(), this.ventanas.get(this.posicion).getWidth(), this.ventanas.get(this.posicion).getHeight());
		
		this.ventanas.get(indice).setVisibleAndHide(true, this.ventanas.get(this.posicion));
	}

	/**
	 * Muestra el dialogo anterior
	 * @param finalizar Boton finalizar
	 * @param siguiente Boton siguiente
	 * @param anterior 	Boton anterior
	 */
	protected void anteriorActionPerformed(JButton anterior, JButton siguiente, 
			JButton finalizar) {
		// Nos movemos al indice anterior
		int indice = this.posicion - 1;
		
		// Mantenemos el tamano y posicion de la ventana actual en la ventana anterior
		this.ventanas.get(this.posicion-1).setBounds(this.ventanas.get(this.posicion).getX(), this.ventanas.get(this.posicion).getY(), this.ventanas.get(this.posicion).getWidth(), this.ventanas.get(this.posicion).getHeight());
		
		this.ventanas.get(indice).setVisibleAndHide(true, this.ventanas.get(this.posicion));
	}
	
	/**
	 * Cambia el tamaño de la ventana al tamaño maximo de pantalla menos el tamaño de la barra de tareas de windows
	 */
	public void maximizarActionPerformed(){
		JAccessibilityDialogWizard j = JAccessibilityDialogWizard.getJAccessibilityDialogWizard(this);

		JAccessibilityDialogWizard.actualPositionX = j.getX();
		JAccessibilityDialogWizard.actualPositionY = j.getY();
		JAccessibilityDialogWizard.actualWidth = j.getWidth();
		JAccessibilityDialogWizard.actualHeight = j.getHeight();
		
		//Se obtienen las dimensiones totales disponibles para mostrar una ventana
		Rectangle rect =  GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();

		//Se obtienen las dimensiones de maximizado
		int maxWidth = (int)rect.getWidth();
		int maxHeight = (int)rect.getHeight();
				
		//Se hace el resize
		j.setBounds(0,0, maxWidth, maxHeight);
				
		//Se deshabilita el botón de maximizar puesto que se ha pulsado.
		this.maximizar.setEnabled(false);
		this.restaurar.setEnabled(true);
	}
	
	/**
	 * Restaura el tamaño de la ventana a la posicion anterior al maximizado
	 */
	public void restaurarActionPerformed(){
		JAccessibilityDialogWizard j = JAccessibilityDialogWizard.getJAccessibilityDialogWizard(this);
		if (JAccessibilityDialogWizard.actualPositionX != -1 && JAccessibilityDialogWizard.actualPositionY != -1 && JAccessibilityDialogWizard.actualWidth != -1 && JAccessibilityDialogWizard.actualHeight != -1){
			j.setBounds(JAccessibilityDialogWizard.actualPositionX, JAccessibilityDialogWizard.actualPositionY, JAccessibilityDialogWizard.actualWidth, JAccessibilityDialogWizard.actualHeight);
		} else {
			Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
			if (Platform.getOS().equals(Platform.OS.LINUX)){
	            j.setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH_LINUX) / 2, (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT_LINUX) / 2, Constants.WIZARD_INITIAL_WIDTH_LINUX, Constants.WIZARD_INITIAL_HEIGHT_LINUX);
			} else{
	            j.setBounds((screenSize.width - Constants.WIZARD_INITIAL_WIDTH) / 2, (screenSize.height - Constants.WIZARD_INITIAL_HEIGHT) / 2, Constants.WIZARD_INITIAL_WIDTH, Constants.WIZARD_INITIAL_HEIGHT);
			}
    		j.setMinimumSize(new Dimension(j.getSize().width, j.getSize().height));
		}
		//Se deshabilita el botón de restaurar puesto que se ha pulsado.
		this.maximizar.setEnabled(true);
		this.restaurar.setEnabled(false);
	}
	
	/**
	 * Guarda el tamaño y posicion de la ventana antes de cerrarse
	 */
	public void saveSizePosition(){
		// Guardamos la posición y tamaño actual de la ventana sólo en caso de no estar maximizada por configuración
    	if (!GeneralConfig.isMaximized()){
			JAccessibilityDialogWizard j = JAccessibilityDialogWizard.getJAccessibilityDialogWizard(this);
			PrincipalGUI.wizardActualPositionX = j.getX();
			PrincipalGUI.wizardActualPositionY = j.getY();
			PrincipalGUI.wizardActualWidth = j.getWidth();
			PrincipalGUI.wizardActualHeight = j.getHeight();
    	}
	}
}

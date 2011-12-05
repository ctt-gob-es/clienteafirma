package es.gob.afirma.ui.wizardUtils;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JToolTip;

import es.gob.afirma.core.misc.Platform;
import es.gob.afirma.ui.principal.Main;
import es.gob.afirma.ui.principal.PrincipalGUI;
import es.gob.afirma.ui.utils.Constants;
import es.gob.afirma.ui.utils.CustomDialog;
import es.gob.afirma.ui.utils.GeneralConfig;
import es.gob.afirma.ui.utils.JAccessibilityDialogWizard;
import es.gob.afirma.ui.utils.Messages;
import es.gob.afirma.ui.utils.Utils;

public class BotoneraSuperior extends JPanel {
	private static final long serialVersionUID = 1L;
	private Dimension dimensiones = new Dimension(603, 70);
	private List<JDialogWizard> ventanas;
	private int posicion;
	private JButton siguiente = null;
	private JButton finalizar = null;
	/**
	 * Boton de restaurar.
	 */
	private JButton restoreButton = null;
	
	/**
	 * Boton de maximizar.
	 */
	private JButton maximizeButton = null;
	
	public List<JDialogWizard> getVentanas() {
		return this.ventanas;
	}
	
	/**
	 * Genera una botonera con la configuracion predefinida
	 * @param ventanas	Listado que contiene todas las ventanas en orden de aparicion
	 * @param posicion	Numero de la pagina
	 */
	public BotoneraSuperior(List<JDialogWizard> ventanas, Integer posicion) {
		this.ventanas = ventanas;
		this.posicion = posicion;
		initParamenters();
	}
	
	/**
	 * Genera una botonera con unas dimensiones dadas
	 * @param dimensiones	Dimensiones de la botonera
	 */
	public BotoneraSuperior(Dimension dimensiones) {
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
        setLayout(new FlowLayout(FlowLayout.RIGHT, 1, 1));
        setBackground(Color.WHITE);
        if (Main.isOSHighContrast){
        	setOpaque(false);
        }
        setBorder(null);
        
        createAccessibilityButtonsPanel();

//        // Definicion de botones
//        maximizar = new JButton();
//        final JButton anterior = new JButton();
//        siguiente = new JButton();
//        final JButton cancelar = new JButton();
//        finalizar = new JButton();
//        
//        JPanel panelMaximizar = new JPanel(new GridLayout(1, 1));
//        //Boton maximizar
//        maximizar.setText(Messages.getString("Wizard.maximizar"));
//        maximizar.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.maximizar") + ". " + Messages.getString("Wizard.maximizar.description"));
//        maximizar.setName("maximizar");
//        maximizar.setMnemonic(KeyEvent.VK_M);
//        maximizar.addActionListener(new ActionListener() {
//			public void actionPerformed(ActionEvent e) {
//				maximizarActionPerformed();
//			}
//		});
//        Utils.remarcar(maximizar);
//        Utils.setContrastColor(maximizar);
//        Utils.setFontBold(maximizar);
//        
//        panelMaximizar.add(maximizar);
//        add(panelMaximizar);
//        
//        JPanel panelRestaurar = new JPanel(new GridLayout(1, 1));
//	    // Boton restaurar
//	    restaurar = new JButton();
//	    restaurar.setText(Messages.getString("Wizard.restaurar"));
//	    restaurar.setName("restaurar");
//	    restaurar.getAccessibleContext().setAccessibleName(Messages.getString("Wizard.restaurar") + ". " + Messages.getString("Wizard.restaurar.description"));
//	    restaurar.setMnemonic(KeyEvent.VK_R);
//	    restaurar.addActionListener(new ActionListener() {
//	    	public void actionPerformed(ActionEvent e) {
//	    		restaurarActionPerformed();
//			}
//		});
//	    Utils.remarcar(restaurar);
//        Utils.setContrastColor(restaurar);
//	    Utils.setFontBold(restaurar);
//	    
//	    panelRestaurar.add(restaurar);
//	    add(panelRestaurar);
//	    
//	    //Control de habilitado de los botones de maximizar y restaurar según la configuración de
//	    //accesibilidad
//	    if (GeneralConfig.isMaximized()){
//        	maximizar.setEnabled(false);
//        } else {
//        	restaurar.setEnabled(false);
//        }
//        
//        //Espacio entre botones
//		JPanel panelVacio = new JPanel();
//		panelVacio.setPreferredSize(new Dimension(60, 10));
//		add(panelVacio);		
//		
//		JPanel panelAnterior = new JPanel(new GridLayout(1, 1));
//    	// Boton anterior
//		 int paginas = this.ventanas.size() - 1;
//	     if (this.posicion == 0 || paginas == this.posicion)
//        	anterior.setEnabled(false);
//        else {
//        	 anterior.setMnemonic(KeyEvent.VK_A); //Mnem�nico para el bot�n de anterior
//        	anterior.setEnabled(true);
//        }
//        anterior.setText(Messages.getString("Wizard.anterior")); // NOI18N
//
//        anterior.addActionListener(new ActionListener() {
//            public void actionPerformed(ActionEvent evt) {
//                anteriorActionPerformed(anterior, siguiente, finalizar);
//            }
//        });
//        Utils.remarcar(anterior);
//        Utils.setContrastColor(anterior);
//        Utils.setFontBold(anterior);
//       
//        panelAnterior.add(anterior);
//        add(panelAnterior);
//        
//        JPanel panelSiguiente = new JPanel(new GridLayout(1, 1));
//        // Boton siguiente
//        if (this.ventanas.size() == 1 || paginas == this.posicion)
//        	siguiente.setVisible(false);
//        else {
//        	siguiente.setMnemonic(KeyEvent.VK_S); //Mnem�nico para el bot�n de siguiente
//        	siguiente.setVisible(true);
//        }
//        siguiente.setText(Messages.getString("Wizard.siguiente")); // NOI18N
//        
//        siguiente.addActionListener(new ActionListener() {
//            public void actionPerformed(ActionEvent evt) {
//                siguienteActionPerformed(anterior, siguiente, finalizar);
//            }
//        });
//        Utils.remarcar(siguiente);
//        Utils.setContrastColor(siguiente);
//        Utils.setFontBold(siguiente);
//        
//        panelSiguiente.add(siguiente);
//        add(panelSiguiente);
//
//        // Espacio entre botones
//		panelVacio = new JPanel();
//		panelVacio.setSize(new Dimension(20, 10));
//		add(panelVacio);
//        
//		JPanel panelCancelar = new JPanel(new GridLayout(1, 1));
//        // Boton cancelar
//		if (paginas == this.posicion)
//			cancelar.setVisible(false);
//        else {
//        	cancelar.setMnemonic(KeyEvent.VK_C); //Mnem�nico para el bot�n de cancelar
//        	cancelar.setVisible(true);
//        }
//        cancelar.setText(Messages.getString("Wizard.cancelar")); // NOI18N
//        cancelar.addActionListener(new ActionListener() {
//            public void actionPerformed(ActionEvent evt) {
//            	saveSizePosition();
//            	for (JDialogWizard ventana : BotoneraSuperior.this.ventanas)
//            		ventana.dispose();
//            }
//        });
//        Utils.remarcar(cancelar);
//        Utils.setContrastColor(cancelar);
//        Utils.setFontBold(cancelar);
//        
//        panelCancelar.add(cancelar);
//        add(panelCancelar);
//
//        JPanel panelFinalizar = new JPanel(new GridLayout(1, 1));
//        // Boton finalizar
//        if (this.ventanas.size() == 1 || paginas == this.posicion) {
//            finalizar.setMnemonic(KeyEvent.VK_F); //Mnemonico para el boton de finalizar
//            finalizar.setVisible(true);
//        } else 
//        	finalizar.setVisible(false);
//
//        finalizar.setText(Messages.getString("Wizard.finalizar")); // NOI18N
//
//        finalizar.addActionListener(new ActionListener() {
//            public void actionPerformed(ActionEvent evt) {
//            	saveSizePosition();
//            	for (JDialogWizard ventana : BotoneraSuperior.this.ventanas)
//            		ventana.dispose();
//            }
//        });
//        Utils.remarcar(finalizar);
//        Utils.setContrastColor(finalizar);
//        Utils.setFontBold(finalizar);
//        
//        panelFinalizar.add(finalizar);
//        add(panelFinalizar);
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
		
		//Se asigna un botón por defecto al wizard
		if (this.ventanas.get(indice) instanceof JAccessibilityDialogWizard) {
			//Se obtiene el diálogo
			JAccessibilityDialogWizard wizard = (JAccessibilityDialogWizard)this.ventanas.get(indice);
			//Se comprueba si estamos en la última ventana del wizard
			if (indice < this.ventanas.size()-1) 
				wizard.getRootPane().setDefaultButton(wizard.getBotonera().getSiguiente());
			else
				wizard.getRootPane().setDefaultButton(wizard.getBotonera().getFinalizar());
		}
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
		
		//Se asigna un botón por defecto al wizard
		if (this.ventanas.get(indice) instanceof JAccessibilityDialogWizard) {
			this.ventanas.get(indice).getRootPane().setDefaultButton(((JAccessibilityDialogWizard)this.ventanas.get(indice)).getBotonera().getSiguiente());
		}
		
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
				
		//Se hace el resize dependiendo del so
		if (!Platform.getOS().equals(Platform.OS.LINUX)){
			j.setBounds(0,0, maxWidth, maxHeight);
		} else {
			j.setBounds(0,0, maxWidth, maxHeight - Constants.maximizeVerticalMarginLinux);
		}
				
		//Se deshabilita el botón de maximizar puesto que se ha pulsado.
		this.maximizeButton.setEnabled(false);
		this.restoreButton.setEnabled(true);
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
		this.maximizeButton.setEnabled(true);
		this.restoreButton.setEnabled(false);
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

	/**
	 * Devuelve el boton de siguiente.
	 * @return boton de siguiente.
	 */
	public JButton getSiguiente() {
		return siguiente;
	}
	
	/**
	 * Devuelve el boton de finalizar.
	 * @return boton de finalizar.
	 */
	public JButton getFinalizar() {
		return finalizar;
	}

	/**
	 * Se crea el panel de botones de accesibilidad.
	 */
	private void createAccessibilityButtonsPanel() {
		//this.accessibilityButtonsPanel = new JPanel(new GridBagLayout());
		
		//Panel que va a contener los botones de accesibilidad
		JPanel panel = new JPanel(new GridBagLayout());
		panel.setBackground(Color.WHITE);
		if (Main.isOSHighContrast){
        	panel.setOpaque(false);
        }
		Utils.setContrastColor(panel);
		
		//panel.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
		//panel.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
		//panel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.RAISED));
		//panel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
		//panel.setBorder(BorderFactory.createLineBorder(Color.BLACK));
		//panel.setBorder(BorderFactory.createCompoundBorder());
		//panel.setBorder(BorderFactory.createMatteBorder(1, 1, 1, 1, Color.BLACK));
		
		//Restricciones para los botones
		GridBagConstraints consButtons = new GridBagConstraints();
		consButtons.fill = GridBagConstraints.BOTH;
		consButtons.gridx = 0;
		consButtons.gridy = 0;
		consButtons.weightx = 1.0;
		consButtons.weighty = 1.0;
		consButtons.insets = new Insets(0,0,0,0);  //right padding
		//consButtons.anchor=GridBagConstraints.EAST;
		
		//Restore button
		JPanel restorePanel = new JPanel();
		//this.restoreButton = getButton("r", KeyEvent.VK_R );
		ImageIcon imageIconRestore= new ImageIcon(CustomDialog.class.getResource("/resources/images/restore.png"));
		this.restoreButton = new JButton(imageIconRestore);
		this.restoreButton.setMnemonic(KeyEvent.VK_R );
		this.restoreButton.setToolTipText(Messages.getString("Wizard.restaurar.description"));
		this.restoreButton.getAccessibleContext().setAccessibleName(this.restoreButton.getToolTipText());
		
		
		Dimension dimension = new Dimension(20,20);
		this.restoreButton.setPreferredSize(dimension);
		
		//this.restoreButton.setBorder(null); //Eliminar Borde, ayuda a centrar el iconod el boton
		//this.restoreButton.setContentAreaFilled(false); //area del boton invisible
		this.restoreButton.setName("restaurar");
		Utils.remarcar(this.restoreButton);
		restorePanel.setBackground(Color.WHITE);
		if (Main.isOSHighContrast){
        	restorePanel.setOpaque(false);
        }
		Utils.setContrastColor(restorePanel);
		restorePanel.add(this.restoreButton);
		this.restoreButton.addActionListener(new ActionListener() {
	    	public void actionPerformed(ActionEvent e) {
	    		restaurarActionPerformed();
			}
		});
		
		
		panel.add(restorePanel, consButtons);
		
		
		consButtons.gridx = 1;
		//consButtons.weightx = 0.5;
		consButtons.insets = new Insets(0,0,0,0);  //right padding
		
		//Maximize button
		JPanel maximizePanel = new JPanel();

		ImageIcon imageIconMaximize= new ImageIcon(CustomDialog.class.getResource("/resources/images/maximize.png"));
		this.maximizeButton = new JButton(imageIconMaximize);
		this.maximizeButton.setMnemonic(KeyEvent.VK_M );
		this.maximizeButton.setToolTipText(Messages.getString("Wizard.maximizar.description"));
		this.maximizeButton.getAccessibleContext().setAccessibleName(this.maximizeButton.getToolTipText());

		//this.maximizeButton.setBorder(null); //Eliminar Borde, ayuda a centrar el iconod el boton
		//this.maximizeButton.setContentAreaFilled(false); //area del boton invisible
		
		this.maximizeButton.setName("maximizar");
		//Se asigna una dimension por defecto
		this.maximizeButton.setPreferredSize(dimension);
				
		Utils.remarcar(this.maximizeButton);
		//maximizePanel.add(this.maximizeButton, consMaximizePanel);
		maximizePanel.setBackground(Color.WHITE);
		if (Main.isOSHighContrast){
        	maximizePanel.setOpaque(false);
        }
		Utils.setContrastColor(maximizePanel);
		maximizePanel.add(this.maximizeButton);
		
		JToolTip tooltip = maximizeButton.createToolTip();
		tooltip.setTipText(Messages.getString("Wizard.maximizar"));
		tooltip.setVisible(true);
		
		
		/*this.maximizeButton.addFocusListener(new FocusListener() {
			public void focusLost(FocusEvent e) {
				
				ToolTipManager.sharedInstance().registerComponent(this);
				ToolTipManager.sharedInstance().setInitialDelay(0) ;
			}
			public void focusGained(FocusEvent e) {
				//Se muestra un borde en el botón cuando este tiene el foco
				botonAyuda.setBorder(BorderFactory.createLineBorder(Color.ORANGE, 1));
			}
		});*/
		
		this.maximizeButton.addActionListener(new ActionListener() {
		    	public void actionPerformed(ActionEvent e) {
		    		maximizarActionPerformed();
				}
			});

		
		panel.add(maximizePanel, consButtons);

		//Se añade al panel general
		//Restricciones para el panel de botones
		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.NONE;
		c.gridx = 0;
		c.gridy = 0;
		c.weightx = 1.0;
		c.weighty = 1.0;
		//c.insets = new Insets(3,3,0,3);
		c.insets = new Insets(0,0,0,0); 
		c.anchor=GridBagConstraints.EAST;
		this.add(panel, c);
		
		
		// Habilitado/Deshabilitado de botones restaurar/maximizar
    	if (GeneralConfig.isMaximized()){
    		//Se deshabilita el botón de maximizado
    		this.maximizeButton.setEnabled(false);
    		//Se habilita el botón de restaurar
    		this.restoreButton.setEnabled(true);
    	} else {
    		//Se habilita el botón de maximizado
    		this.maximizeButton.setEnabled(true);
    		//Se deshabilita el botón de restaurar
    		this.restoreButton.setEnabled(false);
    	}
		
	}
}
